;2016/09/12 The idea here is to prep the FFT chunks BEFORE doing the E-over-B test, so that we can calc the timestamps, frequency
;limits and bandpass filter BEFORE calculating the Poynting flux
PRO ALFVEN_STATS_6_SPECTRAL__METHODE_DEUX, $
   FILENAME=filename, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   ANALYSE_NOISE=analyse_noise, $
   T1=t1, $
   T2=t2, $
   FILTERFREQ=filterfreq, $
   BURST=burst, $
   HEAVY=heavy, $
   UCLA_MAG_DESPIN=ucla_mag_despin, $
   KEEP_ALFVEN_ONLY=keep_alfven_only, $
   PNG_SUMPLOT=png_sumplot, $
   PNG_OUREVENTS=png_ourevents, $
   DONTSHOWPLOTS=dontShowPlots, $
   CONT_IF_FILE_EXISTS=cont_if_file_exists

  ;; COMPILE_OPT idl2
  ;; COMPILE_OPT strictArr

  IF N_ELEMENTS(ucla_mag_despin)     EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval)  EQ 0 THEN below_auroral_oval  = 1
  outDir = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'

  IF KEYWORD_SET(png_sumplot) THEN BEGIN
     SET_PLOT_DIR,outPlotDir,/FOR_SDT,ADD_SUFF='/as6_spectral/'
  ENDIF

  IF N_ELEMENTS(only_128Ss_data)     EQ 0 THEN only_128Ss_data     = 1
  IF N_ELEMENTS(yLim_to_mag_rolloff) EQ 0 THEN yLim_to_mag_rolloff = 1
  yLimUpper         = 23

  ;;For FFTs of Mag and E data
  FFTLen            = 1024
  nFFTAvg           = 1

  maxPeriod         = 1/100.
  minPeriod         = 1/132.
  eFSampFact        = 4         ;The fact is, E-field data gets sampled a million times faster!
  FFTFreqRange      = [0.1,19.0]
  FFTSlide          = 0.0

  current_threshold = 1.0       ;microA/m^2
  delta_b_threshold = 5.0       ; nT
  delta_E_threshold = 10.0      ; mV/m
  esa_j_delta_bj_ratio_threshold = 0.02
  electron_eflux_ionos_threshold = 0.05 ;ergs/cm^2/s
  eb_to_alfven_speed = 10.0             ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
                                ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  ;;energy ranges
  IF not keyword_set(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  IF not keyword_set(energy_ions) THEN energy_ions = [0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; IF no data exists, return to main
  t = 0
  dat = get_fa_ees(t,/st)
  IF dat.valid eq 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
;     return
  ENDIF

  ;; Electron current - line plot
  IF keyword_set(burst) THEN BEGIN
     get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,NAME='Je',ENERGY=energy_electrons
  ENDIF ELSE BEGIN
     get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,NAME='Je',ENERGY=energy_electrons
  endelse
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep = WHERE(FINITE(tmpj.y) NE 0)
  tmpj.x = tmpj.x[keep]
  tmpj.y = tmpj.y[keep]
  
  keep = WHERE(ABS(tmpj.y) GT 0.0)
  tx = tmpj.x[keep]
  ty = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order = sort(tx)
  tx = tx[time_order]
  ty = ty[time_order]
  
  
  ;;throw away the first 10  points since they are often corrupted
  IF not keyword_set(burst) THEN BEGIN
     STORE_DATA,'Je',DATA={x:tx[10:N_ELEMENTS(tx)-1],y:ty[10:N_ELEMENTS(tx)-1]}
  ENDIF ELSE BEGIN
     STORE_DATA,'Je',DATA={x:tx,y:ty}
  endelse
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  GET_DATA,'Je',DATA=je
  GET_FA_ORBIT,/TIME_ARRAY,je.x
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  IF KEYWORD_SET(below_auroral_oval) THEN BEGIN
     keep = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr='--below_aur_oval'
  ENDIF ELSE BEGIN
     keep = WHERE(ABS(ilat.y) GT AURORAL_ZONE(mlt.y,7,/lat)/(!DPI)*180.)
     belowAurOvalStr=''
  ENDELSE

  STORE_DATA,'Je',DATA={x:je.x[keep],y:je.y[keep]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je = MAKE_ARRAY(N_ELEMENTS(Je.x),/double)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j] = ABS(Je.x[j]-Je.x[j-1])
  endfor
  part_res_Je[0] = part_res_Je[1]
  gap = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start = [0,WHERE(part_res_je GT 10.0)]
     separate_stop = [WHERE(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start = [0]
     separate_stop = [N_ELEMENTS(Je.x)-1]
  endelse
  
  ;;remove esa burp when switched on
  IF not keyword_set(burst) THEN BEGIN
     turn_on = WHERE(part_res_je GT 300.0)
     IF turn_on[0] NE -1 THEN BEGIN
        turn_on_separate = MAKE_ARRAY(N_ELEMENTS(turn_on),/double)
        FOR j=0,N_ELEMENTS(turn_on)-1 DO turn_on_separate[j] = WHERE(separate_start EQ turn_on[j])
        separate_start[turn_on_separate+1] = separate_start[turn_on_separate+1]+5
     ENDIF
  ENDIF

  ;;identify time indices for each interval
  count = 0.0
  FOR j=0,N_ELEMENTS(separate_start)-1 DO BEGIN
     IF (separate_stop[j]-separate_start[j]) GT 10 THEN BEGIN
        count = count+1
        IF count EQ 1.0 THEN BEGIN
           time_range_indices = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        ENDIF ELSE BEGIN
           time_range_indices = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        endelse
     ENDIF
  endfor
  
  ;;identify interval times
  time_ranges          = je.x[time_range_indices]
  number_of_intervals  = N_ELEMENTS(time_ranges[*,0])
  
  print,'number_of_intervals',number_of_intervals
  
  ji_tot               = MAKE_ARRAY(number_of_intervals,/double)
  ji_up_tot            = MAKE_ARRAY(number_of_intervals,/double)
  jee_tot              = MAKE_ARRAY(number_of_intervals,/double)
  Ji_tot_alf           = MAKE_ARRAY(number_of_intervals,/double)
  Ji_up_tot_alf        = MAKE_ARRAY(number_of_intervals,/double)
  Jee_tot_alf          = MAKE_ARRAY(number_of_intervals,/double)
  
  ;;get despun mag data IF keyword set
  IF keyword_set(ucla_mag_despin) THEN ucla_mag_despin
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN
     print,'time_range',TIME_TO_STR(time_ranges[jjj,0]),TIME_TO_STR(time_ranges[jjj,1])
     
     ;;get orbit number for filenames		
     GET_DATA,'ORBIT',DATA=tmp
     orbit = tmp.y[0]
     orbit_num = STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)

                                ;filename for output file
     IF KEYWORD_SET(burst) THEN BEGIN
        curfile = outDir + 'batch_output__burst/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr + '--burst'
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
           curfile = outDir + 'batch_output/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr + '--ucla_mag_despin'
        ENDIF ELSE BEGIN
           curfile = outDir + 'batch_output/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr
        ENDELSE
     ENDELSE
     
     ;;make sure we're not overwriting
     IF FILE_TEST(curfile) THEN BEGIN
        IF NOT KEYWORD_SET(cont_if_file_exists) THEN BEGIN
           right_now = strmid(timestamp(),0,13)
           curfile = curfile + "--" + right_now
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(cont_if_file_exists) THEN BEGIN
              PRINT,"Not overwriting file " + curfile + "! Returning..."
              RETURN
           ENDIF
        ENDELSE
     ENDIF
     
     je_tmp_time = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     
     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}
     
     ;;get fields quantities
     ;; data_valid = 1.0
     ;; dat = get_fa_fields('MagDC',t,/start)
     ;; IF dat.valid eq 0 THEN BEGIN
     ;;    print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     ;;    data_valid = 0.0
     ;; ENDIF ELSE BEGIN
     ;;    IF not keyword_set(ucla_mag_despin) THEN field=get_fa_fields('MagDC',time_ranges[jjj,0],time_ranges[jjj,1],/store)
     ;;    dat = get_fa_fields('V5-V8_S',t,/start)
     ;;    IF dat.valid eq 0 THEN BEGIN
     ;;       print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
     ;;       data_valid = 0.0
     ;;    ENDIF ELSE BEGIN
     ;;       spacecraft_potential = get_fa_fields('V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;       efieldV58 = get_fa_fields('V5-V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;       efieldV1214 = get_fa_fields('V1-V2_S',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;       IF efieldV1214.valid eq 0 THEN BEGIN
     ;;          print,'No V1-V2_S data - trying V1-V4_S'
     ;;          efieldV1214 = get_fa_fields('V1-V4_S',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;          IF efieldV1214.valid eq 0 AND KEYWORD_SET(burst) THEN BEGIN
     ;;             print,'No V1-V4_S data - trying V1-V2_4k (burst)'
     ;;             efieldV1214 = get_fa_fields('V1-V2_4k',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;             IF efieldV1214.valid eq 0 THEN BEGIN
     ;;                print,'No V1-V2_4k data - trying V1-V4_4k (burst)'
     ;;                efieldV1214 = get_fa_fields('V1-V4_4k',time_ranges[jjj,0],time_ranges[jjj,1])
     ;;                IF efieldV1214.valid eq 0 THEN BEGIN
     ;;                   print,'No FAST fields data-get_fa_fields returned invalid data'
     ;;                   data_valid = 0.0
     ;;                ENDIF
     ;;             ENDIF
     ;;          ENDIF ELSE BEGIN
     ;;             print,'No FAST fields data-get_fa_fields returned invalid data'
     ;;             data_valid = 0.0
     ;;          endelse
     ;;       ENDIF 
     ;;    endelse

     ;; endelse
     
     dat = get_fa_fields('MagDC',t,/start)
     IF dat.valid EQ 0 THEN BEGIN
        PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     ENDIF 
     
     ;;Get two types of potential. Which DO we like better?
     spacecraft_potential = GET_FA_FIELDS('V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     sc_pot2              = GET_FA_POTENTIAL(time_ranges[jjj,0],time_ranges[jjj,1])

     ;; IF data_valid NE 0.0 THEN BEGIN
     IF (sc_pot2.valid) AND (dat.valid) THEN BEGIN
        
        ;;get E field and B field on same time scale
        ;; efields_combine=combinets({x:efieldV1214.time,y:efieldV1214.comp1},{x:efieldV58.time,y:efieldV58.comp1})
        ;; FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
        
        ;;get magnitude of electric and magnetic field
        ;; FOR k=0,10,1 DO BEGIN
        ;;    print, "This is efieldV1214.comp1["+STRING(k)+"]: " + STRING(efieldV1214.comp1[k])
        ;;    print, "This is efieldV58.comp1["+STRING(k)+"]: " + STRING(efieldV58.comp1[k])
        ;;    print, "This is efields_combine["+STRING(k)+"]: " + STRING(efields_combine[k])
        ;; endfor
        ;; help, efieldV1214,/str
        ;; help, efieldV58,/str
        ;; help,efields_combine
        ;; efield={x:efieldV1214.time,y:SQRT(efieldV1214.comp1^2+efields_combine^2)}


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get Mag and E field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;	E component: “E_along_V”
        ;;	B components: Two components perpendicular to E_along_V
        ;;		a. Use Strangeway despinning, find coords that complement E_along_V
        ;;		b. Need to use Bob model to subtract the background field

        IF not keyword_set(ucla_mag_despin) THEN BEGIN
           GET_DATA,'MagDCcomp1',DATA=magx
           GET_DATA,'MagDCcomp2',DATA=magy
           GET_DATA,'MagDCcomp3',DATA=magz
        ENDIF ELSE BEGIN
           GET_DATA,'dB_fac_v',DATA=db_fac
           mintime = MIN(ABS(time_ranges[jjj,0]-db_fac.x),ind1)
           mintime = MIN(ABS(time_ranges[jjj,1]-db_fac.x),ind2)
                                ;   From UCLA_MAG_DESPIN:
           magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} ;   "Field-aligned velocity-based coordinates defined as:    "
           magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} ;   "z (ind 2)-along B, y (ind 1)-cross track (BxV), x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)
           magz = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}
        endelse
        
        ;;E field
        FA_FIELDS_DESPIN,efieldV58,efieldV1214,/SLOW
        ;; GET_DATA,'E_NEAR_B',DATA=eNearB
        GET_DATA,'E_ALONG_V',DATA=eAlongV
        IF SIZE(eAlongV,/TYPE) NE 8 THEN BEGIN
           PRINT,"Couldn't get E_ALONG_V!" 
           STOP
        ENDIF
        ;; OPTIONS,'E_ALONG_V','ytitle','E Along V!C(mV/m)'
        ;; YLIM,'E_ALONG_V',-1000,1000

        ;;Now check sorted/dupes
        CHECK_DUPES,magz.x,HAS_DUPES=magHasDupes, $
                    IS_SORTED=magIsSort,OUT_UNIQ_I=magUniq_i,/QUIET
        IF magHasDupes OR ~magIsSort THEN BEGIN
           PRINT,'Mag has dupes/is not sorted! Sorting ...'
           magz = {x:magz.x[magUniq_i],y:magz.y[magUniq_i]}
        ENDIF

        CHECK_DUPES,eAlongV.x,HAS_DUPES=eAVHasDupes, $
                    IS_SORTED=eAVIsSort,OUT_UNIQ_I=eAVUniq_i,/QUIET
        IF eAVHasDupes OR ~eAVIsSort THEN BEGIN
           PRINT,'EAV has dupes/is not sorted! Sorting ...'
           eAlongV = {x:eAlongV.x[eAVUniq_i],y:eAlongV.y[eAVUniq_i]}
        ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;ii. Restrict to periods with 128 S/s 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        magPeriod        = magz.x
        magPeriod[*]     = 0
        magPeriod[0:-2]  = (SHIFT(magz.x,-1)-magz.x)[0:-2]
        magPeriod[-1]    = magPeriod[-2]
        bigGaps          = WHERE(magPeriod GT 10,nBigGaps)
        IF nBigGaps GT 0 THEN BEGIN
           PRINT,'Big gaps magz! Decide!'
        ENDIF
        CASE 1 OF
           KEYWORD_SET(only_128Ss_data): BEGIN
              keepMag_i  = WHERE(magPeriod LE maxPeriod AND $
                                       magPeriod GE minPeriod,nBigGaps)
           END
           ELSE: BEGIN
              keepMag_i  = WHERE(magPeriod LE maxPeriod,nBigGaps)

              ;;Get data with appropriate sample rates
              CHECK_DUPES,magPeriod,HAS_DUPES=magPeriodDupes, $
                          IS_SORTED=magPeriodIsSort,OUT_UNIQ_I=magPeriodUniq_i,/QUIET
              
              PRINT,'You still need to handle this ... someday'

           END
        ENDCASE
        GET_STREAKS,keepMag_i,START_I=strtMag_ii,STOP_I=stpMag_ii, $
                    OUT_STREAKLENS=magStreakLens, $
                    MIN_STREAK_TO_KEEP=FFTLen
        
        eAVPeriod        = eAlongV.x
        eAVPeriod[*]     = 0
        eAVPeriod[0:-2]  = (SHIFT(eAlongV.x,-1)-eAlongV.x)[0:-2]
        eAVPeriod[-1]    = eAVPeriod[-2]
        bigGaps          = WHERE(eAVPeriod GT 10,nBigGaps)
        IF nBigGaps GT 0 THEN BEGIN
           PRINT,'Big gaps eAlongV! Decide!'
        ENDIF
        CASE 1 OF
           KEYWORD_SET(only_128Ss_data): BEGIN
              keepeAV_i  = WHERE(eAVPeriod LE (maxPeriod/eFSampFact) AND $
                                       eAVPeriod GE (minPeriod/eFSampFact),nBigGaps)
           END
           ELSE: BEGIN
              keepeAV_i  = WHERE(eAVPeriod LE maxPeriod,nBigGaps)

              ;;Get data with appropriate sample rates
              CHECK_DUPES,eAVPeriod,HAS_DUPES=eAVPeriodDupes, $
                          IS_SORTED=eAVPeriodIsSort,OUT_UNIQ_I=eAVPeriodUniq_i,/QUIET
              
           END
        ENDCASE
        GET_STREAKS,keepeAV_i,START_I=strteAV_ii,STOP_I=stpeAV_ii, $
                    OUT_STREAKLENS=eAVStreakLens, $
                    MIN_STREAK_TO_KEEP=FFTLen
        
        ;;Loop over streaks, smooth with Hanning window
        nMagSmooth  = TOTAL(stpMag_ii-strtMag_ii)
        regMag      = MAKE_ARRAY(nMagSmooth,VALUE=0.0)
        smoothMag   = MAKE_ARRAY(nMagSmooth,VALUE=0.0)
        smoothMagT  = MAKE_ARRAY(nMagSmooth,VALUE=0.0,/DOUBLE)
        gotEm       = 0
        FOR k=0,N_ELEMENTS(strtMag_ii)-1 DO BEGIN
           
           tmpInds              = gotEm + [strtMag_ii[k]:stpMag_ii[k]] - strtMag_ii[k]
           tmpData              = magz.y[keepMag_i[strtMag_ii[k]:stpMag_ii[k]]]

           regMag[tmpInds]      = tmpData
           smoothMag[tmpInds]   = FF_SMOOTH(tmpData,FFTLen-1, $
                                          SM_FUNCT=HANNING(FFTLen-1) ) ;, $
                                          ;; /DETREND)
           smoothMagT[tmpInds]  = magz.x[keepMag_i[strtMag_ii[k]:stpMag_ii[k]]]

           gotEm               += (stpMag_ii[k]-strtMag_ii[k])
        ENDFOR

        neAVSmooth  = TOTAL(stpeAV_ii-strteAV_ii)
        regeAV      = MAKE_ARRAY(neAVSmooth,VALUE=0.0)
        smootheAV   = MAKE_ARRAY(neAVSmooth,VALUE=0.0)
        smootheAVT  = MAKE_ARRAY(neAVSmooth,VALUE=0.0,/DOUBLE)
        gotEm       = 0
        FOR k=0,N_ELEMENTS(strteAV_ii)-1 DO BEGIN
           
           tmpInds              = gotEm + [strteAV_ii[k]:stpeAV_ii[k]] - strteAV_ii[k]
           tmpData              = eAlongV.y[keepeAV_i[strteAV_ii[k]:stpeAV_ii[k]]]

           regeAV[tmpInds]      = tmpData
           smootheAV[tmpInds]   = FF_SMOOTH(tmpData,FFTLen-1, $
                                          SM_FUNCT=HANNING(FFTLen-1) ) ;, $
                                          ;; /DETREND)
           smootheAVT[tmpInds]  = eAlongV.x[keepeAV_i[strteAV_ii[k]:stpeAV_ii[k]]]

           gotEm               += (stpeAV_ii[k]-strteAV_ii[k])
        ENDFOR


        ;;Get data with the right stuff
        ;; magTmp  = {x:magz.x[keepMag_i], $
        ;;            y:magz.y[keepMag_i]}
        ;; eAVTmp  = {x:eAlongV.x[keepeAV_i], $
        ;;            y:eAlongV.y[keepeAV_i]}
        magTmp  = {x:smoothMagT, $
                   y:regMag}
        eAVTmp  = {x:smootheAVT, $
                   y:regeAV}

        magSm   = {x:smoothMagT, $
                   y:smoothMag}
        eAVSm   = {x:smootheAVT, $
                   y:smootheAV}
        

        
        ;; magzTmp = {TIME:        magz.x[keepMag_i], $
        ;;            COMP1:       magz.y[keepMag_i], $
        magzTmp = {TIME:        magTmp.x, $
                   COMP1:       magTmp.y, $
        ;; magzTmp            = {TIME         : magz.x                , $
        ;;                       COMP1        : magz.y                , $
                              NCOMP        : 1                     , $
                              DATA_NAME    : 'Cross-track MagData' , $
                              VALID        : 1                     , $
                              PROJECT_NAME : 'FAST'                , $
                              UNITS_NAME   : 'nT'                  , $
                              CALIBRATED   : 1}
        ;; magzFilt           = {TIME         : magz.x                , $
        ;;                       COMP1        : magz.y                , $
        ;; magzFilt           = {TIME         : magSm.x                , $
        ;;                       COMP1        : magSm.y                , $
        magzFilt           = {TIME         : magTmp.x                , $
                              COMP1        : magTmp.y                , $
                              NCOMP        : 1                     , $
                              DATA_NAME    : 'Cross-track MagData' , $
                              VALID        : 1                     , $
                              PROJECT_NAME : 'FAST'                , $
                              UNITS_NAME   : 'nT'                  , $
                              CALIBRATED   : 1}

        FA_FIELDS_FILTER,magzFilt,FFTFreqRange

        ;; eAlongVTmp      = {TIME      :  eAlongV.x[keepeAV_i], $
        ;;                    COMP1     :  eAlongV.y[keepeAV_i], $
        eAlongVTmp      = {TIME      :  eAVTmp.x, $
                           COMP1     :  eAVTmp.y, $
        ;; eAlongVTmp         = {TIME         : eAlongV.x  , $
        ;;                       COMP1        : eAlongV.y  , $
                              NCOMP        : 1          , $
                              VALID        : 1          , $
                              DATA_NAME    :'E Along V' , $
                              PROJECT_NAME : 'FAST'     , $
                              UNITS_NAME   : 'mV/m'     , $
                              CALIBRATED   : 1}
        ;; eAlongVFilt        = {TIME         : eAlongV.x  , $
        ;;                       COMP1        : eAlongV.y  , $
        ;; eAlongVFilt        = {TIME         : eAVSm.x  , $
        ;;                       COMP1        : eAVSm.y  , $
        eAlongVFilt        = {TIME         : eAVTmp.x  , $
                              COMP1        : eAVTmp.y  , $
                              NCOMP        : 1          , $
                              VALID        : 1          , $
                              DATA_NAME    :'E Along V' , $
                              PROJECT_NAME : 'FAST'     , $
                              UNITS_NAME   : 'mV/m'     , $
                              CALIBRATED   : 1}
        
        FA_FIELDS_FILTER,eAlongVFilt,FFTFreqRange
        FA_FIELDS_COMBINE,magzTmp,eAlongVTmp, $
                          RESULT=eAlongVInterp, $
                          /INTERP, $
                          DELT_T=minPeriod, $
                          /TALK
        FA_FIELDS_COMBINE,magzFilt,eAlongVFilt, $
                          RESULT=eAlongVFiltInterp, $
                          /INTERP, $
                          DELT_T=minPeriod, $
                          /TALK

        eAlongVInterp      = {TIME         : magzTmp.time   , $
                              COMP1        : eAlongVInterp  , $
                              NCOMP        : 1              , $
                              DATA_NAME    : 'eAVInterp', $
                              VALID        :	1              , $
                              PROJECT_NAME :	'FAST'         , $
                              UNITS_NAME   :	'mV/m'         , $
                              CALIBRATED   :	1}
        eAlongVFiltInterp  = {TIME         : magzTmp.time   , $
                              COMP1        : eAlongVFiltInterp  , $
                              NCOMP        : 1              , $
                              DATA_NAME    : 'eAVInterpFilt', $
                              VALID        :	1              , $
                              PROJECT_NAME :	'FAST'         , $
                              UNITS_NAME   :	'mV/m'         , $
                              CALIBRATED   :	1}

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;iii. Fourier transform/Hanning window for E and B-field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;	1. Fourier details
        ;;		a. Decide length of Fourier transform
        ;;		b. High frequency set by roll-off of fluxgate mag.
        ;;		c. 2pi lambda_e sets low-frequency end
        ;;			i. Use density model already in place for calculating omega_p

        ;;Transform!
        spec = FA_FIELDS_SPEC(magzTmp, $
                              /STORE, $
                              T_NAME='MagSpec', $
                              STRUCTURE=magSpec, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(eAlongVTmp, $
                              /STORE, $
                              T_NAME='EAVSpec', $
                              STRUCTURE=eAVSpec, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(magzFilt, $
                              /STORE, $
                              T_NAME='MagSpecFilt', $
                              STRUCTURE=magSpecFilt, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(eAlongVFilt, $
                              /STORE, $
                              T_NAME='EAVSpecFilt', $
                              STRUCTURE=eAVSpecFilt, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(eAlongVFiltInterp, $
                              /STORE, $
                              T_NAME='EAVSpecFiltInterp', $
                              STRUCTURE=eAVSpecFiltInterp, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)

        ;;Make some adjustments to the data
        GET_DATA,'MagSpec',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'MagSpec',DATA=tmp
        OPTIONS,'MagSpec','ytitle','Frequency (Hz)'
        OPTIONS,'MagSpec','zTitle',magSpec.units_name
        ZLIM,'MagSpec',1e-3,1e4,1
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'MagSpec',0,yLimUpper,0

        GET_DATA,'MagSpecFilt',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'MagSpecFilt',DATA=tmp
        OPTIONS,'MagSpecFilt','ytitle','Frequency (Hz)'
        OPTIONS,'MagSpecFilt','zTitle',magSpecFilt.units_name
        ZLIM,'MagSpecFilt',1e-3,1e4,1
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'MagSpecFilt',0,yLimUpper,0

        STORE_DATA,'MagZ',DATA=magz

        GET_DATA,'EAVSpec',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'EAVSpec',DATA=TEMPORARY(tmp)
        ZLIM,'EAVSpec',1.e-3,1.e3,1                            ; set z limits
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpec',0,yLimUpper,0
        OPTIONS,'EAVSpec','ytitle','Frequency (Hz)'
        OPTIONS,'EAVSpec','ztitle','Log ' + eAVSpec.units_name ; z title

        GET_DATA,'EAVSpecFilt',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'EAVSpecFilt',DATA=TEMPORARY(tmp)
        ZLIM,'EAVSpecFilt',1.e-3,1.e3,1                            ; set z limits
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpecFilt',0,yLimUpper,0
        OPTIONS,'EAVSpecFilt','ytitle','Frequency (Hz)'
        OPTIONS,'EAVSpecFilt','ztitle',eAVSpecFilt.units_name ; z title

        GET_DATA,'EAVSpecFiltInterp',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'EAVSpecFiltInterp',DATA=TEMPORARY(tmp)
        ZLIM,'EAVSpecFiltInterp',1.e-3,1.e3,1                            ; set z limits
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpecFiltInterp',0,yLimUpper,0
        OPTIONS,'EAVSpecFiltInterp','ytitle','Frequency (Hz)'
        OPTIONS,'EAVSpecFiltInterp','ztitle',eAVSpecFilt.units_name ; z title


        ;;Time to see where this is all happening. Where is the rolloff?
        GET_DATA,'MagSpec',DATA=dat 
        junk = MIN(ABS(dat.x-STR_TO_TIME('1998-03-10/18:50:49')),ind) 
        PRINT,ALOG10(dat.y[ind,*]) 
        junkPlot = PLOT(dat.v,dat.y[ind,*],YLOG=1)

        ;;Prep TPLOT nonsense
        red                     = 250
        green                   = 130
        black                   = 10

        ;; STORE_DATA,'magzPanel',DATA={x:[[magzTmp.time],[magzFilt.time]], $
        ;;                             y:[[magzTmp.comp1],[magzFilt.comp1]]}
        ;; OPTIONS,'magzPanel','tplot_routine','mplot'
        ;; OPTIONS,'magzPanel','labels',['NoFilt','0-20 Hz']
        ;; OPTIONS,'magzPanel','colors',[red,green]
        STORE_DATA,'magzPanel',DATA={x:magzTmp.time, $
                                     y:magzTmp.comp1}
        OPTIONS,'magzPanel','ytitle','E-W Despun!CMag. Field (nT)'
        OPTIONS,'magzPanel','labflag',-1 ;evenly spaced
        OPTIONS,'magzPanel','labels','NoFilt'

        STORE_DATA,'magzFilt',DATA={x:magzFilt.time, $
                                    y:magzFilt.comp1}
        OPTIONS,'magzFilt','colors',red
        OPTIONS,'magzFilt','labels','Filtered'

        ;;What's up with this silly plotting of the gap in mag data for orb 6127?
        ;;I can't make it go away, and yet it doesn't appear in the plots of E-field data!!
        ;; k = 4 & inds = [k*10000:(k+1)*10000]
        ;; this = PLOT(dat.x[inds]-dat.x[0],dat.y[inds])
        ;; GET_DATA,'magzPanel',DATA=dat
        ;; PRINT,TIME_TO_STR(magzTmp.time[60691],/MS)
        ;; PRINT,TIME_TO_STR(magzTmp.comp1[60692],/MS)



        ;; STORE_DATA,'eAVPanel',DATA={x:[eAlongV.x,eAlongVTmp.time], $
        ;;                             y:[eAlongV.y,eAlongVTmp.comp1]}
        ;; OPTIONS,'eAVPanel','tplot_routine','mplot'
        ;; OPTIONS,'eAVPanel','labels',['NoFilt','0-20 Hz']
        OPTIONS,'eAVPanel','labels','NoFilt'
        ;; OPTIONS,'eAVPanel','colors',[red,green]
        ;; OPTIONS,'eAVPanel','colors',red
        STORE_DATA,'eAVPanel',DATA={x:eAlongVTmp.time, $
                                    y:eAlongVTmp.comp1}
        OPTIONS,'eAVPanel','ytitle','E along V (mV/m)'
        OPTIONS,'eAVPanel','labflag',-1 ;evenly spaced

        STORE_DATA,'eAVFilt',DATA={x:eAlongVFilt.time, $
                                   y:eAlongVFilt.comp1}
        OPTIONS,'eAVFilt','colors',red
        ;; OPTIONS,'eAVFilt','labels','0-20 Hz'
        OPTIONS,'eAVFilt','labels','Filtered'
        ;; OPTIONS,'eAVFilt','labflag',-1 ;evenly spaced
        ;; OPTIONS,'eAVFilt','labpos',-1.5

        ;;Plot, also overplot good Alfvén events

        ;;Little test to see diff
        ;; this = magzfilt.comp1-magztmp.comp1
        ;; plot = PLOT(this)

        ;; TPLOT,['magzPanel','eAVPanel','MagSpec','EAVSpec']
        TPLOT,['magzPanel','eAVPanel','MagSpec','MagSpecFilt','EAVSpec','EAVSpecFiltInterp']
        ;; TPLOT,['magzPanel','eAVPanel','MagSpec','MagSpecFilt','EAVSpec','EAVSpecFilt']
        TPLOT_PANEL,VARIABLE='magzPanel',OPLOTVAR='magzFilt'
        TPLOT_PANEL,VARIABLE='eAVPanel',OPLOTVAR='eAVFilt'

        LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime, $
                                 /DO_DESPUNDB, $
                                 GOOD_I=good_i, $
                                 HEMI__GOOD_I='BOTH'
        ii = WHERE(maximus.orbit[good_i] EQ orbit,nOrb)

        STORE_DATA,'alfTimes',DATA={x:cdbTime[good_i[ii]], $
                                    y:MAKE_ARRAY(nOrb,VALUE=10)}
        OPTIONS,'alfTimes','psym',1 ;Plus
        TPLOT_PANEL,VARIABLE='MagSpec',OPLOTVAR='alfTimes'

        ;; OPLOT,cdbtime[good_i[ii]]-t,MAKE_ARRAY(N_ELEMENTS(ii),VALUE=10),PSYM=1
        ;; PRINT,maximus.time[good_i[ii]]

        magAlf_i = VALUE_LOCATE(magSpec.time,cdbTime[good_i[ii]])
        magAlf_t = magSpec.time[magAlf_i[UNIQ(magAlf_i)]]

        FOR lm=0,N_ELEMENTS(magAlf_t)-1 DO BEGIN
           PRINT,FORMAT='(I0,T10,A0)',lm,TIME_TO_STR(magAlf_t[lm],/MS)
        ENDFOR

        ;;Trying to figure out appropriate threshold values for E

        GET_DATA,'EAVSpecFiltInterp',DATA=tmpE
        tmpE.y[WHERE(~FINITE(tmpE.y))] = 0.0
        ;; junk = MIN(ABS(dat.x-STR_TO_TIME('1998-03-10/18:50:49')),ind)
        ;; junk = MIN(ABS(dat.x-STR_TO_TIME('1998-03-10/18:52:13.178')),ind)
        junk = MIN(ABS(tmpE.x-STR_TO_TIME('1998-03-10/19:20:48.412')),ind)
        doze = PLOT(tmpE.v, $
                    ;; ALOG10(tmpE.y[ind,*]), $
                    tmpE.y[ind,*], $
                    YLOG=1, $
                    XRANGE=FFTFreqRange, $
                    ;; YRANGE=ALOG10([1e-3,1e4]), $
                    YRANGE=[1e-3,1e4], $
                    XTITLE='Frequency (Hz)', $
                    YTITLE=eAVSpecFilt.units_name, $
                    TITLE=TIME_TO_STR(tmpE.x[ind],/MS))


        GET_DATA,'MagSpecFilt',DATA=tmpB        
        tmpB.y[WHERE(~FINITE(tmpE.y))] = 0.0
        junk = MIN(ABS(tmpB.x-STR_TO_TIME('1998-03-10/19:20:48.412')),ind)
        doze = PLOT(tmpB.v, $
                    ;; ALOG10(tmpB.y[ind,*]), $
                    tmpB.y[ind,*], $
                    YLOG=1, $
                    XRANGE=FFTFreqRange, $
                    ;; YRANGE=ALOG10([1e-3,1e4]), $
                    YRANGE=[1e-3,1e4], $
                    XTITLE='Frequency (Hz)', $
                    YTITLE=magSpecFilt.units_name, $
                    TITLE=TIME_TO_STR(tmpB.x[ind],/MS))
        ;; e_over_b = 
        ;;magz.y=smooth(magz.y,40)
        ;; STORE_DATA,'Magz_smooth',DATA={x:magz.x,y:magz.y}
        ;; IF keyword_set(filterfreq) THEN BEGIN
           
        ;;    magz = filter(magz,filterfreq,'magfilt','l')
        ;;    ;;remove end effects of the filter by cutting off the first/last 2s
        ;;    sf = magz.x[1]-magz.x[0]
        ;;    np = N_ELEMENTS(magz.x)
        ;;    padding = round(2.0/sf)
        ;;    magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
        ;;    STORE_DATA,'MagZ',DATA=magz
        ;; ENDIF
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get density estimates for all good times
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;now get orbit quantities
        GET_DATA,'ORBIT',DATA=orb
        GET_DATA,'MLT',DATA=mlt
        GET_DATA,'ALT',DATA=alt
        GET_DATA,'ILAT',DATA=ilat
        GET_DATA,'fa_vel',DATA=vel
        

        ephemI_magSpec = VALUE_LOCATE(mlt.x,magSpec.time)
        
        magOrb   = orb.y[ephemI_magSpec]
        magAlt   = alt.y[ephemI_magSpec]	
        magMLT   = mlt.y[ephemI_magSpec]	
        magILAT  = ilat.y[ephemI_magSpec]
        magSpd   = SQRT(vel.y[ephemI_magSpec,0]^2 + $
                        vel.y[ephemI_magSpec,1]^2 + $
                        vel.y[ephemI_magSpec,2]^2)*1000.0                        ;Speed in m/s
        magDens  = DENS_MLT(magAlt,magMLT,OUT_VA_KM_PER_SEC=va_mlt) ;in cm^-3
        magDens *= 1.0e6 ;Dens est in m^-3
        va_mlt  *= 1000. ;Get estimate of v_A in meters

        ;;What's the lower bound on frequency?
        ;; const    = 0.0029949602 ;in meter^0.5
        const    = 2.9949602e-8 ;in meter^0.5
        lowFreq  = const * SQRT(magDens) * magSpd

        PRINT,lowFreq
        ;; fMin     = MIN(

        nThings  = N_ELEMENTS(magDens)
        ESpecSum = MAKE_ARRAY(nThings,/FLOAT)
        BSpecSum = MAKE_ARRAY(nThings,/FLOAT)
        
        freqs        = tmpB.v
        upFreqBound  = FFTFreqRange[1]
        
        FOR m=0,nThings-1 DO BEGIN
           tmpF_i    = WHERE(freqs GE lowFreq[m] AND freqs LE upFreqBound)
         ESpecSum[m] = TOTAL(tmpE.y[m,tmpF_i])
         BSpecSum[m] = TOTAL(tmpB.y[m,tmpF_i])
        ENDFOR

        ;; ESpecSum *= 1e-3
        ;; BSpecSum *= 1e-9

        ;;Apply

        E_over_B  = (ESpecSum*1e-3)/(BSpecSum*1e-9)
        winFFT_i  = WHERE( ( (E_over_B/va_mlt) GE 1.0/eb_to_alfven_speed ) AND $
                           ( (E_over_B/va_mlt) LE 10.0*eb_to_alfven_speed ),nWin)

        GET_STREAKS,winFFT_i,START_I=strtWin_ii,STOP_I=stpWin_ii, $
                    SINGLE_I=singleWin_ii, $
                    OUT_STREAKLENS=FFTStreakLens
        nFFTStreaks = N_ELEMENTS(FFTStreakLens)
        magz_Alf_i  = MAKE_ARRAY(2,nFFTStreaks,/LONG)
        FOR m=0,nFFTStreaks-1 DO BEGIN
           junk     = MIN(ABS(tmpB.x[winFFT_i[strtWin_ii[m]]]-magzTmp.time),magz_start_i)
           junk     = MIN(ABS(tmpB.x[winFFT_i[stpWin_ii[m]]]-magzTmp.time),magz_stop_i)
           magz_Alf_i[*,m] = [magz_start_i,magz_stop_i]
        ENDFOR

        STOP
        ;;iv. Calculate Poynting flux
        ;;v. Screenings
        ;;	1. Keep all data, but let screenings happen on the fly
        ;;		a. Change in B (instrument threshold)
        ;;		b. Change in E (instrument threshold)
        ;;		c. E-over-B ratio
        ;;	2. Two ideas from Chris. Could use:
        ;;		a. Requirement that Pflux be > 1 mW/m2, corresponding to visible aurora
        ;;		b. Requirement that E and B be above noise level (“but maybe it’s all noise!”)


        ;;get mag and efield data on same time scale
        ;;SMH Try this to make fa_fields_combine stop crying                        
        ;; magz   = {time:magz.x,comp1:magz.y,ncomp:1}
        ;; efield = {time:efield.x,comp1:efield.y}
        
        
        ;; fields=combinets(magz,efield)
        ;; FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
        ;; fields = {time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

        ;;I'm hoping this means magz is pared down somewhere else

        ;; dens=combinets(magz,langmuir)
        ;; langmuir={time:langmuir.x,comp1:langmuir.y,ncomp:1}
        ;; FA_FIELDS_COMBINE,magz,langmuir,result=dens,/talk
        ;; dens={time:magz.time,comp1:magz.comp1,comp2:dens,ncomp:2}

        ;; magz={x:magz.time,y:magz.comp1}
        ;; langmuir={x:langmuir.time,y:langmuir.comp1}

        ;;get the prootn cyc frequency for smoothing the e field data later
        proton_cyc_freq = 1.6e-19*SQRT(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
        ;;get_orbit data
        get_fa_orbit,je_tmp_time,/time_array,/all
        
        ;;define loss cone angle
        GET_DATA,'ALT',DATA=alt
        loss_cone_alt = alt.y[0]*1000.0
        lcw = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
        GET_DATA,'ILAT',DATA=ilat
        north_south = ABS(ilat.y[0])/ilat.y[0]
        
        IF north_south EQ -1 THEN BEGIN
           e_angle=[180.-lcw,180+lcw] ; for Southern Hemis.
           ;;i_angle=[270.0,90.0]	
           ;;elimnate ram from data
           i_angle=[180.0,360.0]
           i_angle_up=[270.0,360.0]
           
        ENDIF ELSE BEGIN
           e_angle=[360.-lcw,lcw] ;	for Northern Hemis.
           ;;i_angle=[90.,270.0]
           ;;eliminate ram from data
           i_angle=[0.0,180.0]
           i_angle_up=[90.0,180.0]
           
        endelse
        
        
        ;;get fields mode
        fields_mode = get_fa_fields('DataHdr_1032',time_ranges[jjj,0],time_ranges[jjj,1])
        
        ;;get the spacecraft potential per spin
        spin_period = 4.946     ; seconds
        
        ;;get_sample_rate
        v8={x:spacecraft_potential.time,y:spacecraft_potential.comp1}
        
        v8_dt = ABS(v8.x-shift(v8.x,-1))
        v8_dt[0] = v8_dt[1]
        v8_dt[N_ELEMENTS(v8.x)-1] = v8_dt[N_ELEMENTS(v8.x)-2]

        ;;get maxima within a 1 spin window
        j_range = WHERE(v8.x LT v8.x(N_ELEMENTS(v8.x)-1)-spin_period)
        index_max = MAX(j_range)
        print,index_max
        pot = MAKE_ARRAY(N_ELEMENTS(v8.x),/double)
        FOR j=0L,index_max DO BEGIN
           ;;spin_range=WHERE(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
           spin_range = j+findgen(ceil(spin_period/V8_dt(j)))
           pot[j] = MAX(ABS(v8.y[spin_range]),ind)
           sign = v8.y[spin_range[ind]]/ABS(v8.y[spin_range[ind]])
           pot[j]=sign*pot[j]
                                ;print,j,pot[j]
        endfor
        pot[index_max+1:N_ELEMENTS(v8.x)-1] = pot[j_range[index_max]]
        sc_pot={x:v8.x,y:pot}
        STORE_DATA,'S_Pot',DATA=sc_pot ;note this is actualy the negative of the sp. potential this corrected in the file output

        ;;get moments/integrals of various fluxes
        IF keyword_set(burst) THEN BEGIN

           get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='JEe_tot',ENERGY=energy_electrons
           get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='JEe',angle=e_angle,ENERGY=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='Je',ENERGY=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='Je_lc',ENERGY=energy_electrons,angle=e_angle
           
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='JEi',ENERGY=energy_ions
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='Ji',ENERGY=energy_ions
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='JEi_up',ENERGY=energy_ions,angle=i_angle
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                      NAME='Ji_up',ENERGY=energy_ions,angle=i_angle
           
        ENDIF ELSE BEGIN
           
           get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='JEe_tot',ENERGY=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='JEe',angle=e_angle,ENERGY=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='Je',ENERGY=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='Je_lc',ENERGY=energy_electrons,angle=e_angle,sc_pot=sc_pot
           
           get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='JEi',ENERGY=energy_ions,angle=i_angle,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='Ji',ENERGY=energy_ions,angle=i_angle,sc_pot=sc_pot
           get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='JEi_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
                          NAME='Ji_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           
           ;; IF keyword_set(heavy) THEN BEGIN
              
           ;;    get_2dt_pot,'je_2d','fa_tsp_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='JEp_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;;    get_2dt_pot,'je_2d','fa_tso_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='JEo_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;;    get_2dt_pot,'je_2d','fa_tsh_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='JEh_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
           ;;    get_2dt_pot,'j_2d','fa_tsp_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='Jp_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;;    get_2dt_pot,'j_2d','fa_tso_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='Jo_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;;    get_2dt_pot,'j_2d','fa_tsh_eq',t1=time_ranges[jjj,0],t2=time_ranges[jjj,1], $
           ;;                NAME='Jh_up',ENERGY=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;; ENDIF
           
        endelse
        
        GET_DATA,'Je',DATA=tmp
        GET_DATA,'Ji',DATA=tmpi
        ;;remove crap
        keep1 = WHERE(finite(tmp.y) NE 0 and finite(tmpi.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        keep2 = WHERE(ABS(tmp.y) GT 0.0 and ABS(tmpi.y) GT 0.0)
        je_tmp_time = tmp.x(keep2)
        je_tmp_data = tmp.y(keep2)
        STORE_DATA,'Je',DATA={x:je_tmp_time,y:je_tmp_data}
        
        GET_DATA,'JEe',DATA=tmp
        ;;remove crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        jee_tmp_time = tmp.x(keep2)
        jee_tmp_data = tmp.y(keep2)
        STORE_DATA,'JEe',DATA={x:jee_tmp_time,y:jee_tmp_data}
        
        GET_DATA,'JEe_tot',DATA=tmp
        ;;remove crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        jee_tot_tmp_time = tmp.x(keep2)
        jee_tot_tmp_data = tmp.y(keep2)
        STORE_DATA,'JEe_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_tmp_data}
        
        GET_DATA,'Je_lc',DATA=tmp
        ;;remove_crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        je_lc_tmp_time = tmp.x(keep2)
        je_lc_tmp_data = tmp.y(keep2)
        STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:je_lc_tmp_data}
        
        GET_DATA,'Ji',DATA=tmp
        ;;remove crap	
        ;;keep1=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep2=WHERE(ABS(tmp.y) GT 0.0)
        ji_tmp_time = tmp.x(keep2)
        ji_tmp_data = 2.0*tmp.y(keep2) ;;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
        
        GET_DATA,'JEi',DATA=tmp
        ;;remove crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        jEi_tmp_time = tmp.x(keep2)
        jEi_tmp_data = tmp.y(keep2)
        STORE_DATA,'JEi',DATA={x:jEi_tmp_time,y:jEi_tmp_data}
        
        GET_DATA,'JEi_up',DATA=tmp
        ;;remove crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        jEi_up_tmp_time = tmp.x(keep2)
        jEi_up_tmp_data = tmp.y(keep2)
        STORE_DATA,'JEi_up',DATA={x:jEi_up_tmp_time,y:jEi_up_tmp_data}
        
        GET_DATA,'Ji_up',DATA=tmp
        ;;remove crap
        ;;keep=WHERE(finite(tmp.y) NE 0)
        tmp.x = tmp.x(keep1)
        tmp.y = tmp.y(keep1)
        ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        ji_up_tmp_time = tmp.x(keep2)
        ji_up_tmp_data = 2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        STORE_DATA,'Ji_up',DATA={x:ji_up_tmp_time,y:ji_up_tmp_data}
        
        
        ;; IF keyword_set(heavy) THEN BEGIN
           
        ;;    GET_DATA,'JEp_up',DATA=tmp
        ;;    ;;remove crap
        ;;    keep1 = WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    keep2 = WHERE(ABS(tmp.y) GT 0.0)
        ;;    jEp_up_tmp_time = tmp.x(keep2)
        ;;    jEp_up_tmp_data = tmp.y(keep2)
        ;;    STORE_DATA,'JEp_up',DATA={x:jEp_up_tmp_time,y:jEp_up_tmp_data}
           
        ;;    GET_DATA,'Jp_up',DATA=tmp
        ;;    ;;remove crap
        ;;    ;;keep=WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        ;;    jp_up_tmp_time = tmp.x(keep2)
        ;;    jp_up_tmp_data = 2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        ;;    STORE_DATA,'Jp_up',DATA={x:jp_up_tmp_time,y:jp_up_tmp_data}
           
           
        ;;    GET_DATA,'JEo_up',DATA=tmp
        ;;    ;;remove crap
        ;;    ;;keep=WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        ;;    jEo_up_tmp_time = tmp.x(keep2)
        ;;    jEo_up_tmp_data = tmp.y(keep2)
        ;;    STORE_DATA,'JEo_up',DATA={x:jEo_up_tmp_time,y:jEo_up_tmp_data}
           
        ;;    GET_DATA,'Jo_up',DATA=tmp
        ;;    ;;remove crap
        ;;    ;;keep=WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        ;;    jo_up_tmp_time = tmp.x(keep2)
        ;;    jo_up_tmp_data = 2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        ;;    STORE_DATA,'Jo_up',DATA={x:jo_up_tmp_time,y:jo_up_tmp_data}
           
           
        ;;    GET_DATA,'JEh_up',DATA=tmp
        ;;    ;;remove crap
        ;;    keep1 = WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    keep2 = WHERE(ABS(tmp.y) GT 0.0)
        ;;    jEh_up_tmp_time = tmp.x(keep2)
        ;;    jEh_up_tmp_data = tmp.y(keep2)
        ;;    STORE_DATA,'JEh_up',DATA={x:jEh_up_tmp_time,y:jEh_up_tmp_data}
           
        ;;    GET_DATA,'Jh_up',DATA=tmp
        ;;    ;;remove crap
        ;;                         ;keep=WHERE(finite(tmp.y) NE 0)
        ;;    tmp.x = tmp.x(keep1)
        ;;    tmp.y = tmp.y(keep1)
        ;;    ;;keep=WHERE(ABS(tmp.y) GT 0.0)
        ;;    jh_up_tmp_time = tmp.x(keep2)
        ;;    jh_up_tmp_data = 2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        ;;    STORE_DATA,'Jh_up',DATA={x:jh_up_tmp_time,y:jh_up_tmp_data}
           
        ;; ENDIF
        ;;get ion end electron characteristic energies
        
        chare=(jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11
        chare_tot=(jee_tot_tmp_data/je_tmp_data)*6.242*1.0e11
        charei=(JEi_up_tmp_data/ji_up_tmp_data)*6.242*1.0e11
        STORE_DATA,'CharE',DATA={x:jee_tmp_time,y:chare}
        STORE_DATA,'CharE_tot',DATA={x:jee_tot_tmp_time,y:chare_tot}
        STORE_DATA,'CharEi',DATA={x:jei_up_tmp_time,y:charei}
        
        ;;Scale electron energy flux to 100km, pos flux earthward
        GET_DATA,'ILAT',DATA=tmp
        sgn_flx = tmp.y/ABS(tmp.y)
        GET_DATA,'B_model',DATA=tmp1
        GET_DATA,'BFOOT',DATA=tmp2
        mag1 = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
        mag2 = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
        ratio = (mag2/mag1)
        jee_ionos_tmp_data = sgn_flx*jee_tmp_data*ratio
        STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
        
        jee_tot_ionos_tmp_data = sgn_flx*jee_tot_tmp_data*ratio
        STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}
        
        GET_DATA,'fa_vel',DATA=vel
        speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
        
        ;;get position of each mag point
        ;;samplingperiod=magz.x(300)-magz.x(299)
        ;;position=MAKE_ARRAY(N_ELEMENTS(magz.x),/double)
        ;;position=speed(300)*samplingperiod*findgen(N_ELEMENTS(magz.x))
        ;;speed_mag_point=speed(300)
        
        old_pos = 0.
        position = MAKE_ARRAY(N_ELEMENTS(magz.x),/double)
        speed_mag_point = MAKE_ARRAY(N_ELEMENTS(magz.x),/double)
        FOR j=0L,N_ELEMENTS(magz.x)-2 DO BEGIN
           speed_point_ind = MIN(ABS(vel.x-magz.x[j]),ind)
           ;;print,ind
           speed_mag_point[j]=speed[ind]
           samplingperiod = magz.x(j+1)-magz.x[j]
           ;;position=MAKE_ARRAY(N_ELEMENTS(magz.x),/double)
           position[j]=old_pos+speed_mag_point[j]*samplingperiod
           old_pos = position[j]
        endfor
        
        
        ;;calculate the total ion outflow for this interval
        part_res_ji = MAKE_ARRAY(N_ELEMENTS(ji_up_tmp_time),/double)
        position_ji = MAKE_ARRAY(N_ELEMENTS(Ji_up_tmp_time),/double)
        position_ji[0]=0.0
        FOR j=1,N_ELEMENTS(ji_tmp_time)-1 DO BEGIN
           part_res_ji[j]=ABS(ji_up_tmp_time(j-1)-ji_up_tmp_time[j])
           IF part_res_ji[j] EQ 0.0 THEN part_res_ji[j]=part_res_ji(j-1)
           position_ji[j]=position_ji(j-1)+speed[j]*part_res_Ji[j]
        endfor
        part_res_Ji[0]=part_res_Ji[1]
        ji_tot[jjj]=int_tabulated(position_ji,ji_tmp_data*SQRT(ratio))       ;mapped to ionosphere sqrt due to intergration in x 
        ji_up_tot[jjj]=int_tabulated(position_ji,ji_up_tmp_data*SQRT(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        
        print,'ji_tot',ji_tot[jjj]
        
        ;;calculate the total electron downflux at the spacecraft altitude over this interval
        part_res_je = MAKE_ARRAY(N_ELEMENTS(jee_tmp_data),/double)
        position_je = MAKE_ARRAY(N_ELEMENTS(jee_tmp_time),/double)
        position_je[0]=0.0
        FOR j=1,N_ELEMENTS(je_tmp_time)-1 DO BEGIN
           part_res_je[j]=ABS(jee_tmp_time(j-1)-jee_tmp_time[j])
           IF part_res_je[j] EQ 0.0 THEN part_res_je[j]=part_res_je(j-1)
           position_je[j]=position_je(j-1)+speed[j]*part_res_Je[j]
        endfor
        part_res_Je[0]=part_res_Je[1]
        jee_tot[jjj]=int_tabulated(position_je,jee_tmp_data*SQRT(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        ;;calculate the current from mag
        deltaBX = deriv(position,magz.y)
        jtemp = ABS(1.0e-3*(deltaBx)/1.26e-6)
        sign_jtemp = ABS(deltaBx)/deltaBx
        STORE_DATA,'jtemp',DATA={x:magz.x,y:jtemp}

        ;;terminate the intervals before the last point
        IF sign_jtemp(N_ELEMENTS(jtemp)-1)*sign_jtemp(N_ELEMENTS(jtemp)-2) NE -1 THEN sign_jtemp[N_ELEMENTS(jtemp)-1] = -1*sign_jtemp[N_ELEMENTS(jtemp)-1]

        
        ;;IF we want to save a summary plot
        IF KEYWORD_SET(png_sumplot) THEN BEGIN
           cgPS_Open, outPlotDir+'as5Spec_orbit' + STRCOMPRESS(orbit_num+'_'+STRING(jjj),/REMOVE_ALL) + '.ps', font=1
           loadct,39
           !p.charsize=1.3
           tplot,['Je','CharE','JEei','Ji','JEi','MagZ','jtemp'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges[jjj,0],time_ranges[jjj,1]]
           cgPS_Close, /PNG, /Delete_PS, Width=1000
        ENDIF ELSE BEGIN 
           IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN
              window,0,xsize=600,ysize=800
              loadct,39
              !p.charsize=1.3
              tplot,['Je','CharE','JEei','Ji','JEi','MagZ'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges[jjj,0],time_ranges[jjj,1]]
           ENDIF
        ENDELSE

        start_points=[0]
        stop_points=[0]
        
        ;;get current intervals
        FOR j=1L,N_ELEMENTS(sign_jtemp)-2 DO BEGIN

           IF sign_jtemp[j]+sign_jtemp(j-1) EQ 0.0 THEN BEGIN
              start_points=[start_points,j]
           ENDIF
           IF sign_jtemp[j]+sign_jtemp(j+1) EQ 0.0 THEN BEGIN
              stop_points=[stop_points,j]
           ENDIF

        endfor

        IF sign_jtemp[0]+sign_jtemp[1] NE 0.0 THEN BEGIN
           stop_points = stop_points[1:N_ELEMENTS(stop_points)-1]
        ENDIF

        ;;eliminate single points
        non_single_points = WHERE(stop_points NE start_points)

        start_points = start_points(non_single_points)
        stop_points = stop_points(non_single_points)

        ;;define the current intervals
        ;;in this array 	0-interval start index
        ;;		1-interval stop index
        ;;		2-sign of the current (field-aligned is pos)
        ;;		3-validity of the point-i.e does it satisfy the thresholds
        ;;		4-maximum size of the current in that interval
        ;;		5-maximum size of the current from the Electron esa
        ;;		6-maximum size of the electron energy flux mapped to the ionosphere
        ;;		7-integrated downgoing electron flux over that interval at ionosphere
        ;;		8-maximum characteristic electron energy from that interval
        ;;		9-maximum ion energy flux
        ;;		10-maximum ion flux
        ;;		11-maximum upgoing ion flux
        ;;		12-integrated upgoing ion flux over the interval at the ionosphere
        ;;		13-integrated upgoing only ion flux over the interval at the ionosphere
        ;;		14-maximum characteristic ion energy
        ;;		15-time width of the current filament in s
        ;;		16-width of the current filament at the s/c altitude
        ;;		17-magnetic field amplitude (nT)
        ;;		18-electric field amplitude (mV/m)
        ;;		19-Orbit number
        ;;		20-max current time (based on location of max current
        ;;		21-max current altitude
        ;;		22-max current MLT
        ;;		23- max current ILAT
        ;;		24-average value of B
        ;;		25-average value of E
        ;;		26-field sample rate
        ;;		27-fields mode		 
        ;;		28-maximum upgoing proton flux
        ;;		29-maximum characteristic proton energy
        ;;		30-maximum upgoing oxygen flux
        ;;		31-maximum characteristic oxygen energy
        ;;		32-maximum upgoing helium flux
        ;;		33-maximum characteristic helium energy
        ;;		34-spacecraft potential -1.*V8_S
        ;;		35-langmuir probe number
        ;;		36-max L. current over interval
        ;;		37-min L.current over interval
        ;;		38-median L.current over interval
        ;;		39-maximum characteristic electron energy from total distribution from that interval
        ;;		40-maximum size of the electron energy flux from total distribution mapped to the ionosphere
        ;;		41-integrated downgoing electron flux from total distribution over that interval at ionosphere

        
        current_intervals = MAKE_ARRAY(N_ELEMENTS(start_points),42,/double)
        current_intervals[*,0] = start_points
        current_intervals[*,1] = stop_points
        current_intervals[*,2] = sign_jtemp(start_points)
        current_intervals[*,3] = 1
        
        intervalparts_electrons_old=-1
        intervalparts_ions_old=-1
        valid_old = 0.0
        FOR j=0L,N_ELEMENTS(start_points)-1 DO BEGIN
           
           ;;define the interval points 
           intervalfields = (current_intervals[j,0])+FINDGEN(current_intervals[j,1]+1-current_intervals[j,0])
           tempz = magz.y[intervalfields]
           fields_res_interval = magz.x[intervalfields]-magz.x[intervalfields-1]
           ;;help,magz,/st
           ;;print,'current_indices ',current_intervals[j,0],current_intervals[j,1]
           intervalparts_electrons = WHERE(je_tmp_time GE magz.x(current_intervals[j,0]) and je_tmp_time LE magz.x(current_intervals[j,1]))
           intervalparts_ions = WHERE(ji_up_tmp_time GE magz.x(current_intervals[j,0]) and ji_up_tmp_time LE magz.x(current_intervals[j,1]))
           IF intervalparts_electrons[0] EQ -1 THEN BEGIN
              minitime = MIN(ABS(je_tmp_time-magz.x(current_intervals[j,0])),intervalparts_electrons)
           ENDIF
           IF intervalparts_ions[0] EQ -1 THEN BEGIN
              minitime = MIN(ABS(ji_up_tmp_time-magz.x(current_intervals[j,0])),intervalparts_ions)
           ENDIF

           ;;get the current from b and determine IF to keep this event
           jmax = MAX(jtemp[intervalfields],indjmax)
           current_intervals[j,4] = jmax*sign_jtemp(start_points[j])
           IF jmax LE current_threshold THEN BEGIN
              current_intervals[j,3] = 0.0
           ENDIF
           
           ;;define the time of the max current
           current_intervals[j,20] = magz.x[intervalfields[indjmax]]
           
           
           ;;get the electron current and determine IF to keep this event
           sign=-1.*je_tmp_data[intervalparts_electrons]/ABS(je_tmp_data[intervalparts_electrons])
           maxJe = MAX(ABS(je_tmp_data[intervalparts_electrons]),ind)
           maxJe = maxJe*sign[ind]*1.6e-9 ;;in microA/m2
           current_intervals[j,5] = maxJe
           IF ABS(maxJe)/ABS(jmax) LE esa_j_delta_bj_ratio_threshold THEN BEGIN
              current_intervals[j,3] = 0.0
           ENDIF
           
           ;;get the electron energy flux and dtermine IF to keep this event
           ;;print,'intervalparts_electrons',intervalparts_electrons
           ;;help,jee_tmp_time
           ;;help,je_tmp_time
           ;;print,'jee start stop ',TIME_TO_STR(jee_tmp_time[0],/ms),TIME_TO_STR(jee_tmp_time(N_ELEMENTS(jee_tmp_time)-1),/ms)
           ;;print,'je start stop ',TIME_TO_STR(je_tmp_time[0],/ms),TIME_TO_STR(jee_tmp_time(N_ELEMENTS(jee_tmp_time)-1),/ms)
           
           sign = jee_ionos_tmp_data[intervalparts_electrons]/ABS(jee_ionos_tmp_data[intervalparts_electrons]) ;note corrected direction (i.e.-1) from Alfven_stats_4-positive is now really downwards
           maxJEe_ionos = MAX(ABS(jee_ionos_tmp_data[intervalparts_electrons]),ind)
           maxJEe_ionos = maxJEe_ionos*sign[ind]
           
           sign = jee_tot_ionos_tmp_data[intervalparts_electrons]/ABS(jee_tot_ionos_tmp_data[intervalparts_electrons])
           maxJEe_tot_ionos = MAX(ABS(jee_tot_ionos_tmp_data[intervalparts_electrons]),ind)
           maxJEe_tot_ionos = maxJEe_tot_ionos*sign[ind]



           current_intervals[j,6] = maxJEe_ionos
           current_intervals[j,40] = maxJEe_tot_ionos
           IF ABS(maxJEe_ionos) LE electron_eflux_ionos_threshold and ABS(maxJEe_tot_ionos-maxJEe_ionos) LE electron_eflux_ionos_threshold THEN BEGIN ;note change from previously when only downgoing fluxes where considered.
              current_intervals[j,3] = 0.0				      
           ENDIF
           
           ;; IF keyword_set(heavy) THEN BEGIN
              
           ;;    minitime = MIN(ABS(Jp_up_tmp_time-current_intervals[j,20]),ind_OH)
           ;;    current_intervals[j,28] = Jp_up_tmp_data(ind_OH)
           ;;    C_Ep = JEp_up_tmp_data(ind_OH)/Jp_up_tmp_data(ind_OH)*6.242*1.0e11
           ;;    current_intervals[j,29] = C_Ep
              
           ;;    current_intervals[j,30] = Jo_up_tmp_data(ind_OH)
           ;;    C_Eo = JEo_up_tmp_data(ind_OH)/Jo_up_tmp_data(ind_OH)*6.242*1.0e11
           ;;    current_intervals[j,31] = C_Eo
              
           ;;    minitime = MIN(ABS(Jh_up_tmp_time-current_intervals[j,20]),ind_h)
           ;;    current_intervals[j,32] = Jh_up_tmp_data(ind_h)
           ;;    C_Eh = JEh_up_tmp_data(ind_h)/Jh_up_tmp_data(ind_h)*6.242*1.0e11
           ;;    current_intervals[j,33] = C_Eh
              
           ;; ENDIF
           
           ;;get width of current filament in time (s)
           time_width = magz.x(current_intervals[j,1])-magz.x(current_intervals[j,0])
           
           current_intervals[j,15] = time_width
           ;;get width of the current filament at this altitude
           
           width = speed_mag_point(current_intervals[j,0])*ABS(magz.x(current_intervals[j,0])-magz.x(current_intervals[j,1]))
           ;;print,'speed',speed_mag_point(current_intervals[j,0])
           current_intervals[j,16] = width
           
           ;;get the integrated electron dflux in ionosphere over this interval
           IF intervalparts_electrons[0] NE -1 THEN BEGIN
              IF N_ELEMENTS(intervalparts_electrons) EQ 1 THEN BEGIN 
                 
                 current_intervals[j,7] = width*jee_tmp_data[intervalparts_electrons]
                 current_intervals[j,41] = width*jee_tot_tmp_data[intervalparts_electrons]
              ENDIF ELSE BEGIN
                 ;;interpolate particle data to same resolution as the fields data
                 jee_tmp_data_fields_res_interval = interpol(jee_tmp_data[intervalparts_electrons],jee_tmp_time[intervalparts_electrons],magz.x[intervalfields])
                 jee_tot_tmp_data_fields_res_interval = interpol(jee_tot_tmp_data[intervalparts_electrons],jee_tot_tmp_time[intervalparts_electrons],magz.x[intervalfields])
                 current_intervals[j,7] = int_tabulated(findgen(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,jee_tmp_data_fields_res_interval,/double)
                 current_intervals[j,41] = int_tabulated(findgen(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,jee_tot_tmp_data_fields_res_interval,/double)
                 
              endelse
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              current_intervals[j,7] = current_intervals[j,7]*SQRT(ratio(intervalparts_electrons[0]))
              current_intervals[j,41] = current_intervals[j,41]*SQRT(ratio(intervalparts_electrons[0]))
           ENDIF
           
           
           
           ;;get integrated ion outflow mapped to ionosphere over this interval
           IF intervalparts_ions[0] NE -1 THEN BEGIN
              IF N_ELEMENTS(intervalparts_ions) EQ 1 THEN BEGIN 
                 ;;IF intervalparts_ions[0] NE intervalparts_ions_old(N_ELEMENTS(intervalparts_ions_old)-1) or valid_old EQ 0.0 THEN BEGIN
                 
                 current_intervals[j,12] = width*ji_tmp_data[intervalparts_ions]
                 current_intervals[j,13] = width*ji_up_tmp_data[intervalparts_ions]
                 ;;ENDIF
              ENDIF ELSE BEGIN
                 ;;IF  intervalparts_ions[0] EQ intervalparts_ions_old(N_ELEMENTS(intervalparts_ions_old)-1) and valid_old EQ 1.0 THEN intervalparts_ions=intervalparts_ions(1:N_ELEMENTS(intervalparts_ions)-1)
                 ;;IF N_ELEMENTS(intervalparts_ions) EQ 1 THEN BEGIN 
                 ;;current_intervals[j,12] = speed[intervalparts_ions]*part_res_ji[intervalparts_ions]*ji_up_tmp_data[intervalparts_ions]/2.0
                 
                 ;;ENDIF ELSE BEGIN
                 
                 
                 ;;interpolate particle data to same resolaution as the fields data
                 ji_tmp_data_fields_res_interval = INTERPOL(ji_tmp_data[intervalparts_ions],ji_tmp_time[intervalparts_ions],magz.x[intervalfields])
                 ji_up_tmp_data_fields_res_interval = INTERPOL(ji_up_tmp_data[intervalparts_ions],ji_up_tmp_time[intervalparts_ions],magz.x[intervalfields])
                 
                 current_intervals[j,12] = INT_TABULATED(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,ji_tmp_data_fields_res_interval,/double)
                 current_intervals[j,13] = INT_TABULATED(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,ji_up_tmp_data_fields_res_interval,/double)
                 ;;print,'ji_tot_alf',Ji_tot_alf[jjj]
                 
                 
                 ;;endelse
                 
              endelse
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              current_intervals[j,12] = current_intervals[j,12]*SQRT(ratio[intervalparts_ions[0]])
              current_intervals[j,13] = current_intervals[j,13]*SQRT(ratio[intervalparts_ions[0]])
           ENDIF
           
           ;;get max electron characteristic energy over this interval
           C_E = MAX(charE[intervalparts_electrons])
           C_E_tot = MAX(charE_tot[intervalparts_electrons])

           current_intervals[j,8] = C_E
           current_intervals[j,39] = C_E_tot

           ;;get max upgoing ion energy flux over this interval
           maxJEi = MAX(ABS(jei_up_tmp_data[intervalparts_ions]),ind)
           current_intervals[j,9] = maxJEi
           
           ;;get max ion flux over this interval
           sign_ion=-1.*ji_tmp_data[intervalparts_ions]/ABS(ji_tmp_data[intervalparts_ions])
           maxJi = MAX(ABS(ji_tmp_data[intervalparts_ions]),ind)
           maxJi = maxJi*sign_ion[ind]
           current_intervals[j,10] = maxJi
           
           ;;get max upgoing ion flux over this interval
           maxJi_up = MAX(ABS(ji_up_tmp_data[intervalparts_ions]),ind)
           current_intervals[j,11] = maxJi_up
           
           ;;get max characteristic ion energy over this interval
           C_Ei = MAX(charEi[intervalparts_ions])
           current_intervals[j,14] = C_Ei
           
           
           
           ;;fields sample period
           current_intervals[j,26] = magz.x[intervalfields[indjmax]+1]-magz.x[intervalfields[indjmax]]
           
           ;;get mag field amplitude
           db = MAX(magz.y[intervalfields])-MIN(magz.y[intervalfields])
           median_db = MEDIAN(magz.y[intervalfields])
           current_intervals[j,17] = db
           current_intervals[j,24] = median_db
           IF db LT delta_b_threshold THEN current_intervals[j,3] = 0.0 ;threshold for reliablity of identification
           
           ;;get elec field amplitude
           ;;smooth to below proton gyro freq.
           smooth_int = CEIL((1./proton_cyc_freq[intervalfields[indjmax]])/current_intervals[j,26])
           IF smooth_int GT 1.0 and smooth_int LE N_ELEMENTS(intervalfields)/4.0 THEN BEGIN
              efield_smooth = SMOOTH(fields.comp2[intervalfields],smooth_int) 
           ENDIF ELSE BEGIN
              efield_smooth = fields.comp2[intervalfields]
           ENDELSE 
           de = MAX(efield_smooth)-MIN(efield_smooth)
           median_de = MEDIAN(fields.comp2[intervalfields])
           current_intervals[j,18] = de
           current_intervals[j,25] = median_de
           IF de LT delta_E_threshold THEN current_intervals[j,3] = 0.0 ;threshold for reliablity of identification
           
           ;;get max and min L. probe currents
           smooth_int = ceil((1./proton_cyc_freq[intervalfields[indjmax]])/current_intervals[j,26])
           ;; IF smooth_int GT 1.0 and smooth_int LE N_ELEMENTS(intervalfields)/4.0 THEN dens_smooth=smooth(dens.comp2[intervalfields],smooth_int) ELSE dens_smooth=dens.comp2[intervalfields]
           
           ;; dens_max = MAX(dens_smooth)
           ;; dens_min = MIN(dens_smooth)
           ;; probe_time = MIN(ABS(dens_probe.x-magz.x[intervalfields[indjmax]]),probe_ind)
           
           
           ;; median_dens = MEDIAN(dens.comp2[intervalfields])
           ;; current_intervals[j,35] = dens_probe.y(probe_ind)
           ;; current_intervals[j,36] = dens_max
           ;; current_intervals[j,37] = dens_min
           ;; current_intervals[j,38] = median_dens
           current_intervals[j,35] = 0.
           current_intervals[j,36] = 0.
           current_intervals[j,37] = 0.
           current_intervals[j,38] = 0.

           ;;now get orbit quantities
           GET_DATA,'ORBIT',DATA=orb
           GET_DATA,'MLT',DATA=mlt
           GET_DATA,'ALT',DATA=alt
           GET_DATA,'ILAT',DATA=ilat

           mintime = MIN(ABS(mlt.x-magz.x[intervalfields[indjmax]]),ind)
           
           current_intervals[j,19] = orb.y[ind]
           current_intervals[j,21] = alt.y[ind]	
           current_intervals[j,22] = mlt.y[ind]	
           current_intervals[j,23] = ilat.y[ind]
           
           ;;fields_mode
           mintime = MIN(ABS(fields_mode.time-magz.x[intervalfields[indjmax]]),ind)
           current_intervals[j,27] = fields_mode.comp1(13,ind)
           
           ;;sc potential
           mintime = MIN(ABS(sc_pot.x-magz.x[intervalfields[indjmax]]),ind)
           current_intervals[j,34]=-1*sc_pot.y[ind]
           
           ;;e over b test
           PRINT,"Need to insert Alfvén speed from above"
           WAIT,1.0
           ;; va = 1000.0*alfven_speed_mlt(current_intervals[j,21],current_intervals[j,22])
           e_over_b=(1.0e-3*current_intervals[j,18])/(current_intervals[j,17]*1.0e-9)
           IF e_over_b/va LT 1.0/eb_to_alfven_speed THEN current_intervals[j,3] = 0.0
           
           intervalparts_electrons_old = intervalparts_electrons
           intervalparts_ions_old = intervalparts_ions	
           valid_old = current_intervals[j,3]
        endfor
        
     ENDIF ELSE BEGIN
        STOP
     ENDELSE
     
;;remove crap data
     keep = WHERE(current_intervals[*,3] NE 0.0)
     print,'keep',keep
     IF keyword_set(keep_alfven_only) THEN BEGIN
        IF keep[0] EQ -1 THEN BEGIN
           PRINT,"No meaningful data here! Not producing file..."
           keep = !NULL
        ENDIF ELSE BEGIN
           print,'number of events: ',N_ELEMENTS(keep)
           current_intervals = current_intervals[keep,*]
        ENDELSE
     ENDIF

;;IF jjj GT 0 or not keyword_set(filename) then
;;filename = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/'+'Dartmouth_as5_dflux_'+STRCOMPRESS(orbit_num+'_'+string[jjj]+"_magcal_v"
;;+ STRING(version)+"_burst",/REMOVE_ALL)
     IF jjj GT 0 or not keyword_set(filename) THEN filename = curfile

     print,filename,jjj
     openw,unit1,filename,/get_lun
     PRINTF,unit1,N_ELEMENTS(current_intervals[*,0]),N_ELEMENTS(keep)

     PRINTF,unit1,' Column No.  	1-Orbit number'
     PRINTF,unit1,'			2-Alfvenic = 1 non-Alfvenic = 0'
     PRINTF,unit1,'			3-max current time (based on location of max current'
     PRINTF,unit1,'			4-max current altitude'
     PRINTF,unit1,'			5-max current MLT'
     PRINTF,unit1,'			6-max current ILAT'			
     PRINTF,unit1,'			7-maximum size of the delta B current in that interval'
     PRINTF,unit1,'			8-maximum size of the current from the Electron esa at s/c alt.'
     PRINTF,unit1,'			9-maximum size of the electron energy flux from loss cone mapped to the ionosphere-positive is downwards'
     PRINTF,unit1,'			10-maximum size of the electron energy flux from total distribution mapped to the ionosphere-positive is downwards'
     PRINTF,unit1,'			11-integrated electron flux from loss cone over that interval at ionosphere'
     PRINTF,unit1,'			12-integrated electron flux from total distribution over that interval at ionosphere'
     PRINTF,unit1,'			13-maximum characteristic electron energy from loss cone over that interval'
     PRINTF,unit1,'			14-maximum characteristic electron energy from total distribution over that interval'
     PRINTF,unit1,'			15-maximum ion energy flux at the s/c altitude'
     PRINTF,unit1,'			16-maximum ion flux at the s/c altitude'
     PRINTF,unit1,'			17-maximum upgoing ion flux at the s/c altitude'
     PRINTF,unit1,'			18-integrated ion flux over the interval at ionosphere'
     PRINTF,unit1,'			19-integrated upgoing only ion flux over the interval at ionosphere'
     PRINTF,unit1,'			20-maximum characteristic ion energy'
     PRINTF,unit1,'			21-width of the current fiament in time (s)'
     PRINTF,unit1,'			22-width of the current filament in m at the s/c altitude'
     PRINTF,unit1,'			23-magnetic field amplitude (nT)'
     PRINTF,unit1,'			24-electric field amplitude (mV/m)'
     PRINTF,unit1,'			25-fields sample period'				
     PRINTF,unit1,'			26-fields mode'
     PRINTF,unit1,'			27-maximum upgoing proton flux'
     PRINTF,unit1,'			28-maximum upgoing proton characteristic energy'
     PRINTF,unit1,'			29-maximum upgoing oxygen flux'
     PRINTF,unit1,'			30-maximum upgoing oxygen characteristic energy'
     PRINTF,unit1,'			31-maximum upgoing helium flux'
     PRINTF,unit1,'			32-maximum upgoing helium characteristic energy'
     PRINTF,unit1,'			33-spacecraft potential'
     PRINTF,unit1,'			34-Langmuir probe number'
     PRINTF,unit1,'			35-max langmuir probe current over interval'
     PRINTF,unit1,'			36-min lamgmuir probe current over interval'
     PRINTF,unit1,'			37-median langmuir probe current over interval'
     PRINTF,unit1,'			38-interval start time'
     PRINTF,unit1,'			39-interval stop time'

     PRINTF,unit1,FORMAT='("total electron dflux at ionosphere from single integ.",T68,G16.6)',Jee_tot[jjj]
     PRINTF,unit1,FORMAT='("total electron dflux at ionosphere from total of intervals",T68,G16.6)',TOTAL(current_intervals[*,7])
     PRINTF,unit1,FORMAT='("total Alfven electron dflux at ionosphere",T68,G16.6)',TOTAL(current_intervals[keep,7])
     PRINTF,unit1,FORMAT='("total ion outflow at ionosphere from single integ",T68,G16.6)',Ji_tot[jjj]
     PRINTF,unit1,FORMAT='("total ion outflow at ionosphere from total of intervals",T68,G16.6)',TOTAL(current_intervals[*,12])
     PRINTF,unit1,FORMAT='("total Alfven ion outflow at ionosphere",T68,G16.6)',TOTAL(current_intervals[keep,12])
     PRINTF,unit1,FORMAT='("total upward only ion outflow at ionosphere from single integ.",T68,G16.6)',Ji_up_tot[jjj]
     PRINTF,unit1,FORMAT='("total upward only ion outflow at ionosphere from total of intervals",T68,G16.6)',TOTAL(current_intervals[*,13])
     PRINTF,unit1,FORMAT='("total Alfven upward only ion outflow at ionosphere",T68,G16.6)',TOTAL(current_intervals[keep,13])						

     FOR jj=0L,N_ELEMENTS(current_intervals[*,0])-1 DO BEGIN

        ;;Want pngs of each of OUR events?
        IF KEYWORD_SET(png_ourevents) THEN BEGIN
           ;;cur_time = str_to_time(data_chast.time[jj])
           ;;IF cur_time GT time_ranges[jjj,0] AND cur_time LT time_ranges[jjj,1] THEN BEGIN
           fname = outPlotDir+'orb_' + STRCOMPRESS(orbit_num+'_'+STRING(jjj)+'_' + $
                                                   STRING(jj),/REMOVE_ALL) + $
                   '--Dart_as5Spec_event_'+STRCOMPRESS(jj,/REMOVE_ALL)+'.ps'
           plotstr = "B!Dz!N and J!Dmag!N for Dartmouth spec event " + str(jj)
           tplot_options,'title',plotstr
           cgPS_Open,fname,font=1
           loadct,39
           !p.charsize = 1.3
;;                      tfirst = magz.x(current_intervals[jj,0])
;;                      tlast = magz.x(current_intervals[jj,1])
           tplot,['MagZ','jtemp'] ,var_label=['ALT','MLT','ILAT'], $
                 trange=[magz.x[current_intervals[jj,0]],magz.x[current_intervals[jj,1]]]
           cgPS_Close, /PNG,/delete_ps, WIDTH=1000
;;            ENDIF
           ;;; ENDIF ELSE PRINT,$
           ;;  FORMAT='("Chaston event[",I-0,"]: ",A-0," outside range (FOR jjj=",I-0,")")',$
           ;;  jj,data_chast.time[jj],jjj
        ENDIF
        IF KEYWORD_SET(png_lots_of_quantities_ourevents) THEN BEGIN
           STORE_DATA,'eField',DATA={x:fields.time,y:fields.comp2,yTitle:'E!Dsp!N'}
           fname = outPlotDirDir+'/orb_' + $
                   STRCOMPRESS(orbit_num+'_'+STRING(jjj)+'_'+STRING(jj),/REMOVE_ALL) + $
                   '--Dart_specEvent_'+STRCOMPRESS(jj,/REMOVE_ALL)+'.ps'
           plotstr = "Event " + str(jj)
           tplot_options,'title',plotstr
           cgPS_Open,fname,font=1
           loadct,39
           !p.charsize = 1.3
           tplot,['MagZ','jtemp','eField'] , $
                 VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[magz.x[current_intervals[jj,0]],magz.x[current_intervals[jj,1]]]
           CGPS_CLOSE, /PNG, /DELETE_PS
        ENDIF
        PRINTF,unit1,FORMAT='(I9,G13.6,A24,34G13.6,A24,A24)',current_intervals[jj,19],current_intervals[jj,3],TIME_TO_STR(current_intervals[jj,20],/ms),$
;			PRINTF,unit1,FORMAT='(I9,G13.6,A24,31G13.6)',current_intervals[jj,19],current_intervals[jj,3],TIME_TO_STR(current_intervals[jj,20],/ms),$
               current_intervals[jj,21],current_intervals[jj,22],current_intervals[jj,23],current_intervals[jj,4],$
               current_intervals[jj,5],current_intervals[jj,6],current_intervals[jj,40],current_intervals[jj,7],$
               current_intervals[jj,41],current_intervals[jj,8],current_intervals[jj,39],$ ;;Counting from 1, jj,39 is item 14
               current_intervals[jj,9],current_intervals[jj,10],current_intervals[jj,11],current_intervals[jj,12],$
               current_intervals[jj,13],current_intervals[jj,14],current_intervals[jj,15],current_intervals[jj,16],$
               current_intervals[jj,17],current_intervals[jj,18],current_intervals[jj,26],current_intervals[jj,27],$
               current_intervals[jj,28],current_intervals[jj,29],current_intervals[jj,30],current_intervals[jj,31],$
               current_intervals[jj,32],current_intervals[jj,33],current_intervals[jj,34],current_intervals[jj,35],$
               current_intervals[jj,36],current_intervals[jj,37],current_intervals[jj,38], $
               TIME_TO_STR(magz.x(current_intervals[jj,0]),/ms), TIME_TO_STR(magz.x(current_intervals[jj,1]),/ms)

        
     endfor
     free_lun,unit1


  endfor

END
