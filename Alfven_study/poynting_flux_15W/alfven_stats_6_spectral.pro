PRO ALFVEN_STATS_6_SPECTRAL, $
   FILENAME=filename, $
   BELOW_AURORAL_OVAL=below_auroral_oval, $
   SHOW_ALF_EVENTS_FROM_MAXIMUS=show_maximus_events, $
   COMPARE_AVERAGE_POYNTING_FLUXES=compare_pFluxes__specMethod_maximus, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   T1=t1, $
   T2=t2, $
   BURST=burst, $
   ONLY_FASTSRVY_DATA=only_128Ss_data, $
   OVERRIDE_FREQ_BOUNDS=override_freq_bounds, $
   MIN_FREQ=min_freq, $
   MAX_FREQ=max_freq, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   FULL_PFLUX_CALC=full_pFlux, $
   UCLA_MAG_DESPIN=ucla_mag_despin, $
   KEEP_ALFVEN_ONLY=keep_alfven_only, $
   NO_PLOTS=no_plots, $
   PS_SUMPLOT=ps_sumplot, $
   PNG_OUREVENTS=png_ourevents, $
   BIGWINDOW=bigWindow, $
   DONTSHOWPLOTS=dontShowPlots, $
   CONT_IF_FILE_EXISTS=cont_if_file_exists, $
   B_AND_E_SPECPLOTS=B_and_E_specPlots, $
   PFLUXALONGB__SPECPLOT=pFluxAlongB__specPlot, $
   EOVERBPLOT=EoverBPlot, $
   PFLUX_PLOTS=pFlux_plots, $
   THRESHOLD_POWER_SPECTRA_PLOTS=threshold_power_spectra_plots, $
   MAGJPLOT=magJplot, $
   MAKE_DIAGNOSTIC_FILE=make_diagFile, $
   SAVE_LIL_DATA_PACKAGE=save_lil_package, $
   SAVE_PS=save_ps, $
   SAVE_PARTICLE_DATA=save_particle_data, $
   BATCH_MODE=batch_mode

  ;; COMPILE_OPT idl2
  ;; COMPILE_OPT strictArr

  outDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Alfven_study/poynting_flux_15W/'

  IF KEYWORD_SET(ps_sumplot) OR KEYWORD_SET(save_ps) THEN BEGIN
     SET_PLOT_DIR,outPlotDir,/FOR_SDT,ADD_SUFF='/as6_spectral/'
  ENDIF

  IF N_ELEMENTS(full_pFlux         ) EQ 0 THEN full_pFlux          = 1
  IF N_ELEMENTS(include_E_near_B   ) EQ 0 THEN include_E_near_B    = KEYWORD_SET(full_pFlux)
  IF N_ELEMENTS(ucla_mag_despin    ) EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval ) EQ 0 THEN below_auroral_oval  = 1
  IF N_ELEMENTS(only_128Ss_data    ) EQ 0 THEN only_128Ss_data     = 1
  IF N_ELEMENTS(yLim_to_mag_rolloff) EQ 0 THEN yLim_to_mag_rolloff = 1

  IF KEYWORD_SET(only_128Ss_data) THEN BEGIN
     PRINT,"Excluding periods for which sample rate of FG mag is ≤ 128 S/s …"
  ENDIF

  ;;The way this works is that we estimate f_spA ≤ k_perp * λe < f_spB
  ;;Frequency details under "***Frequency conditions***"
  f_spA          = 0.1
  f_spB          = 1./f_spA
  freqRes        = 0.125D         ;Frequencies rounded to this number
  ;; freqRes     = 0.1D         ;Frequencies rounded to this number
  FGMagRolloff   = 20.0          ;in Hz


  ;;For FFTs of Mag and E data
  FFTLen         = 1024
  nFFTAvg        = 1

  maxPeriod      = 1/100.
  minPeriod      = 1/132.
  eFSampFact     = 4         ;The fact is, E-field data gets sampled a million times faster!

  freqPlotRange  = [0.1,FGMagRolloff]
  ;; override_freq  = [0.0,FGMagRolloff]
  override_freq  = [0.125,0.5] ;Brambles et al. [2011] supplementary matieral

  FFTSlide       = 1.0

  ;;Filter stuff
  lowPole        = 8 ;Slay that low-freq garbage
  highPole       = 4
  FFTdb          = 50 ;For digital filter coeffs. IDL doc says "50 is a good choice."

  ;;***Thresholds on power spectra***        
  ;;(See section with the same label below to investigate how I came up with these)
  BSpecThresh    = 0.01          ;in     nT^2/Hz
  ESpecThresh    = 0.01          ;in (mV/m)^2/Hz
  ;;...And thresholds on integrated spectra
  BIntegThresh   = 0.2           ; in nT
  EIntegThresh   = 0.2           ; in mV/m
  ebAlfRat       = 10.0

  ;;Other screenings
  current_threshold               = 1.0  ;microA/m^2
  delta_b_threshold               = 2.5  ; nT
  delta_E_threshold               = 2.0 ; mV/m
  esa_j_delta_bj_ratio_threshold  = 0.02
  electron_eflux_ionos_threshold  = 0.05  ;ergs/cm^2/s
  eb_to_alfven_speed              = 10.0 ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
                                ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  mu_0                            = DOUBLE(4.0D*!PI*1e-7)

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  IF NOT KEYWORD_SET(energy_ions)     THEN energy_ions       = [0.,500.]   ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; IF no data exists, return to main
  t   = 0
  dat = GET_FA_EES(t,/ST)
  IF dat.valid eq 0 THEN BEGIN
     PRINT,' ERROR: No FAST electron survey data -- get_fa_ees(t,/ST) returned invalid data'
     RETURN
  ENDIF

  ;; Electron current - line plot
  IF KEYWORD_SET(burst) THEN BEGIN
     GET_2DT_TS,'j_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  ENDIF ELSE BEGIN
     GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  ENDELSE
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  IF SIZE(tmpj,/TYPE) NE 8 THEN BEGIN
     PRINT,' ERROR: Bogus current data!'
     RETURN
  ENDIF

  keep        = WHERE(FINITE(tmpj.y) NE 0)
  tmpj.x      = tmpj.x[keep]
  tmpj.y      = tmpj.y[keep]
  
  keep        = WHERE(ABS(tmpj.y) GT 0.0)
  tx          = tmpj.x[keep]
  ty          = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order  = SORT(tx)
  tx          = tx[time_order]
  ty          = ty[time_order]
  
  ;;kill dupes
  dupe_i      = WHERE(ABS(tx[1:-1]-tx[0:-2]) LT 0.0001,nDupes, $
                      COMPLEMENT=keep,NCOMPLEMENT=nKeep)
  PRINT,STRCOMPRESS(nDupes,/REMOVE_ALL) + ' Je duplicates here'
  tx          = tx[keep]
  ty          = ty[keep]
  
  ;;throw away the first 10  points since they are often corrupted
  IF NOT KEYWORD_SET(burst) THEN BEGIN
     STORE_DATA,'Je',DATA={x:tx[10:nKeep-1],y:ty[10:nKeep-1]}
  ENDIF ELSE BEGIN
     STORE_DATA,'Je',DATA={x:tx,y:ty}
  ENDELSE
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  GET_DATA,'Je',DATA=je
  GET_FA_ORBIT,/TIME_ARRAY,je.x
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  IF KEYWORD_SET(below_auroral_oval) THEN BEGIN
     keep             = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr  = 'below_aur_oval'
  ENDIF ELSE BEGIN
     keep             = WHERE(ABS(ilat.y) GT AURORAL_ZONE(mlt.y,7,/LAT)/(!DPI)*180.)
     belowAurOvalStr  = ''
  ENDELSE

  STORE_DATA,'Je',DATA={x:je.x[keep],y:je.y[keep]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je         = MAKE_ARRAY(N_ELEMENTS(Je.x),/DOUBLE)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j]   = ABS(Je.x[j]-Je.x[j-1])
  endfor
  part_res_Je[0]      = part_res_Je[1]
  gap                 = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start   = [0,WHERE(part_res_je GT 10.0)]
     separate_stop    = [WHERE(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start   = [0]
     separate_stop    = [N_ELEMENTS(Je.x)-1]
  endelse
  
  ;;remove esa burp when switched on
  IF not KEYWORD_SET(burst) THEN BEGIN
     turn_on = WHERE(part_res_je GT 300.0)
     IF turn_on[0] NE -1 THEN BEGIN
        turn_on_separate = MAKE_ARRAY(N_ELEMENTS(turn_on),/DOUBLE)
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
  
  PRINT,'number_of_intervals',number_of_intervals
  
  ji_tot               = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  ji_up_tot            = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  jee_tot              = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Ji_tot_alf           = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Ji_up_tot_alf        = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Jee_tot_alf          = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  
  ;;get despun mag data IF keyword set
  IF KEYWORD_SET(ucla_mag_despin) THEN ucla_mag_despin,ORBIT=orbit
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN

     ;;get orbit number for filenames		
     IF ~KEYWORD_SET(ucla_mag_despin) THEN BEGIN
        GET_DATA,'ORBIT',DATA=tmp
        orbit  = tmp.y[0]
     ENDIF
     orbit_num    = STRCOMPRESS(STRING(orbit),/REMOVE_ALL)
     orbItvlSuff  = STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)

     IF KEYWORD_SET(make_diagFile) THEN BEGIN
        diagFile = outDir + 'AS6--diagnostic_file--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + orbItvlSuff

        OPENW,diagLun,diagFile,/GET_LUN
     ENDIF

     tmpT1 = time_ranges[jjj,0]
     tmpT2 = time_ranges[jjj,1]

     PRINT,'time_range',TIME_TO_STR(tmpT1),TIME_TO_STR(tmpT2)
     IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'time_range', $
                                               TIME_TO_STR(tmpT1),TIME_TO_STR(tmpT2)
     
     ;;filename for output file
     IF KEYWORD_SET(burst) THEN BEGIN

        bonusDir    = 'batch_output__burst/'
        bonusSuff   = belowAurOvalStr + '--burst'

     ENDIF ELSE BEGIN

        bonusDir  = 'batch_output/'

        IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
           bonusSuff = belowAurOvalStr + '--ucla_mag_despin'
        ENDIF ELSE BEGIN
           bonusSuff = belowAurOvalStr
        ENDELSE

     ENDELSE
     curFile     = outDir+bonusDir+'Dartmouth_as6_spectral_'+orbItvlSuff+'--'+bonusSuff
     ptclFile    = outDir+'particle_data/'+'Dartmouth_as6_particle_data--'+ $
                   orbItvlSuff + '--' + bonusSuff
     tmpPlotName = 'Dartmouth_as6_spectral_'+orbItvlSuff+'--'+bonusSuff

     ;;make sure we're not overwriting
     IF FILE_TEST(curfile) THEN BEGIN
        IF NOT KEYWORD_SET(cont_if_file_exists) THEN BEGIN
           right_now  = strmid(timestamp(),0,13)
           curfile    = curfile + "--" + right_now
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
     
     magDC = GET_FA_FIELDS('MagDC',t,/START,/CALIBRATE,/REPAIR)
     ;; dat = get_fa_fields('MagDC',t,/START)
     ;; IF magDC.valid EQ 0 THEN BEGIN
     ;;    PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     ;;    RETURN
     ;; ENDIF 
     
     ;;Get two types of potential. Which DO we like better?
     ;;JOURNAL__20160917__SEE_IF_SDT_CANNED_SC_POT[...] shows why I chose canned SDT routines
     sc_pot  = GET_FA_POTENTIAL(tmpT1,tmpT2, $
                                 ;; /SPIN, $
                                 /REPAIR)

     ;; IF data_valid NE 0.0 THEN BEGIN
     IF (sc_pot.valid) AND (magDC.valid) THEN BEGIN
        
        sc_pot  = {x:sc_pot.time, $
                   y:(-1.)*sc_pot.comp1, $ ;;Reverse sign of pot for use with GET_2DT_TS_POT
                   valid:sc_pot.valid} 

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get Mag and E field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;	E component: “E_along_V”
        ;;	B components: Two components perpendicular to E_along_V
        ;;		a. Use Strangeway despinning, find coords that complement E_along_V
        ;;		b. Need to use Bob model to subtract the background field

        IF not KEYWORD_SET(ucla_mag_despin) THEN BEGIN
           GET_DATA,'MagDCcomp1',DATA=magx
           GET_DATA,'MagDCcomp2',DATA=magy
           GET_DATA,'MagDCcomp3',DATA=magz
        ENDIF ELSE BEGIN
           GET_DATA,'dB_fac_v',DATA=db_fac
           IF SIZE(db_fac,/TYPE) NE 8 THEN BEGIN
              PRINT,"Couldn't get despun mag data! Outta sight ..."
              RETURN
           ENDIF

           mintime = MIN(ABS(tmpT1-db_fac.x),ind1)
           mintime = MIN(ABS(tmpT2-db_fac.x),ind2)
           ;;   From UCLA_MAG_DESPIN: "Field-aligned velocity-based coordinates defined as:    "
           ;;x (ind 0)-along track ((BxV)xB),
           ;;y (ind 1)-cross track (BxV), 
           ;;z (ind 2)-along B" (I added "ind" marks)
           magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} 
           magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} 
           magz = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

           GET_DATA,'MAG_FLAGS',DATA=magFlag
           nFlag = N_ELEMENTS(magFlag.x)
           badB = WHERE(magFlag.y GE 16,nBadB)
           IF nBadB GT 0 THEN BEGIN
              PRINT,'Bad magfield data! Removing ...'
              IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'Bad magfield data! Removing ...'
              rmInds = !NULL
              keepInds = LINDGEN(N_ELEMENTS(magz.x))
              FOR k=0,nBadB-1 DO BEGIN
                 temp1 = magFlag.x[badB[k]]
                 temp2 = badB[k] EQ (nFlag-1) ? $
                         magFlag.x[badB[k]] + (magFlag.x[badB[k]]-magFlag.x[badB[k]-1]) : $
                         magFlag.x[badB[k]+1]
                 IF (temp2 LT magz.x[0]) OR (temp1 GT magz.x[-1]) THEN CONTINUE

                 mintime = MIN(ABS(temp1-magz.x),ind1)
                 mintime = MIN(ABS(temp2-magz.x),ind2)
                 rmInds = [rmInds, (ind1[0] EQ ind2[0]) ? ind1 : [ind1:ind2]]
              ENDFOR
              PRINT,'Junking ' + STRCOMPRESS(N_ELEMENTS(rmInds),/REMOVE_ALL) + ' bad mag inds ...'
              IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'Junking ' + STRCOMPRESS(N_ELEMENTS(rmInds),/REMOVE_ALL) + ' bad mag inds ...'
              REMOVE,rmInds,keepInds
              magx = {x:db_fac.x[keepInds],y:db_fac.y[keepInds,0]} 
              magy = {x:db_fac.x[keepInds],y:db_fac.y[keepInds,2]} 
              magz = {x:db_fac.x[keepInds],y:db_fac.y[keepInds,1]}
           ENDIF


        ENDELSE
        
        STORE_DATA,'MagZ',DATA=magz

        ;;E field
        FA_FIELDS_DESPIN,efieldV58,efieldV1214, $
                         /SHADOW_NOTCH,/SINTERP, $ ;Why? Because RJS does it in his summary plot
                         T1=tmpT1,T2=tmpT2 ;,/SLOW ;procedure
        ;; GET_DATA,'E_NEAR_B',DATA=eNearB

        IF ~efieldV58.valid OR ~efieldV58.valid THEN BEGIN
           PRINT,FORMAT='("efieldV58.valid : ",I0)',efieldV58.valid
           PRINT,FORMAT='("efieldV1214.valid : ",I0)',efieldV1214.valid
           PRINT,'Returning ...'
           RETURN
        ENDIF

        GET_DATA,'E_ALONG_V',DATA=eAlongV
        IF SIZE(eAlongV,/TYPE) NE 8 THEN BEGIN
           PRINT,"Couldn't get E_ALONG_V!" 
           STOP
        ENDIF

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           GET_DATA,'E_NEAR_B',DATA=eNearB
        ENDIF

        ;;Now check sorted/dupes
        CHECK_DUPES,magz.x,HAS_DUPES=magHasDupes, $
                    IS_SORTED=magIsSort,OUT_UNIQ_I=magUniq_i,/QUIET
        IF magHasDupes OR ~magIsSort THEN BEGIN
           PRINT,'Mag has dupes/is not sorted! Sorting ...'
           magx = {x:magx.x[magUniq_i],y:magx.y[magUniq_i]}
           magy = {x:magy.x[magUniq_i],y:magy.y[magUniq_i]}
           magz = {x:magz.x[magUniq_i],y:magz.y[magUniq_i]}

        ENDIF

        CHECK_DUPES,eAlongV.x,HAS_DUPES=eAVHasDupes, $
                    IS_SORTED=eAVIsSort,OUT_UNIQ_I=eAVUniq_i,/QUIET
        IF eAVHasDupes OR ~eAVIsSort THEN BEGIN
           PRINT,'EAV has dupes/is not sorted! Sorting ...'
           eAlongV = {x:eAlongV.x[eAVUniq_i],y:eAlongV.y[eAVUniq_i]}
           STORE_DATA,'E_ALONG_V',DATA=eAlongV

           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              eNearB = {x:eNearB.x[eAVUniq_i],y:eNearB.y[eAVUniq_i]}
              STORE_DATA,'E_ALONG_V',DATA=eNearB
           ENDIF
        ENDIF

        ;;Get eAlongV at same res (I think this always means reducing the res. of eField
        ;; magzTmp      = {TIME         : magz.x                , $
        ;;                 COMP1        : magz.y                , $
        ;;                 NCOMP        : 1                     , $
        ;;                 DATA_NAME    : 'Cross-track MagData' , $
        ;;                 VALID        : 1                     , $
        ;;                 PROJECT_NAME : 'FAST'                , $
        ;;                 UNITS_NAME   : 'nT'                  , $
        ;;                 CALIBRATED   : 1}
        ;; eAlongVTmp   = {TIME         :  eAlongV.x            , $
        ;;                 COMP1        :  eAlongV.y            , $
        ;;                 NCOMP        : 1                     , $
        ;;                 VALID        : 1                     , $
        ;;                 DATA_NAME    :'E Along V'            , $
        ;;                 PROJECT_NAME : 'FAST'                , $
        ;;                 UNITS_NAME   : 'mV/m'                , $
        ;;                 CALIBRATED   : 1}
        ;; FA_FIELDS_COMBINE,magzTmp,eAlongVTmp, $
                          ;; eAlongVTmp, $
        FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                          {TIME:eAlongV.x,COMP1:eAlongV.y}, $
                          RESULT=eAVInterpDat, $
                          /INTERP, $
                          /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          ;; DELT_T=minPeriod, $
                          /TALK

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                             {TIME:eNearB.x,COMP1:eNearB.y}, $
                             RESULT=eNearBInterpDat, $
                             /INTERP, $
                             /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                             ;; DELT_T=minPeriod, $
                             /TALK
        ENDIF

        ;; ;;Make sure we have same num points 
        ;; IF N_ELEMENTS(eAVInterpDat) NE N_ELEMENTS(magz.x) THEN BEGIN 
        ;;    PRINT,"Bogus! Why didn't these match?" 
        ;;    RETURN 
        ;; ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;ii. Restrict to periods with 128 S/s?
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;NOTE, we just use FA_FIELDS_BUFS to separate into various 
        ;;     sampling frequencies because frankly, it does a great job.
        ;;See JOURNAL__20160916__MAKE_SURE … addressing this question, 
        ;;    and try feeding it orbit 6127. You'll see.
        FA_FIELDS_BUFS,{time:magz.x},FFTLen, $
                       BUF_STARTS=strtM_i, $
                       BUF_ENDS=stopM_i, $
                       DELTA_T=1.0e-5 ;Allowable error

        ntot = long(nFFTAvg) * long(FFTLen)
        ;; BREAK THE DATA INTO BUFFERS
        FA_FIELDS_BUFS,{time:magz.x}, ntot, $
                       BUF_STARTS=strtM_i, $
                       BUF_ENDS=stopM_i, $
                       DELTA_T=1.0e-5 ;Allowable error
                       
        
        IF strtM_i[0] EQ stopM_i[0] THEN BEGIN
           PRINT, 'FA_FIELDS_SPEC: Unable to extract continuous buffers from'
           PRINT, 'time series data - try changing NFFTAVG and FFTLEN to get a '
           PRINT, 'smaller buffer size.'
           RETURN
        endif
        
        nbufs = N_ELEMENTS(strtM_i)
        
        ;; ESTIMATE THE SIZE OF THE RESULT
        all        = TOTAL( stopM_i-strtM_i ) + nbufs    
        num_ffts   = LONG(all/(ntot *  FFTSlide))
        num_freqs  = FFTLen/2+1.
        timeFFT    = DBLARR(num_ffts)
        FFTCount      = 0l
        dt         = FLTARR(nbufs)
        
        fftBin_i   = MAKE_ARRAY(2,num_ffts,/LONG) ;You'll want this guy. 
                                                  ;He tells you just where exactly you are.
        FOR i=0,nbufs-1 DO BEGIN
           n_start           = LONG(strtM_i[i])
           n_stop            = n_start + ntot - 1l

           dt[i]             = magz.x[n_start+1] - magz.x[n_start]
           
           WHILE (n_stop LE stopM_i[i]) DO BEGIN            
              timeFFT[FFTCount]    = magz.x[(n_start+n_stop)/2]
              fftBin_i[*,FFTCount] = [n_start,n_stop]
              FFTCount++
              n_start           = n_start + LONG(ntot*FFTSlide)            
              n_stop            = n_start + ntot - 1l
           ENDWHILE
           
           ;;Humdiddle
           norm_dt           = MEDIAN(dt)
           abnorm            = WHERE(dt NE norm_dt)    
           
        ENDFOR
        
        ;;Shrink these arrays
        ;; keepMe               = WHERE(timeFFT GE 1.0,nKeepFFT)
        timeFFT              = timeFFT[0:FFTCount-1]
        fftBin_i             = fftBin_i[*,LINDGEN(FFTCount)]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get density estimates for all good times
        ;;
        ;;We use the density model already in place from 
        ;;   ALFVEN_STATS_5 for calculating omega_p
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;First, orb quantities
        GET_DATA,'ORBIT',DATA=orb
        GET_DATA,'MLT',DATA=mlt
        GET_DATA,'ALT',DATA=alt
        GET_DATA,'ILAT',DATA=ilat
        GET_DATA,'fa_vel',DATA=vel
        
        ephemI_dBpSpec = VALUE_LOCATE(mlt.x,timeFFT)
        
        magOrb   = orb.y[ephemI_dBpSpec]
        magAlt   = alt.y[ephemI_dBpSpec]	
        magMLT   = mlt.y[ephemI_dBpSpec]	
        magILAT  = ilat.y[ephemI_dBpSpec]
        magSpd   = SQRT(vel.y[ephemI_dBpSpec,0]^2 + $
                        vel.y[ephemI_dBpSpec,1]^2 + $
                        vel.y[ephemI_dBpSpec,2]^2)*1000.0           ;Speed in m/s
        magDens  = DENS_MLT(magAlt,magMLT,OUT_VA_KM_PER_SEC=va_mlt) ;in cm^-3
        magDens *= 1.0e6                                            ;Dens est in m^-3
        va_mlt  *= 1000.                                            ;Estimate of v_A in meters
        
        ;;What are the bounds on frequency?
        ;;As noted at beginning, we estimate f_spA ≤ k_perp * λe < f_spB
        ;; const    = 2.9949602e-8 ;in meter^0.5
        const    = 2.9949602e-8  ;in meter^0.5
        freqLim  = const * SQRT(magDens) * magSpd

        sRates     = 1.D/REFORM((magz.x[fftBin_i[0,*]+1]-magz.x[fftBin_i[0,*]]))

        ;;Now get oxygen gyro freq
        ;;...Need DC mag field for this
        magDC_i = VALUE_LOCATE(magDC.time,timeFFT)

        ;;Oxygen cyclotron frequency is one of three possible upper bounds
        ;;NOTE: Use DC mag data! Despun data gives perturb. in B, which shortchanges cyc. freq.!
        oxy_cycDC = 1.6e-19*SQRT(magDC.comp1[magDC_i]^2+$
                                 magDC.comp2[magDC_i]^2+$
                                 magDC.comp3[magDC_i]^2)* $
                  1.0e-9/2.6567e-26/(2.*!DPI) ; in Hz

        fGRollers  = FGMagRolloff * sRates / (128.) ;;Rolloff for sampling at whatever frequency
        ;;************Frequency conditions************
        ;;->LOW frequency bound is given by condition f_spA ≤ k_perp * λe
        ;;->HIGH frequency bound is least of either 
        ;;   a. oxygen cyc. freq., 
        ;;   b. freq. arising from k_perp * λe ≤ f_spB, or 
        ;;   c. rolloff of the fluxgate magnetometer's recursive filter

        freqBounds = [CEIL(TRANSPOSE(freqLim*f_spA)/freqRes), $
                      FLOOR(TRANSPOSE((oxy_cycDC < (fGRollers < ( freqLim*f_spB ) ) ) ) $
                            /freqRes)]*freqRes

        IF KEYWORD_SET(override_freq_bounds) THEN BEGIN
           freqBounds[0,*] = override_freq[0]
           freqBounds[1,*] = override_freq[1]
        ENDIF
        IF KEYWORD_SET(min_freq) THEN BEGIN
           freqBounds[0,*] = freqBounds[0,*] > min_freq        
           PRINT,'Minimum frequency set to ' + STRCOMPRESS(min_freq,/REMOVE_ALL) + ' Hz'
        ENDIF
        IF KEYWORD_SET(max_freq) THEN BEGIN
           freqBounds[1,*] = freqBounds[1,*] < max_freq
           PRINT,'Maximum frequency set to ' + STRCOMPRESS(max_freq,/REMOVE_ALL) + ' Hz'
        ENDIF


        filtdBp  = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filtdBB  = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filtdBv  = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filteAV  = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filteNB  = KEYWORD_SET(include_E_near_B) ? $
                   MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0) : !NULL

        FOR k=0,FFTCount-1 DO BEGIN

           IF KEYWORD_SET(only_128Ss_data) THEN BEGIN
              IF (sRates[k] GE 1./minPeriod) OR (sRates[k] LE 1./maxPeriod) THEN CONTINUE
           ENDIF

           tmpI    = [fftBin_i[0,k]:fftBin_i[1,k]]

           ;;Check for streaks
           helper = 0
           IF k LT (FFTCount-1) THEN BEGIN
              WHILE ( fftBin_i[0,k+helper+1] EQ (fftBin_i[1,k+helper]+1) ) AND $
                 ( ( k+helper ) LT FFTCount-2  )                           AND $
                 (ABS(sRates[k]-sRates[k+helper+1]) LT 0.5) $ ;;Make sure samprates same
              DO BEGIN
                 tmpI = [tmpI,[fftBin_i[0,k+helper+1]:fftBin_i[1,k+helper+1]]]
                 helper++
              ENDWHILE
              ;; PRINT,'Catted ' + STRCOMPRESS(helper,/REMOVE_ALL) + 'FFT ind things'
              k += helper
           ENDIF

           dBp        = {TIME         : magz.x[tmpI]          , $
                         COMP1        : magz.y[tmpI]          , $
                         NCOMP        : 1                     , $
                         DATA_NAME    : 'Cross-track MagData' , $
                         VALID        : 1                     , $
                         PROJECT_NAME : 'FAST'                , $
                         UNITS_NAME   : 'nT'                  , $
                         CALIBRATED   : 1}
           dBB        = {TIME         : magy.x[tmpI]          , $
                         COMP1        : magy.y[tmpI]          , $
                         NCOMP        : 1                     , $
                         DATA_NAME    : 'along-B MagData'     , $
                         VALID        : 1                     , $
                         PROJECT_NAME : 'FAST'                , $
                         UNITS_NAME   : 'nT'                  , $
                         CALIBRATED   : 1}
           dBv        = {TIME         : magx.x[tmpI]          , $
                         COMP1        : magx.y[tmpI]          , $
                         NCOMP        : 1                     , $
                         DATA_NAME    : 'along-v MagData'     , $
                         VALID        : 1                     , $
                         PROJECT_NAME : 'FAST'                , $
                         UNITS_NAME   : 'nT'                  , $
                         CALIBRATED   : 1}

           eAVInterp  = {TIME         : magz.x[tmpI]          , $
                         COMP1        : eAVInterpDat[tmpI]    , $
                         NCOMP        : 1                     , $
                         DATA_NAME    : 'eAlongVStuff'        , $
                         VALID        : 1                     , $
                         PROJECT_NAME : 'FAST'                , $
                         UNITS_NAME   : 'mV/m'                , $
                         CALIBRATED   : 1}

           FA_FIELDS_FILTER,dBp,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,dBB,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,dBv,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,eAVInterp,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]

           filtdBp[tmpI]  = dBp.comp1
           filtdBB[tmpI] = dBB.comp1
           filtdBv[tmpI] = dBv.comp1
           filteAV[tmpI]  = eAVInterp.comp1

           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              tmpG    = {TIME         : magz.x[tmpI]       , $
                         COMP1        : eNearBInterpDat[tmpI] , $
                         NCOMP        : 1                  , $
                         DATA_NAME    : 'eNearBStuff'      , $
                         VALID        : 1                  , $
                         PROJECT_NAME : 'FAST'             , $
                         UNITS_NAME   : 'mV/m'             , $
                         CALIBRATED   : 1}
              FA_FIELDS_FILTER,tmpG,freqBounds[*,k], $
                               DB=FFTdb, $
                               POLES=[lowPole,highPole]
              filteNB[tmpI]  = tmpG.comp1
           ENDIF

        ENDFOR

        magzTmp      = {TIME         : magz.x              , $
                        COMP1        : magz.y              , $
                        NCOMP        : 1                   , $
                        DATA_NAME    : 'Cross-track dB'    , $
                        VALID        : 1                   , $
                        PROJECT_NAME : 'FAST'              , $
                        UNITS_NAME   : 'nT'                , $
                        CALIBRATED   : 1}

        dBpFilt     = {TIME         : magz.x              , $
                        COMP1        : filtdBp             , $
                        NCOMP        : 1                   , $
                        DATA_NAME    : 'X-track dB (filt)' , $
                        VALID        : 1                   , $
                        PROJECT_NAME : 'FAST'              , $
                        UNITS_NAME   : 'nT'                , $
                        CALIBRATED   : 1}

        eAlongVTmp   = {TIME         :  eAlongV.x          , $
                        COMP1        :  eAlongV.y          , $
                        NCOMP        : 1                   , $
                        VALID        : 1                   , $
                        DATA_NAME    :'E Along V'          , $
                        PROJECT_NAME : 'FAST'              , $
                        UNITS_NAME   : 'mV/m'              , $
                        CALIBRATED   : 1}

        eAlongVFilt  = {TIME         : magz.x              , $
                        COMP1        : filteAV             , $
                        NCOMP        : 1                   , $
                        VALID        : 1                   , $
                        DATA_NAME    :'EAV_intrpFilt'      , $
                        PROJECT_NAME : 'FAST'              , $
                        UNITS_NAME   : 'mV/m'              , $
                        CALIBRATED   : 1}
        
        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           eNearBTmp   = {TIME         :  eNearB.x      , $
                          COMP1        :  eNearB.y      , $
                          NCOMP        : 1              , $
                          VALID        : 1              , $
                          DATA_NAME    :'E Along V'     , $
                          PROJECT_NAME : 'FAST'         , $
                          UNITS_NAME   : 'mV/m'         , $
                          CALIBRATED   : 1}
           eNearBFilt  = {TIME         : magz.x         , $
                          COMP1        : filteNB        , $
                          NCOMP        : 1              , $
                          VALID        : 1              , $
                          DATA_NAME    :'ENB_intrpFilt' , $
                          PROJECT_NAME : 'FAST'         , $
                          UNITS_NAME   : 'mV/m'         , $
                          CALIBRATED   : 1}
        ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;iii. Fourier transform/Hanning window for E and B-field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;Transform! Make available to TPLOT! Clean!
        TRANSFORM_CLEAN_AND_LOAD,magzTmp, $
                                 /STORE, $
                                 T_NAME='dBpSpec', $
                                 PANEL_NAME='dBpPanel', $
                                 STRUCTURE=dBpSpec, $
                                 NPTS=FFTLen, $
                                 N_AVE=nFFTAvg, $
                                 SLIDE=FFTSlide, $
                                 SPECTHRESHVAL=BSpecThresh

        TRANSFORM_CLEAN_AND_LOAD,eAlongVTmp, $
                                 /STORE, $
                                 T_NAME='EAVSpec', $
                                 PANEL_NAME='eAVPanel', $
                                 STRUCTURE=eAVSpec, $
                                 NPTS=FFTLen, $
                                 N_AVE=nFFTAvg, $
                                 SLIDE=FFTSlide, $
                                 SPECTHRESHVAL=ESpecThresh

        TRANSFORM_CLEAN_AND_LOAD,dBpFilt, $
                                 /STORE, $
                                 T_NAME='dBpSpecFilt', $
                                 STRUCTURE=dBpSpecFilt, $
                                 PANEL_NAME='dBpFilt', $
                                 NPTS=FFTLen, $
                                 N_AVE=nFFTAvg, $
                                 SLIDE=FFTSlide, $
                                 SPECTHRESHVAL=BSpecThresh

        TRANSFORM_CLEAN_AND_LOAD,eAlongVFilt, $
                                 /STORE, $
                                 T_NAME='EAVSpecFilt', $
                                 PANEL_NAME='eAVFilt', $
                                 STRUCTURE=eAVSpecFilt, $
                                 NPTS=FFTLen, $
                                 N_AVE=nFFTAvg, $
                                 SLIDE=FFTSlide, $
                                 SPECTHRESHVAL=ESpecThresh

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           ;; spec = FA_FIELDS_SPEC(eNearBFilt, $
           TRANSFORM_CLEAN_AND_LOAD,eNearBTmp, $
                                    /STORE, $
                                    T_NAME='ENBSpec', $
                                    PANEL_NAME='eNBPanel', $
                                    STRUCTURE=eNBSpecFilt, $
                                    NPTS=FFTLen, $
                                    N_AVE=nFFTAvg, $
                                    SLIDE=FFTSlide, $
                                    SPECTHRESHVAL=ESpecThresh
           TRANSFORM_CLEAN_AND_LOAD,eNearBFilt, $
                                    /STORE, $
                                    T_NAME='ENBSpecFilt', $
                                    PANEL_NAME='eNBFilt', $
                                    STRUCTURE=eNBSpecFilt, $
                                    NPTS=FFTLen, $
                                    N_AVE=nFFTAvg, $
                                    SLIDE=FFTSlide, $
                                    SPECTHRESHVAL=ESpecThresh
        ENDIF

        IF ~KEYWORD_SET(no_plots) THEN BEGIN

           PREP_AND_PLOT_AS6_TPLOTS, $
              DBPSPEC=dBpSpec, $
              MSPECFILT=dBpSpecFilt, $
              EAVSPEC=eAVSpec, $
              EAVSPFILT=eAVSpecFilt, $
              ENBSPFILT=eNBSpecFilt, $
              INCLUDE_E_NEAR_B=include_E_near_B, $
              YLIM_TO_MAG_ROLLOFF=yLim_to_mag_rolloff, $
              BIGWINDOW=bigWindow, $
              SAVE_PS=save_ps, $
              PLOTDIR=outPlotDir, $
              PLOTNAME=tmpPlotName

           IF KEYWORD_SET(threshold_power_spectra_plots) THEN BEGIN
              THRESHOLD_POWER_SPECTRA_PLOTS, $
                 orbit, $
                 FREQPLOTRANGE=freqPlotRange
           ENDIF

        ENDIF

        IF KEYWORD_SET(show_maximus_events) OR KEYWORD_SET(save_lil_package) $
           OR KEYWORD_SET(compare_pFluxes__specMethod_maximus)               $
        THEN BEGIN
           LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime, $
                                    /DO_DESPUNDB, $
                                    GOOD_I=good_i, $
                                    HEMI__GOOD_I='BOTH', $
                                    /NO_MEMORY_LOAD

           ii = WHERE(maximus.orbit[good_i] EQ orbit,nOrb)

           IF ii[0] NE -1 THEN BEGIN

              great_i = good_i[ii]
              IF KEYWORD_SET(show_maximus_events) AND ~KEYWORD_SET(no_plots) THEN BEGIN
                 STORE_DATA,'alfTimes',DATA={x:cdbTime[great_i], $
                                             y:MAKE_ARRAY(nOrb,VALUE=10)}
                 OPTIONS,'alfTimes','psym',1 ;Plus
                 TPLOT_PANEL,VARIABLE='dBpSpecFilt',OPLOTVAR='alfTimes'
              ENDIF

              ;; PRINT,maximus.time[great_i]

              maximus_t = cdbTime[great_i]
              width_t   = maximus.width_time[great_i]

              magAlf_i  = VALUE_LOCATE(dBpSpec.time,maximus_t)
              magAlf_t  = dBpSpec.time[magAlf_i[UNIQ(magAlf_i)]]

              maxPFlux  = maximus.pFluxEst[great_i]

              ;; FOR lm=0,N_ELEMENTS(magAlf_t)-1 DO BEGIN
              ;;    PRINT,FORMAT='(I0,T10,A0)',lm,TIME_TO_STR(magAlf_t[lm],/MS)
              ;; ENDFOR

           ENDIF ELSE BEGIN
              PRINT,"No good Alf events here!"
              great_i    = [-1]
              maxPFlux   = -1
           ENDELSE
        ENDIF

        ;;E-Over-B Test
        ESpecSum = MAKE_ARRAY(FFTCount,/FLOAT)
        BSpecSum = MAKE_ARRAY(FFTCount,/FLOAT)
        Efreqs   = eAVSpecFilt.yAxis*1000.0
        Bfreqs   = dBpSpecFilt.yAxis*1000.0
        IF ~ARRAY_EQUAL(Efreqs,Bfreqs) THEN STOP
        FOR m=0,FFTCount-1 DO BEGIN
           ;;Frequency limits
           tmpF_i    = WHERE( ( Efreqs GE freqBounds[0,m] ) AND $
                              ( Efreqs LE freqBounds[1,m] ) )

           ;;"Intergrate," as some have it, and apply test
           ;; ESpecSum[m] = TOTAL(eAVSpecFilt.y[m,tmpF_i])
           ;; BSpecSum[m] = TOTAL(dBpSpecFilt.y[m,tmpF_i])
           ESpecSum[m] = INT_TABULATED(Efreqs[tmpF_i], $
                                       eAVSpecFilt.comp1[m,tmpF_i])
           BSpecSum[m] = INT_TABULATED(Bfreqs[tmpF_i],dBpSpecFilt.comp1[m,tmpF_i])

        ENDFOR

        ;;APPLY E-OVER-B TEST
        ;;The winning FFT will correspond to the correct sample rate, $
        ;;will meet thresholds on integrated power spectrum amplitudes, $
        ;;and will, of course, satisfy this order-of-mag E-over-B test
        E_over_B  = MAKE_ARRAY(N_ELEMENTS(BSpecSum),VALUE=0,/FLOAT)
        nz_B      = WHERE(BSpecSum GE 1e-15)
        E_over_B[nz_B]  = (SQRT(ESpecSum[nz_B])*1e-3)/(SQRT(BSpecSum[nz_B])*1e-9)
        winFFT_i  = WHERE( ( (E_over_B/va_mlt) GE 1.0/eb_to_alfven_speed )      AND $
                           ( (E_over_B/va_mlt) LE eb_to_alfven_speed     )      AND $
                           (sRates LE 1./minPeriod) AND (sRates GE 1./maxPeriod) AND $
                           (SQRT(ESpecSum) GE EIntegThresh)                      AND $
                           (SQRT(BSpecSum) GE BIntegThresh),nWinFFT)

        IF winFFT_i[0] EQ -1 THEN BEGIN
           PRINT,"Didn't get any Alfvénic ANYTHING here. Next interval (or orbit) ..."
           CONTINUE
        ENDIF

        winAlfFFT_i   = FFTBin_i[*,winFFT_i]
        diffAlfFFT_i  = winAlfFFT_i[1,*]-winAlfFFT_i[0,*]
        nWinPts       = TOTAL(diffAlfFFT_i) + nWinFFT
        winAlf_i      = MAKE_ARRAY(nWinPts,/LONG) ;The full array of Alfvénic indices stuff!

        soFar         = 0
        FOR k=0,nWinFFT-1 DO BEGIN
           next       = diffAlfFFT_i[k]
           winAlf_i[soFar:(soFar+next)] = [winAlfFFT_i[0,k]:winAlfFFT_i[1,k]]
           soFar     += next + 1
        ENDFOR
        

        PRINT,'Got ' + STRCOMPRESS(nWinPts,/REMOVE_ALL) + ' winning indices !'

        IF ~KEYWORD_SET(no_plots) THEN BEGIN

           IF KEYWORD_SET(B_and_E_specPlots) THEN BEGIN
              B_AND_E_SPECPLOTS,BSpecSum,ESpecSum, $
                                timeFFT, $
                                winFFT_i
           ENDIF

           dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])
           x_values        = UTC_TO_JULDAY(timeFFT)
           xRange          = [MIN(x_values),MAX(x_values)]
           xTickFormat     = 'LABEL_DATE'
           xTickUnits      = 'Time'

           BFieldCol       = 'Black'
           EFieldCol       = 'Red'
           pFluxCol        = 'Blue'
           ;; window          = WINDOW(DIMENSIONS=[800,600])
           margin          = [0.12, 0.12, 0.12, 0.12]


           IF KEYWORD_SET(pFluxAlongB__specPlot) THEN BEGIN

              pFBSpecMag      = SQRT(BSpecSum)*1.e-9*SQRT(ESpecSum)/mu_0
              pFMagPlot       = PLOT(x_values[winFFT_i], $
                                     pFBSpecMag[winFFT_i], $
                                     YTITLE='EM Flux along B (mW/m$^2$)', $
                                     COLOR=pFluxCol, $
                                     XRANGE=xRange, $
                                     XTICKFORMAT=xTickFormat, $
                                     XTICKUNITS=xTickUnits, $
                                     SYMBOL='*', $
                                     LINESTYLE='')

           ENDIF
           
           IF KEYWORD_SET(EoverBPlot) THEN BEGIN
              
              EoverBPlot  = PLOT(x_values, $
                                 E_over_B/va_mlt, $
                                 ;; NAME='E-f', $
                                 YTITLE='E_over_B/v$_A$', $
                                 COLOR=EFieldCol, $
                                 SYMBOL='*', $
                                 LINESTYLE='', $
                                 YRANGE=[1e-3,1e3], $
                                 /YLOG, $
                                 ;; XTITLE='Time', $
                                 ;; YTITLE='(mV/m)$^2$', $
                                 ;; YTITLE='mV/m', $
                                 XRANGE=xRange, $
                                 XTICKFORMAT=xTickFormat, $
                                 XTICKUNITS=xTickUnits) 
           ENDIF

        ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;iv. Calculate Poynting flux
        ;;v. Screenings
        ;;1. Keep all data, but let screenings happen on the fly
        ;;	a. Change in B (instrument threshold)
        ;;	b. Change in E (instrument threshold)
        ;;	c. E-over-B ratio
        ;;2. Two ideas from Chris. Could use:
        ;;	a. Requirement that Pflux be > 1 mW/m2, corresponding to visible aurora
        ;;	b. Requirement that E and B be above noise level (“but maybe it’s all noise!”)

        ;;Get Poynting flux ests
        ;;pFluxB : Poynting flux along B
        ;;pFluxP : Poynting flux perp to B and to (Bxv)xB
        ;;         (Negative sign comes out of S = 1/μ_0 * E x B 
        ;;          for {b,v,p} "velocity-based" coord system)
        IF KEYWORD_SET(full_pFlux) OR KEYWORD_SET(save_lil_package) THEN BEGIN
           pFluxB = (filtdBp*filteAV)/mu_0*1e-9
           pFluxP = (filteNB*filtdBv-1.*filtdBB*filteAV)/mu_0*1e-9

           pFluxV = (-1.)*filteNB*filtdBp/mu_0*1e-9

        ENDIF ELSE BEGIN
           pFluxB =       filtdBp*filteAV/mu_0*1e-9
           pFluxP = (-1.)*filtdBB*filteAV/mu_0*1e-9
           
           pFluxV = MAKE_ARRAY(N_ELEMENTS(filtdBB),VALUE=0.,/FLOAT)

        ENDELSE

        IF ~KEYWORD_SET(no_plots) THEN BEGIN

           IF KEYWORD_SET(pFlux_plots) THEN BEGIN
              PFLUX_PLOTS,pFluxP,pFluxB, $
                          magz, $
                          winAlf_i, $
                          MAXIMUS_PFLUX=maxPFlux, $
                          CDBTIME=cdbTime, $
                          MAXIMUS_INDS=great_i, $
                          SHOW_MAXIMUS_EVENTS=(KEYWORD_SET(show_maximus_events) AND $
                                               (great_i[0] NE -1))
           ENDIF

        ENDIF

        IF KEYWORD_SET(compare_pFluxes__specMethod_maximus) OR $
           KEYWORD_SET(save_lil_package) THEN BEGIN
           COMPARE_PFLUXES_W_MAXIMUS,nWinFFT, $
                                     winFFT_i, $
                                     winAlfFFT_i, $
                                     winAlf_i, $
                                     sRates, $
                                     pFluxB, $
                                     pFluxP, $
                                     great_i, $
                                     cdbTime, $
                                     magz, $
                                     maxPFlux, $
                                     OUT_PFSTATS=pfStats, $
                                     OUT_TAVGFFTPFLUXB=tAvgFFTPFluxB, $
                                     OUT_TAVGFFTPFLUXP=tAvgFFTPFluxP, $
                                     OUT_TAVGMAXPFLUX =tAvgMaxPFlux

        ENDIF ELSE BEGIN
           pfStats = -1
        ENDELSE

        ;;We used to get the proton cyc frequency for smoothing the e field data later
        ;;Now we don't, because the bandpass thing does all the smoothing one could hope for.
        ;; proton_cyc_freq = 1.6e-19*SQRT(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
        IF KEYWORD_SET(save_lil_package) THEN BEGIN

           GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
           GET_DATA,'ORBIT',DATA=orb
           GET_DATA,'MLT',DATA=mlt
           GET_DATA,'ALT',DATA=alt
           GET_DATA,'ILAT',DATA=ilat
           GET_DATA,'fa_vel',DATA=vel
           magMLT  = mlt.y
           magILAT = ilat.y
           magTJUL = UTC_TO_JULDAY(orb.x)
           magTUTC = orb.x

           ;; GET_FA_ORBIT,cdbTime[great_i],/TIME_ARRAY,/ALL
           ;; GET_DATA,'ORBIT',DATA=orb
           ;; GET_DATA,'MLT',DATA=mlt
           ;; GET_DATA,'ALT',DATA=alt
           ;; GET_DATA,'ILAT',DATA=ilat
           ;; GET_DATA,'fa_vel',DATA=vel

           ;; maxMLT  = mlt.y
           ;; maxILAT = ilat.y
           ;; maxTJUL = UTC_TO_JULDAY(orb.x)
           ;; maxTUTC = orb.x
           good_i = good_i[ii]

           savePackageName = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + "--orb_" + orbit_num + $
                             '--pFlux_package.sav'
           PRINT,"Saving data package to " + savePackageName + ' ...'
           SAVE,maxPFlux, $
                good_i, $
                pFluxB, $
                pFluxP, $
                pfStats, $
                filteAV,filteNB,filtdBB,filtdBp, $
                freqBounds, $
                fftBin_i,winAlf_i,winAlfFFT_i, $
                eAVSpecFilt,dBpSpecFilt, $
                tAvgFFTPFluxB, $
                tAvgFFTPFluxP, $
                tAvgMaxPFlux, $
                E_over_B, $
                winFFT_i, $
                magMLT,magILAT,magTJul,magTUTC, $
                FILENAME=outDir+'pFlux_package/'+savePackageName

           IF ~KEYWORD_SET(batch_mode) THEN BEGIN
              response = ''
              cont     = 0
              WHILE ~cont DO BEGIN
                 READ,response,PROMPT='Quit now? (y/n)'
                 CASE STRUPCASE(response) OF
                    'Y': BEGIN
                       cont = 1
                       RETURN
                    END
                    'N': BEGIN
                       cont = 1
                    END
                    ELSE: BEGIN
                       PRINT,"Invalid! Try again"
                       cont = 0
                    END
                 ENDCASE
              ENDWHILE
           ENDIF
        ENDIF

        ;;get_orbit data
        GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
        
        GET_DATA,'fa_vel',DATA=vel
        speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
        
        ;;Options. Do you want to go by magz, or filtered magz?
        magFriend        = {x:magz.x,y:magz.y}
        magFriend        = {x:magFriend.x,y:filtdBp}

        ;;get position of each mag point
        position         = MAKE_ARRAY(N_ELEMENTS(magFriend.x),/DOUBLE)
        speed_point_inds = VALUE_CLOSEST2(vel.x,magFriend.x)
        speed_mag_point  = speed[speed_point_inds]
        position[0:-2]   = TOTAL((magFriend.x[1:-1]-magFriend.x[0:-2])*speed_mag_point,/CUMULATIVE)

        ;;calculate the current from mag
        deltaBX          = DERIV(position,magFriend.y)
        jtemp            = ABS(1.0e-3*(deltaBx)/1.26e-6)
        sign_jtemp       = ABS(deltaBx)/deltaBx
        STORE_DATA,'jtemp',DATA={x:magFriend.x,y:jtemp}

        ;;Apply current threshold
        winAlf_i         = winAlf_i[WHERE(jtemp[winAlf_i] GE current_threshold)]

        IF ~KEYWORD_SET(no_plots) AND KEYWORD_SET(magJplot) THEN BEGIN

           MAGJPLOT,magz,jtemp,sign_jtemp,winAlf_i

        ENDIF

        ;;define loss cone angle
        GET_DATA,'ALT',DATA=alt
        loss_cone_alt = alt.y[0]*1000.0
        lcw = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
        GET_DATA,'ILAT',DATA=ilat
        north_south = ABS(ilat.y[0])/ilat.y[0]
        
        IF north_south EQ -1 THEN BEGIN
           e_angle = [180.-lcw,180+lcw] ; for Southern Hemis.
           ;;i_angle = [270.0,90.0]	
           ;;elimnate ram from data
           i_angle = [180.0,360.0]
           i_angle_up = [270.0,360.0]
           
        ENDIF ELSE BEGIN
           e_angle = [360.-lcw,lcw] ;	for Northern Hemis.
           ;;i_angle = [90.,270.0]
           ;;eliminate ram from data
           i_angle = [0.0,180.0]
           i_angle_up = [90.0,180.0]
           
        ENDELSE
        
        ;;get fields mode
        fields_mode = GET_FA_FIELDS('DataHdr_1032',tmpT1,tmpT2)
        
        ;;get moments/integrals of various fluxes
        ;; IF KEYWORD_SET(burst) THEN BEGIN

        ;;    GET_2DT_TS,'je_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='JEe_tot',ENERGY=energy_electrons
        ;;    GET_2DT_TS,'j_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='Je_tot',ENERGY=energy_electrons

        ;;    ;;Now loss-coners
        ;;    GET_2DT_TS,'je_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='JEe_lc',ANGLE=e_angle,ENERGY=energy_electrons
        ;;    GET_2DT_TS,'j_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle
           
        ;;    GET_2DT_TS,'je_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='JEi_tot',ENERGY=energy_ions
        ;;    GET_2DT_TS,'j_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='Ji_tot',ENERGY=energy_ions

        ;;    ;;Now players' club
        ;;    GET_2DT_TS,'je_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='JEi_up',ENERGY=energy_ions,ANGLE=i_angle
        ;;    GET_2DT_TS,'j_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
        ;;               NAME='Ji_up',ENERGY=energy_ions,ANGLE=i_angle
           
        ;; ENDIF ELSE BEGIN
           
           GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='JEe_tot',ENERGY=energy_electrons,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='Je_tot',ENERGY=energy_electrons,SC_POT=sc_pot

           ;;Now loss-coners
           GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='JEe_lc',ANGLE=e_angle, $
                          ENERGY=energy_electrons,SC_POT=sc_pot, $
                          /CALIB
           GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='Je_lc',ENERGY=energy_electrons, $
                          ANGLE=e_angle,SC_POT=sc_pot, $
                          /CALIB
           
           ;;Now all ions
           GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='JEi_tot',ENERGY=energy_ions, $
                          ANGLE=i_angle,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='Ji_tot',ENERGY=energy_ions, $
                          ANGLE=i_angle,SC_POT=sc_pot, $
                          /CALIB

           ;;Now players' club
           GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='JEi_up',ENERGY=energy_ions, $
                          ANGLE=i_angle_up,SC_POT=sc_pot, $
                          /CALIB
           GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='Ji_up',ENERGY=energy_ions, $
                          ANGLE=i_angle_up,SC_POT=sc_pot, $
                          /CALIB
        ;; ENDELSE
        
           GET_DATA,'Je_tot',DATA=tmp
           GET_DATA,'Ji_tot',DATA=tmpi
           ;;remove crap
           keep1           = WHERE(FINITE(tmp.y) NE 0 and FINITE(tmpi.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           keep2           = WHERE(ABS(tmp.y) GT 0.0 and ABS(tmpi.y) GT 0.0)
           jeTotTmp_time   = tmp.x[keep2]
           jeTotTmp        = tmp.y[keep2]
           STORE_DATA,'Je_tot',DATA={x:jeTotTmp_time,y:jeTotTmp}
           
           GET_DATA,'JEe_tot',DATA=tmp
           ;;remove crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jeeTotTmp_time  = tmp.x[keep2]
           jeeTotTmp       = tmp.y[keep2]
           STORE_DATA,'JEe_tot',DATA={x:jeeTotTmp_time,y:jeeTotTmp}
           
           GET_DATA,'Je_lc',DATA=tmp
           ;;remove_crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jeLCTmp_time    = tmp.x[keep2]
           jeLCTmp         = tmp.y[keep2]
           STORE_DATA,'Je_lc',DATA={x:jeLCTmp_time,y:jeLCTmp}
           
           GET_DATA,'JEe_lc',DATA=tmp
           ;;remove crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jeeLC_time      = tmp.x[keep2]
           jeeLC           = tmp.y[keep2]
           STORE_DATA,'JEe_lc',DATA={x:jeeLC_time,y:jeeLC}
           
           GET_DATA,'Ji_tot',DATA=tmp
           ;;remove crap	
           ;;keep1         = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep2         = WHERE(ABS(tmp.y) GT 0.0)
           jiTotTmp_time   = tmp.x[keep2]
           jiTotTmp        = 2.0*tmp.y[keep2] ;;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
           STORE_DATA,'Ji_tot',DATA={x:jiTotTmp_time,y:jiTotTmp}
           
           GET_DATA,'JEi_tot',DATA=tmp
           ;;remove crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jEiTotTmp_time  = tmp.x[keep2]
           jEiTotTmp       = tmp.y[keep2]
           STORE_DATA,'JEi_tot',DATA={x:jEiTotTmp_time,y:jEiTotTmp}
           
           GET_DATA,'JEi_up',DATA=tmp
           ;;remove crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jEiUpTmp_time   = tmp.x[keep2]
           jEiUpTmp        = tmp.y[keep2]
           STORE_DATA,'JEi_up',DATA={x:jEiUpTmp_time,y:jEiUpTmp}
           
           GET_DATA,'Ji_up',DATA=tmp
           ;;remove crap
           ;;keep          = WHERE(FINITE(tmp.y) NE 0)
           tmp.x           = tmp.x[keep1]
           tmp.y           = tmp.y[keep1]
           ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
           jiUpTmp_time    = tmp.x[keep2]
           jiUpTmp         = 2.0*tmp.y[keep2] ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
           STORE_DATA,'Ji_up',DATA={x:jiUpTmp_time,y:jiUpTmp}
        
        
           IF KEYWORD_SET(save_particle_data) THEN BEGIN
              SAVE_PARTICLE_DATA,ptclFile, $
                                 {x:jiTotTmp_time,y:jiTotTmp}, $
                                 {x:jEiTotTmp_time,y:jEiTotTmp}, $
                                 {x:jeeLC_time,y:jeeLC}, $
                                 {x:jeLCTmp_time,y:jeLCTmp}, $
                                 {x:jeeTotTmp_time,y:jeeTotTmp}, $
                                 {x:jeTotTmp_time,y:jeTotTmp}, $
                                 {x:jiUpTmp_time,y:jiUpTmp}, $
                                 {x:jEiUpTmp_time,y:jEiUpTmp}
              
              ;; Ji_tot  = {x:jiTotTmp_time,y:jiTotTmp}
              ;; JEi_tot = {x:jEiTotTmp_time,y:jEiTotTmp}
              ;; JEe_lc  = {x:jeeLC_time,y:jeeLC}
              ;; Je_lc   = {x:jeLCTmp_time,y:jeLCTmp}
              ;; JEe_tot = {x:jeeTotTmp_time,y:jeeTotTmp}
              ;; Je_tot  = {x:jeTotTmp_time,y:jeTotTmp}
              ;; Ji_up   = {x:jiUpTmp_time,y:jiUpTmp}
              ;; JEi_up  = {x:jEiUpTmp_time,y:jEiUpTmp}

              ;; Ji_tot  = !NULL
              ;; JEi_tot = !NULL
              ;; JEe_lc  = !NULL
              ;; Je_lc   = !NULL
              ;; JEe_tot = !NULL
              ;; Je_tot  = !NULL
              ;; Ji_up   = !NULL
              ;; JEi_up  = !NULL
           ENDIF


        ;;get ion end electron characteristic energies
        chare      = (jeeLC/jeLCTmp)*6.242*1.0e11
        chare_tot  = (jeeTotTmp/jeTotTmp)*6.242*1.0e11
        charei     = (jEiUpTmp/jiUpTmp)*6.242*1.0e11
        STORE_DATA,'CharE',DATA={x:jeeLC_time,y:chare}
        STORE_DATA,'CharE_tot',DATA={x:jeeTotTmp_time,y:chare_tot}
        STORE_DATA,'CharEi',DATA={x:jEiUpTmp_time,y:charei}
        
        ;;Scale electron energy flux to 100km, pos flux earthward
        GET_DATA,'ILAT',DATA=tmp
        sgn_flx = tmp.y/ABS(tmp.y)
        GET_DATA,'B_model',DATA=tmp1
        GET_DATA,'BFOOT',DATA=tmp2
        mag1 = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
        mag2 = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
        ratio = mag2/mag1
        jee_ionos_tmp_data = sgn_flx*jeeLC*ratio
        STORE_DATA,'JEei',DATA={x:jeeLC_time,y:jee_ionos_tmp_data}
        
        jee_tot_ionos_tmp_data = sgn_flx*jeeTotTmp*ratio
        STORE_DATA,'JEei_tot',DATA={x:jeeTotTmp_time,y:jee_tot_ionos_tmp_data}
        
        ;;IF we want to save a summary plot
        IF KEYWORD_SET(ps_sumplot) THEN BEGIN
           CGPS_OPEN,outPlotDir+'Dartmouth_as6_spectral--summary--orbit' + $
                     STRCOMPRESS(orbit_num,/REMOVE_ALL)+ $
                     '_'+STRCOMPRESS(jjj,/REMOVE_ALL) + '.ps', $
                     FONT=1
           LOADCT,39
           !P.CHARSIZE = 1.3
           TPLOT,['Je_tot','CharE','JEei','Ji_tot','JEi_tot','MagZ','jtemp'] , $
                 VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[tmpT1,tmpT2]
           CGPS_CLOSE,WIDTH=1000
        ENDIF ELSE BEGIN 
           IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN
              sumWInd = 0
              WINDOW,sumWInd,XSIZE=600,YSIZE=800
              LOADCT,39
              !P.CHARSIZE = 1.3
              TPLOT,['Je_tot','CharE','JEei','Ji_tot','JEi_tot','MagZ'] , $
                    VAR_LABEL=['ALT','MLT','ILAT'], $
                    TRANGE=[tmpT1,tmpT2],WINDOW=sumWInd
           ENDIF
        ENDELSE

        ;;define the FFT intervals
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

        
        ;;get intervals for looping
        start_points = REFORM(winAlfFFT_i[0,*])
        stop_points  = REFORM(winAlfFFT_i[1,*])
        
        FFTintervals       = MAKE_ARRAY(N_ELEMENTS(start_points),42,/DOUBLE)
        FFTintervals[*,0]  = start_points
        FFTintervals[*,1]  = stop_points
        FFTintervals[*,2]  = sign_jtemp(start_points)
        FFTintervals[*,3]  = 1
        
        intervalparts_electrons_old = -1
        intervalparts_ions_old      = -1
        valid_old                   = 0.0
        FOR j=0L,N_ELEMENTS(start_points)-1 DO BEGIN
           
           ;;define the interval points 
           itvlFields       = (FFTintervals[j,0])+FINDGEN(FFTintervals[j,1]+1-FFTintervals[j,0])
           tempz            = magFriend.y[itvlFields]
           fldRes_itvl      = magFriend.x[itvlFields]-magFriend.x[itvlFields-1] ;;Getting particle data at resolution of fields data

           ;;help,magFriend,/ST
           ;;IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'current_indices ',FFTintervals[j,0],FFTintervals[j,1]
           itvl_electrons   = WHERE(jeTotTmp_time GE magFriend.x(FFTintervals[j,0]) AND jeTotTmp_time LE magFriend.x(FFTintervals[j,1]))
           itvl_ions        = WHERE(jiUpTmp_time  GE magFriend.x(FFTintervals[j,0]) AND jiUpTmp_time  LE magFriend.x(FFTintervals[j,1]))

           IF itvl_electrons[0] EQ -1 THEN BEGIN
              minitime = MIN(ABS(jeTotTmp_time-magFriend.x(FFTintervals[j,0])),itvl_electrons)
           ENDIF

           IF itvl_ions[0] EQ -1 THEN BEGIN
              minitime = MIN(ABS(jiUpTmp_time-magFriend.x(FFTintervals[j,0])),itvl_ions)
           ENDIF

           ;;get the current from b and determine IF to keep this event
           jmax                  = MAX(jtemp[itvlFields],indjmax)
           FFTintervals[j,4]     = jmax*sign_jtemp(start_points[j])
           IF jmax LE current_threshold THEN BEGIN
              FFTintervals[j,3]  = 0.0
           ENDIF
           
           ;;define the time of the max current
           FFTintervals[j,20] = magFriend.x[itvlFields[indjmax]]
           
           
           ;;get the electron current and determine IF to keep this event
           sign                  = -1. * jeTotTmp[itvl_electrons] / ABS(jeTotTmp[itvl_electrons])
           maxJe                 = MAX(ABS(jeTotTmp[itvl_electrons]),ind)
           maxJe                 = maxJe*sign[ind]*1.6e-9 ;;in microA/m2
           FFTintervals[j,5]     = maxJe
           IF ABS(maxJe)/ABS(jmax) LE esa_j_delta_bj_ratio_threshold THEN BEGIN
              FFTintervals[j,3]  = 0.0
           ENDIF
           
           ;;get the electron energy flux and dtermine IF to keep this event
           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'itvl_electrons',itvl_electrons
           ;;help,jeeLC_time
           ;;help,jeTotTmp_time
           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'jee start stop ',TIME_TO_STR(jeeLC_time[0],/MS)   ,TIME_TO_STR(jeeLC_time(N_ELEMENTS(jeeLC_time)-1),/MS)
           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'je  start stop ',TIME_TO_STR(jeTotTmp_time[0],/MS),TIME_TO_STR(jeeLC_time(N_ELEMENTS(jeeLC_time)-1),/MS)
           
           ;;note corrected direction (i.e.-1) from Alfven_stats_4-positive is now really downwards
           sign              = jee_ionos_tmp_data[itvl_electrons]/ABS(jee_ionos_tmp_data[itvl_electrons]) 
           maxJEe_ionos      = MAX(ABS(jee_ionos_tmp_data[itvl_electrons]),ind)
           maxJEe_ionos      = maxJEe_ionos*sign[ind]
           
           sign              = jee_tot_ionos_tmp_data[itvl_electrons]/ABS(jee_tot_ionos_tmp_data[itvl_electrons])
           maxJEe_tot_ionos  = MAX(ABS(jee_tot_ionos_tmp_data[itvl_electrons]),ind)
           maxJEe_tot_ionos  = maxJEe_tot_ionos*sign[ind]



           FFTintervals[j,6]     = maxJEe_ionos
           FFTintervals[j,40]    = maxJEe_tot_ionos
           IF (ABS(maxJEe_ionos) LE electron_eflux_ionos_threshold) AND $
              ( ABS(maxJEe_tot_ionos-maxJEe_ionos) LE electron_eflux_ionos_threshold ) $
           THEN BEGIN           ;note change from previously when only downgoing fluxes where considered.
              FFTintervals[j,3]  = 0.0				      
           ENDIF
           
           ;;get width of current filament in time (s)
           time_width          = magFriend.x(FFTintervals[j,1])-magFriend.x(FFTintervals[j,0])
           
           FFTintervals[j,15]  = time_width
           ;;get width of the current filament at this altitude
           
           width               = speed_mag_point[FFTintervals[j,0]]*ABS(magFriend.x(FFTintervals[j,0])-magFriend.x(FFTintervals[j,1]))
           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'speed',speed_mag_point(FFTintervals[j,0])
           FFTintervals[j,16]  = width
           
           ;;get the integrated electron dflux in ionosphere over this interval
           IF itvl_electrons[0] NE -1 THEN BEGIN
              IF N_ELEMENTS(itvl_electrons) EQ 1 THEN BEGIN 
                 
                 FFTintervals[j,7]          = width*jeeLC[itvl_electrons]
                 FFTintervals[j,41]         = width*jeeTotTmp[itvl_electrons]
              ENDIF ELSE BEGIN
                 ;;interpolate particle data to same resolution as the fields data
                 jeeLC_fldRes_itvl      = INTERPOL(jeeLC[itvl_electrons],jeeLC_time[itvl_electrons],magFriend.x[itvlFields])
                 jeeTotTmp_fldRes_itvl  = INTERPOL(jeeTotTmp[itvl_electrons],jeeTotTmp_time[itvl_electrons],magFriend.x[itvlFields])
                 FFTintervals[j,7]          = INT_TABULATED(FINDGEN(N_ELEMENTS(itvlFields))*speed_mag_point[itvlFields]*fldRes_itvl,jeeLC_fldRes_itvl,/DOUBLE)
                 FFTintervals[j,41]         = INT_TABULATED(FINDGEN(N_ELEMENTS(itvlFields))*speed_mag_point[itvlFields]*fldRes_itvl,jeeTotTmp_fldRes_itvl,/DOUBLE)
                 
              ENDELSE
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              FFTintervals[j,7]             = FFTintervals[j,7]*SQRT(ratio(itvl_electrons[0]))
              FFTintervals[j,41]            = FFTintervals[j,41]*SQRT(ratio(itvl_electrons[0]))
           ENDIF
           
           
           
           ;;get integrated ion outflow mapped to ionosphere over this interval
           IF itvl_ions[0] NE -1 THEN BEGIN
              IF N_ELEMENTS(itvl_ions) EQ 1 THEN BEGIN 

                 
                 FFTintervals[j,12]        = width*jiTotTmp[itvl_ions]
                 FFTintervals[j,13]        = width*jiUpTmp[itvl_ions]

              ENDIF ELSE BEGIN
                 ;;interpolate particle data to same resolaution as the fields data
                 jiTotTmp_fldRes_itvl  = INTERPOL(jiTotTmp[itvl_ions],jiTotTmp_time[itvl_ions],magFriend.x[itvlFields])
                 jiUpTmp_fldRes_itvl   = INTERPOL(jiUpTmp[itvl_ions],jiUpTmp_time[itvl_ions],magFriend.x[itvlFields])
                 
                 FFTintervals[j,12]        = INT_TABULATED(FINDGEN(N_ELEMENTS(itvlFields))*speed_mag_point[itvlFields]*fldRes_itvl,jiTotTmp_fldRes_itvl,/DOUBLE)
                 FFTintervals[j,13]        = INT_TABULATED(FINDGEN(N_ELEMENTS(itvlFields))*speed_mag_point[itvlFields]*fldRes_itvl,jiUpTmp_fldRes_itvl,/DOUBLE)
                 
              ENDELSE
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              FFTintervals[j,12]           = FFTintervals[j,12]*SQRT(ratio[itvl_ions[0]])
              FFTintervals[j,13]           = FFTintervals[j,13]*SQRT(ratio[itvl_ions[0]])
           ENDIF
           
           ;;get max electron characteristic energy over this interval
           C_E                 = MAX(charE[itvl_electrons])
           C_E_tot             = MAX(charE_tot[itvl_electrons])

           FFTintervals[j,8]   = C_E
           FFTintervals[j,39]  = C_E_tot

           ;;get max upgoing ion energy flux over this interval
           maxJEi             = MAX(ABS(jEiUpTmp[itvl_ions]),ind)
           FFTintervals[j,9]  = maxJEi
           
           ;;get max ion flux over this interval
           sign_ion            = -1.*jiTotTmp[itvl_ions]/ABS(jiTotTmp[itvl_ions])
           maxJi               = MAX(ABS(jiTotTmp[itvl_ions]),ind)
           maxJi               = maxJi*sign_ion[ind]
           FFTintervals[j,10]  = maxJi
           
           ;;get max upgoing ion flux over this interval
           sign_ion            = -1.*jiUpTmp[itvl_ions]/ABS(jiUpTmp[itvl_ions])
           maxJi_up            = MAX(ABS(jiUpTmp[itvl_ions]),ind)
           maxJi_up            = maxJi_up*sign_ion[ind]
           FFTintervals[j,11]  = maxJi_up
           
           ;;get max characteristic ion energy over this interval
           C_Ei                = MAX(charEi[itvl_ions])
           FFTintervals[j,14]  = C_Ei
           
           
           
           ;;fields sample period
           FFTintervals[j,26]    = magFriend.x[itvlFields[indjmax]+1]-magFriend.x[itvlFields[indjmax]]
           
           ;;get mag field amplitude
           db                    = MAX(magFriend.y[itvlFields])-MIN(magFriend.y[itvlFields])
           median_db             = MEDIAN(magFriend.y[itvlFields])
           FFTintervals[j,17]    = db
           FFTintervals[j,24]    = median_db
           IF db LT delta_b_threshold THEN BEGIN
              FFTintervals[j,3]  = 0.0 ;threshold for reliablity of identification
           ENDIF
           
           ;;get elec field amplitude
           ;;smooth to below proton gyro freq.

           ;; smooth_int = CEIL((1./proton_cyc_freq[itvlFields[indjmax]])/FFTintervals[j,26])
           ;; IF smooth_int GT 1.0 and smooth_int LE N_ELEMENTS(itvlFields)/4.0 THEN BEGIN
           ;;    efield_smooth = SMOOTH(fields.comp2[itvlFields],smooth_int) 
           ;; ENDIF ELSE BEGIN
           ;;    efield_smooth = fields.comp2[itvlFields]
           ;; ENDELSE 
           ;; de = MAX(efield_smooth)-MIN(efield_smooth)
           ;; median_de = MEDIAN(fields.comp2[itvlFields])

           de         = MAX(filteAV[itvlFields])-MIN(filteAV[itvlFields])
           median_de  = MEDIAN(filteAV[itvlFields])

           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'de (mV/m) : ',de
           IF KEYWORD_SET(make_diagFile) THEN PRINTF,diagLun,'de (mV/m) : ',de

           FFTintervals[j,18] = de
           FFTintervals[j,25] = median_de
           IF de LT delta_E_threshold THEN BEGIN
              FFTintervals[j,3] = 0.0 ;threshold for reliablity of identification
           ENDIF
           
           FFTintervals[j,35] = 0. ;35-langmuir probe number         
           FFTintervals[j,36] = 0. ;36-max L. current over interval  
           FFTintervals[j,37] = 0. ;37-min L.current over interval   
           FFTintervals[j,38] = 0. ;38-median L.current over interval

           ;;now get orbit quantities
           GET_DATA,'ORBIT',DATA=orb
           GET_DATA,'MLT',DATA=mlt
           GET_DATA,'ALT',DATA=alt
           GET_DATA,'ILAT',DATA=ilat

           mintime = MIN(ABS(mlt.x-magFriend.x[itvlFields[indjmax]]),ind)
           
           FFTintervals[j,19] = orb.y[ind]
           FFTintervals[j,21] = alt.y[ind]	
           FFTintervals[j,22] = mlt.y[ind]	
           FFTintervals[j,23] = ilat.y[ind]
           
           ;;fields_mode
           mintime             = MIN(ABS(fields_mode.time-magFriend.x[itvlFields[indjmax]]),ind)
           FFTintervals[j,27]  = fields_mode.comp1(13,ind)
           
           ;;sc potential
           mintime             = MIN(ABS(sc_pot.x-magFriend.x[itvlFields[indjmax]]),ind)
           FFTintervals[j,34]  = -1*sc_pot.y[ind]
           
           ;;e over b test
           va                  = 1000.0*ALFVEN_SPEED_MLT(FFTintervals[j,21],FFTintervals[j,22])

           e_over_b__dEdB      = (1.0e-3*FFTintervals[j,18])/(FFTintervals[j,17]*1.0e-9)
           IF (e_over_b__dEdB/va LT 1.0/eb_to_alfven_speed) THEN BEGIN
              FFTintervals[j,3]  = 0.0
           ENDIF
           
           itvl_electrons_old  = itvl_electrons
           itvl_ions_old       = itvl_ions	
           valid_old           = FFTintervals[j,3]
        ENDFOR
        
     ENDIF ELSE BEGIN
        CONTINUE
     ENDELSE
     
     ;;remove crap data
     keep = WHERE(FFTintervals[*,3] NE 0.0)
     PRINT,'keep',keep
     IF KEYWORD_SET(keep_alfven_only) THEN BEGIN
        IF keep[0] EQ -1 THEN BEGIN
           PRINT,"No meaningful data here! Not producing file..."
           keep = !NULL
        ENDIF ELSE BEGIN
           PRINT,'number of events: ',N_ELEMENTS(keep)
           FFTintervals = FFTintervals[keep,*]
        ENDELSE
     ENDIF

     IF jjj GT 0 OR NOT KEYWORD_SET(filename) THEN filename = curfile

     PRINT,filename,jjj
     OPENW,unit1,filename,/GET_LUN
     PRINTF,unit1,N_ELEMENTS(FFTintervals[*,0]),N_ELEMENTS(keep)

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
     PRINTF,unit1,FORMAT='("total electron dflux at ionosphere from total of intervals",T68,G16.6)',TOTAL(FFTintervals[*,7])
     PRINTF,unit1,FORMAT='("total Alfven electron dflux at ionosphere",T68,G16.6)',TOTAL(FFTintervals[keep,7])
     PRINTF,unit1,FORMAT='("total ion outflow at ionosphere from single integ",T68,G16.6)',Ji_tot[jjj]
     PRINTF,unit1,FORMAT='("total ion outflow at ionosphere from total of intervals",T68,G16.6)',TOTAL(FFTintervals[*,12])
     PRINTF,unit1,FORMAT='("total Alfven ion outflow at ionosphere",T68,G16.6)',TOTAL(FFTintervals[keep,12])
     PRINTF,unit1,FORMAT='("total upward only ion outflow at ionosphere from single integ.",T68,G16.6)',Ji_up_tot[jjj]
     PRINTF,unit1,FORMAT='("total upward only ion outflow at ionosphere from total of intervals",T68,G16.6)',TOTAL(FFTintervals[*,13])
     PRINTF,unit1,FORMAT='("total Alfven upward only ion outflow at ionosphere",T68,G16.6)',TOTAL(FFTintervals[keep,13])						

     FOR jj=0L,N_ELEMENTS(FFTintervals[*,0])-1 DO BEGIN

        ;;Want pngs of each of OUR events?
        IF KEYWORD_SET(png_ourevents) THEN BEGIN
           fname = outPlotDir+'orb_' + STRCOMPRESS(orbit_num+'_'+STRING(jjj)+'_' + $
                                                   STRING(jj),/REMOVE_ALL) + $
                   '--Dart_as6_spec--event_'+STRCOMPRESS(jj,/REMOVE_ALL)+'.ps'
           plotstr = "B!Dz!N and J!Dmag!N for Dartmouth spec event " + str(jj)
           TPLOT_OPTIONS,'title',plotstr
           CGPS_OPEN,fname,FONT=1
           LOADCT,39
           !P.CHARSIZE = 1.3


           TPLOT,['MagFriend','jtemp'] ,VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[magFriend.x[FFTintervals[jj,0]],magFriend.x[FFTintervals[jj,1]]]
           CGPS_CLOSE, /PNG,/DELETE_PS, WIDTH=1000

        ENDIF
        IF KEYWORD_SET(png_lots_of_quantities_ourevents) THEN BEGIN
           STORE_DATA,'eField',DATA={x:fields.time,y:fields.comp2,yTitle:'E!Dsp!N'}
           fname = outPlotDirDir+'/orb_' + $
                   STRCOMPRESS(orbit_num+'_'+STRING(jjj)+'_'+STRING(jj),/REMOVE_ALL) + $
                   '--Dart_specEvent_'+STRCOMPRESS(jj,/REMOVE_ALL)+'.ps'
           plotstr = "Event " + str(jj)
           TPLOT_OPTIONS,'title',plotstr
           CGPS_OPEN,fname,FONT=1
           LOADCT,39
           !p.charsize = 1.3
           TPLOT,['MagFriend','jtemp','eField'] , $
                 VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[magFriend.x[FFTintervals[jj,0]],magFriend.x[FFTintervals[jj,1]]]
           CGPS_CLOSE, /PNG, /DELETE_PS
        ENDIF
        PRINTF,unit1,FORMAT='(I9,G13.6,A24,34G13.6,A24,A24)',FFTintervals[jj,19],FFTintervals[jj,3],TIME_TO_STR(FFTintervals[jj,20],/MS),$
               FFTintervals[jj,21],FFTintervals[jj,22],FFTintervals[jj,23],FFTintervals[jj,4],$
               FFTintervals[jj,5],FFTintervals[jj,6],FFTintervals[jj,40],FFTintervals[jj,7],$
               FFTintervals[jj,41],FFTintervals[jj,8],FFTintervals[jj,39],$ ;;Counting from 1, jj,39 is item 14
               FFTintervals[jj,9],FFTintervals[jj,10],FFTintervals[jj,11],FFTintervals[jj,12],$
               FFTintervals[jj,13],FFTintervals[jj,14],FFTintervals[jj,15],FFTintervals[jj,16],$
               FFTintervals[jj,17],FFTintervals[jj,18],FFTintervals[jj,26],FFTintervals[jj,27],$
               FFTintervals[jj,28],FFTintervals[jj,29],FFTintervals[jj,30],FFTintervals[jj,31],$
               FFTintervals[jj,32],FFTintervals[jj,33],FFTintervals[jj,34],FFTintervals[jj,35],$
               FFTintervals[jj,36],FFTintervals[jj,37],FFTintervals[jj,38], $
               TIME_TO_STR(magFriend.x(FFTintervals[jj,0]),/MS), $
               TIME_TO_STR(magFriend.x(FFTintervals[jj,1]),/MS)

        
     ENDFOR
     FREE_LUN,unit1

     IF KEYWORD_SET(make_diagFile) THEN BEGIN
        CLOSE,diagLun
        FREE_LUN,diagLun
     ENDIF

  ENDFOR

END

PRO TRANSFORM_CLEAN_AND_LOAD,data, $
                             STORE=store, $
                             T_NAME=T_name, $
                             PANEL_NAME=panel_name, $
                             STRUCTURE=struct, $
                             NPTS=nPts, $
                             N_AVE=n_ave, $
                             SLIDE=slide, $
                             SPECTHRESHVAL=specThreshVal
  


  spec = FA_FIELDS_SPEC(data, $
                        /STORE, $
                        T_NAME=T_name, $
                        STRUCTURE=struct, $
                        NPTS=nPts, $
                        N_AVE=n_ave, $
                        SLIDE=slide)

  IF KEYWORD_SET(panel_name) THEN BEGIN
     STORE_DATA,panel_name,DATA={x:data.time, $
                                 y:data.comp1}
  ENDIF

  GET_DATA,T_name,DATA=tmp        
  tmp.y[WHERE(~FINITE(tmp.y) OR (tmp.y LT specThreshVal) )] = 0.0
  tmp.v *= 1000.                ;To Hz
  STORE_DATA,T_name,DATA=tmp

END

PRO PREP_AND_PLOT_AS6_TPLOTS, $
   DBPSPEC=dBpSpec, $
   MSPECFILT=dBpSpecFilt, $
   EAVSPEC=eAVSpec, $
   EAVSPFILT=eAVSpecFilt, $
   ENBSPFILT=eNBSpecFilt, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   YLIM_TO_MAG_ROLLOFF=yLim_to_mag_rolloff, $
   BIGWINDOW=bigWindow, $
   SAVE_PS=save_ps, $
   PLOTDIR=plotDir, $
   PLOTNAME=tmpPlotName

  ;;Prep TPLOT nonsense
  red                     = 250
  green                   = 130
  black                   = 10

  yLimUpper         = 20

  ;;Window stuff
  xWinSize    = KEYWORD_SET(bigWindow) ? 1100 : 800
  yWinSize    = KEYWORD_SET(bigWindow) ? 800  : 600

  ;;Make some adjustments to the data
  dBpSpecLims      = [1e-3,1e2]
  eAVSpecLims      = [1.e-2,1.e2]

  OPTIONS,'dBpSpec','ytitle','Frequency!C(Hz)'
  OPTIONS,'dBpSpec','zTitle',dBpSpec.units_name
  ZLIM,'dBpSpec',dBpSpecLims[0],dBpSpecLims[1],1
  OPTIONS,'dBpSpec','panel_size',2.0
  IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'dBpSpec',0,yLimUpper,0

  OPTIONS,'dBpSpecFilt','ytitle','Frequency!C(Hz)'
  OPTIONS,'dBpSpecFilt','zTitle','E-W B-field!C!C'+dBpSpecFilt.units_name
  ZLIM,'dBpSpecFilt',dBpSpecLims[0],dBpSpecLims[1],1
  IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'dBpSpecFilt',0,yLimUpper,0
  OPTIONS,'dBpSpecFilt','panel_size',2.0

  ZLIM,'EAVSpec',eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
  IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpec',0,yLimUpper,0
  OPTIONS,'EAVSpec','ytitle','Frequency!C(Hz)'
  OPTIONS,'EAVSpec','ztitle','Log ' + eAVSpec.units_name ; z title
  OPTIONS,'EAVSpec','panel_size',2.0

  ZLIM,'EAVSpecFilt',eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
  IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpecFilt',0,yLimUpper,0
  OPTIONS,'EAVSpecFilt','ztitle','E along V!C!C' + eAVSpecFilt.units_name
  OPTIONS,'EAVSpecFilt','ytitle','Frequency!C(Hz)'
  ;; OPTIONS,'EAVSpecFilt','ztitle',eAVSpecFilt.units_name ; z title
  OPTIONS,'EAVSpecFilt','panel_size',2.0

  IF KEYWORD_SET(include_E_near_B) THEN BEGIN
     ZLIM,'ENBSpecFilt',eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
     IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'ENBSpecFilt',0,yLimUpper,0
     OPTIONS,'ENBSpecFilt','ztitle','E Near B!C!C'+eNBSpecFilt.units_name
     OPTIONS,'ENBSpecFilt','ytitle','Frequency!C(Hz)'
     OPTIONS,'ENBSpecFilt','panel_size',2.0
  ENDIF

  ;;Time to see where this is all happening. Where is the rolloff?
  ;; GET_DATA,'dBpSpec',DATA=dat 
  ;; junk = MIN(ABS(dat.x-STR_TO_TIME('1998-03-10/18:50:49')),ind) 
  ;; PRINT,ALOG10(dat.y[ind,*]) 
  ;; junkPlot = PLOT(dat.v,dat.y[ind,*],YLOG=1,YRANGE=dBpSpecLims)

  OPTIONS,'dBpPanel','ytitle','E-W B-field!C!C(nT)'
  OPTIONS,'dBpPanel','labflag',-1 ;evenly spaced
  OPTIONS,'dBpPanel','labels','NoFilt'
  OPTIONS,'dBpPanel','panel_size',2.0

  OPTIONS,'dBpFilt','colors',red
  OPTIONS,'dBpFilt','labels','Filtered'

  
  OPTIONS,'eAVPanel','labels','NoFilt'
  OPTIONS,'eAVPanel','ytitle','E along V!C!C(mV/m)'
  OPTIONS,'eAVPanel','labflag',-1 ;evenly spaced
  OPTIONS,'eAVPanel','panel_size',2.0

  OPTIONS,'eAVFilt','colors',red
  OPTIONS,'eAVFilt','labels','Filtered'

  IF KEYWORD_SET(include_E_near_B) THEN BEGIN
     OPTIONS,'eNBPanel','labels','Not Filtered'

     OPTIONS,'eNBPanel','ytitle','E Near B!C!C(mV/m)'
     OPTIONS,'eNBPanel','labflag',-1 ;evenly spaced
     OPTIONS,'eNBPanel','panel_size',2.0

     OPTIONS,'eNBFilt','colors',red
     OPTIONS,'eNBFilt','labels','Filtered'
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;PLOTS

  ;;Little test to see diff
  ;; this = dBpFilt.comp1-magztmp.comp1
  ;; plot = PLOT(this)

  ;; TPLOT,['dBpPanel','eAVPanel','dBpSpec','dBpSpecFilt','EAVSpec','EAVSpecFiltInterp']
  tPlotArr = ['dBpPanel','eAVPanel','dBpSpec','EAVSpec','dBpSpecFilt','EAVSpecFilt']
  IF KEYWORD_SET(include_E_near_B) THEN tPlotArr = [tPlotArr[0:1],'eNBPanel', $
                                                    tPlotArr[2:5],'ENBSpecFilt']

  IF KEYWORD_SET(save_ps) THEN BEGIN
     POPEN,plotDir+tmpPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
  ENDIF ELSE BEGIN
     myWindow = 8
     WINDOW,myWindow,XSIZE=xWinSize,YSIZE=yWinSize
  ENDELSE

  TPLOT,tPlotArr, $
        TRANGE=(KEYWORD_SET(t1) AND KEYWORD_SET(t2)) ? [t1,t2] : !NULL, $
        WINDOW=myWindow

  TPLOT_PANEL,VARIABLE='dBpPanel',OPLOTVAR='dBpFilt'
  TPLOT_PANEL,VARIABLE='eAVPanel',OPLOTVAR='eAVFilt'
  TPLOT_PANEL,VARIABLE='eNBPanel',OPLOTVAR='eNBFilt'

  IF KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
  ENDIF

END

;; PRO INIT_OTHER_AS6_PLOTS,timeFFT

;;   COMMON,AS6PLOT, $
;;          AS6__xVals, $
;;          AS6__xRange, $
;;          AS6__xTickFormat, $
;;          AS6__xTickUnits, $
;;          AS6__BFieldCol, $
;;          AS6__EFieldCol, $
;;          AS6__pFluxCol, $
;;          AS6__plotMargin

;;   dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])
;;   x_values        = UTC_TO_JULDAY(timeFFT)
;;   xRange          = [MIN(x_values),MAX(x_values)]
;;   xTickFormat     = 'LABEL_DATE'
;;   xTickUnits      = 'Time'

;;   BFieldCol       = 'Black'
;;   EFieldCol       = 'Red'
;;   pFluxCol        = 'Blue'
;;   window          = WINDOW(DIMENSIONS=[800,600])
;;   margin          = [0.12, 0.12, 0.12, 0.12]

;; END

PRO THRESHOLD_POWER_SPECTRA_PLOTS, $
   orbit, $
   FREQPLOTRANGE=freqPlotRange

  COMPILE_OPT idl2

  ;;***Thresholds on power spectra***
  ;;All of the following plots are for trying to figure out 
  ;;  appropriate threshold values for E and B

  GET_DATA,'EAVSpecFilt',DATA=eAVSpecFilt
  GET_DATA,'EAVSpecFilt',DATA=eAVSpecFilt

  CASE orbit OF
     6127: BEGIN
        plotTime = '1998-03-10/18:52:13.178'
     END
     9859: BEGIN
        plotTime = '1999-02-17/18:50:00'
        plotTime = '1999-02-17/18:46:20'
     END
     ELSE: BEGIN
        plotTime = TIME_TO_STR(timeFFT[0],/MSEC)
     END
  ENDCASE
  ;; plotTime = '1998-03-10/18:50:49'
  ;; plotTime = '1998-03-10/19:20:48.412'
  junk = MIN(ABS(eAVSpecFilt.x-STR_TO_TIME(plotTime)),ind)
  doze = PLOT(eAVSpecFilt.v, $
              ;; ALOG10(eAVSpecFilt.y[ind,*]), $
              eAVSpecFilt.y[ind,*], $
              YLOG=1, $
              XRANGE=freqPlotRange, $
              YRANGE=eAVSpecLims, $
              ;; YRANGE=ALOG10([dBpSpecLims[0],dBpSpecLims[1]]), $
              XTITLE='Frequency (Hz)', $
              YTITLE=eAVSpecFilt.units_name, $
              TITLE=TIME_TO_STR(eAVSpecFilt.x[ind],/MS))


  junk = MIN(ABS(dBpSpecFilt.x-STR_TO_TIME(plotTime)),ind)
  doze = PLOT(dBpSpecFilt.v, $
              ;; ALOG10(dBpSpecFilt.y[ind,*]), $
              dBpSpecFilt.y[ind,*], $
              YLOG=1, $
              XRANGE=freqPlotRange, $
              ;; YRANGE=ALOG10([dBpSpecLims[0],dBpSpecLims[1]]), $
              YRANGE=[dBpSpecLims[0],dBpSpecLims[1]], $
              XTITLE='Frequency (Hz)', $
              YTITLE=dBpSpecFilt.units_name, $
              TITLE=TIME_TO_STR(dBpSpecFilt.x[ind],/MS))

END

PRO B_AND_E_SPECPLOTS,BSpecSum,ESpecSum, $
                      timeFFT, $
                      winFFT_i

  COMPILE_OPT idl2

  dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])
  x_values        = UTC_TO_JULDAY(timeFFT)
  xRange          = [MIN(x_values),MAX(x_values)]
  xTickFormat     = 'LABEL_DATE'
  xTickUnits      = 'Time'

  BFieldCol       = 'Black'
  EFieldCol       = 'Red'
  pFluxCol        = 'Blue'
  window          = WINDOW(DIMENSIONS=[800,600])
  margin          = [0.12, 0.12, 0.12, 0.12]

  BPlot       = PLOT(x_values[winFFT_i], $
                     (SQRT(BSpecSum))[winFFT_i], $
                     ;; NAME='E-f', $
                     COLOR=BFieldCol, $
                     SYMBOL='+', $
                     LINESTYLE='', $
                     AXIS_STYLE=1, $
                     ;; XTITLE='Time', $
                     ;; YTITLE='(mV/m)$^2$', $
                     YTITLE='nT', $
                     XRANGE=xRange, $
                     XTICKFORMAT=xTickFormat, $
                     XTICKUNITS=xTickUnits, $
                     MARGIN=margin, $
                     ;; /OVERPLOT, $
                     CURRENT=window)

  EPlot       = PLOT(x_values[winFFT_i], $
                     (SQRT(ESpecSum))[winFFT_i], $
                     ;; NAME='E-f', $
                     COLOR=EFieldCol, $
                     SYMBOL='*', $
                     LINESTYLE='', $
                     AXIS_STYLE=0, $
                     YRANGE=SQRT([MIN(ESpecSum),MAX(ESpecSum)]), $
                     ;; XTITLE='Time', $
                     ;; YTITLE='(mV/m)$^2$', $
                     ;; YTITLE='mV/m', $
                     XRANGE=xRange, $
                     XTICKFORMAT=xTickFormat, $
                     XTICKUNITS=xTickUnits, $
                     MARGIN=margin, $
                     ;; /OVERPLOT, $
                     CURRENT=window)

  EAxis           = AXIS('Y',LOCATION='RIGHT', $
                         TITLE='mV/m', $
                         COLOR=EFieldCol, $
                         TARGET=EPlot, $
                         AXIS_RANGE=SQRT([MIN(ESpecSum),MAX(ESpecSum)]))


END

PRO PFLUX_PLOTS,pFluxP,pFluxB, $
                magz, $
                winAlf_i, $
                MAXIMUS_PFLUX=maxPFlux, $
                CDBTIME=cdbTime, $
                MAXIMUS_INDS=great_i, $
                SHOW_MAXIMUS_EVENTS=show_maximus_events

  COMPILE_OPT idl2

  window2     = WINDOW(DIMENSIONS=[800,600])
  symTransp   = 70

  x_values    = UTC_TO_JULDAY(magz.x)

  ;; yRange      = [MIN(pFluxP),MAX(pFluxP)]
  pFluxyRange = [(MIN(pFluxP) < MIN(pFluxB)),(MAX(pFluxP) > MAX(pFluxB))]

  IF KEYWORD_SET(show_maximus_events) AND (great_i[0] NE -1) THEN BEGIN
     ;; maxPFluxRange = [MIN(maxPFlux),MAX(maxPFlux)]

     pFluxyRange[0] = pFluxyRange[0] < MIN(maxPFlux)
     pFluxyRange[1] = pFluxyRange[1] > MAX(maxPFlux)

     maxPFluxRange = pFluxYRange
  ENDIF

  @plot_stormstats_defaults.pro

  pFluxBPlot  = PLOT(x_values[winAlf_i], $
                     pFluxB[winAlf_i], $
                     NAME='Along B', $
                     COLOR=BFieldCol, $
                     SYMBOL='+', $
                     LINESTYLE='', $
                     AXIS_STYLE=KEYWORD_SET(show_maximus_events) ? 1 : !NULL, $
                     SYM_TRANSPARENCY=symTransp, $
                     ;; XTITLE='Time', $
                     ;; YTITLE='(mV/m)$^2$', $
                     YTITLE='Poynting Flux (mW/m$^2$)', $
                     XRANGE=xRange, $
                     YRANGE=pFluxyRange, $
                     XTICKFORMAT=xTickFormat, $
                     XTICKUNITS=xTickUnits, $
                     XTICKFONT_SIZE=xTickFont_size, $
                     XTICKFONT_STYLE=xTickFont_style, $
                     YTICKFONT_SIZE=yTickFont_size, $
                     YTICKFONT_STYLE=yTickFont_style, $
                     MARGIN=KEYWORD_SET(show_maximus_events) ? margin : !NULL, $
                     ;; /OVERPLOT, $
                     CURRENT=window2)

  pFluxPPlot  = PLOT(x_values[winAlf_i], $
                     pFluxP[winAlf_i], $
                     NAME='Cross-track', $
                     COLOR=EFieldCol, $
                     SYMBOL='*', $
                     LINESTYLE='', $
                     SYM_TRANSPARENCY=symTransp, $
                     YRANGE=pFluxyRange, $
                     XRANGE=xRange, $
                     XTICKFORMAT=xTickFormat, $
                     XTICKUNITS=xTickUnits, $
                     MARGIN=KEYWORD_SET(show_maximus_events) ? margin : !NULL, $
                     ;; OVERPLOT=KEYWORD_SET(show_maximus_events), $
                     /OVERPLOT, $
                     CURRENT=window2)

  IF KEYWORD_SET(show_maximus_events) AND (great_i[0] NE -1) THEN BEGIN

     maxPFluxPlot  = PLOT(UTC_TO_JULDAY(cdbTime[great_i]), $
                          maxPFlux, $
                          NAME='IAW Database', $
                          COLOR='Blue', $
                          SYMBOL='+', $
                          LINESTYLE='', $
                          AXIS_STYLE=0, $
                          SYM_TRANSPARENCY=symTransp, $
                          YRANGE=maxPFluxRange, $
                          XRANGE=xRange, $
                          XTICKFORMAT=xTickFormat, $
                          XTICKUNITS=xTickUnits, $
                          MARGIN=margin, $
                          ;; /OVERPLOT, $
                          CURRENT=window2)

     IAWFluxAxis  = AXIS('Y',LOCATION='RIGHT', $
                         TITLE='(mW/m$^2$)', $
                         COLOR='Blue', $
                         TARGET=maxPFluxPlot, $
                         AXIS_RANGE=maxPFluxRange)

     legend = LEGEND(TARGET=[pFluxBPlot,pFluxPPlot,maxPFluxPlot], $
                     FONT_SIZE=14, $
                     /NORMAL, $
                     POSITION=[0.38,0.88])
  ENDIF ELSE BEGIN

     legend = LEGEND(TARGET=[pFluxBPlot,pFluxPPlot], $
                     FONT_SIZE=14, $
                     /NORMAL, $
                     POSITION=[0.38,0.88])
  ENDELSE
  ;; pFluxBPlot  = PLOT(x_values[winAlf_i], $
  ;;                     pFluxB[winAlf_i], $
  ;;                    ;; NAME='E-f', $
  ;;                    COLOR=BFieldCol, $
  ;;                    SYMBOL='+', $
  ;;                    LINESTYLE='', $
  ;;                    AXIS_STYLE=1, $
  ;;                    SYM_TRANSPARENCY=symTransp, $
  ;;                    ;; XTITLE='Time', $
  ;;                    ;; YTITLE='(mV/m)$^2$', $
  ;;                    YTITLE='PFlux along B!CmW/m$^2$', $
  ;;                    XRANGE=xRange, $
  ;;                    YRANGE=pFluxyRange, $
  ;;                    XTICKFORMAT=xTickFormat, $
  ;;                    XTICKUNITS=xTickUnits, $
  ;;                    MARGIN=margin, $
  ;;                    ;; /OVERPLOT, $
  ;;                    CURRENT=window2)

  ;; pFluxPPlot  = PLOT(x_values[winAlf_i], $
  ;;                    pFluxP[winAlf_i], $
  ;;                    ;; NAME='E-f', $
  ;;                    COLOR=EFieldCol, $
  ;;                    SYMBOL='*', $
  ;;                    LINESTYLE='', $
  ;;                    ;; AXIS_STYLE=0, $
  ;;                    SYM_TRANSPARENCY=symTransp, $
  ;;                    YRANGE=pFluxyRange, $
  ;;                    ;; XTITLE='Time', $
  ;;                    ;; YTITLE='(mV/m)$^2$', $
  ;;                    ;; YTITLE='mV/m', $
  ;;                    XRANGE=xRange, $
  ;;                    XTICKFORMAT=xTickFormat, $
  ;;                    XTICKUNITS=xTickUnits, $
  ;;                    MARGIN=margin, $
  ;;                    /OVERPLOT, $
  ;;                    CURRENT=window2)

  ;; pFluxPPlot  = PLOT(x_values[winAlf_i], $
  ;;                    pFluxP[winAlf_i], $
  ;;                    ;; NAME='E-f', $
  ;;                    COLOR=EFieldCol, $
  ;;                    SYMBOL='*', $
  ;;                    LINESTYLE='', $
  ;;                    AXIS_STYLE=0, $
  ;;                    SYM_TRANSPARENCY=symTransp, $
  ;;                    YRANGE=[MIN(pFluxP),MAX(pFluxP)], $
  ;;                    ;; XTITLE='Time', $
  ;;                    ;; YTITLE='(mV/m)$^2$', $
  ;;                    ;; YTITLE='mV/m', $
  ;;                    XRANGE=xRange, $
  ;;                    XTICKFORMAT=xTickFormat, $
  ;;                    XTICKUNITS=xTickUnits, $
  ;;                    MARGIN=margin, $
  ;;                    ;; /OVERPLOT, $
  ;;                    CURRENT=window2)

  ;; pFluxPAxis  = AXIS('Y',LOCATION='RIGHT', $
  ;;                        TITLE='Cross-track pFlux!C(mW/m$^2$)', $
  ;;                        COLOR=EFieldCol, $
  ;;                        TARGET=pFluxPPlot, $
  ;;                        AXIS_RANGE=[MIN(pFluxP),MAX(pFluxP)])

 END

PRO MAGJPLOT,magz,jtemp,sign_jtemp,winAlf_i

  COMPILE_OPT idl2

  dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])
  x_values        = UTC_TO_JULDAY(magz.x)
  xRange          = [MIN(x_values),MAX(x_values)]
  xTickFormat     = 'LABEL_DATE'
  xTickUnits      = 'Time'

  BFieldCol       = 'Black'
  EFieldCol       = 'Red'
  pFluxCol        = 'Blue'
  window3         = WINDOW(DIMENSIONS=[800,600])
  symTransp       = 70
  margin          = [0.12, 0.12, 0.12, 0.12]

  sjTemp          = sign_jtemp*jtemp
  jPlot           = PLOT(x_values[winAlf_i], $
                         sjTemp[winAlf_i], $
                         ;; NAME='E-f', $
                         COLOR='Black', $
                         SYMBOL='+', $
                         LINESTYLE='', $
                         AXIS_STYLE=1, $
                         SYM_TRANSPARENCY=symTransp, $
                         ;; XTITLE='Time', $
                         ;; YTITLE='(mV/m)$^2$', $
                         YTITLE='Alf Current ($\mu$A/m$^2$)', $
                         XRANGE=xRange, $
                         YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
                         XTICKFORMAT=xTickFormat, $
                         XTICKUNITS=xTickUnits, $
                         MARGIN=margin, $
                         ;; /OVERPLOT, $
                         CURRENT=window3)

END

PRO COMPARE_PFLUXES_W_MAXIMUS,nWinFFT, $
                              winFFT_i, $
                              winAlfFFT_i, $
                              winAlf_i, $
                              sRates, $
                              pFluxB, $
                              pFluxP, $
                              great_i, $
                              cdbTime, $
                              magz, $
                              maxPFlux, $
                              OUT_PFSTATS=pfStats, $
                              OUT_TAVGFFTPFLUXB=tAvgFFTPFluxB, $
                              OUT_TAVGFFTPFLUXP=tAvgFFTPFluxP, $
                              OUT_TAVGMAXPFLUX =tAvgMaxPFlux

  COMPILE_OPT idl2

  tAvgFFTPFluxB = MAKE_ARRAY(nWinFFT,/FLOAT,VALUE=-999.)
  tAvgFFTPFluxP = MAKE_ARRAY(nWinFFT,/FLOAT,VALUE=-999.)
  tAvgMaxPFlux  = MAKE_ARRAY(nWinFFT,/FLOAT,VALUE=-999.)
  
  totTime       = 0.
  totMax_ii     = !NULL
  totWeightPFB  = 0
  totWeightPFP  = 0
  FOR k=0,nWinFFT-1 DO BEGIN
     strtMag_i     = winAlfFFT_i[0,k]
     stopMag_i     = winAlfFFT_i[1,k]
     tmpLength     = FLOAT(stopMag_i-strtMag_i+1)

     tmpTime       = tmpLength / sRates[winFFT_i[k]]

     weightPFB     = TOTAL(pFluxB[strtMag_i:stopMag_i])
     weightPFP     = TOTAL(pFluxP[strtMag_i:stopMag_i])

     tAvgFFTPfluxB[k] = weightPFB / tmpLength
     tAvgFFTPfluxP[k] = weightPFP / tmpLength

     totWeightPFB += weightPFB
     totWeightPFP += weightPFP
     totTime      += tmpTime

  ENDFOR

  IF great_i[0] NE -1 THEN BEGIN
     FOR k=0,nWinFFT-1 DO BEGIN

        ;;Get Maximus Pfluxes falling in this neighborhood
        tmpMax_ii     = WHERE((cdbTime[great_i] GE magz.x[winAlfFFT_i[0,k]]) AND $
                              (cdbTime[great_i] LE magz.x[winAlfFFT_i[1,k]]),nTmpMax)
        totMax_ii     = [totMax_ii,tmpMax_ii]

        IF nTmpMax GT 0 THEN BEGIN
           tAvgMaxPFlux[k] = TOTAL(maxPFlux[tmpMax_ii]*width_t[tmpMax_ii]) / tmpTime
        ENDIF

     ENDFOR

     totMaxPFlux  = TOTAL(maxPFlux[totMax_ii]*width_t[totMax_ii])/totTime
     meanMaxPFlux = MEAN(maxPFlux[totMax_ii])
     lgMnMaxPFlux = MEAN(ALOG10(maxPFlux[totMax_ii]))
     mednMaxPFlux = MEDIAN(maxPFlux[totMax_ii])
  ENDIF ELSE BEGIN
     totMaxPFlux  = -999
     meanMaxPFlux = -999
     lgMnMaxPFlux = -999
     mednMaxPFlux = -999
  ENDELSE

  totPFB = totWeightPFB/totTime
  totPFP = totWeightPFP/totTime

  meanPFB = MEAN(pFluxB[winAlf_i])   
  meanPFP = MEAN(pFluxP[winAlf_i])   

  lgMnPFB = MEAN(ALOG10(ABS(pFluxB[winAlf_i])))
  lgMnPFP = MEAN(ALOG10(ABS(pFluxP[winAlf_i]))) 

  mednPFB = MEDIAN(pFluxB[winAlf_i])   
  mednPFP = MEDIAN(pFluxP[winAlf_i])   

  posWinAlfB_i = CGSETINTERSECTION(winAlf_i,WHERE(pFluxB GT 0))
  negWinAlfB_i = CGSETINTERSECTION(winAlf_i,WHERE(pFluxB LT 0))

  posWinAlfP_i = CGSETINTERSECTION(winAlf_i,WHERE(pFluxP GT 0))
  negWinAlfP_i = CGSETINTERSECTION(winAlf_i,WHERE(pFluxP LT 0))

  IF posWinAlfB_i[0] NE -1 THEN BEGIN
     posMeanPFB = MEAN(pFluxB[posWinAlfB_i])   
     posLgMnPFB = MEAN(ALOG10(pFluxB[posWinAlfB_i]))
     posMednPFB = MEDIAN(pFluxB[posWinAlfB_i])   
  ENDIF ELSE BEGIN
     posMeanPFB = -999
     posLgMnPFB = -999
     posMednPFB = -999
  ENDELSE

  IF posWinAlfP_i[0] NE -1 THEN BEGIN
     posMeanPFP = MEAN(pFluxP[posWinAlfP_i])   
     posLgMnPFP = MEAN(ALOG10(pFluxP[posWinAlfP_i]))
     posMednPFP = MEDIAN(pFluxP[posWinAlfP_i])   
  ENDIF ELSE BEGIN
     posMeanPFP = -999
     posLgMnPFP = -999
     posMednPFP = -999
  ENDELSE

  IF negWinAlfB_i[0] NE -1 THEN BEGIN
     negMeanPFB = MEAN(pFluxB[negWinAlfB_i])   
     negLgMnPFB = MEAN(ALOG10(ABS(pFluxB[negWinAlfB_i])))
     negMednPFB = MEDIAN(pFluxB[negWinAlfB_i])   
  ENDIF ELSE BEGIN
     negMeanPFB = -999
     negLgMnPFB = -999
     negMednPFB = -999
  ENDELSE

  IF negWinAlfP_i[0] NE -1 THEN BEGIN
     negMeanPFP = MEAN(pFluxP[negWinAlfP_i])   
     negLgMnPFP = MEAN(ALOG10(ABS(pFluxP[negWinAlfP_i])))
     negMednPFP = MEDIAN(pFluxP[negWinAlfP_i])   
  ENDIF ELSE BEGIN
     negMeanPFP = -999
     negLgMnPFP = -999
     negMednPFP = -999
  ENDELSE

  pfStats = {total:{maximus:totMaxPFlux, $
                    PFB:totPFB, $
                    PFP:totPFP, $
                    time:totTime}, $
             mean:{maximus:meanMaxPFlux, $
                   PFB:meanPFB, $
                   PFP:meanPFP}, $
             posMean:{PFB:posMeanPFB, $
                      PFP:posMeanPFP}, $
             negMean:{PFB:negMeanPFB, $
                      PFP:negMeanPFP}, $
             logMean:{maximus:lgMnMaxPFlux, $
                      PFB:lgMnPFB, $
                      PFP:lgMnPFP}, $
             posLgMn:{PFB:posLgMnPFB, $
                      PFP:posLgMnPFP}, $
             negLgMn:{PFB:negLgMnPFB, $
                      PFP:negLgMnPFP}, $
             median:{maximus:mednMaxPFlux, $
                     PFB:mednPFB, $
                     PFP:mednPFP}, $
             posMedn:{PFB:posMednPFB, $
                      PFP:posMednPFP}, $
             negMedn:{PFB:negMednPFB, $
                      PFP:negMednPFP}}

  PRINT,'Time-averaged Poynting flux over this little period '
  PRINT,FORMAT='(A0,T25,A0,T50,A0)',"Maximus", $
        "Spec Method (along B)", $
        "Spec Method (perp)"
  FOR k=0,nWinFFT-1 DO BEGIN
     PRINT,FORMAT='(G10.4,T25,G10.4,T50,G10.4)', $
           tAvgMaxPFlux[k], $
           tAvgFFTPFluxB[k], $
           tAvgFFTPFluxP[k]
  ENDFOR

  PRINT,''
  PRINT,'Totals'
  PRINT,FORMAT='(G10.4,T25,G10.4,T50,G10.4)', $
        totMaxPFlux, $
        totPFB, $
        totPFP
  PRINT,''
  PRINT,'Straight Averages'
  PRINT,FORMAT='(G10.4,T25,G10.4,T50,G10.4)', $
        meanMaxPFlux, $
        meanPFB     , $
        meanPFP
  PRINT,''
  PRINT,'Log Averages'
  PRINT,FORMAT='(G10.4,T25,G10.4,T50,G10.4)', $
        lgMnMaxPFlux, $
        lgMnPFB, $
        lgMnPFP
  PRINT,''
  PRINT,'Medians'
  PRINT,FORMAT='(G10.4,T25,G10.4,T50,G10.4)', $
        mednMaxPFlux, $
        mednPFB, $
        mednPFP

END

PRO SAVE_PARTICLE_DATA,ptclFile, $
                       Ji_tot, $
                       JEi_tot, $
                       JEe_lc, $
                       Je_lc, $
                       JEe_tot, $
                       Je_tot, $
                       Ji_up, $
                       JEi_up 

  COMPILE_OPT idl2

  PRINT,"Saving particle data ..."
  SAVE, $
     Ji_tot  , $
     JEi_tot , $
     JEe_lc  , $
     Je_lc   , $
     JEe_tot , $
     Je_tot  , $
     Ji_up   , $
     JEi_up  , $
     FILENAME=ptclFile


END