PRO ALFVEN_STATS_6_SPECTRAL, $
   FILENAME=filename, $
   BELOW_AURORAL_OVAL=below_auroral_oval, $
   SHOW_ALF_EVENTS_FROM_MAXIMUS=show_maximus_events, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   ANALYSE_NOISE=analyse_noise, $
   T1=t1, $
   T2=t2, $
   BURST=burst, $
   ONLY_FASTSRVY_DATA=only_128Ss_data, $
   OVERRIDE_FREQ_BOUNDS=override_freq_bounds, $
   UCLA_MAG_DESPIN=ucla_mag_despin, $
   KEEP_ALFVEN_ONLY=keep_alfven_only, $
   PNG_SUMPLOT=png_sumplot, $
   PNG_OUREVENTS=png_ourevents, $
   DONTSHOWPLOTS=dontShowPlots, $
   CONT_IF_FILE_EXISTS=cont_if_file_exists

  ;; COMPILE_OPT idl2
  ;; COMPILE_OPT strictArr

  outDir = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'

  IF KEYWORD_SET(png_sumplot) THEN BEGIN
     SET_PLOT_DIR,outPlotDir,/FOR_SDT,ADD_SUFF='/as6_spectral/'
  ENDIF

  IF N_ELEMENTS(ucla_mag_despin)     EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval)  EQ 0 THEN below_auroral_oval  = 1
  IF N_ELEMENTS(only_128Ss_data)     EQ 0 THEN only_128Ss_data     = 1
  IF N_ELEMENTS(yLim_to_mag_rolloff) EQ 0 THEN yLim_to_mag_rolloff = 1
  yLimUpper         = 20

  IF KEYWORD_SET(only_128Ss_data) THEN BEGIN
     PRINT,"Excluding periods for which sample rate of FG mag is ≤ 128 S/s …"
  ENDIF

  ;;The way this works is that we estimate f_spA ≤ k_perp * λe < f_spB
  ;;Frequency details under "***Frequency conditions***"
  f_spA         = 0.05
  f_spB         = 1./f_spA
  freqRes       = 0.05D         ;Frequencies rounded to this number
  ;; freqRes       = 0.1D         ;Frequencies rounded to this number
  FGMagRolloff  = 20.0          ;in Hz


  ;;For FFTs of Mag and E data
  FFTLen        = 1024
  nFFTAvg       = 1

  maxPeriod     = 1/100.
  minPeriod     = 1/132.
  eFSampFact    = 4         ;The fact is, E-field data gets sampled a million times faster!
  freqPlotRange  = [0.1,FGMagRolloff]
  override_freq  = [0.0,FGMagRolloff]
  FFTSlide      = 1.0

  ;;***Thresholds on power spectra***        
  ;;(See section with the same label below to investigate how I came up with these)
  BSpecThresh   = 0.01          ;in     nT^2/Hz
  ESpecThresh   = 0.01          ;in (mV/m)^2/Hz
  ;;...And thresholds on integrated spectra
  BIntegThresh  = 0.2           ; in nT
  EIntegThresh  = 0.2           ; in mV/m
  ebAlfRat      = 10.0

  ;;Other screenings
  current_threshold               = 1.0  ;microA/m^2
  delta_b_threshold               = 5.0  ; nT
  delta_E_threshold               = 10.0 ; mV/m
  esa_j_delta_bj_ratio_threshold  = 0.02
  electron_eflux_ionos_threshold  = 0.05  ;ergs/cm^2/s
  eb_to_alfven_speed              = 50.0 ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
                                ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  IF NOT KEYWORD_SET(energy_ions)     THEN energy_ions       = [0.,500.]   ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; IF no data exists, return to main
  t   = 0
  dat = GET_FA_EES(t,/ST)
  IF dat.valid eq 0 THEN BEGIN
     PRINT,' ERROR: No FAST electron survey data -- get_fa_ees(t,/ST) returned invalid data'
;     RETURN
  ENDIF

  ;; Electron current - line plot
  IF KEYWORD_SET(burst) THEN BEGIN
     GET_2DT_TS,'j_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  ENDIF ELSE BEGIN
     GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  endelse
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
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
  
  
  ;;throw away the first 10  points since they are often corrupted
  IF not KEYWORD_SET(burst) THEN BEGIN
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
     keep             = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr  = '--below_aur_oval'
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
  IF KEYWORD_SET(ucla_mag_despin) THEN ucla_mag_despin
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN
     tmpT1 = time_ranges[jjj,0]
     tmpT2 = time_ranges[jjj,1]

     PRINT,'time_range',TIME_TO_STR(tmpT1),TIME_TO_STR(tmpT2)
     

     ;;get orbit number for filenames		
     GET_DATA,'ORBIT',DATA=tmp
     orbit      = tmp.y[0]
     orbit_num  = STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)

                                ;filename for output file
     IF KEYWORD_SET(burst) THEN BEGIN
        curfile     = outDir + 'batch_output__burst/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr + '--burst'
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
           curfile  = outDir + 'batch_output/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr + '--ucla_mag_despin'
        ENDIF ELSE BEGIN
           curfile  = outDir + 'batch_output/'+'Dartmouth_as5_spectral_'+STRCOMPRESS(orbit_num,/REMOVE_ALL)+'_'+STRCOMPRESS(jjj,/REMOVE_ALL)+'--'+belowAurOvalStr
        ENDELSE
     ENDELSE
     
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

     sc_pot  = {x:sc_pot.time, $
                y:(-1.)*sc_pot.comp1, $ ;;Reverse sign of pot here for use with GET_2DT_TS_POT
                valid:sc_pot.valid} 

     ;; IF data_valid NE 0.0 THEN BEGIN
     IF (sc_pot.valid) AND (magDC.valid) THEN BEGIN
        
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
           mintime = MIN(ABS(tmpT1-db_fac.x),ind1)
           mintime = MIN(ABS(tmpT2-db_fac.x),ind2)
           ;;   From UCLA_MAG_DESPIN: "Field-aligned velocity-based coordinates defined as:    "
           ;;x (ind 0)-along track ((BxV)xB),
           ;;y (ind 1)-cross track (BxV), 
           ;;z (ind 2)-along B" (I added "ind" marks)
           magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} 
           magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} 
           magz = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}
        endelse
        
        ;;E field
        FA_FIELDS_DESPIN,efieldV58,efieldV1214 ;,/SLOW
        ;; GET_DATA,'E_NEAR_B',DATA=eNearB
        GET_DATA,'E_ALONG_V',DATA=eAlongV
        IF SIZE(eAlongV,/TYPE) NE 8 THEN BEGIN
           PRINT,"Couldn't get E_ALONG_V!" 
           STOP
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
                          RESULT=eAlongVInterp, $
                          /INTERP, $
                          /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          ;; DELT_T=minPeriod, $
                          /TALK
        ;; ;;Make sure we have same num points 
        ;; IF N_ELEMENTS(eAlongVInterp) NE N_ELEMENTS(magz.x) THEN BEGIN 
        ;;    PRINT,"Bogus! Why didn't these match?" 
        ;;    RETURN 
        ;; ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;ii. Restrict to periods with 128 S/s?
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;NOTE, we just use FA_FIELDS_BUFS to separate into various sampling frequencies because frankly, it does a great job.
        ;;See JOURNAL__20160916__MAKE_SURE … addressing this question, and try feeding it orbit 6127. You'll see.
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
        
        fftBin_i   = MAKE_ARRAY(2,num_ffts,/LONG)
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
        
        ;; keepMe               = WHERE(timeFFT GE 1.0,nKeepFFT)
        timeFFT              = timeFFT[0:FFTCount-1]
        fftBin_i             = fftBin_i[*,LINDGEN(FFTCount)]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get density estimates for all good times
        ;;We use the density model already in place from ALFVEN_STATS_5 for calculating omega_p
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;First, orb quantities
        GET_DATA,'ORBIT',DATA=orb
        GET_DATA,'MLT',DATA=mlt
        GET_DATA,'ALT',DATA=alt
        GET_DATA,'ILAT',DATA=ilat
        GET_DATA,'fa_vel',DATA=vel
        
        ephemI_magSpec = VALUE_LOCATE(mlt.x,timeFFT)
        
        magOrb   = orb.y[ephemI_magSpec]
        magAlt   = alt.y[ephemI_magSpec]	
        magMLT   = mlt.y[ephemI_magSpec]	
        magILAT  = ilat.y[ephemI_magSpec]
        magSpd   = SQRT(vel.y[ephemI_magSpec,0]^2 + $
                        vel.y[ephemI_magSpec,1]^2 + $
                        vel.y[ephemI_magSpec,2]^2)*1000.0                        ;Speed in m/s
        magDens  = DENS_MLT(magAlt,magMLT,OUT_VA_KM_PER_SEC=va_mlt)              ;in cm^-3
        magDens *= 1.0e6                                                         ;Dens est in m^-3
        va_mlt  *= 1000.                                                         ;Get estimate of v_A in meters

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

        fGRollers  = FGMagRolloff * sRates / (128.) ;;The rolloff for sampling at whatever frequency
        ;;************Frequency conditions************
        ;;->LOW frequency bound is given by condition f_spA ≤ k_perp * λe
        ;;->HIGH frequency bound is least of either 
        ;;   a. oxygen cyc. freq., 
        ;;   b. freq. arising from k_perp * λe ≤ f_spB, or 
        ;;   c. rolloff of the fluxgate magnetometer's recursive filter
        ;; freqBounds = ROUND([TRANSPOSE(freqLim*f_spA), $
        ;;                     TRANSPOSE((oxy_cycDC < (fGRollers < ( freqLim*f_spB ) ) ) )] $
        ;;                    /freqRes)*freqRes
        freqBounds = [TRANSPOSE(freqLim*f_spA), $
                      TRANSPOSE((oxy_cycDC < (fGRollers < ( freqLim*f_spB ) ) ) )]
        IF KEYWORD_SET(override_freq_bounds) THEN BEGIN
           freqBounds[0,*] = override_freq[0]
           freqBounds[1,*] = override_freq[1]
        ENDIF
        
        ;; filtMag    = MAKE_ARRAY(3,N_ELEMENTS(magz.y),VALUE=0.0)
        filtMag    = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filtMag2   = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filteAV    = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)

        FOR k=0,FFTCount-1 DO BEGIN

           IF KEYWORD_SET(only_128Ss_data) THEN BEGIN
              IF (sRates[k] GE 1./minPeriod) OR (sRates[k] LE 1./maxPeriod) THEN CONTINUE
           ENDIF

           tmpI    = [fftBin_i[0,k]:fftBin_i[1,k]]

           tmpB    = {  TIME         : magz.x[tmpI]          , $
                        ;; COMP1        : magx.y[tmpI]          , $
                        ;; COMP2        : magy.y[tmpI]          , $
                        ;; COMP3        : magz.y[tmpI]          , $
                        ;; COMP4        : eAlongVInterp[tmpI]   , $
                        ;; NCOMP        : 4                     , $
                        COMP1        : magz.y[tmpI]          , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'Cross-track MagData' , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}
           tmpBAlt  = {  TIME         : magy.x[tmpI]          , $
                        COMP1        : magy.y[tmpI]          , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'Cross-track MagData' , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}

           tmpE    = {  TIME         : magz.x[tmpI]          , $
                        ;; COMP1        : magx.y[tmpI]          , $
                        ;; COMP2        : magy.y[tmpI]          , $
                        ;; COMP3        : magz.y[tmpI]          , $
                        COMP1        : eAlongVInterp[tmpI]   , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'eAlongVStuff'        , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}

           ;; FA_FIELDS_FILTER,tmp,freqPlotRange
           ;; FA_FIELDS_FILTER,tmp,freqBounds[*,k]
           FA_FIELDS_FILTER,tmpB,freqBounds[*,k]
           FA_FIELDS_FILTER,tmpBAlt,freqBounds[*,k]
           FA_FIELDS_FILTER,tmpE,freqBounds[*,k]

           ;; filtMag[*,tmpI] = [[tmp.comp1],[tmp.comp2],[tmp.comp3]]
           ;; filteAV[tmpI]   = tmp.comp4
           ;; filtMag[*,tmpI] = [[tmp.comp1],[tmp.comp2],[tmp.comp3]]
           filtMag[tmpI]  = tmpB.comp1
           filtMag2[tmpI] = tmpBAlt.comp1
           filteAV[tmpI]  = tmpE.comp1
        ENDFOR

        ;;UNDER CONSTRUCTION 
        ;;I was trying to be expeditious here and filter more stuff on the fly, but 
        ;;    I think it's no use
        ;;UNDER CONSTRUCTION

        ;; fHist       = HISTOGRAM(roundedFreq, $
        ;;                         BINSIZE=freqRes/2.D,LOCATIONS=Freqs, $
        ;;                         REVERSE_INDICES=fH_revI, $
        ;;                         MIN=MIN(roundedFreq)-freqRes/4.D)
        ;; omegaP_i    = WHERE(fHist GT 0,nOmegaPEsts)
        ;; FOR k=0,nOmegaPEsts-1 DO BEGIN
        ;;    ;;Any streaks to be had?
        ;;    tmpi = omegaP_i[k]
        ;;    tmpFreq = Freqs[omegaP_i[k]]
        ;;    PRINT,tmpFreq

        ;;    IF fH_revI[tmpi] EQ fH_revI[tmpi+1] THEN CONTINUE

        ;;    tmpFreqI = fH_revI[fH_revI[tmpi]:fH_revI[tmpi+1]-1]

           
        ;;    GET_STREAKS,tmpFreqI,START_I=tmpStrt_ii,STOP_I=tmpStop_ii, $
        ;;                SINGLE_I=tmpSing_ii, $
        ;;                OUT_STREAKLENS=tmpStrkLen
        ;;    PRINT,tmpStrkLen

        ;;    nFFTsThisLoop = N_ELEMENTS(tmpFreqI)
        ;;    IF N_ELEMENTS(tmpStrkLen) EQ 1 THEN BEGIN

        ;;       filtInds     = [fftBin_i[0,tmpFreqI],fftBin_i[1,tmpFreqI]]
        ;;       nFilterLoops = 1
              
        ;;    ENDIF ELSE BEGIN
        ;;       tmpFFTBins = fftBin_i[*,tmpFreqI]
        ;;       diffs      = tmpFFTBins[0,1:-1]-tmpFFTBins[1,0:-2]

        ;;       GET_STREAKS,diffs,START_I=tmpStrt_ii,STOP_I=tmpStop_ii, $
        ;;                   SINGLE_I=tmpSing_ii, $
        ;;                   OUT_STREAKLENS=tmpStrkLen
        ;;       nFilterLoops = N_ELEMENTS(tmpStrkLen)
        ;;    ENDELSE

        ;;    PRINT,'N Filter Loops   : ' + STRCOMPRESS(nFilterLoops,/REMOVE_ALL)
        ;;    PRINT,'N FFTs this loop : ' + STRCOMPRESS(nFFTsThisLoop,/REMOVE_ALL)
        ;;    ;; tmpMagZ  = {
        ;;    ;; FA_FIELDS_FILTER,magzFilt,freqPlotRange
        ;;    ;; FA_FIELDS_FILTER,eAlongVFilt,freqPlotRange ;Warning: don't use RECURSIVE KW--
        ;;    ;;                      ;causes phase shift

        ;; ENDFOR



        ;;Now just pick up periods with 128 S/s, in case we want them
        ;; periodM     = magz.x[strtM_i+1] - magz.x[strtM_i]
        ;; periodE     = eAlongV.x[strtE_i+1] - eAlongV.x[strtE_i]
        ;; goodMStreak = WHERE(periodM LE maxPeriod AND periodM GE minPeriod,nGoodM)
        ;; goodEStreak = WHERE(periodE LE maxPeriod/eFSampFact AND $
        ;;                     periodE GE minPeriod/eFSampFact,nGoodE)

        magzTmp      = {TIME         : magz.x                , $
                        COMP1        : magz.y                , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'Cross-track dB' , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}
        ;; magzFilt     = {TIME         : magz.x                , $
        ;;                 COMP1        : magz.y                , $
        magzFilt     = {TIME         : magz.x                , $
                        ;; COMP1        : filtMag[2,*]          , $
                        COMP1        : filtMag               , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'X-track dB (filt)'   , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}
        ;; magyFilt     = {TIME         : magy.x                , $ ;remember, this is along B
        ;;                 COMP1        : filtMag[1,*]          , $
        ;;                 NCOMP        : 1                     , $
        ;;                 DATA_NAME    : 'b Along B!Do!N'      , $
        ;;                 VALID        : 1                     , $
        ;;                 PROJECT_NAME : 'FAST'                , $
        ;;                 UNITS_NAME   : 'nT'                  , $
        ;;                 CALIBRATED   : 1}
        ;; magxFilt     = {TIME         : magx.x                , $ ;remember, this along track
        ;;                 COMP1        : filtMag[0,*]          , $
        ;;                 NCOMP        : 1                     , $
        ;;                 DATA_NAME    : 'b Along Track'       , $
        ;;                 VALID        : 1                     , $
        ;;                 PROJECT_NAME : 'FAST'                , $
        ;;                 UNITS_NAME   : 'nT'                  , $
        ;;                 CALIBRATED   : 1}

        ;; eAlongVTmp   = {TIME      :  eAVTmp.x                , $
        ;;                 COMP1     :  eAVTmp.y                , $
        eAlongVTmp   = {TIME         :  eAlongV.x            , $
                        COMP1        :  eAlongV.y            , $
                        NCOMP        : 1                     , $
                        VALID        : 1                     , $
                        DATA_NAME    :'E Along V'            , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}

        ;; eAlongVFilt  = {TIME         : eAVTmp.x              , $
        ;;                 COMP1        : eAVTmp.y              , $
        ;; eAlongVFilt  = {TIME         : eAlongV.x             , $
        ;;                 COMP1        : eAlongV.y             , $
        eAlongVFilt  = {TIME         : magz.x                , $
                        COMP1        : filteAV               , $
                        NCOMP        : 1                     , $
                        VALID        : 1                     , $
                        DATA_NAME    :'EAV_intrpFilt'        , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}
        

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;iii. Fourier transform/Hanning window for E and B-field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;Transform!
        spec = FA_FIELDS_SPEC(magzTmp, $
                              /STORE, $
                              T_NAME='MagSpec', $
                              STRUCTURE=magSpec, $
                              NPTS=FFTLen, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(eAlongVTmp, $
                              /STORE, $
                              T_NAME='EAVSpec', $
                              STRUCTURE=eAVSpec, $
                              NPTS=FFTLen, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(magzFilt, $
                              /STORE, $
                              T_NAME='MagSpecFilt', $
                              STRUCTURE=magSpecFilt, $
                              NPTS=FFTLen, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)
        spec = FA_FIELDS_SPEC(eAlongVFilt, $
                              /STORE, $
                              T_NAME='EAVSpecFilt', $
                              STRUCTURE=eAVSpecFilt, $
                              NPTS=FFTLen, $
                              N_AVE=nFFTAvg, $
                              SLIDE=FFTSlide)

        ;;Make some adjustments to the data
        magSpecLims      = [1e-3,1e2]
        eAVSpecLims      = [1.e-2,1.e2]
        GET_DATA,'MagSpec',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'MagSpec',DATA=tmp
        OPTIONS,'MagSpec','ytitle','Frequency (Hz)'
        OPTIONS,'MagSpec','zTitle',magSpec.units_name
        ZLIM,'MagSpec',magSpecLims[0],magSpecLims[1],1
        OPTIONS,'MagSpec','panel_size',2.0
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'MagSpec',0,yLimUpper,0

        GET_DATA,'MagSpecFilt',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'MagSpecFilt',DATA=tmp
        OPTIONS,'MagSpecFilt','ytitle','Frequency (Hz)'
        OPTIONS,'MagSpecFilt','zTitle',magSpecFilt.units_name
        ZLIM,'MagSpecFilt',magSpecLims[0],magSpecLims[1],1
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'MagSpecFilt',0,yLimUpper,0
        OPTIONS,'MagSpecFilt','panel_size',2.0

        STORE_DATA,'MagZ',DATA=magz

        GET_DATA,'EAVSpec',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'EAVSpec',DATA=TEMPORARY(tmp)
        ZLIM,'EAVSpec',eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpec',0,yLimUpper,0
        OPTIONS,'EAVSpec','ytitle','Frequency (Hz)'
        OPTIONS,'EAVSpec','ztitle','Log ' + eAVSpec.units_name ; z title
        OPTIONS,'EAVSpec','panel_size',2.0

        GET_DATA,'EAVSpecFilt',DATA=tmp
        tmp.V *= 1000.
        STORE_DATA,'EAVSpecFilt',DATA=TEMPORARY(tmp)
        ZLIM,'EAVSpecFilt',eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
        IF KEYWORD_SET(yLim_to_mag_rolloff) THEN YLIM,'EAVSpecFilt',0,yLimUpper,0
        OPTIONS,'EAVSpecFilt','ytitle','Frequency (Hz)'
        OPTIONS,'EAVSpecFilt','ztitle',eAVSpecFilt.units_name ; z title
        OPTIONS,'EAVSpecFilt','panel_size',2.0

        ;;Time to see where this is all happening. Where is the rolloff?
        ;; GET_DATA,'MagSpec',DATA=dat 
        ;; junk = MIN(ABS(dat.x-STR_TO_TIME('1998-03-10/18:50:49')),ind) 
        ;; PRINT,ALOG10(dat.y[ind,*]) 
        ;; junkPlot = PLOT(dat.v,dat.y[ind,*],YLOG=1,YRANGE=magSpecLims)

        ;;Prep TPLOT nonsense
        red                     = 250
        green                   = 130
        black                   = 10

        STORE_DATA,'magzPanel',DATA={x:magzTmp.time, $
                                     y:magzTmp.comp1}
        OPTIONS,'magzPanel','ytitle','E-W Despun!CMag. Field (nT)'
        OPTIONS,'magzPanel','labflag',-1 ;evenly spaced
        OPTIONS,'magzPanel','labels','NoFilt'
        OPTIONS,'magzPanel','panel_size',2.0

        STORE_DATA,'magzFilt',DATA={x:magzFilt.time, $
                                    y:magzFilt.comp1}
        OPTIONS,'magzFilt','colors',red
        OPTIONS,'magzFilt','labels','Filtered'


        OPTIONS,'eAVPanel','labels','NoFilt'
        STORE_DATA,'eAVPanel',DATA={x:eAlongVTmp.time, $
                                    y:eAlongVTmp.comp1}
        OPTIONS,'eAVPanel','ytitle','E along V (mV/m)'
        OPTIONS,'eAVPanel','labflag',-1 ;evenly spaced
        OPTIONS,'eAVPanel','panel_size',2.0

        STORE_DATA,'eAVFilt',DATA={x:eAlongVFilt.time, $
                                   y:eAlongVFilt.comp1}
        OPTIONS,'eAVFilt','colors',red
        OPTIONS,'eAVFilt','labels','Filtered'

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;PLOTS

        ;;Little test to see diff
        ;; this = magzfilt.comp1-magztmp.comp1
        ;; plot = PLOT(this)

        ;; TPLOT,['magzPanel','eAVPanel','MagSpec','MagSpecFilt','EAVSpec','EAVSpecFiltInterp']
        TPLOT,['magzPanel','eAVPanel','MagSpec','EAVSpec','MagSpecFilt','EAVSpecFilt'], $
              TRANGE=(KEYWORD_SET(t1) AND KEYWORD_SET(t2)) ? [t1,t2] : !NULL
        TPLOT_PANEL,VARIABLE='magzPanel',OPLOTVAR='magzFilt'
        TPLOT_PANEL,VARIABLE='eAVPanel',OPLOTVAR='eAVFilt'

        IF KEYWORD_SET(show_maximus_events) THEN BEGIN
           LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime, $
                                    /DO_DESPUNDB, $
                                    GOOD_I=good_i, $
                                    HEMI__GOOD_I='BOTH'
           ii = WHERE(maximus.orbit[good_i] EQ orbit,nOrb)

           STORE_DATA,'alfTimes',DATA={x:cdbTime[good_i[ii]], $
                                       y:MAKE_ARRAY(nOrb,VALUE=10)}
           OPTIONS,'alfTimes','psym',1 ;Plus
           TPLOT_PANEL,VARIABLE='MagSpecFilt',OPLOTVAR='alfTimes'

           ;; PRINT,maximus.time[good_i[ii]]

           magAlf_i = VALUE_LOCATE(magSpec.time,cdbTime[good_i[ii]])
           magAlf_t = magSpec.time[magAlf_i[UNIQ(magAlf_i)]]

           FOR lm=0,N_ELEMENTS(magAlf_t)-1 DO BEGIN
              PRINT,FORMAT='(I0,T10,A0)',lm,TIME_TO_STR(magAlf_t[lm],/MS)
           ENDFOR

        ENDIF

        ;;***Thresholds on power spectra***
        ;;All of the following plots are for trying to figure out appropriate threshold values for E and B

        ;; GET_DATA,'EAVSpecFiltInterp',DATA=tmpE
        GET_DATA,'EAVSpecFilt',DATA=tmpE
        GET_DATA,'MagSpecFilt',DATA=tmpB        
        tmpE.y[WHERE(~FINITE(tmpE.y) OR tmpE.y LT ESpecThresh)] = 0.0
        tmpB.y[WHERE(~FINITE(tmpB.y) OR tmpB.y LT BSpecThresh)] = 0.0
        ;; junk = MIN(ABS(dat.x-STR_TO_TIME()),ind)

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
        junk = MIN(ABS(tmpE.x-STR_TO_TIME(plotTime)),ind)
        doze = PLOT(tmpE.v, $
                    ;; ALOG10(tmpE.y[ind,*]), $
                    tmpE.y[ind,*], $
                    YLOG=1, $
                    XRANGE=freqPlotRange, $
                    YRANGE=eAVSpecLims, $
                    ;; YRANGE=ALOG10([magSpecLims[0],magSpecLims[1]]), $
                    XTITLE='Frequency (Hz)', $
                    YTITLE=eAVSpecFilt.units_name, $
                    TITLE=TIME_TO_STR(tmpE.x[ind],/MS))


        junk = MIN(ABS(tmpB.x-STR_TO_TIME(plotTime)),ind)
        doze = PLOT(tmpB.v, $
                    ;; ALOG10(tmpB.y[ind,*]), $
                    tmpB.y[ind,*], $
                    YLOG=1, $
                    XRANGE=freqPlotRange, $
                    ;; YRANGE=ALOG10([magSpecLims[0],magSpecLims[1]]), $
                    YRANGE=[magSpecLims[0],magSpecLims[1]], $
                    XTITLE='Frequency (Hz)', $
                    YTITLE=magSpecFilt.units_name, $
                    TITLE=TIME_TO_STR(tmpB.x[ind],/MS))


        ;;E-Over-B Test
        nThings  = N_ELEMENTS(magDens)
        ESpecSum = MAKE_ARRAY(nThings,/FLOAT)
        BSpecSum = MAKE_ARRAY(nThings,/FLOAT)
        
        FOR m=0,nThings-1 DO BEGIN
           ;;Frequency limits
           tmpF_i    = WHERE(tmpE.v GE freqBounds[0,m] AND tmpE.v LE freqBounds[1,m])

           ;;"Intergrate," as some have it, and apply test
           ;; ESpecSum[m] = TOTAL(tmpE.y[m,tmpF_i])
           ;; BSpecSum[m] = TOTAL(tmpB.y[m,tmpF_i])
           ESpecSum[m] = INT_TABULATED(tmpE.v[tmpF_i],tmpE.y[m,tmpF_i])
           BSpecSum[m] = INT_TABULATED(tmpB.v[tmpF_i],tmpB.y[m,tmpF_i])

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
                           (SQRT(BSpecSum) GE BIntegThresh),nFFTWin)

        IF winFFT_i[0] EQ -1 THEN BEGIN
           PRINT,"Didn't get any Alfvénic ANYTHING here. Next interval (or orbit) ..."
           CONTINUE
        ENDIF

        winAlfFFT_i   = FFTBin_i[*,winFFT_i]
        diffAlfFFT_i  = winAlfFFT_i[1,*]-winAlfFFT_i[0,*]
        nWin          = TOTAL(diffAlfFFT_i) + nFFTWin
        winAlf_i      = MAKE_ARRAY(nWin,/LONG)

        soFar         = 0
        FOR k=0,nFFTWin-1 DO BEGIN
           next       = diffAlfFFT_i[k]
           winAlf_i[soFar:(soFar+next)] = [winAlfFFT_i[0,k]:winAlfFFT_i[1,k]]
           soFar     += next + 1
        ENDFOR
        

        PRINT,'Got ' + STRCOMPRESS(nWin,/REMOVE_ALL) + ' winning indices !'
        ;; GET_STREAKS,winFFT_i,START_I=strtWin_ii,STOP_I=stopWin_ii, $
        ;;             SINGLE_I=singleWin_ii, $
        ;;             OUT_STREAKLENS=FFTStreakLens
        ;; nFFTStreaks = N_ELEMENTS(FFTStreakLens)
        ;; magz_Alf_i  = MAKE_ARRAY(2,nFFTStreaks,/LONG)
        ;; FOR m=0,nFFTStreaks-1 DO BEGIN
        ;;    junk     = MIN(ABS(tmpB.x[winFFT_i[strtWin_ii[m]]]-magzTmp.time),magz_start_i)
        ;;    junk     = MIN(ABS(tmpB.x[winFFT_i[stopWin_ii[m]]]-magzTmp.time),magz_stop_i)
        ;;    magz_Alf_i[*,m] = [magz_start_i,magz_stop_i]
        ;; ENDFOR

        dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])
        x_values        = UTC_TO_JULDAY(timeFFT)
        xRange          = [MIN(x_values),MAX(x_values)]
        xTickFormat     = 'LABEL_DATE'
        xTickUnits      = 'Time'

        BFieldCol       = 'Black'
        EFieldCol       = 'Red'
        pFluxCol        = 'Blue'
        window          = WINDOW(DIMENSIONS=[800,600])
        margin          = [0.15, 0.25, 0.15, 0.15]

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

        mu_0           = DOUBLE(4.0D*!PI*1e-7)
        pFBMag         = SQRT(BSpecSum)*1.e-9*SQRT(ESpecSum)/mu_0
        pFMagPlot      = PLOT(x_values[winFFT_i], $
                              pFBMag[winFFT_i], $
                              YTITLE='EM Flux along B (mW/m$^2$)', $
                              COLOR=pFluxCol, $
                              XRANGE=xRange, $
                              XTICKFORMAT=xTickFormat, $
                              XTICKUNITS=xTickUnits, $
                              SYMBOL='*', $
                              LINESTYLE='')

                              
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
                           XTICKUNITS=xTickUnits);, $
                           ;; /OVERPLOT, $
                           ;; CURRENT=window)

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

        ;;Get Poynting flux ests
        pFluxB =       filtMag *filteAV/mu_0        ;Poynting flux along B
        pFluxP = (-1.)*filtMag2*filteAV/mu_0        ;Poynting flux perp to B and to (Bxv)xB
                                                    ;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ;;get the proton cyc frequency for smoothing the e field data later
        proton_cyc_freq = 1.6e-19*SQRT(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
        ;;get_orbit data
        GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
        
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
           
        endelse
        
        ;;get fields mode
        fields_mode = GET_FA_FIELDS('DataHdr_1032',tmpT1,tmpT2)
        
        ;;get moments/integrals of various fluxes
        IF KEYWORD_SET(burst) THEN BEGIN

           GET_2DT_TS,'je_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
                      NAME='JEe_tot',ENERGY=energy_electrons
           GET_2DT_TS,'j_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
                      NAME='Je_tot',ENERGY=energy_electrons

           ;;Now loss-coners
           GET_2DT_TS,'je_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
                      NAME='JEe_lc',ANGLE=e_angle,ENERGY=energy_electrons
           GET_2DT_TS,'j_2d_b','fa_eeb',T1=tmpT1,T2=tmpT2, $
                      NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle
           
           GET_2DT_TS,'je_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
                      NAME='JEi_tot',ENERGY=energy_ions
           GET_2DT_TS,'j_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
                      NAME='Ji_tot',ENERGY=energy_ions

           ;;Now players' club
           GET_2DT_TS,'je_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
                      NAME='JEi_up',ENERGY=energy_ions,ANGLE=i_angle
           GET_2DT_TS,'j_2d_b','fa_ieb',T1=tmpT1,T2=tmpT2, $
                      NAME='Ji_up',ENERGY=energy_ions,ANGLE=i_angle
           
        ENDIF ELSE BEGIN
           
           GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='JEe_tot',ENERGY=energy_electrons,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='Je_tot',ENERGY=energy_electrons,SC_POT=sc_pot

           ;;Now loss-coners
           GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='JEe_lc',ANGLE=e_angle,ENERGY=energy_electrons,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=tmpT1,T2=tmpT2, $
                          NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle,SC_POT=sc_pot
           
           GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='JEi_tot',ENERGY=energy_ions,ANGLE=i_angle,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='Ji_tot',ENERGY=energy_ions,ANGLE=i_angle,SC_POT=sc_pot

           ;;Now players' club
           GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='JEi_up',ENERGY=energy_ions,ANGLE=i_angle_up,SC_POT=sc_pot
           GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=tmpT1,T2=tmpT2, $
                          NAME='Ji_up',ENERGY=energy_ions,ANGLE=i_angle_up,SC_POT=sc_pot
           
        endelse
        
        GET_DATA,'Je_tot',DATA=tmp
        GET_DATA,'Ji_tot',DATA=tmpi
        ;;remove crap
        keep1        = WHERE(FINITE(tmp.y) NE 0 and FINITE(tmpi.y) NE 0)
        tmp.x        = tmp.x[keep1]
        tmp.y        = tmp.y[keep1]
        keep2        = WHERE(ABS(tmp.y) GT 0.0 and ABS(tmpi.y) GT 0.0)
        je_tmp_time  = tmp.x[keep2]
        je_tmp_data  = tmp.y[keep2]
        STORE_DATA,'Je_tot',DATA={x:je_tmp_time,y:je_tmp_data}
        
        GET_DATA,'JEe_lc',DATA=tmp
        ;;remove crap
        ;;keep            = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep            = WHERE(ABS(tmp.y) GT 0.0)
        jee_tmp_time      = tmp.x[keep2]
        jee_tmp_data      = tmp.y[keep2]
        STORE_DATA,'JEe_lc',DATA={x:jee_tmp_time,y:jee_tmp_data}
        
        GET_DATA,'JEe_tot',DATA=tmp
        ;;remove crap
        ;;keep            = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep            = WHERE(ABS(tmp.y) GT 0.0)
        jee_tot_tmp_time  = tmp.x[keep2]
        jee_tot_tmp_data  = tmp.y[keep2]
        STORE_DATA,'JEe_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_tmp_data}
        
        GET_DATA,'Je_lc',DATA=tmp
        ;;remove_crap
        ;;keep            = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep            = WHERE(ABS(tmp.y) GT 0.0)
        je_lc_tmp_time    = tmp.x[keep2]
        je_lc_tmp_data    = tmp.y[keep2]
        STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:je_lc_tmp_data}
        
        GET_DATA,'Ji_tot',DATA=tmp
        ;;remove crap	
        ;;keep1           = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep2           = WHERE(ABS(tmp.y) GT 0.0)
        ji_tmp_time       = tmp.x[keep2]
        ji_tmp_data       = 2.0*tmp.y[keep2] ;;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        STORE_DATA,'Ji_tot',DATA={x:ji_tmp_time,y:ji_tmp_data}
        
        GET_DATA,'JEi_tot',DATA=tmp
        ;;remove crap
        ;;keep            = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep            = WHERE(ABS(tmp.y) GT 0.0)
        jEi_tmp_time      = tmp.x[keep2]
        jEi_tmp_data      = tmp.y[keep2]
        STORE_DATA,'JEi_tot',DATA={x:jEi_tmp_time,y:jEi_tmp_data}
        
        GET_DATA,'JEi_up',DATA=tmp
        ;;remove crap
        ;;keep            = WHERE(FINITE(tmp.y) NE 0)
        tmp.x             = tmp.x[keep1]
        tmp.y             = tmp.y[keep1]
        ;;keep            = WHERE(ABS(tmp.y) GT 0.0)
        jEi_up_tmp_time   = tmp.x[keep2]
        jEi_up_tmp_data   = tmp.y[keep2]
        STORE_DATA,'JEi_up',DATA={x:jEi_up_tmp_time,y:jEi_up_tmp_data}
        
        GET_DATA,'Ji_up',DATA=tmp
        ;;remove crap
        ;;keep          = WHERE(FINITE(tmp.y) NE 0)
        tmp.x           = tmp.x[keep1]
        tmp.y           = tmp.y[keep1]
        ;;keep          = WHERE(ABS(tmp.y) GT 0.0)
        ji_up_tmp_time  = tmp.x[keep2]
        ji_up_tmp_data  = 2.0*tmp.y[keep2] ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        STORE_DATA,'Ji_up',DATA={x:ji_up_tmp_time,y:ji_up_tmp_data}
        
        
        ;;get ion end electron characteristic energies
        chare = (jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11
        chare_tot = (jee_tot_tmp_data/je_tmp_data)*6.242*1.0e11
        charei = (JEi_up_tmp_data/ji_up_tmp_data)*6.242*1.0e11
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
        ratio = mag2/mag1
        jee_ionos_tmp_data = sgn_flx*jee_tmp_data*ratio
        STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
        
        jee_tot_ionos_tmp_data = sgn_flx*jee_tot_tmp_data*ratio
        STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}
        
        GET_DATA,'fa_vel',DATA=vel
        speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
        
        ;;get position of each mag point
        old_pos = 0.
        position = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
        speed_mag_point = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
        FOR j=0L,N_ELEMENTS(magz.x)-2 DO BEGIN
           speed_point_ind = MIN(ABS(vel.x-magz.x[j]),ind)

           speed_mag_point[j] = speed[ind]
           samplingperiod = magz.x(j+1)-magz.x[j]

           position[j] = old_pos+speed_mag_point[j]*samplingperiod
           old_pos = position[j]
        endfor
        
        
        ;;calculate the total ion outflow for this interval
        part_res_ji        = MAKE_ARRAY(N_ELEMENTS(ji_up_tmp_time),/DOUBLE)
        position_ji        = MAKE_ARRAY(N_ELEMENTS(Ji_up_tmp_time),/DOUBLE)
        position_ji[0]     = 0.0
        FOR j=1,N_ELEMENTS(ji_tmp_time)-1 DO BEGIN
           part_res_ji[j]  = ABS(ji_up_tmp_time(j-1)-ji_up_tmp_time[j])

           IF part_res_ji[j] EQ 0.0 THEN part_res_ji[j] = part_res_ji(j-1)

           position_ji[j]  = position_ji(j-1)+speed[j]*part_res_Ji[j]
        endfor
        part_res_Ji[0]     = part_res_Ji[1]
        ji_tot[jjj]        = int_tabulated(position_ji,ji_tmp_data*SQRT(ratio))       ;mapped to ionosphere sqrt due to intergration in x 
        ji_up_tot[jjj]     = int_tabulated(position_ji,ji_up_tmp_data*SQRT(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        
        PRINT,'ji_tot : ',ji_tot[jjj]
        
        ;;calculate the total electron downflux at the spacecraft altitude over this interval
        part_res_je        = MAKE_ARRAY(N_ELEMENTS(jee_tmp_data),/DOUBLE)
        position_je        = MAKE_ARRAY(N_ELEMENTS(jee_tmp_time),/DOUBLE)
        position_je[0]     = 0.0
        FOR j=1,N_ELEMENTS(je_tmp_time)-1 DO BEGIN
           part_res_je[j]  = ABS(jee_tmp_time(j-1)-jee_tmp_time[j])
           IF part_res_je[j] EQ 0.0 THEN part_res_je[j] = part_res_je(j-1)
           position_je[j]  = position_je(j-1)+speed[j]*part_res_Je[j]
        endfor
        part_res_Je[0]     = part_res_Je[1]
        jee_tot[jjj]       = int_tabulated(position_je,jee_tmp_data*SQRT(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        ;;calculate the current from mag
        ;; deltaBX     = deriv(position,magz.y)
        ;; jtemp       = ABS(1.0e-3*(deltaBx)/1.26e-6)
        ;; sign_jtemp  = ABS(deltaBx)/deltaBx
        ;; STORE_DATA,'jtemp',DATA={x:magz.x,y:jtemp}

        ;;terminate the intervals before the last point
        IF sign_jtemp[N_ELEMENTS(jtemp)-1]*sign_jtemp[N_ELEMENTS(jtemp)-2] NE -1 THEN BEGIN
           sign_jtemp[N_ELEMENTS(jtemp)-1] = -1*sign_jtemp[N_ELEMENTS(jtemp)-1]
        ENDIF

        
        ;;IF we want to save a summary plot
        IF KEYWORD_SET(png_sumplot) THEN BEGIN
           CGPS_OPEN, outPlotDir+'as5Spec_orbit' + STRCOMPRESS(orbit_num+'_'+STRING(jjj), $
                                                               /REMOVE_ALL) + '.ps',FONT=1
           LOADCT,39
           !P.CHARSIZE = 1.3
           TPLOT,['Je_tot','CharE','JEei','Ji_tot','JEi_tot','MagZ','jtemp'] , $
                 VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[tmpT1,tmpT2]
           CGPS_CLOSE,/PNG,/DELETE_PS,WIDTH=1000
        ENDIF ELSE BEGIN 
           IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
              LOADCT,39
              !P.CHARSIZE = 1.3
              TPLOT,['Je_tot','CharE','JEei','Ji_tot','JEi_tot','MagZ'] , $
                    VAR_LABEL=['ALT','MLT','ILAT'], $
                    TRANGE=[tmpT1,tmpT2]
           ENDIF
        ENDELSE

        start_points = [0]
        stop_points = [0]
        
        ;;get current intervals
        FOR j=1L,N_ELEMENTS(sign_jtemp)-2 DO BEGIN

           IF sign_jtemp[j]+sign_jtemp(j-1) EQ 0.0 THEN BEGIN
              start_points = [start_points,j]
           ENDIF
           IF sign_jtemp[j]+sign_jtemp(j+1) EQ 0.0 THEN BEGIN
              stop_points = [stop_points,j]
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

        
        current_intervals = MAKE_ARRAY(N_ELEMENTS(start_points),42,/DOUBLE)
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
           ;;help,magz,/ST
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
           ;;print,'jee start stop ',TIME_TO_STR(jee_tmp_time[0],/MS),TIME_TO_STR(jee_tmp_time(N_ELEMENTS(jee_tmp_time)-1),/MS)
           ;;print,'je start stop ',TIME_TO_STR(je_tmp_time[0],/MS),TIME_TO_STR(jee_tmp_time(N_ELEMENTS(jee_tmp_time)-1),/MS)
           
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
                 current_intervals[j,7] = int_tabulated(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,jee_tmp_data_fields_res_interval,/DOUBLE)
                 current_intervals[j,41] = int_tabulated(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,jee_tot_tmp_data_fields_res_interval,/DOUBLE)
                 
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
                 ;;IF  intervalparts_ions[0] EQ intervalparts_ions_old(N_ELEMENTS(intervalparts_ions_old)-1) and valid_old EQ 1.0 THEN intervalparts_ions = intervalparts_ions(1:N_ELEMENTS(intervalparts_ions)-1)
                 ;;IF N_ELEMENTS(intervalparts_ions) EQ 1 THEN BEGIN 
                 ;;current_intervals[j,12] = speed[intervalparts_ions]*part_res_ji[intervalparts_ions]*ji_up_tmp_data[intervalparts_ions]/2.0
                 
                 ;;ENDIF ELSE BEGIN
                 
                 
                 ;;interpolate particle data to same resolaution as the fields data
                 ji_tmp_data_fields_res_interval = INTERPOL(ji_tmp_data[intervalparts_ions],ji_tmp_time[intervalparts_ions],magz.x[intervalfields])
                 ji_up_tmp_data_fields_res_interval = INTERPOL(ji_up_tmp_data[intervalparts_ions],ji_up_tmp_time[intervalparts_ions],magz.x[intervalfields])
                 
                 current_intervals[j,12] = INT_TABULATED(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,ji_tmp_data_fields_res_interval,/DOUBLE)
                 current_intervals[j,13] = INT_TABULATED(FINDGEN(N_ELEMENTS(intervalfields))*speed_mag_point[intervalfields]*fields_res_interval,ji_up_tmp_data_fields_res_interval,/DOUBLE)
                 
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
           ;; va = 1000.0*alfven_speed_mlt(current_intervals[j,21],current_intervals[j,22])
           e_over_b = (1.0e-3*current_intervals[j,18])/(current_intervals[j,17]*1.0e-9)
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
     PRINT,'keep',keep
     IF KEYWORD_SET(keep_alfven_only) THEN BEGIN
        IF keep[0] EQ -1 THEN BEGIN
           PRINT,"No meaningful data here! Not producing file..."
           keep = !NULL
        ENDIF ELSE BEGIN
           PRINT,'number of events: ',N_ELEMENTS(keep)
           current_intervals = current_intervals[keep,*]
        ENDELSE
     ENDIF

     IF jjj GT 0 or not KEYWORD_SET(filename) THEN filename = curfile

     PRINT,filename,jjj
     OPENW,unit1,filename,/GET_LUN
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
           fname = outPlotDir+'orb_' + STRCOMPRESS(orbit_num+'_'+STRING(jjj)+'_' + $
                                                   STRING(jj),/REMOVE_ALL) + $
                   '--Dart_as5Spec_event_'+STRCOMPRESS(jj,/REMOVE_ALL)+'.ps'
           plotstr = "B!Dz!N and J!Dmag!N for Dartmouth spec event " + str(jj)
           TPLOT_OPTIONS,'title',plotstr
           CGPS_OPEN,fname,FONT=1
           LOADCT,39
           !P.CHARSIZE = 1.3


           TPLOT,['MagZ','jtemp'] ,VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[magz.x[current_intervals[jj,0]],magz.x[current_intervals[jj,1]]]
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
           TPLOT,['MagZ','jtemp','eField'] , $
                 VAR_LABEL=['ALT','MLT','ILAT'], $
                 TRANGE=[magz.x[current_intervals[jj,0]],magz.x[current_intervals[jj,1]]]
           CGPS_CLOSE, /PNG, /DELETE_PS
        ENDIF
        PRINTF,unit1,FORMAT='(I9,G13.6,A24,34G13.6,A24,A24)',current_intervals[jj,19],current_intervals[jj,3],TIME_TO_STR(current_intervals[jj,20],/MS),$
               current_intervals[jj,21],current_intervals[jj,22],current_intervals[jj,23],current_intervals[jj,4],$
               current_intervals[jj,5],current_intervals[jj,6],current_intervals[jj,40],current_intervals[jj,7],$
               current_intervals[jj,41],current_intervals[jj,8],current_intervals[jj,39],$ ;;Counting from 1, jj,39 is item 14
               current_intervals[jj,9],current_intervals[jj,10],current_intervals[jj,11],current_intervals[jj,12],$
               current_intervals[jj,13],current_intervals[jj,14],current_intervals[jj,15],current_intervals[jj,16],$
               current_intervals[jj,17],current_intervals[jj,18],current_intervals[jj,26],current_intervals[jj,27],$
               current_intervals[jj,28],current_intervals[jj,29],current_intervals[jj,30],current_intervals[jj,31],$
               current_intervals[jj,32],current_intervals[jj,33],current_intervals[jj,34],current_intervals[jj,35],$
               current_intervals[jj,36],current_intervals[jj,37],current_intervals[jj,38], $
               TIME_TO_STR(magz.x(current_intervals[jj,0]),/MS), TIME_TO_STR(magz.x(current_intervals[jj,1]),/MS)

        
     endfor
     FREE_LUN,unit1


  endfor

END