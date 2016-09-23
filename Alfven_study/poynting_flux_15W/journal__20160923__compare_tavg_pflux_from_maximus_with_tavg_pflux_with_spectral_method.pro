;;09/23/16
PRO JOURNAL__20160923__COMPARE_TAVG_PFLUX_FROM_MAXIMUS_WITH_TAVG_PFLUX_WITH_SPECTRAL_METHOD, $
   FILENAME=filename, $
   BELOW_AURORAL_OVAL=below_auroral_oval, $
   SHOW_ALF_EVENTS_FROM_MAXIMUS=show_maximus_events, $
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
   CONT_IF_FILE_EXISTS=cont_if_file_exists, $
   SAVE_LIL_DATA_PACKAGE=save_lil_package

  outDir = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'

  IF N_ELEMENTS(full_pFlux     )     EQ 0 THEN full_pFlux          = 1
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
  override_freq  = [0.0,FGMagRolloff]
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
  delta_b_threshold               = 5.0  ; nT
  delta_E_threshold               = 10.0 ; mV/m
  esa_j_delta_bj_ratio_threshold  = 0.02
  electron_eflux_ionos_threshold  = 0.05  ;ergs/cm^2/s
  eb_to_alfven_speed              = 10.0 ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
                                ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  mu_0           = DOUBLE(4.0D*!PI*1e-7)

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  IF NOT KEYWORD_SET(energy_ions)     THEN energy_ions       = [0.,500.]   ;use 0.0 for lower bound since the sc_pot is used to set this

  ;;For plots
  xWinSize    = KEYWORD_SET(bigWindow) ? 1100 : 800
  yWinSize    = KEYWORD_SET(bigWindow) ? 800  : 600

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
              REMOVE,rmInds,keepInds
              magx = {x:db_fac.x[keepInds],y:db_fac.y[keepInds]} 
              magy = {x:db_fac.x[keepInds],y:db_fac.y[keepInds]} 
              magz = {x:db_fac.x[keepInds],y:db_fac.y[keepInds]}
           ENDIF


        ENDELSE
        
        ;;E field
        FA_FIELDS_DESPIN,efieldV58,efieldV1214,T1=tmpT1,T2=tmpT2 ;,/SLOW
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

           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              eNearB = {x:eNearB.x[eAVUniq_i],y:eNearB.y[eAVUniq_i]}
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
                          RESULT=eAlongVInterp, $
                          /INTERP, $
                          /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          ;; DELT_T=minPeriod, $
                          /TALK

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                             {TIME:eNearB.x,COMP1:eNearB.y}, $
                             RESULT=eNearBInterp, $
                             /INTERP, $
                             /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                             ;; DELT_T=minPeriod, $
                             /TALK
        ENDIF
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
        
        fftBin_i   = MAKE_ARRAY(2,num_ffts,/LONG) ;You'll want this guy. He tells you just where exactly you are.
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
        freqBounds = ROUND([TRANSPOSE(freqLim*f_spA), $
                            TRANSPOSE((oxy_cycDC < (fGRollers < ( freqLim*f_spB ) ) ) )] $
                           /freqRes)*freqRes
        ;; freqBounds = [TRANSPOSE(freqLim*f_spA), $
        ;;               TRANSPOSE((oxy_cycDC < (fGRollers < ( freqLim*f_spB ) ) ) )]
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


        ;; filtMag    = MAKE_ARRAY(3,N_ELEMENTS(magz.y),VALUE=0.0)
        filtMag    = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filtMag2   = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filtMag3   = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filteAV    = MAKE_ARRAY(N_ELEMENTS(magz.y),VALUE=0.0)
        filteNB    = KEYWORD_SET(include_E_near_B) ? $
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
                        DATA_NAME    : 'along-B MagData' , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}
           tmpBAlt2  = {  TIME         : magx.x[tmpI]          , $
                        COMP1        : magx.y[tmpI]          , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'along-v MagData' , $
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
           FA_FIELDS_FILTER,tmpB,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,tmpBAlt,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,tmpBAlt2,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
           FA_FIELDS_FILTER,tmpE,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]

           ;; filtMag[*,tmpI] = [[tmp.comp1],[tmp.comp2],[tmp.comp3]]
           ;; filteAV[tmpI]   = tmp.comp4
           ;; filtMag[*,tmpI] = [[tmp.comp1],[tmp.comp2],[tmp.comp3]]
           filtMag[tmpI]  = tmpB.comp1
           filtMag2[tmpI] = tmpBAlt.comp1
           filtMag3[tmpI] = tmpBAlt2.comp1
           filteAV[tmpI]  = tmpE.comp1

           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              tmpG    = {  TIME         : magz.x[tmpI]          , $
                           ;; COMP1        : magx.y[tmpI]          , $
                           ;; COMP2        : magy.y[tmpI]          , $
                           ;; COMP3        : magz.y[tmpI]          , $
                           COMP1        : eNearBInterp[tmpI]   , $
                           NCOMP        : 1                     , $
                           DATA_NAME    : 'eNearBStuff'        , $
                           VALID        : 1                     , $
                           PROJECT_NAME : 'FAST'                , $
                           UNITS_NAME   : 'mV/m'                , $
                           CALIBRATED   : 1}
              FA_FIELDS_FILTER,tmpG,freqBounds[*,k], $
                            DB=FFTdb, $
                            POLES=[lowPole,highPole]
              filteNB[tmpI]  = tmpG.comp1
           ENDIF

        ENDFOR

        magzTmp      = {TIME         : magz.x                , $
                        COMP1        : magz.y                , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'Cross-track dB' , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}

        magzFilt     = {TIME         : magz.x                , $
                        ;; COMP1        : filtMag[2,*]          , $
                        COMP1        : filtMag               , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'X-track dB (filt)'   , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'nT'                  , $
                        CALIBRATED   : 1}

        eAlongVTmp   = {TIME         :  eAlongV.x            , $
                        COMP1        :  eAlongV.y            , $
                        NCOMP        : 1                     , $
                        VALID        : 1                     , $
                        DATA_NAME    :'E Along V'            , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}

        eAlongVFilt  = {TIME         : magz.x                , $
                        COMP1        : filteAV               , $
                        NCOMP        : 1                     , $
                        VALID        : 1                     , $
                        DATA_NAME    :'EAV_intrpFilt'        , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}
        
        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           eNearBTmp   = {TIME         :  eNearB.x            , $
                           COMP1        :  eNearB.y            , $
                           NCOMP        : 1                     , $
                           VALID        : 1                     , $
                           DATA_NAME    :'E Along V'            , $
                           PROJECT_NAME : 'FAST'                , $
                           UNITS_NAME   : 'mV/m'                , $
                           CALIBRATED   : 1}
           eNearBFilt  = {TIME         : magz.x                , $
                           COMP1        : filteNB               , $
                           NCOMP        : 1                     , $
                           VALID        : 1                     , $
                           DATA_NAME    :'ENB_intrpFilt'        , $
                           PROJECT_NAME : 'FAST'                , $
                           UNITS_NAME   : 'mV/m'                , $
                           CALIBRATED   : 1}
        ENDIF

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

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           spec = FA_FIELDS_SPEC(eNearBFilt, $
                                 /STORE, $
                                 T_NAME='ENBSpecFilt', $
                                 STRUCTURE=eNBSpecFilt, $
                                 NPTS=FFTLen, $
                                 N_AVE=nFFTAvg, $
                                 SLIDE=FFTSlide)
        ENDIF

        ;;Clean 'em up
        GET_DATA,'EAVSpecFilt',DATA=tmpE
        GET_DATA,'MagSpecFilt',DATA=tmpB        
        tmpE.y[WHERE(~FINITE(tmpE.y) OR tmpE.y LT ESpecThresh)] = 0.0
        tmpB.y[WHERE(~FINITE(tmpB.y) OR tmpB.y LT BSpecThresh)] = 0.0
        tmpE.v *= 1000. ;To Hz
        tmpB.v *= 1000. ;To Hz

        IF KEYWORD_SET(show_maximus_events) OR KEYWORD_SET(save_lil_package) THEN BEGIN
           LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime, $
                                    /DO_DESPUNDB, $
                                    GOOD_I=good_i, $
                                    HEMI__GOOD_I='BOTH'
           ii = WHERE(maximus.orbit[good_i] EQ orbit,nOrb)

           IF KEYWORD_SET(show_maximus_events) AND ~KEYWORD_SET(no_plots) THEN BEGIN
              STORE_DATA,'alfTimes',DATA={x:cdbTime[good_i[ii]], $
                                          y:MAKE_ARRAY(nOrb,VALUE=10)}
              OPTIONS,'alfTimes','psym',1 ;Plus
              TPLOT_PANEL,VARIABLE='MagSpecFilt',OPLOTVAR='alfTimes'
           ENDIF

           ;; PRINT,maximus.time[good_i[ii]]

           magAlf_i = VALUE_LOCATE(magSpec.time,cdbTime[good_i[ii]])
           magAlf_t = magSpec.time[magAlf_i[UNIQ(magAlf_i)]]

           maxPFlux      = maximus.pFluxEst[good_i[ii]]

           ;; FOR lm=0,N_ELEMENTS(magAlf_t)-1 DO BEGIN
           ;;    PRINT,FORMAT='(I0,T10,A0)',lm,TIME_TO_STR(magAlf_t[lm],/MS)
           ;; ENDFOR

        ENDIF

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

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        IF KEYWORD_SET(full_pFlux) OR KEYWORD_SET(save_lil_package) THEN BEGIN
           pFluxB = filtMag*filteAV/mu_0 ;Poynting flux along B
           pFluxP = (filteNB*filtMag3-1.*filtMag2*filteAV)/mu_0 ;Poynting flux perp to B and to (Bxv)xB
                                ;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFluxV = (-1.)*filteNB*filtMag/mu_0

        ENDIF ELSE BEGIN
           pFluxB =       filtMag *filteAV/mu_0 ;Poynting flux along B
           pFluxP = (-1.)*filtMag2*filteAV/mu_0 ;Poynting flux perp to B and to (Bxv)xB
                                ;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ENDELSE
        pFluxB *= 1e-9 ;Junk that nano prefix in nT
        pFluxP *= 1e-9

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

           ;; GET_FA_ORBIT,cdbTime[good_i[ii]],/TIME_ARRAY,/ALL
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
           SAVE,maxPFlux,good_i,pFluxB,pFluxP,magMLT,magILAT,magTJul,magTUTC, $
                FILENAME=outDir+savePackageName

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

        ;;get_orbit data
        GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
        
        GET_DATA,'fa_vel',DATA=vel
        speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
        
        ;;Options. Do you want to go by magz, or filtered magz?
        magFriend        = {x:magz.x,y:magz.y}
        magFriend        = {x:magFriend.x,y:filtMag}

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

     ENDIF

  ENDFOR

END
