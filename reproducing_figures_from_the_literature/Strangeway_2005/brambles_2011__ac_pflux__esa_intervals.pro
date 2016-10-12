;;2016/09/26
;;It's better to use the ESA intervals, methinks
PRO BRAMBLES_2011__AC_PFLUX__ESA_INTERVALS, $
   TPLOT_VARS=tplot_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   FIELDS_INTERP=do_fields_interp, $
   FIELDS_SPLINE=do_fields_spline, $
   ONLY_FASTSRVY_DATA=only_128Ss_data, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   FULL_PFLUX_CALC=full_pFlux, $
   USE_FAC_V=use_fac_v, $
   USE_FAC=use_fac, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   BATCH_MODE=batch_mode, $
   NO_HASH_UPDATE=no_hash_update

; Input needed on:
; (a) Northern/southern hemisphere limits
; (b) ESA data limits
; (c) DSP calibration

  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Brambles_et_al_2011/'
  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--full_pFlux--interp'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--full_pFlux--interp--128Ss'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--128Ss'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--absVals'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--absvals'

  hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--absVals'
  outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--absvals'

  IF KEYWORD_SET(plot_north)     THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south)     THEN outPlotName += '--' + 'SOUTH'


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults

  IF N_ELEMENTS(full_pFlux) EQ 0 THEN full_pFlux = 0

  IF N_ELEMENTS(only_128Ss_data) EQ 0 THEN only_128Ss_data = 0

  IF KEYWORD_SET(full_pFlux)     THEN BEGIN
     include_E_near_B = 1
  ENDIF

  CASE 1 OF
     KEYWORD_SET(use_eField_fit_variables): BEGIN
        eAV_variable = 'EFIT_ALONG_V'
        eNB_variable = 'EFIT_NEAR_B'

     END
     ELSE: BEGIN
        eAV_variable = 'E_ALONG_V'
        eNB_variable = 'E_NEAR_B'
     END
  ENDCASE

  IF N_ELEMENTS(do_fields_interp) EQ 0 AND ~KEYWORD_SET(do_fields_spline) THEN BEGIN
     do_fields_interp  = 1
  ENDIF
  IF N_ELEMENTS(do_fields_spline) EQ 0 THEN BEGIN
     do_fields_spline  = 0
  ENDIF

  ;;Update hashfile name and outPlotName
  plotPref = SETUP_STRANGEWAY_BRAMBLES_PLOTPREF($
             USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
             ONLY_FASTSRVY_DATA=only_128Ss_data, $
             INCLUDE_E_NEAR_B=include_E_near_B, $
             FULL_PFLUX_CALC=full_pFlux, $
             FIELDS_INTERP=do_fields_interp, $
             FIELDS_SPLINE=do_fields_spline)
             
  hashFile    += plotPref
  outPlotName += plotPref


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;More defaults

  ;;According to supplementary material in Brambles et al. [2011]
  minFreq    = 0.125 ;Hz
  maxFreq    = 0.5   ;Hz

  freqBounds = [minFreq,maxFreq]

  ;;Filter stuff
  lowPole        = 8 ;Slay that low-freq garbage
  highPole       = 8
  FFTdb          = 50 ;For digital filter coeffs. IDL doc says "50 is a good choice."
  FFTdb          = !NULL ;For digital filter coeffs. IDL doc says "50 is a good choice."


  ;;For FFTs of Mag and E data
  FFTLen         = 1024
  nFFTAvg        = 1

  maxPeriod      = 1/100.
  minPeriod      = 1/132.
  eFSampFact     = 4         ;The fact is, E-field data gets sampled a million times faster!

  ;; freqPlotRange  = [0.1,FGMagRolloff]
  ;; ;; override_freq  = [0.0,FGMagRolloff]
  ;; override_freq  = [0.125,0.5] ;Brambles et al. [2011] supplementary matieral

  FFTSlide       = 1.0

; Under development - R. J. Strangeway 4/4/08

; Program will use db_fac_v if E field data are available, other use dB_fac
; over-ride with use_fac_v and use_fac keywords

; Step 0 - safety measure - delete all tplot quantities if found

  @tplot_com   ;Provides data_quants variable

  @startup

  IF ~KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR,0
  ENDIF

  @strway_stuff                 ;List of orbits used, energy thresholds, etc.

  IF N_ELEMENTS(use_fac) EQ 0 AND N_ELEMENTS(use_fac_v) EQ 0 THEN use_fac = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

  nn           = N_ELEMENTS(data_quants)

  if (nn gt 1) then for n = nn-1L,1L,-1L do STORE_DATA,data_quants(n).name,/delete

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 1 - DC Mag data

  ucla_mag_despin,tw_mat=tw_mat,orbit=orbit,spin_axis=spin_axis,delta_phi=delta_phi

  IF (N_ELEMENTS(orbit) GT 0) THEN BEGIN

     orbString           = STRING(FORMAT='(I0)',orbit)
     outPlotName        += '--' + orbString

     ;;Get time and Je info
     check =  LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit, $
                                           RETURN_STRUCT=return_struct, $
                                           /USE_DUPELESS_FILES, $
                                           JE_OUT=je, $
                                           TIME_RANGES_OUT=time_ranges, $
                                           TIME_RANGE_INDICES_OUT=time_range_indices, $
                                           NINTERVALS_OUT=number_of_intervals, $
                                           ;; OUT_JEFILENAME=jeFileName, $
                                           ;; CLEAN_DUPES=clean_dupes, $
                                           /QUIET)

     IF check[0] EQ -1 THEN BEGIN
        PRINT,"Couldn't get Je time info for orbit " + orbString + '!!!'
        PRINT,"Out ..."
        RETURN
     ENDIF

     IF N_ELEMENTS(je.x) LE 1 THEN BEGIN
        PRINT,'Insufficient data to do anything awesome! Returning ...'
        RETURN
     ENDIF

     ;;  if orbit > 9936 return (temporary fix)

     if (orbit gt 9936) then begin

        print,""
        print,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        print,""
        return

     endif

     ;; got mag data, set time limits, delete unused tplot variables, set tplot_vars

     STORE_DATA,'BDATA',/delete
     STORE_DATA,'BFIT',/delete
     STORE_DATA,'Bx_sp',/delete
     STORE_DATA,'By_sp',/delete
     STORE_DATA,'Bz_sp',/delete
     STORE_DATA,'Bx_sc',/delete
     STORE_DATA,'By_sc',/delete
     STORE_DATA,'Bz_sc',/delete
     STORE_DATA,'Bx_sp_sm',/delete
     STORE_DATA,'By_sp_sm',/delete
     STORE_DATA,'Bz_sp_sm',/delete
     STORE_DATA,'B_gei',/delete
     STORE_DATA,'B_sm',/delete
     STORE_DATA,'dB_sc',/delete
     STORE_DATA,'dB_gei',/delete
     STORE_DATA,'spin_freq',/delete
     STORE_DATA,'spin_phase',/delete
     STORE_DATA,'TORQ_X',/delete
     STORE_DATA,'TORQ_Y',/delete
     STORE_DATA,'TORQ_Z',/delete
     STORE_DATA,'BX_DEL',/delete
     STORE_DATA,'BY_DEL',/delete
     STORE_DATA,'BZ_DEL',/delete
     STORE_DATA,'BFIX',/delete
     STORE_DATA,'TW_ZX',/delete
     STORE_DATA,'TW_ZY',/delete
     STORE_DATA,'TW_YY',/delete
     STORE_DATA,'TW_YX',/delete
     STORE_DATA,'O_X',/delete
     STORE_DATA,'O_Y',/delete
     STORE_DATA,'B_model_old',/delete
     STORE_DATA,'Delta_B_model',/delete
     STORE_DATA,'despun_to_gei',/delete
     STORE_DATA,'gei_to_sm',/delete
     STORE_DATA,'gei_to_fac',/delete
     STORE_DATA,'gei_to_fac_v',/delete

  ENDIF ELSE BEGIN

     PRINT,"Couldn't pick up orb info from UCLA_MAG_DESPIN. OUT!"
     RETURN
  ENDELSE

  structList             = LIST()
  FOR jj=0,number_of_intervals-1 DO BEGIN

     cant_pFlex     = 0         ;because we CAN pFlex!

     itvlString     = STRCOMPRESS(jj,/REMOVE_ALL)

     tmpPlotName    = outPlotName + '__itvl_' + itvlString

     t1 = time_ranges[jj,0]
     t2 = time_ranges[jj,1]

     tmp_je_indices = time_range_indices[jj,*]

     ;;Clean up based on ILAT
     GET_FA_ORBIT,je.x,/TIME_ARRAY,/DEFINITIVE,/ALL
     GET_DATA,'ILAT',DATA=ilat
     IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF
     IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF

     ;;Make sure we have data where we want it.
     keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        PRINT,'No data above min ILAT. Skipping this interval ...'
        CONTINUE
     ENDIF
     
     ;;Trim time series, if necessary
     IF nKeep LT N_ELEMENTS(je.x) THEN BEGIN
        closest1 = MIN(ABS(t1-je.x[keep]),tmpII_t1)
        closest2 = MIN(ABS(t2-je.x[keep]),tmpII_t2)

        ;;If more than 30 s from previous mark, we're in doubt
        IF (closest1 GT 30) OR (closest2 GT 30) THEN BEGIN
           PRINT,'Either t1 or t2 is more than thirty seconds from the previous mark ...'
           PRINT,'Questionable, indeed. Skipping this interval ...'
           CONTINUE
        ENDIF

        t1             = je.x[keep[tmpII_t1]]
        t2             = je.x[keep[tmpII_t2]]
        tmp_je_indices = [keep[tmpII_t1],keep[tmpII_t2]]
     ENDIF

     tLimit_all     = [t1,t2]

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))
     
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Get Mag and E field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;	E component: “E_along_V”
     ;;	B components: Two components perpendicular to E_along_V
     ;;		a. Use Strangeway despinning, find coords that complement E_along_V
     ;;		b. Need to use Bob model to subtract the background field

     GET_DATA,'dB_fac_v',DATA=db_fac
     IF SIZE(db_fac,/TYPE) NE 8 THEN BEGIN
        PRINT,"Couldn't get despun mag data! Outta sight ..."
        RETURN
     ENDIF

     mintime = MIN(ABS(t1-db_fac.x),ind1)
     mintime = MIN(ABS(t2-db_fac.x),ind2)
     ;;   From UCLA_MAG_DESPIN: "Field-aligned velocity-based coordinates defined as:    "
     ;;x (ind 0)-along track ((BxV)xB),
     ;;y (ind 1)-cross track (BxV), 
     ;;z (ind 2)-along B" (I added "ind" marks)
     magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} 
     magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} 
     magz = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

     nMag = N_ELEMENTS(magz.y)

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


     ;;E field
     FA_FIELDS_DESPIN,efieldV58,efieldV1214, $
                      /SHADOW_NOTCH,/SINTERP, $ ;Why? Because RJS does it in his summary plot
                      T1=t1,T2=t2               ;,/SLOW ;procedure

     IF ~efieldV58.valid OR ~efieldV58.valid THEN BEGIN
        PRINT,FORMAT='("efieldV58.valid : ",I0)',efieldV58.valid
        PRINT,FORMAT='("efieldV1214.valid : ",I0)',efieldV1214.valid
        PRINT,'Returning ...'
        RETURN
     ENDIF

     GET_DATA,eAV_variable,DATA=eAlongV
     IF SIZE(eAlongV,/TYPE) NE 8 THEN BEGIN
        PRINT,"Couldn't get " + eAV_variable + '!'
        STOP
     ENDIF

     IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
        maxeFitPeriod = MAX(eAlongV.x[1:-1]-eAlongV.x[0:-2]) < 2.8
     ENDIF


     IF KEYWORD_SET(include_E_near_B) THEN BEGIN
        GET_DATA,eNB_variable,DATA=eNearB

        IF SIZE(eNearB,/TYPE) NE 8 THEN BEGIN
           PRINT,"Couldn't get " + eNB_variable + '!'
           STOP
        ENDIF
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

     ;;Get eAlongV at same res (I think this always means reducing the res. of eField, unless using EFIT variables

     FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                       {TIME:eAlongV.x,COMP1:eAlongV.y}, $
                       RESULT=eAlongVInterp, $
                       INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                       SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                       SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                              maxeFitPeriod : !NULL, $
                       /TALK

     IF KEYWORD_SET(include_E_near_B) THEN BEGIN
        FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                          {TIME:eNearB.x,COMP1:eNearB.y}, $
                          RESULT=eNearBInterp, $
                          INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                          SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                          SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                                 maxeFitPeriod : !NULL, $
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

     sRates     = 1.D/REFORM((magz.x[fftBin_i[0,*]+1]-magz.x[fftBin_i[0,*]]))

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
           k += helper
        ENDIF

        dBp  = {TIME         : magz.x[tmpI]          , $
                COMP1        : magz.y[tmpI]          , $
                NCOMP        : 1                     , $
                DATA_NAME    : 'Cross-track MagData' , $
                VALID        : 1                     , $
                PROJECT_NAME : 'FAST'                , $
                UNITS_NAME   : 'nT'                  , $
                CALIBRATED   : 1}
        dBB  = {TIME         : magy.x[tmpI]          , $
                COMP1        : magy.y[tmpI]          , $
                NCOMP        : 1                     , $
                DATA_NAME    : 'along-B MagData' , $
                VALID        : 1                     , $
                PROJECT_NAME : 'FAST'                , $
                UNITS_NAME   : 'nT'                  , $
                CALIBRATED   : 1}
        dBv  = {TIME         : magx.x[tmpI]          , $
                COMP1        : magx.y[tmpI]          , $
                NCOMP        : 1                     , $
                DATA_NAME    : 'along-v MagData' , $
                VALID        : 1                     , $
                PROJECT_NAME : 'FAST'                , $
                UNITS_NAME   : 'nT'                  , $
                CALIBRATED   : 1}

        tmpE = {TIME         : magz.x[tmpI]          , $
                COMP1        : eAlongVInterp[tmpI]   , $
                NCOMP        : 1                     , $
                DATA_NAME    : 'eAlongVStuff'        , $
                VALID        : 1                     , $
                PROJECT_NAME : 'FAST'                , $
                UNITS_NAME   : 'mV/m'                , $
                CALIBRATED   : 1}

        FA_FIELDS_FILTER,dBp, $
                         ;; freqBounds[*,k], $
                         freqBounds, $
                         DB=FFTdb, $
                         POLES=[lowPole,highPole]
        FA_FIELDS_FILTER,dBB, $
                         ;; freqBounds[*,k], $
                         freqBounds, $
                         DB=FFTdb, $
                         POLES=[lowPole,highPole]
        FA_FIELDS_FILTER,dBv, $
                         ;; freqBounds[*,k], $
                         freqBounds, $
                         DB=FFTdb, $
                         POLES=[lowPole,highPole]
        FA_FIELDS_FILTER,tmpE, $
                         ;; freqBounds[*,k], $
                         freqBounds, $
                         DB=FFTdb, $
                         POLES=[lowPole,highPole]

        filtMag[tmpI]  = dBp.comp1
        filtMag2[tmpI] = dBB.comp1
        filtMag3[tmpI] = dBv.comp1
        filteAV[tmpI]  = tmpE.comp1

        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           tmpG    = {  TIME         : magz.x[tmpI]          , $
                        COMP1        : eNearBInterp[tmpI]   , $
                        NCOMP        : 1                     , $
                        DATA_NAME    : 'eNearBStuff'        , $
                        VALID        : 1                     , $
                        PROJECT_NAME : 'FAST'                , $
                        UNITS_NAME   : 'mV/m'                , $
                        CALIBRATED   : 1}
           FA_FIELDS_FILTER,tmpG, $
                            ;; freqBounds[*,k], $
                            freqBounds, $
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
     ;; spec = FA_FIELDS_SPEC(magzTmp, $
     ;;                       /STORE, $
     ;;                       T_NAME='MagSpec', $
     ;;                       STRUCTURE=magSpec, $
     ;;                       NPTS=FFTLen, $
     ;;                       N_AVE=nFFTAvg, $
     ;;                       SLIDE=FFTSlide)
     ;; spec = FA_FIELDS_SPEC(eAlongVTmp, $
     ;;                       /STORE, $
     ;;                       T_NAME='EAVSpec', $
     ;;                       STRUCTURE=eAVSpec, $
     ;;                       NPTS=FFTLen, $
     ;;                       N_AVE=nFFTAvg, $
     ;;                       SLIDE=FFTSlide)
     ;; spec = FA_FIELDS_SPEC(magzFilt, $
     ;;                       /STORE, $
     ;;                       T_NAME='MagSpecFilt', $
     ;;                       STRUCTURE=magSpecFilt, $
     ;;                       NPTS=FFTLen, $
     ;;                       N_AVE=nFFTAvg, $
     ;;                       SLIDE=FFTSlide)
     ;; spec = FA_FIELDS_SPEC(eAlongVFilt, $
     ;;                       /STORE, $
     ;;                       T_NAME='EAVSpecFilt', $
     ;;                       STRUCTURE=eAVSpecFilt, $
     ;;                       NPTS=FFTLen, $
     ;;                       N_AVE=nFFTAvg, $
     ;;                       SLIDE=FFTSlide)

     ;; IF KEYWORD_SET(include_E_near_B) THEN BEGIN
     ;;    spec = FA_FIELDS_SPEC(eNearBFilt, $
     ;;                          /STORE, $
     ;;                          T_NAME='ENBSpecFilt', $
     ;;                          STRUCTURE=eNBSpecFilt, $
     ;;                          NPTS=FFTLen, $
     ;;                          N_AVE=nFFTAvg, $
     ;;                          SLIDE=FFTSlide)
     ;; ENDIF

     ;; ;;Clean 'em up
     ;; GET_DATA,'EAVSpecFilt',DATA=tmpE
     ;; GET_DATA,'MagSpecFilt',DATA=tmpB        
     ;; tmpE.y[WHERE(~FINITE(tmpE.y) OR tmpE.y LT ESpecThresh)] = 0.0
     ;; tmpB.y[WHERE(~FINITE(tmpB.y) OR tmpB.y LT BSpecThresh)] = 0.0
     ;; tmpE.v *= 1000.         ;To Hz
     ;; tmpB.v *= 1000.         ;To Hz


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
     IF KEYWORD_SET(full_pFlux) THEN BEGIN

        pFluxB = filtMag*filteAV/mu_0                           ;Poynting flux along B
        pFluxP = (filteNB*filtMag3-1.*filtMag2*filteAV)/mu_0    ;Poynting flux perp to B and to (Bxv)xB
        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
        pFluxV = (-1.)*filteNB*filtMag/mu_0

     ENDIF ELSE BEGIN
        pFluxB =       filtMag *filteAV/mu_0    ;Poynting flux along B
        pFluxP = (-1.)*filtMag2*filteAV/mu_0    ;Poynting flux perp to B and to (Bxv)xB
        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

     ENDELSE
     pFluxB *= 1e-9 ;Junk that nano prefix in nT
     pFluxP *= 1e-9

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now let's interp everyone to 1-s resolution
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;;Mag first
     sRates = 1./(magzFilt.time[1:-1]-magzFilt.time[0:-2])
     FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                       ;; {TIME:data.x,COMP1:data.y}, $
                       magzFilt, $
                       RESULT=datInterp, $
                       INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                       SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                       ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=(1.01)/MIN(sRates), $
                       /TALK

     mag      = {x:magzFilt.time,y:magzFilt.comp1}
     mag1s    = datInterp

     data = {x:[[tS_1s],[tS_1s]], $
             y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                [TEMPORARY(datInterp)]]}

     STORE_DATA,'dB_ac_interp',DATA=data

     dLimit = {spec:0, ystyle:1, yrange:[-200.,200.], $
               ytitle:'dB Perp.!C!C[AC] (nT)', $
               panel_size:3}

     OPTIONS,'dB_ac_interp','colors',[normColorI,normColorI]
     OPTIONS,'dB_ac_interp','tplot_routine','mplot'
     STORE_DATA,'dB_ac_interp',DLIMITS=dLimit
     OPTIONS,'dB_ac_interp','x_no_interp',1
     OPTIONS,'dB_ac_interp','y_no_interp',1

     tmpDatStruct = CREATE_STRUCT("mag",mag)
     tmp1sStruct  = CREATE_STRUCT("mag",mag1s)

     tplot_vars   = 'dB_ac_interp'

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
     endif

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now E-field
     sRates = 1./(eAlongVFilt.time[1:-1]-eAlongVFilt.time[0:-2])
     FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                       eAlongVFilt, $
                       RESULT=datInterp, $
                       INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                       SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                       ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=(1.01)/MIN(sRates), $
                       /TALK

     nEField  = N_ELEMENTS(eAlongV.y)
     eField   = {x:eAlongVFilt.time,y:eAlongVFilt.comp1}
     eField1s = datInterp

     tmpDatStruct = CREATE_STRUCT(tmpDatStruct,"eField",eField)
     tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,"eField",eField1s)

     data = {x:tS_1s, $
             y:datInterp}

     data = {x:[[tS_1s],[tS_1s]], $
             y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                [data.y]]}

     STORE_DATA,'E_ALONG_V_AC',DATA=data,dlimit=dlimit
     OPTIONS,'E_ALONG_V_AC','yrange',0
     OPTIONS,'E_ALONG_V_AC','ytitle','E along V!Dsc!N!C!C[AC] (mV/m)'
     OPTIONS,'E_ALONG_V_AC','colors',[normColorI,normColorI]
     OPTIONS,'E_ALONG_V_AC','panel_size',2
     OPTIONS,'E_ALONG_V_AC','x_no_interp',1
     OPTIONS,'E_ALONG_V_AC','y_no_interp',1

     IF (N_ELEMENTS(tplot_vars) EQ 0) THEN BEGIN
        tplot_vars=['E_ALONG_V_AC']
     ENDIF ELSE BEGIN
        tplot_vars=['E_ALONG_V_AC',tplot_vars]
     ENDELSE

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now Poynting flux
     pFluxB[WHERE(~FINITE(pFluxB))] = 0.0

     FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                       {TIME:magzFilt.time,comp1:pFluxB}, $
                       RESULT=datInterp, $
                       INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                       SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                       ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=(1.01)/MIN(sRates), $
                       /TALK

     pFluxB = {x:magzFilt.time,y:pFluxB}
     pFluxB1s = datInterp

     STORE_DATA,'pFlux_ac',DATA={x:[[tS_1s],[tS_1s]], $
                                 y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                                    [pFluxB1s]]}

     dLimit = {spec:0, ystyle:1, yrange:[-10.,10.], $
               ytitle:'Poynting Flux!C!C[AC] (mW/m!U2!N)', $
               panel_size:3}
     OPTIONS,'pFlux_ac','colors',[normColorI,normColorI]
     STORE_DATA,'pFlux_ac',DLIMITS=dLimit
     OPTIONS,'pFlux_ac','x_no_interp',1
     OPTIONS,'pFlux_ac','y_no_interp',1

     IF (N_ELEMENTS(tplot_vars) EQ 0) THEN tplot_vars=['pFlux_ac'] $
     ELSE tplot_vars=['pFlux_ac',tplot_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
     ENDIF
     tmpDatStruct = CREATE_STRUCT(tmpDatStruct,"pFluxB",pFluxB)
     tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,"pFluxB",pFluxB1s)


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now Poynting flux
     pFluxP[WHERE(~FINITE(pFluxP))] = 0.0

     FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                       {TIME:magzFilt.time,comp1:pFluxP}, $
                       RESULT=datInterp, $
                       INTERP=KEYWORD_SET(do_fields_interp) ? 1 : !NULL, $
                       SPLINE=KEYWORD_SET(do_fields_spline) ? 1 : !NULL, $
                       ;; SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=(1.01)/MIN(sRates), $
                       /TALK

     pFluxP = {x:magzFilt.time,y:pFluxP}
     pFluxP1s = datInterp

     STORE_DATA,'pFluxP_ac',DATA={x:[[tS_1s],[tS_1s]], $
                                  y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                                     [pFluxP1s]]}

     dLimit = {spec:0, ystyle:1, yrange:[-10.,10.], $
               ytitle:'Poynting Flux!Dperp!N!C!C[AC] (mW/m!U2!N)', $
               panel_size:3}
     OPTIONS,'pFluxP_ac','colors',[normColorI,normColorI]
     STORE_DATA,'pFluxP_ac',DLIMITS=dLimit
     OPTIONS,'pFluxP_ac','x_no_interp',1
     OPTIONS,'pFluxP_ac','y_no_interp',1

     IF (N_ELEMENTS(tplot_vars) EQ 0) THEN tplot_vars=['pFluxP_ac'] $
     ELSE tplot_vars=['pFluxP_ac',tplot_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
     ENDIF
     tmpDatStruct = CREATE_STRUCT(tmpDatStruct,"pFluxP",pFluxP)
     tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,"pFluxP",pFluxP1s)


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; STEP 7 - Clean up and return

     ;; determine tlimit_north and tlimit_south also change plot title

     GET_DATA,'LAT',data=data

     if (N_ELEMENTS(data.y) le 0) then return

     bb = where (data.y gt 10,nn)
     if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

     bb = where (data.y lt -10,nn)
     if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

     hemisph = getenv('FAST_ORBIT_HEMISPHERE')

     GET_DATA,'ORBIT',data=data
     nn = N_ELEMENTS(data.y)/2
     orbit = data.y(nn)
     orbit_lab = strcompress(string(orbit,format="(i5.4)"),/remove_all)
     tplot_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

     ;; force tplot_vars to be all the panels unless no_blank_panels is set


     IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Brambles_et_al_2011'
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+tmpPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN
              ;; CGPS_OPEN, './plots/McFadden_et_al_1998--Fig_1.ps',FONT=0,XSIZE=4,YSIZE=7
              POPEN,plotDir+tmpPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO,FONT_SIZE=8
              ;; DEVICE,SET_FONT='Garamond*15'
              ;; !P.FONT = -1
           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE

        CASE 1 OF
           KEYWORD_SET(plot_north): BEGIN
              tLims = tlimit_north

           END
           KEYWORD_SET(plot_south): BEGIN
              tLims = tlimit_south
           END
           ELSE: BEGIN
              ;; tLims = [t1ZoomStr,t2ZoomStr]
           END
        ENDCASE

        LOADCT2,40
        TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tLims


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

     ENDIF
     
     PRINT,''
     PRINT,'************************************************************'
     PRINT,FORMAT='("Interval",T40,": ",I0)',jj
     PRINT,FORMAT='("N Bfield",T40,": ",I0)',nMag
     PRINT,FORMAT='("N Efield",T40,": ",I0)',nEField
     PRINT,FORMAT='("N 1-s",T40,": ",I0)',N_ELEMENTS(tS_1s)
     PRINT,''

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Gather data and STORE
     SDT_names = {MLT           : 'MLT'          , $          
                  ILAT          : 'ILAT'         , $         
                  ALT           : 'ALT'          , $          
                  E_ALONG_V_AC  : 'E_ALONG_V_AC' , $ 
                  dB_ac_interp  : 'dB_ac_interp' , $ 
                  pFlux_ac      : 'pFlux_ac'     , $     
                  ;; Je         : 'Je'           , $        
                  pFluxP_ac     : 'pFluxP_ac'} ; , $    
                  ;; Ji : 'Ji', $        
                  ;; DSP_integ : 'DSP_integ'}

     tmpStruct = ASSEMBLE_BRAMBLES_2011_STRUCT(tS_1s, $
                                               orbit, $
                                               tmpDatStruct, $
                                               tmp1sStruct, $
                                               minILAT, $
                                               SDT_NAMES=SDT_names, $
                                               SANS_INTEGRATION=sans_integration)

     PRINT,"Adding struct for interval " + itvlString + " in orbit " + orbString + ' ...'
     structList.Add,tmpStruct

     ;; ENDIF

  ENDFOR

  IF ~KEYWORD_SET(no_hash_update) THEN BEGIN
     IF FILE_TEST(outDir+hashFile) THEN BEGIN
        PRINT,"Restoring hash file " + hashFile + " ..."
        RESTORE,outDir+hashFile

        CASE (WHERE((brHash.Keys()).ToArray() EQ orbit))[0] OF
           -1: BEGIN
              PRINT,'Adding stuff from orbit ' + orbString + ' ...'
              brHash  = brHash + ORDEREDHASH(orbit,structList)
           END
           ELSE: BEGIN
              PRINT,'Replacing hash entry for orbit ' + orbString + ' ...'
              brHash[orbit] = structList
           END
        ENDCASE

        PRINT,'Saving Brambles statistics hash ...'
        SAVE,brHash,FILENAME=outDir+hashFile
     ENDIF ELSE BEGIN
        PRINT,'Creating Brambles statistics hash for orbit ' + orbString + ' ...'
        brHash = ORDEREDHASH(orbit,structList)
        SAVE,brHash,FILENAME=outDir+hashFile
     ENDELSE
  ENDIF

  RETURN

END


