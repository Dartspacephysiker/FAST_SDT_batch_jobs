;2017/05/24
PRO JOURNAL__20170524__TRY_OUT_INTERPING_16HZ_TO_NEAREST_8HZ_POINTS, $
   TPLT_VARS=tPlt_vars, $
   SCREEN_PLOT=screen_plot, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   USE_JE_TBOUNDS=use_Je_tBounds, $
   INCLUDE_PARTICLES=include_particles, $
   DECIMATE_E_AND_B__THEN_CALC_PFLUX=decimate_eb_calc_pFlux, $
   SKIPDSP=skipDSP, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   BATCH_MODE=batch_mode

  COMPILE_OPT IDL2,STRICTARRSUBS

  interp_4Hz_to_1s     = 1 ;Sorry, non-optional right now since I'm including ephemeris data and I don't want to fuss with 20 time series
  interp_8Hz_to_0_125s = 1

  ;; lowFreqBounds   = [0,0.125]
  ;; highFreqBounds  = [0.125,0.5]
  ;; chastFreqBounds = [0.5,10]

  ;; freqBounds        = [[lowFreqBounds],[highFreqBounds],[chastFreqBounds]]
  ;; freqSuffs         = '_' + ['LOW','HIGH','INERTIAL'] 

  ;; highFreqPoles  = [8,8]
  ;; lowFreqPoles   = [8,8]

  IF KEYWORD_SET(include_particles) THEN BEGIN
     PRINT,"Can't include particles!"
     include_particles = 0
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

  @tplot_com ;provides data_quants variable
  @startup
  @strway_stuff

  minILAT = 0 ;negate strway_stuff.pro

  IF ~KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR,0
  ENDIF

  ;;From UCLA_MAG_DESPIN:
  ;;"   Field-aligned coordinates defined as: 
  ;;"   z-along B, y-east (BxR), x-nominally out"
  ;;    (ind 2)    (ind 1)       (ind 0)

  magInd       = 1
  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

; Step 1 - DC Mag data

  @strangeway_2bands__defaults__pflux_efield_bfield.pro
  
  psym_ptcl    = 3              ;period
  symsize_ptcl = 5.0
  nn           = N_ELEMENTS(data_quants)

  IF (nn GT 1) THEN FOR n = nn-1L,1L,-1L DO STORE_DATA,data_quants[n].name,/DELETE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 1 - DC Mag data

  UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

  IF (N_ELEMENTS(orbit) GT 0) THEN BEGIN

     orbString           = STRING(FORMAT='(I0)',orbit)
     TPLOT_OPTIONS,'title','FAST Orbit ' + orbString

     ;; IF KEYWORD_SET(save_individual_orbit) THEN BEGIN
     ;;    indiv_orbFile = indivOrbPref + orbString + '.sav'
     ;; ENDIF


;  if orbit > 9936 return (temporary fix)

     IF (orbit GT 9936) THEN BEGIN

        PRINT,""
        PRINT,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        PRINT,""
        RETURN

     ENDIF

; got mag data, set time limits, delete unused tplot variables, set tPlt_vars

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

     GET_DATA,'dB_fac_v',data=data
     t1           = data.x[0]
     t2           = data.x[N_ELEMENTS(data.x)-1L]
     mag_tBounds  = [t1,t2]
     tPlt_vars    = 'dB_fac_v'
     OPTIONS,'dB_fac_v','panel_size',2
     OPTIONS,'dB_fac','panel_size',2
     OPTIONS,'dB_sm','panel_size',2

     PRINT,FORMAT='(A0,T35,A0,", ",A0)',"MAG beginning/end : ",TIME_TO_STR(t1,/MSEC),TIME_TO_STR(t2,/MSEC)


     ephem_tSeries = data.x
     tBounds       = mag_tBounds

     tPlt_vars = 'dB_fac_v'

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tBounds
     ENDIF

  ENDIF ELSE BEGIN

     PRINT,"Couldn't pick up orb info from UCLA_MAG_DESPIN. OUT!"
     RETURN
  ENDELSE


; step 2 - E field

; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
  sdt_idx = get_sdt_run_idx()

  prog = GETENV('FASTBIN') + '/showDQIs'
  IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
     IF (sdt_idx GE 10) THEN BEGIN
        sidstr = STRING(sdt_idx,FORMAT='(I2)')
     ENDIF ELSE BEGIN
        sidstr = STRING(sdt_idx,FORMAT='(I1)')
     ENDELSE
     SPAWN,[prog, sidstr],result,/NOSHELL
  ENDIF ELSE BEGIN
     SPAWN,prog,result,/NOSHELL
  ENDELSE


  ;;Find out if we have various eField things
  b = WHERE(STRPOS(result,'V1-V4_S') GE 0,nb4)
  IF (nb4 GT 0) THEN IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN nb4 = 0
  b = WHERE(STRPOS(result,'V1-V2_S') GE 0,nb2)
  IF (nb2 GT 0) THEN IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN nb2 = 0
  IF (nb4 GT 0) THEN v12 = GET_FA_FIELDS('V1-V4_S',/DEFAULT) $
  ELSE IF (nb2 GT 0) THEN v12 = GET_FA_FIELDS('V1-V2_S',/DEFAULT)

  b = WHERE(STRPOS(result,'V5-V8_S') GE 0,nb5)
  IF (nb5 GT 0) THEN v58 = GET_FA_FIELDS('V5-V8_S',/DEFAULT)

  got_efield = (nb4 GT 0 OR nb2 GT 0) AND nb5 GT 0

  IF (got_efield) THEN BEGIN

     ;; despin e field data
     FA_FIELDS_DESPIN,v58,v12,/SHADOW_NOTCH,/MAG_NOTCH,/SINTERP

  ENDIF ELSE BEGIN
     PRINT,"Couldn't get E-field data! Out ..."
     RETURN
  ENDELSE

  ;;Pick a time series?
  IF KEYWORD_SET(use_Je_tBounds) THEN BEGIN

     PRINT,"Attempting to use Je tBounds ..."

     ;;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     ;;DEPRECATE ME
     ;;
     ;;Get time and Je info
     check =  LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit, $
                                           RETURN_STRUCT=return_struct, $
                                           /USE_DUPELESS_FILES, $
                                           JE_OUT=je_pristine, $
                                           TIME_RANGES_OUT=time_ranges, $
                                           TIME_RANGE_INDICES_OUT=time_range_indices, $
                                           NINTERVALS_OUT=number_of_intervals, $
                                           ;; OUT_JEFILENAME=jeFileName, $
                                           ;; CLEAN_DUPES=clean_dupes, $
                                           /QUIET)

     ;;Checkups
     IF check[0] EQ -1 THEN BEGIN
        PRINT,"Couldn't get Je time info for orbit " + orbString + '!!!'
        PRINT,"Out ..."
        RETURN
     ENDIF

     IF SIZE(je_pristine,/TYPE) NE 8 THEN BEGIN
        PRINT,"Apparently no ESA interval information for this orbit. Returning ..."
        RETURN
     ENDIF

     IF N_ELEMENTS(je_pristine.x) LE 1 THEN BEGIN
        PRINT,'Insufficient data to do anything awesome! Returning ...'
        RETURN
     ENDIF

     je_tBounds = [je_pristine.x[0],je_pristine.x[-1]]
     PRINT,FORMAT='(A0,T35,A0,", ",A0)',"Je_pristine beginning/end : ", $
           TIME_TO_STR(je_tBounds[0],/MSEC), $
           TIME_TO_STR(je_tBounds[1],/MSEC)
     je_tBounds = [je_pristine.x[0],je_pristine.x[-1]]

     ephem_tSeries = je_pristine.x
     tBounds       = je_tBounds

     t1           = je_pristine.x[0]
     t2           = je_pristine.x[-1]

     itvlTypeName = 'EESAItvl'


  ENDIF ELSE BEGIN

     GET_MAG_TIME_INTERVALS_AND_SAVE,data.x, $
                                     TIME_RANGES_OUT=time_ranges, $
                                     TIME_RANGE_INDICES_OUT=time_range_indices, $
                                     NINTERVALS_OUT=number_of_intervals
     
     itvlTypeName = 'magItvl'

  ENDELSE

  ;;Interp time series
  tS_1s            = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

  ;;We overwrite t1 and t2 later, but don't worry—we're only using them here to get a 1-s time series
  IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN
     ephem_tSeries = tS_1s
  ENDIF

  ;;Clean up based on ILAT
  ;; GET_FA_ORBIT,ephem_tSeries,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=ephem
  GET_FA_ORBIT,ephem_tSeries,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=ephem
  ;; GET_DATA,'ILAT',DATA=ilat
  IF SIZE(ephem,/TYPE) NE 8 THEN BEGIN
     PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
     RETURN
  ENDIF
  IF N_ELEMENTS(ephem.ilat) LE 1 THEN BEGIN
     PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
     RETURN
  ENDIF

  ;;Make sure we have data where we want it.
  IF KEYWORD_SET(minILAT) THEN BEGIN
     keep  = WHERE(ABS(ephem.ilat) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        PRINT,'No data above min ILAT. Out!'
        RETURN
     ENDIF
  ENDIF

  ;;PlotOpt things
  dLimit = {spec:0, $
            ystyle:1, $
            ytitle:'SFlux Wave!C[< 0.125 Hz]!C(mW/m!U2!N)', $
            yticks:7, $      
            ylog:0, $
            yrange:[-4,2], $
            ytickv:[-4,-3,-2,-1,0,1,2], $          
            ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
                       '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
            ;; ylog:1, $
            ;; yrange:[1e-4,1e2], $
            ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $          
            ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
            ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
            colors:normColorI,$
            panel_size:3}
  STORE_DATA,'pFluxLow',DLIMITS=dLimit
  OPTIONS,'pFluxLow','x_no_interp',1
  OPTIONS,'pFluxLow','y_no_interp',1


  dLimit = {spec:0, $
            ystyle:1, $
            ytitle:'SFlux Wave!C[0.125-0.5 Hz]!C(mW/m!U2!N)', $
            yticks:6, $      
            ylog:0, $
            yrange:[-9,2], $
            ytickv:[-8,-6,-4,-2,0,2], $
            ytickname:['10!U-8!N','10!U-6!N','10!U-4!N', $
                       '10!U-2!N','10!U0!N','10!U2!N'], $
            ;; ylog:1, $
            ;; yrange:[1e-4,1e2], $
            ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $          
            ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
            ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
            colors:normColorI ,$
            panel_size:3}
  STORE_DATA,'pFluxHigh',DLIMITS=dLimit
  OPTIONS,'pFluxHigh','x_no_interp',1
  OPTIONS,'pFluxHigh','y_no_interp',1

  ;;Will want these throughout the loop
  GET_DATA,'dB_fac_v',DATA=magData
  GET_DATA,eAV_variable,DATA=eAlongV

  ;;Now loop over stuff
  ;; structList      = LIST()

  ;;Gap dist is NOT A TIME
  ;;Rather, it is the number of allowable average dt's for interpolation of a time series
  gap_dist = 2.5
  FOR jj=0,number_of_intervals-1 DO BEGIN

     itvlString   = STRCOMPRESS(jj,/REMOVE_ALL)

     ;;Pick up times for this interval
     itvlT1       = time_ranges[jj,0]
     itvlT2       = time_ranges[jj,1]

     ;; tmpEphemInds = [time_range_indices[jj,0]:time_range_indices[jj,1]]
     tmpEphemInds = WHERE(ephem.time GE itvlT1 AND ephem.time LE itvlT2,nTmpEphem)
     IF nTmpEphem EQ 0 THEN BEGIN
        PRINT,"AYYAHHHT"
        RETURN
     ENDIF

     ;;Just get that ephem!
     tmpEphem     = {time            : ephem.time   [tmpEphemInds], $
                     orbit           : ephem.orbit  [tmpEphemInds], $  
                     fa_pos          : ephem.fa_pos [tmpEphemInds,*], $
                     alt             : ephem.alt    [tmpEphemInds], $
                     ilat            : ephem.ilat   [tmpEphemInds], $
                     ilng            : ephem.ilng   [tmpEphemInds], $
                     mlt             : ephem.mlt    [tmpEphemInds], $
                     fa_vel          : ephem.fa_vel [tmpEphemInds,*], $
                     bfoot           : ephem.bfoot  [tmpEphemInds,*], $
                     lat             : ephem.lat    [tmpEphemInds], $
                     lng             : ephem.lng    [tmpEphemInds], $
                     flat            : ephem.flat   [tmpEphemInds], $
                     flng            : ephem.flng   [tmpEphemInds], $
                     b_model         : ephem.b_model[tmpEphemInds,*]}


     tmp_tBounds = [itvlT1,itvlT2]

     ;;Interp time series
     IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

        tmpTS_1s         = tS_1s[tmpEphemInds]
        tmpTS_0_0625s    = !NULL

        addFac           = 0.0625D
        k                = 0
        WHILE (k*addFac) LT 1 DO BEGIN
           tmpTS_0_0625s = [[tmpTS_0_0625s],[tS_1s[tmpEphemInds]]+addFac*k]
           
           PRINT,addFac*k
           k++
        ENDWHILE
        tmpTS_0_0625s    = tmpTS_0_0625s[SORT(tmpTS_0_0625s)]
        tmpTS_0_125s     = tmpTS_0_0625s[0:-1:2]

     ENDIF ELSE BEGIN
        STOP
     ENDELSE

     ;; Step 3 - Poynting flux
     mintime = MIN(ABS(tmp_tBounds[0]-magData.x),ind1)
     mintime = MIN(ABS(tmp_tBounds[1]-magData.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable mag data here. Skipping interval ...'
        CONTINUE
     ENDIF

     ;;   From UCLA_MAG_DESPIN:
     ;;   "Field-aligned velocity-based coordinates defined as: "
     ;;   "z (ind 2)-along B, 
     ;;    y (ind 1)-cross track (BxV), 
     ;;    x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)
     magB = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,2]} 
     magP = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,1]}
     magv = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,0]} 

     ;;E-field trim
     mintime = MIN(ABS(tmp_tBounds[0]-eAlongV.x),ind1)
     mintime = MIN(ABS(tmp_tBounds[1]-eAlongV.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable eAlongV data here. Skipping interval ...'
        CONTINUE
     ENDIF

     eAVTmp = {x:eAlongV.x[ind1:ind2], $
               y:eAlongV.y[ind1:ind2]}


     dBv = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magv, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s)
     dBB = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magB, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s)
     dBp = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magP, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s)

     eAV = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           eAVTmp, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s)

  ENDFOR

END
