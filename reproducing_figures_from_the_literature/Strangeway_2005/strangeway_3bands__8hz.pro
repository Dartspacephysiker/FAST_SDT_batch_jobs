;2017/05/19
;The goal
;E field, B field, and Poynting flux in 5 ways
;=============================================
;--> Strangewayian smoothing and decimation to yield DC (0–0.125 Hz) and AC (0.125–0.5 Hz) components
;--> Digital filtering to yield DC (0–0.125 Hz), AC (0.125–0.5 Hz), and Chastonian (0.5–10 Hz) components

;Things we need:
;-->Data products from which to form pFlux: E-field, B-field
;-->Despun mag start/stop time

PRO FA_FILTERER,data, $
                POLES=poles, $
                FREQBOUNDS=freqBounds, $
                DB=FFTdB, $
                DATA_NAME=data_name, $
                SUFFS__DATA_NAME=suffs, $
                UNITS_NAME=units_name

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE SIZE(data,/TYPE) OF
     0: BEGIN
        MESSAGE,"Yeah, right!"
        STOP
     END
     8:
     7: BEGIN
        STOP
     END
  ENDCASE

  nPoles      = N_ELEMENTS(poles)
  nFreqBounds = N_ELEMENTS(freqBounds[0,*])
  nSuffs      = N_ELEMENTS(suffs)

  IF ( (nPoles NE nFreqBounds) AND (nPoles NE 0) ) OR ( (nPoles NE nSuffs) AND (nPoles NE 0) ) THEN BEGIN
     MESSAGE,"Leroy … you're free! (Unequal # freqBounds and poles!)",/CONTINUE
     STOP
  ENDIF

  tmplt  = {  $                 ;TIME         : magz.x[tmpI]          , $
           ;; COMP1        : magx.y[tmpI]          , $
           ;; COMP2        : magy.y[tmpI]          , $
           ;; COMP3        : magz.y[tmpI]          , $
           ;; TIME         : magz.x[tmpI]          , $
           ;; COMP1        : eAlongVInterp[tmpI]   , $
           TIME         : data.x          , $
           COMP1        : data.y   , $
           NCOMP        : 1               , $
           DATA_NAME    : data_name, $
           VALID        : 1               , $
           PROJECT_NAME : 'FAST'          , $
           UNITS_NAME   : units_name      , $
           CALIBRATED   : 1}

  filtereds         = !NULL
  FOR k=0,nPoles-1 DO BEGIN

     tmp            = tmplt
     tmp.data_name += suffs[k]

     PRINT,tmp.data_name + ' ...'
     
     tmpFreqBounds  = freqBounds[*,k]

     IF twoPole THEN BEGIN
        tmpPole     = poles[*,k]
     ENDIF ELSE BEGIN
        tmpPole     = poles[k]
     ENDELSE
     
     FA_FIELDS_FILTER,tmp,tmpFreqBounds, $
                      DB=FFTdb, $
                      POLES=tmpPole

     filtereds = [filtereds,TEMPORARY(tmp)]

  ENDFOR

END

PRO STRANGEWAY_3BANDS__8HZ, $
   TPLT_VARS=tPlt_vars, $
   ;; INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
   SCREEN_PLOT=screen_plot, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   USE_JE_TBOUNDS=use_Je_tBounds, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   INCLUDE_PARTICLES=include_particles, $
   DECIMATE_E_AND_B__THEN_CALC_PFLUX=decimate_eb_calc_pFlux, $
   SKIPDSP=skipDSP, $
   ;; SAVE_INDIVIDUAL_ORBIT=save_individual_orbit, $
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

  ;;Gap dist is NOT A TIME
  ;;Rather, it is the number of allowable average dt's for interpolation of a time series within STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS
  gap_dist = 2.5

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

  @strangeway_3bands__defaults__8hz.pro
  
  psym_ptcl    = 3              ;period
  symsize_ptcl = 5.0
  nn           = N_ELEMENTS(data_quants)

  IF (nn GT 1) THEN FOR n = nn-1L,1L,-1L DO STORE_DATA,data_quants[n].name,/DELETE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 1 - DC Mag data

  UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

  IF (N_ELEMENTS(orbit) GT 0) THEN BEGIN

     orbString           = STRING(FORMAT='(I0)',orbit)
     outPlotName        += '--' + orbString
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

     ;;Also want mag flags for later—because how else will we know if the data are worth a hang?
     GET_DATA,'MAG_FLAGS',DATA=magFlags
     nMagFlagsm1 = N_ELEMENTS(magFlags.x)-1

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



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 6 - VLF data

  ;;DSP_V5-V8HG or DSP_V5-V8
  IF ~KEYWORD_SET(skipDSP) THEN BEGIN

     prog = GETENV('FASTBIN') + '/showDQIs'
     IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
        IF (sdt_idx GE 10) THEN BEGIN
           sidstr = STRING(sdt_idx,FORMAT='(I2)')
        ENDIF ELSE BEGIN
           sidstr = STRING(sdt_idx,FORMAT='(I1)')
        ENDELSE
        SPAWN,[prog, sidstr],result,/NOSHELL
     ENDIF ELSE BEGIN
        SPAWN,prog, result,/NOSHELL
     ENDELSE
     b = WHERE(STRPOS(result,'DspADC_V5-V8HG') GE 0,ndsphg)
     IF (ndsphg GT 0) THEN BEGIN
        IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN ndsphg = 0
     ENDIF
     b = WHERE((STRPOS(result,'DspADC_V5-V8') GE 0) AND $
               (STRPOS(result,'DspADC_V5-V8HG') LT 0),ndsp)
     IF (ndsp GT 0) THEN BEGIN
        IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN ndsp = 0
     ENDIF

     if (ndsphg GT 0) THEN BEGIN
        data = GET_FA_FIELDS('DspADC_V5-V8HG',/DEFAULT) 
     ENDIF ELSE BEGIN
        IF (ndsp GT 0) THEN BEGIN
           data = GET_FA_FIELDS('DspADC_V5-V8',/DEFAULT)
        ENDIF
     ENDELSE
     ndsp = (ndsp GT 0) or (ndsphg GT 0)

     canDSP = nDSP NE 0
     IF ~canDSP THEN BEGIN
        PRINT,'Junk DSP data'
        ;; RETURN
     ENDIF

     IF canDSP THEN BEGIN

        tmp_i = WHERE((data.time GE (tBounds[0]-tBuf)) AND $
                      (data.time LE (tBounds[1]+tBuf)),nTmp)

        IF nTmp GT 1 THEN BEGIN

           nDSP = nTmp

           data   = {x:data.time[tmp_i], y:ALOG10(data.comp1[tmp_i,*]), v:data.yaxis}
           STORE_DATA,'DSP_V5-V8', DATA=data
           dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-14,-4], $
                     ytitle:'AC E 55m!C!C(kHz)', ylog:1, $
                     ztitle: '(V/m)!U2!N/Hz', panel_size:2}
           STORE_DATA,'DSP_V5-V8', dlimit=dlimit
           OPTIONS,'DSP_V5-V8','x_no_interp',1
           OPTIONS,'DSP_V5-V8','y_no_interp',1


           ;;  look for big jumps in time - blank these

           GET_DATA,'DSP_V5-V8',DATA=data
           dt                               = data.x[1:*]-data.x[0:*]
           ntimes                           = N_ELEMENTS(data.x)
           bg                               =WHERE(dt GT 300,ng)
           IF (ng GT 0) THEN BEGIN
              bbb                           = bg-1
              IF (bbb[0] LT 0) THEN bbb[0]  = 0
              add_tag                       = [data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
              flag_dat                      = fltarr(ng*2)+!values.f_nan
              new_tag                       = [data.x,add_tag]
              tsort                         = SORT(new_tag-new_tag[0])
              nvec                          = N_ELEMENTS(data.y)/ntimes
              new_dat                       = FLTARR(N_ELEMENTS(new_tag),nvec)
              FOR nv=0,nvec-1 DO BEGIN
                 new_dat[*,nv]              = [data.y[*,nv],flag_dat]
                 new_dat[*,nv]              = new_dat[tsort,nv]
              ENDFOR
              DATA={x:new_tag[tsort],y:new_dat,v:data.v}
              STORE_DATA,'DSP_V5-V8',DATA=data
           ENDIF

           IF (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars = ['DSP_V5-V8'] ELSE tPlt_vars = ['DSP_V5-V8',tPlt_vars]

           IF (KEYWORD_SET(screen_plot)) THEN BEGIN
              LOADCT2,40
              TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tBounds
           ENDIF

           ;;Now integrate
           data.y    = 10.^data.y
           data.v   *= 1000.

           integData = MAKE_ARRAY(N_ELEMENTS(data.x),VALUE=0.)

           tmpF_i = LINDGEN(N_ELEMENTS(data.v)-1)+1
           FOR m=0,N_ELEMENTS(data.x)-1 DO BEGIN

              ;;"Intergrate," as some have it, and apply test
              integData[m] = INT_TABULATED(data.v[tmpF_i],data.y[m,tmpF_i])
           ENDFOR

           ;;Get rid of the square so that units are V/m
           integData = SQRT(integData)

           dsp      = {x:data.x, $
                       y:integData}

           ;;I'm going to put DSP stuff in the FOR loop that goes over intervals. Right now it's clouding my thinking about how to
           ;;handle ephemeris data
           ;; PRINT,'SMOOTHDSP'
           ;; DSP       = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           ;;             dsp, $
           ;;             INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           ;;             ;; /USE_DOUBLE_STREAKER, $
           ;;             EIGHTHZ_TS=tS_0_125s)


           ;; STORE_DATA,'DSP_integ',DATA={x:data.x,y:data.y}
           ;; STORE_DATA,'DSP_integ',DATA={x:DSP.x,y:dsp.DC+dsp.AC}
           ;; dlimit = {ystyle:1, yrange:[0.0,0.05], $
           ;;           ytitle:'ELF Amplitude (V/m)', $
           ;;           panel_size:3}
           ;; STORE_DATA,'DSP_integ', dlimit=dlimit
           ;; OPTIONS,'DSP_integ','x_no_interp',1
           ;; OPTIONS,'DSP_integ','y_no_interp',1


        ENDIF ELSE BEGIN

        ENDELSE

     ENDIF

  ENDIF ELSE BEGIN
     canDSP       = 0
  ENDELSE

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
  STORE_DATA,'pFluxDC',DLIMITS=dLimit
  OPTIONS,'pFluxDC','x_no_interp',1
  OPTIONS,'pFluxDC','y_no_interp',1


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
  STORE_DATA,'pFluxAC',DLIMITS=dLimit
  OPTIONS,'pFluxAC','x_no_interp',1
  OPTIONS,'pFluxAC','y_no_interp',1

  dLimit = {spec:0, $
            ystyle:1, $
            ytitle:'SFlux Wave!C[0.5-4 Hz]!C(mW/m!U2!N)', $
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
  STORE_DATA,'pFluxACHigh',DLIMITS=dLimit
  OPTIONS,'pFluxACHigh','x_no_interp',1
  OPTIONS,'pFluxACHigh','y_no_interp',1

  IF canDSP THEN BEGIN

     dlimit = {ystyle:1, $
               yrange:[0.0,0.05], $
               ytitle:'ELF Amplitude (V/m)', $
               panel_size:3}
     STORE_DATA,'DSP_integ',dlimit=dlimit
     OPTIONS,'DSP_integ','x_no_interp',1
     OPTIONS,'DSP_integ','y_no_interp',1

  ENDIF

  IF KEYWORD_SET(include_particles) THEN BEGIN

     ;;Je
     YLIM,'Je_tmp',6,12,0                                               ; set y limits
     OPTIONS,'Je_tmp','ytitle','Downgoing Elec.!CFlux!C#/cm!U2!N-s)'    ; set y title
     OPTIONS,'Je_tmp','panel_size',3                                    ; set panel size
     OPTIONS,'Je_tmp','yticks',6                                        ; set y-axis labels
     OPTIONS,'Je_tmp','ytickv',[6,7,8,9,10,11,12]                       ; set y-axis labels
     OPTIONS,'Je_tmp','ytickname',['10!U6!N','10!U7!N','10!U8!N','10!U9!N','10!U10!N', $
                                   '10!U11!N','10!U12!N'] ; set y-axis labels
     OPTIONS,'Je_tmp','ynozero',1
     OPTIONS,'Je_tmp','psym',psym_ptcl          ;period symbol
     OPTIONS,'Je_tmp','symsize',symsize_ptcl    ;period symbol

     ;;JEe
     YLIM,'JEe_tmp',-5,1,0                                                     ; set y limits
     OPTIONS,'JEe_tmp','ytitle','Downgoing Elec.!CEnergy Flux!CmW/(m!U2!N)'    ; set y title
     OPTIONS,'JEe_tmp','panel_size',3                                          ; set panel size
     OPTIONS,'JEe_tmp','yticks',6                                              ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickv',[-5,-4,-3,-2,-1,0,1]                           ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickname',['10!U-5!N','10!U-4!N','10!U-3!N', $
                                    '10!U-2!N','10!U-1!N','10!U0!N','10!U1!N'] ; set y-axis labels
     OPTIONS,'JEe_tmp','x_no_interp',1
     OPTIONS,'JEe_tmp','y_no_interp',1
     OPTIONS,'JEe_tmp','psym',psym_ptcl          ;period symbol
     OPTIONS,'JEe_tmp','symsize',symsize_ptcl    ;period symbol

     ;;Ji
     YLIM,'Ji_tmp',4,10,0                                                  ; set y limits
     OPTIONS,'Ji_tmp','ytitle','Upward Ion!CNumber Flux!C(#/cm!U2!N-s)'    ; set y title
     OPTIONS,'Ji_tmp','panel_size',3                                       ; set panel size
     OPTIONS,'Ji_tmp','yticks',6                                           ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickv',[4,5,6,7,8,9,10]                            ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickname',['10!U4!N','10!U5!N','10!U6!N', $
                                   '10!U7!N','10!U8!N','10!U9!N','10!U10!N'] ; set y-axis labels
     OPTIONS,'Ji_tmp','ynozero',1
     OPTIONS,'Ji_tmp','psym',psym_ptcl          ;period symbol
     OPTIONS,'Ji_tmp','symsize',symsize_ptcl    ;period symbol

  ENDIF

  ;;Will want these throughout the loop
  GET_DATA,'dB_fac_v',DATA=magData
  GET_DATA,eAV_variable,DATA=eAlongV

  infoStruct = {full_pFlux             : BYTE(KEYWORD_SET(full_pFlux)), $
                decimate_eb_calc_pFlux : BYTE(KEYWORD_SET(decimate_eb_calc_pFlux)), $
                interp_4Hz_to_1s       : BYTE(KEYWORD_SET(interp_4Hz_to_1s      )), $
                include_E_near_B       : BYTE(KEYWORD_SET(include_E_near_B)), $
                eField_fit_variables   : BYTE(KEYWORD_SET(use_eField_fit_variables)), $
                skipDSP                : BYTE(KEYWORD_SET(skipDSP))}

  ;;Now loop over stuff
  FOR jj=0,number_of_intervals-1 DO BEGIN

     itvlString   = STRCOMPRESS(jj,/REMOVE_ALL)
     tmpPlotName  = outPlotName + '__' + itvlTypeName + '_' + itvlString
     tmpFile      = indivOrbPref + orbString + '__' + itvlTypeName + '_' + itvlString + '.sav'

     ;;Pick up times for this interval
     itvlT1       = time_ranges[jj,0]
     itvlT2       = time_ranges[jj,1]

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
                     ;; ilng            : ephem.ilng   [tmpEphemInds], $
                     mlt             : ephem.mlt    [tmpEphemInds], $
                     fa_vel          : ephem.fa_vel [tmpEphemInds,*], $
                     bfoot           : ephem.bfoot  [tmpEphemInds,*], $
                     lat             : ephem.lat    [tmpEphemInds], $
                     lng             : ephem.lng    [tmpEphemInds], $
                     ;; flat            : ephem.flat   [tmpEphemInds], $
                     ;; flng            : ephem.flng   [tmpEphemInds], $
                     b_model         : ephem.b_model[tmpEphemInds,*]}


     tmp_tBounds = [itvlT1,itvlT2]

     ;;Interp time series
     ;; tmpTS_1s    = DOUBLE(LINDGEN(CEIL(itvlT2-itvlT1))+ROUND(itvlT1))
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

     ;; Mag flags are nice
     ;; mintime = MIN(ABS(tmp_tBounds[0]-magFlags.x),ind1)
     ;; mintime = MIN(ABS(tmp_tBounds[1]-magFlags.x),ind2)
     mintime = MIN(ABS(tmpTS_1s[0]-magFlags.x),ind1)
     mintime = MIN(ABS(tmpTS_1s[-1]-magFlags.x),ind2)

     ;;Grab one before, if that's not what we have
     IF (magFlags.x[ind1] GT tmp_tBounds[0]) AND (ind1 GT 0) THEN BEGIN
        ind1--
     ENDIF
     ;;Grab one after, if that's not what we have
     IF (magFlags.x[ind2] GT tmp_tBounds[0]) AND (ind2 LT nMagFlagsm1) THEN BEGIN
        ind2++
     ENDIF
     tmpMagFlags = {x  : magFlags.x[ind1:ind2], $
                    y  : magFlags.y[ind1:ind2]}

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

     
     ;; I repeat, from UCLA_MAG_DESPIN:
     ;;"   Field-aligned coordinates defined as: 
     ;;"   z-along B, y-east (BxR), x-nominally out"
     ;;    (ind 2)    (ind 1)       (ind 0)


     dBv = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magv, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s, $
           ERRFILE=errFile, $
           ORBSTRING=orbString)
     IF SIZE(dBv,/TYPE) EQ 7 THEN BEGIN
        RETURN
        ;; err = dBv
        ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
        ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
        ;; CLOSE,thisLun
        ;; FREE_LUN,thisLun
     ENDIF

     dBB = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magB, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s, $
           ERRFILE=errFile, $
           ORBSTRING=orbString)
     IF SIZE(dBB,/TYPE) EQ 7 THEN BEGIN
        RETURN
        ;; err = dBB
        ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
        ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
        ;; CLOSE,thisLun
        ;; FREE_LUN,thisLun
     ENDIF

     dBp = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           magP, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s, $
           ERRFILE=errFile, $
           ORBSTRING=orbString)
     IF SIZE(dBp,/TYPE) EQ 7 THEN BEGIN
        RETURN
        ;; err = dBp
        ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
        ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
        ;; CLOSE,thisLun
        ;; FREE_LUN,thisLun
     ENDIF

     eAV = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
           eAVTmp, $
           INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
           GAP_DIST=gap_dist, $
           EIGHTHZ_TS=tmpTS_0_125s, $
           SIXTEENHZ_TS=tmpTS_0_0625s, $
           ERRFILE=errFile, $
           ORBSTRING=orbString)
     IF SIZE(eAV,/TYPE) EQ 7 THEN BEGIN
        RETURN
        ;; err = eAV
        ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
        ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
        ;; CLOSE,thisLun
        ;; FREE_LUN,thisLun
     ENDIF

     IF KEYWORD_SET(include_E_near_B) THEN BEGIN

        GET_DATA,eNB_variable,DATA=eNearB

        mintime = MIN(ABS(tmp_tBounds[0]-eNearB.x),ind1)
        mintime = MIN(ABS(tmp_tBounds[1]-eNearB.x),ind2)

        IF ind1 EQ ind2 THEN BEGIN
           PRINT,'No usable eNearB data here. Excluding eNearB ...'
           include_E_near_B = 0
           full_pFlux       = 0
        ENDIF ELSE BEGIN

           eNearB = {x:eNearB.x[ind1:ind2], $
                     y:eNearB.y[ind1:ind2]}

           eNB    = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
                    eNearB, $
                    INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
                    GAP_DIST=gap_dist, $
                    EIGHTHZ_TS=tmpTS_0_125s, $
                    SIXTEENHZ_TS=tmpTS_0_0625s, $
                    ERRFILE=errFile, $
                    ORBSTRING=orbString)
           IF SIZE(eNB,/TYPE) EQ 7 THEN BEGIN
              RETURN
              ;; err = eNB
              ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
              ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
              ;; CLOSE,thisLun
              ;; FREE_LUN,thisLun
           ENDIF
        ENDELSE

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Poynting fluxes!
     IF KEYWORD_SET(decimate_eb_calc_pFlux) THEN BEGIN

        ;;In this case, use decimated E and B to calc pFlux

        IF KEYWORD_SET(full_pFlux) THEN BEGIN

           pFBACHigh = dBp.ACHigh*eAV.ACHigh/mu_0 ;Poynting flux along B
           pFPACHigh = (eNB.ACHigh*dBv.ACHigh - $
                        1.*dBB.ACHigh*eAV.ACHigh)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVACHigh = (-1.)*eNB.ACHigh*dBp.ACHigh/mu_0

           pFBAC = dBp.AC*eAV.AC/mu_0 ;Poynting flux along B
           pFPAC = (eNB.AC*dBv.AC - $
                    1.*dBB.AC*eAV.AC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVAC = (-1.)*eNB.AC*dBp.AC/mu_0

           pFBDC  = dBp.DC*eAV.DC/mu_0 ;Poynting flux along B
           pFPDC  = (eNB.DC*dBv.DC - $
                     1.*dBB.DC*eAV.DC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVDC  = (-1.)*eNB.DC*dBp.DC/mu_0

        ENDIF ELSE BEGIN

           pFBACHigh =       dBp.ACHigh *eAV.ACHigh/mu_0 ;Poynting flux along B
           pFPACHigh = (-1.)*dBB.ACHigh*eAV.ACHigh/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

           pFBAC =       dBp.AC *eAV.AC/mu_0 ;Poynting flux along B
           pFPAC = (-1.)*dBB.AC*eAV.AC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

           pFBDC =       dBp.DC *eAV.DC/mu_0 ;Poynting flux along B
           pFPDC = (-1.)*dBB.DC*eAV.DC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ENDELSE

        pFB       = {DC    : TEMPORARY(pFBDC), $
                     AC    : TEMPORARY(pFBAC), $
                     ACHigh: TEMPORARY(pFBACHigh)}
        pFP       = {DC    : TEMPORARY(pFPDC), $
                     AC    : TEMPORARY(pFPAC), $
                     ACHigh:TEMPORARY(pFPACHigh)}

        IF KEYWORD_SET(full_pFlux) THEN BEGIN
           pFV    = {DC    : TEMPORARY(pFVDC), $
                     AC    : TEMPORARY(pFVAC), $
                     ACHigh: TEMPORARY(pFVACHigh)}
        ENDIF

     ENDIF ELSE BEGIN

        ;;In this case calc pFlux from E and B field before decimation, and THEN decimate

        ;;But yes, we must align time series
        eAlongVtoMag = DATA_CUT(eAVTmp,magp.x)

        IF KEYWORD_SET(full_pFlux) THEN BEGIN

           eNearBtoMag = DATA_CUT(eNearB,magp.x)

           pFluxB = {x:magp.x, $
                     y:magp.y*eAlongVtoMag/mu_0} ;Poynting flux along B
           pFluxP = {x:magp.x, $
                     y:(eNearBtoMag*magv.y - $
                        1.*magB.y*eAlongVtoMag)/mu_0} ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFluxV = {x:magp.x, $
                     y:(-1.)*eNearBtoMag*magp.y/mu_0}

        ENDIF ELSE BEGIN

           pFluxB = {x:magp.x, $
                     y:      magp.y*eAlongVtoMag/mu_0} ;Poynting flux along B
           pFluxP = {x:magp.x, $
                     y:(-1.)*magB.y*eAlongVtoMag/mu_0} ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ENDELSE

        ;;Now separate into low and high frequencies
        pFB       = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
                    pFluxB, $
                    INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
                    GAP_DIST=gap_dist, $
                    EIGHTHZ_TS=tmpTS_0_125s, $
                    SIXTEENHZ_TS=tmpTS_0_0625s, $
                    ERRFILE=errFile, $
                    ORBSTRING=orbString)
        IF SIZE(pFB,/TYPE) EQ 7 THEN BEGIN
           RETURN
           ;; err = pFB
           ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
           ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
           ;; CLOSE,thisLun
           ;; FREE_LUN,thisLun
        ENDIF

        pFP       = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
                    pFluxP, $
                    INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
                    GAP_DIST=gap_dist, $
                    EIGHTHZ_TS=tmpTS_0_125s, $
                    SIXTEENHZ_TS=tmpTS_0_0625s, $
                    ERRFILE=errFile, $
                    ORBSTRING=orbString)
        IF SIZE(pFP,/TYPE) EQ 7 THEN BEGIN
           RETURN
           ;; err = pFP
           ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
           ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
           ;; CLOSE,thisLun
           ;; FREE_LUN,thisLun
        ENDIF

        IF KEYWORD_SET(full_pFlux) THEN BEGIN
           pFV    = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
                    pFluxV, $
                    INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
                    GAP_DIST=gap_dist, $
                    EIGHTHZ_TS=tmpTS_0_125s, $
                    SIXTEENHZ_TS=tmpTS_0_0625s, $
                    ERRFILE=errFile, $
                    ORBSTRING=orbString)
           IF SIZE(pFV,/TYPE) EQ 7 THEN BEGIN
              RETURN
              ;; err = pFV
              ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
              ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
              ;; CLOSE,thisLun
              ;; FREE_LUN,thisLun
           ENDIF

        ENDIF        

     ENDELSE

     pFB.DC        *= 1e-9      ;Junk that nano prefix in nT
     pFP.DC        *= 1e-9

     pFB.AC        *= 1e-9      ;Junk that nano prefix in nT
     pFP.AC        *= 1e-9

     pFB.ACHigh    *= 1e-9      ;Junk that nano prefix in nT
     pFP.ACHigh    *= 1e-9

     IF KEYWORD_SET(full_pFlux) THEN BEGIN

        pFV.DC     *= 1e-9
        pFV.AC     *= 1e-9
        pFV.ACHigh *= 1e-9

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;DC Poynting flux

     tField = tmpTS_0_125s
     doDat  = REFORM(pFB.DC,N_ELEMENTS(pFB.DC))

     ;;Make all downgoing, pre-log
     good_i = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.00,nGood, $
                    COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     tField = tField[good_i]
     doDat  = ALOG10(ABS(doDat[good_i]))

     tmp    = {x:tField, $
               y:doDat}
     STORE_DATA,'pFluxDC',DATA=TEMPORARY(tmp)

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxDC'] $
     ELSE tPlt_vars = ['pFluxDC',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tmp_tBounds
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;AC Poynting flux

     tField = tmpTS_0_125s
     doDat  = REFORM(pFB.AC,N_ELEMENTS(pFB.AC))

     ;;Make all downgoing, pre-log
     good_i = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.0,nGood, $
                    COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     tField = tField[good_i]
     doDat  = ALOG10(ABS(doDat[good_i]))
     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxAC',DATA=TEMPORARY(tmp)

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxAC'] $
     ELSE tPlt_vars=['pFluxAC',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tmp_tBounds
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;ACHigh Poynting flux

     tField = tmpTS_0_125s
     doDat  = REFORM(pFB.ACHigh,N_ELEMENTS(pFB.ACHigh))

     ;;Make all downgoing, pre-log
     good_i = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.0,nGood, $
                    COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     tField = tField[good_i]
     doDat  = ALOG10(ABS(doDat[good_i]))
     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxACHigh',DATA=TEMPORARY(tmp)

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxACHigh'] $
     ELSE tPlt_vars=['pFluxACHigh',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tmp_tBounds
     ENDIF


     ;;DSP STUFFFFFFFFFFFFFFFFFFFFFFFFFOOOOOOOOOOOOOOHHHHHHYYYYYEEEEAAAAHHHHH
     ;;DSP trim
     ;;And DSP
     IF canDSP THEN BEGIN

        mintime = MIN(ABS(tmp_tBounds[0]-dsp.x),ind1)
        mintime = MIN(ABS(tmp_tBounds[1]-dsp.x),ind2)

        IF ind1 EQ ind2 THEN BEGIN
           PRINT,'No usable DSP data here. Skipping interval ...'
           CONTINUE
        ENDIF

        STORE_DATA,'DSP_integ',DATA=dsp

        tmpDSP = STRANGEWAY_DECIMATE__8_HZ_THREE_BANDS( $
                 dsp, $
                 INTERP_8HZ_RES_TO_0_125S_TIMESERIES=interp_8Hz_to_0_125s, $
                 GAP_DIST=gap_dist, $
                 /USE_DOUBLE_STREAKER, $
                 EIGHTHZ_TS=tmpTS_0_125s, $
                 SIXTEENHZ_TS=tmpTS_0_0625s, $
                 ERRFILE=errFile, $
                 ORBSTRING=orbString)
        IF SIZE(tmpDSP,/TYPE) EQ 7 THEN BEGIN
           RETURN
           ;; err = tmpDSP
           ;; OPENW,thisLun,errFile,/GET_LUN,/APPEND
           ;; PRINTF,thisLun,FORMAT='("Orbit ",A0," err: ",A0)',orbString,err
           ;; CLOSE,thisLun
           ;; FREE_LUN,thisLun
        ENDIF

     ENDIF

;particles
; Step 4 - Electron junk
     IF KEYWORD_SET(include_particles) THEN BEGIN

        GET_DATA,'Je',DATA=tmp

        tmpJe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                tmp, $
                INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                /NO_SEPARATE_DC_AC, $
                /USE_DOUBLE_STREAKER, $
                ONESEC_TS=tmpTS_1s)
        
        ;;Make all downgoing, pre-log
        good_i   = WHERE(FINITE(tmpJe.y) AND tmpJe.y GT 0.00,nGood, $
                         COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

        STORE_DATA,'Je_tmp',DATA={x:tmpJe.x[good_i],y:ALOG10(tmpJe.y[good_i])}

; Electron energy flux

        ;; t           = 0.0D
        ;; tmp         = GET_FA_EES_C(t,/EN)
        ;; IF tmp.valid EQ 0 THEN BEGIN
        ;;    PRINT,'Junk'
        ;;    RETURN
        ;; ENDIF
        ;; last_index  = LONG(tmp.index)
        ;; t1          = 0.0D
        ;; t2          = 0.0D
        ;; temp        = GET_FA_EES(t1,INDEX=0.0D)
        ;; temp        = GET_FA_EES(t2,INDEX=DOUBLE(last_index))

        ;;Did this up top
        ;; GET_2DT,'je_2d_fs','fa_ees_c',name='JEe',t1=itvlT1,t2=itvlT2,energy=energy_electrons
        GET_DATA,'JEe',DATA=tmp

        tmpJEe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                 tmp, $
                 INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                 /NO_SEPARATE_DC_AC, $
                 /USE_DOUBLE_STREAKER, $
                 ONESEC_TS=tmpTS_1s)
        
        ;;Make all downgoing, pre-log
        good_i   = WHERE(FINITE(tmpJEe.y) AND tmpJEe.y GT 0.00,nGood, $
                         COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

        STORE_DATA,'JEe_tmp',DATA={x:tmpJEe.x[good_i],y:ALOG10(tmpJEe.y[good_i])}

; Step 5 - Ion flux

        ;; IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]
        ;; GET_2DT,'j_2d_fs','fa_ies_c',name='Ji',t1=itvlT1,t2=itvlT2,energy=energy_ions

        GET_DATA,'Ji',DATA=tmp

        tmpJi = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                tmp, $
                INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                /NO_SEPARATE_DC_AC, $
                /USE_DOUBLE_STREAKER, $
                ONESEC_TS=tmpTS_1s)

        ;;Make all upgoing, pre-log
        good_i   = WHERE(FINITE(tmpJi.y) AND tmpJi.y GT 0.0,nGood, $
                         COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

        STORE_DATA,'Ji_tmp',DATA={x:tmpJi.x[good_i],y:ALOG10(tmpJi.y[good_i])}

     ENDIF

; STEP 6 - Clean up and return

     tPlt_vars    = ['dB_fac_v','pFluxDC','pFluxAC','pFluxACHigh']
     IF KEYWORD_SET(include_particles) THEN BEGIN
        tPlt_vars = [tPlt_vars,'JEe_tmp','Je_tmp','Ji_tmp']
     ENDIF

     IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotDirSuff
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+tmpPlotName+'.ps',FONT=0 
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+tmpPlotName,/PORT,FONT=-1
              DEVICE,/PALATINO,FONT_SIZE=8


           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE
        
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tmp_tBounds


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

     ENDIF

     IF ~KEYWORD_SET(include_E_near_B) THEN BEGIN
        eNB           = eAV
        eNB.DC[*]     = 0.
        eNB.AC[*]     = 0.
        eNB.ACHigh[*] = 0.
     ENDIF

     ;; dBp  = {DC        :dBp.DC, $
     ;;         AC        :dBp.AC, $
     ;;         ACHigh    :dBp.ACHigh, $
     ;;         common_ts :1B}

     ;; dBv  = {DC        :dBv.DC, $
     ;;         AC        :dBv.AC, $
     ;;         ACHigh    :dBv.ACHigh}

     ;; dBB  = {DC        :dBB.DC, $
     ;;         AC        :dBB.AC, $
     ;;         ACHigh    :dBB.ACHigh}

     ;; eAV     = {DC:eAV.DC, $
     ;;            AC:eAV.AC, $
     ;;            common_ts:1B}

     ;; eNB     = {DC:eNB.DC, $
     ;;            AC:eNB.AC}

     IF canDSP THEN BEGIN
        tmpDSP  = {DC    :tmpDSP.DC, $
                   AC    :tmpDSP.AC, $
                   ACHigh:tmpDSP.ACHigh}
     ENDIF

     IF KEYWORD_SET(include_particles) THEN BEGIN

        tmpJEe  = {x:tmpJEe.x, $
                   y:tmpJEe.y, $
                   common_ts:1B}

        tmpJe   = {y:tmpJe.y}

        tmpJi   = {y:tmpJi.y}


     ENDIF

     ;; dBp     = {DC:dBp.DC, $
     ;;            AC:dBp.AC, $
     ;;            ACHigh:dBp.ACHigh}

     ;; eAV     = {DC:eAV.DC, $
     ;;            AC:eAV.AC, }

     IF KEYWORD_SET(include_particles) THEN BEGIN

        tmpJEe  = {y:tmpJEe.y, $
                   common_ts:1B}

     ENDIF

     tmpStruct = {dB       : {p                    : TEMPORARY(dBp), $
                              v                    : TEMPORARY(dBv), $
                              B                    : TEMPORARY(dBB)}, $
                  e        : {AlongV               : TEMPORARY(eAV), $
                              NearB                : TEMPORARY(eNB), $
                              dsp                  : canDSP ? TEMPORARY(tmpDSP) : 0B}, $
                  ptcl     : ( KEYWORD_SET(include_particles) ? {jEe:tmpJEe,je:tmpJe,ji:tmpJi} : 0B ), $
                  ;; pFlux : {b          : {x:eAV.x,DC:pFB.DC,AC:pFB.AC}, $
                  ;;          p          : {x:eAV.x,DC:pFP.DC,AC:pFP.AC}, $
                  ;;          v          : {x:eAV.x,DC:pFV.DC,AC:pFV.AC}, $
                  pFlux    : CREATE_STRUCT('p',pFP, $
                                           'v',(KEYWORD_SET(full_pFlux) ? pFV : 0B), $
                                           'b',pFB), $
                  magFlags : TEMPORARY(tmpMagFlags)}

     tmpStruct = CREATE_STRUCT(tmpStruct,'ephem',TEMPORARY(tmpEphem),'info',infoStruct)
     
     ;; PRINT,"Adding struct for interval " + itvlString + " in orbit " + orbString + ' ...'
     ;; structList.Add,tmpStruct

     PRINT,"Saving struct for interval" + itvlString + " in orbit " + orbString + ' ...'
     PRINT,"File: " + tmpFile
     SAVE,tmpStruct,FILENAME=outDir+tmpFile

  ENDFOR

END

