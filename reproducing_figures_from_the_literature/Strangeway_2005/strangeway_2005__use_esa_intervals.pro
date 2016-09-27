;;2016/09/26
;;It's better to use the ESA intervals, methinks
PRO STRANGEWAY_2005__USE_ESA_INTERVALS, $
   TPLOT_VARS=tplot_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   USE_FAC_V=use_fac_v, $
   USE_FAC=use_fac, $
   NO_BLANK_PANELS=no_blank_panels, $
   ;; SAVE_1S_DATA=save_1s_data, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   BATCH_MODE=batch_mode, $
   NO_HASH_UPDATE=no_hash_update

; Input needed on:
; (a) Northern/southern hemisphere limits
; (b) ESA data limits
; (c) DSP calibration


; Under development - R. J. Strangeway 4/4/08

; Program will use fac_v if E field data are available, other use fac_v
; over-ride with use_fac_v and use_fac keywords

; if no_blank_panels is not set procedure will generate tplot data for all the parameters,
; including missing data, for a uniform plot product

; Step 0 - safety measure - delete all tplot quantities if found

  @tplot_com

  @startup

  IF ~KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR,0
  ENDIF

  @strway_stuff                 ;List of orbits used, energy thresholds, etc.

  ;; IF N_ELEMENTS(use_fac) EQ 0 AND N_ELEMENTS(use_fac_v) EQ 0 THEN use_fac = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

  mu_0         = DOUBLE(4.0D*!PI*1e-7)

  tBuf         = 10. ;Allowable difference between t{1,2} and nearest fields data

  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_2005/'
  hashFile     = 'Strangeway_et_al_2005__DC_params--ESA_intervals.sav'
  outPlotName  = 'Strangeway_et_al_2005__ion_outflow--ESA_intervals--Fig_3'

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  nn           = N_ELEMENTS(data_quants)

  if (nn GT 1) THEN for n = nn-1L,1L,-1L do STORE_DATA,data_quants(n).name,/delete

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

     ;;  if orbit > 9936 return (temporary fix)

     if (orbit GT 9936) THEN BEGIN

        PRINT,""
        PRINT,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        PRINT,""
        RETURN

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

     ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
        ;; saveStr          = 'SAVE'
     mag              = 0.
     mag1s            = 0.

     eField           = 0.
     eField1s         = 0.

     pFluxB           = 0.
     pFluxB1s         = 0.

     dsp              = 0.
     dsp1s            = 0.
     ;; ENDIF

     cant_pFlex     = 0         ;because we CAN pFlex!

     itvlString     = STRCOMPRESS(jj,/REMOVE_ALL)

     tmpPlotName    = outPlotName + '__itvl_' + itvlString

     t1 = time_ranges[jj,0]
     t2 = time_ranges[jj,1]

     tmp_je_indices = time_range_indices[jj,*]

     ;;Clean up based on ILAT
     GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL
     GET_DATA,'ILAT',DATA=ilat
     IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
        ;; PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
        ;; RETURN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF
     IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
        ;; PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
        ;; RETURN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF

     ;;Make sure we have data where we want it.
     keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        ;; PRINT,'No data above min ILAT. Out!'
        ;; RETURN
        PRINT,'No data above min ILAT. Skipping this interval ...'
        CONTINUE
     ENDIF
     
     ;;Trim time series, if necessary
     IF nKeep LT N_ELEMENTS(je_pristine.x) THEN BEGIN
        closest1 = MIN(ABS(t1-je_pristine.x[keep]),tmpII_t1)
        closest2 = MIN(ABS(t2-je_pristine.x[keep]),tmpII_t2)

        ;;If more than 30 s from previous mark, we're in doubt
        IF (closest1 GT 30) OR (closest2 GT 30) THEN BEGIN
           PRINT,'Either t1 or t2 is more than thirty seconds from the previous mark ...'
           PRINT,'Questionable, indeed. Skipping this interval ...'
           CONTINUE
        ENDIF

        t1             = je_pristine.x[keep[tmpII_t1]]
        t2             = je_pristine.x[keep[tmpII_t2]]
        tmp_je_indices = [keep[tmpII_t1],keep[tmpII_t2]]
     ENDIF

     tLimit_all     = [t1,t2]

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Mag data
     GET_DATA,'dB_fac_v',DATA=data
     
     tmp_i = WHERE((data.x GE (t1-tBuf)) AND (data.x LE (t2+tBuf)),nTmp)
     IF nTmp GT 0 THEN BEGIN

        nMag = nTmp
        data = {x:data.x[tmp_i], $
                y:data.y[tmp_i]}

        ;; t1 = data.x[0]
        ;; t2 = data.x[N_ELEMENTS(data.x)-1L]
        tlimit_all = [t1,t2]
        tplot_vars = 'dB_fac_v'
        OPTIONS,'dB_fac_v','panel_size',2
        OPTIONS,'dB_fac','panel_size',2
        OPTIONS,'dB_sm','panel_size',2

        if (keyword_set(use_fac)) THEN tplot_vars = 'dB_fac'

        if ~KEYWORD_SET(no_blank_panels) AND ~KEYWORD_SET(use_fac) THEN tplot_vars = 'dB_fac_v'

        if (keyword_set(screen_plot)) THEN BEGIN
           loadct2,40
           tplot,tplot_vars,var=['ALT','ILAT','MLT']
        endif

        ;;Smooth to 4-s resolution
        PRINT,'SMOOTHmag'
        var_name = tplot_vars
        GET_DATA,var_name,DATA=data
        FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
        IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN STOP

        sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
        nBufs  = N_ELEMENTS(strt_i)
        FOR k=0, nBufs-1 DO BEGIN

           tmpI          = [strt_i[k]:stop_i[k]]
           smooth_int    = CEIL(sRates[k]/0.25)

           FOR l=0,2 DO BEGIN
              tmp           = {x:data.x[tmpI], $
                               y:data.y[tmpI,l]}

              smoothed      = SMOOTH(tmp.y,smooth_int)

              data.y[tmpI,l]  = smoothed

           ENDFOR

           PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
        ENDFOR

        ;;From UCLA_MAG_DESPIN:
        ;;"   Field-aligned coordinates defined as:
        ;;"   z-along B, y-east (BxR), x-nominally out"
        ;;    (ind 2)    (ind 1)       (ind 0)

        magInd = 1
        data = {x:data.x, $
                y:REFORM(data.y[*,magInd])}

        ;;Interp to 1-s resolution
        FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                          {TIME:data.x,COMP1:data.y}, $
                          RESULT=datInterp, $
                          /INTERP, $
                          ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          DELT_T=(1.01)/MIN(sRates), $
                          /TALK

        ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
        ;;    ;; saveStr += ',mag1s'
        ;;    ;; tmpDatStruct = CREATE_STRUCT("mag",mag)
        ;;    ;; tmp1sStruct  = CREATE_STRUCT("mag",mag1s)
        mag      = data
        mag1s    = datInterp
        ;; ENDIF

        data = {x:[[tS_1s],[tS_1s]], $
                y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                   [TEMPORARY(datInterp)]]}

        STORE_DATA,'dB_fac_interp',DATA=data

        dLimit = {spec:0, ystyle:1, yrange:[-600., 800.], $
                  ytitle:'dB Perp.!C!C[DC] (nT)', $
                  panel_size:3}

        OPTIONS,'dB_fac_interp','colors',[normColorI,normColorI]
        OPTIONS,'dB_fac_interp','tplot_routine','mplot'
        STORE_DATA,'dB_fac_interp',DLIMITS=dLimit
        OPTIONS,'dB_fac_interp','x_no_interp',1
        OPTIONS,'dB_fac_interp','y_no_interp',1


     ENDIF ELSE BEGIN
        PRINT,'No mag data for this interval! Futile?'
        cant_pFlex = 1
        nMag       = 0

     ENDELSE
     ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
     tmpDatStruct = CREATE_STRUCT("mag",mag)
     tmp1sStruct  = CREATE_STRUCT("mag",mag1s)
        ;; saveStr += ',mag1s'
        ;; mag1s    = 0L
     ;; ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Step 2 - E field

     ;; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
     ;; to get the SDT index for this run.  Otherwise "showDQIs" won't
     ;; return.  If this is old, single-user SDT, "sdt_idx" is returned
     ;; as 255 and we handle the call in the old way.

     sdt_idx = GET_SDT_RUN_idx()

     prog = GETENV('FASTBIN') + '/showDQIs'
     if ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
        if (sdt_idx GE 10) THEN BEGIN
           sidstr = string(sdt_idx, format='(I2)')
        endif else BEGIN
           sidstr = string(sdt_idx, format='(I1)')
        endelse
        spawn, [prog, sidstr], result, /noshell
     endif else BEGIN
        spawn, prog, result, /noshell
     endelse


     b = where (strpos(result,'V1-V4_S') GE 0,nb4)
     if (nb4 GT 0) THEN if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nb4 = 0
     b = where (strpos(result,'V1-V2_S') GE 0,nb2)
     if (nb2 GT 0) THEN if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nb2 = 0
     if (nb4 GT 0) THEN v12=get_fa_fields('V1-V4_S',/all) $
     else if (nb2 GT 0) THEN v12=get_fa_fields('V1-V2_S',/all)

     b = where (strpos(result,'V5-V8_S') GE 0,nb5)
     if (nb5 GT 0) THEN v58=get_fa_fields('V5-V8_S',/all)

     got_efield = (nb4 GT 0 or nb2 GT 0) and nb5 GT 0

     if (got_efield) THEN BEGIN

        ;; despin e field data
        FA_FIELDS_DESPIN,v58,v12, $
                         T1=t1, $
                         T2=t2, $
                         /SHADOW_NOTCH, $
                         /SINTERP

        OPTIONS,'EFIT_ALONG_V','yrange',0
        OPTIONS,'EFIT_ALONG_V','ytitle','E along V!C!C[DC] (mV/m)'
        OPTIONS,'EFIT_ALONG_V','colors',[normColorI,normColorI]
        OPTIONS,'EFIT_ALONG_V','panel_size',2

        ;; reset time limits if needed
        GET_DATA,'EFIT_ALONG_V',DATA=data

        tmp_i = WHERE((data.x GE (t1-tBuf)) AND (data.x LE (t2+tBuf)),nTmp)
        IF nTmp GT 0 THEN BEGIN

           nEField = nTmp
           data    = {x:data.x[tmp_i], $
                      y:data.y[tmp_i]}

           STORE_DATA,'EFIT_ALONG_V',DATA=data

           ;; t1 = data.x[0]
           ;; t2 = data.x[N_ELEMENTS(data.x)-1L]

           if ((t1 lt tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
              PRINT,"WHOA BUDDY. WHY?"
              STOP
              if (t1 lt tlimit_all[0]) THEN tlimit_all[0] = t1
              if (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
              get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
              get_new_igrf,/no_store_old
           endif

; check for southern hemisphere and fix
; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

           GET_DATA,'B_model',DATA=bm
           GET_DATA,'fa_vel',DATA=vel
           GET_DATA,'fa_pos',DATA=pos
           n=N_ELEMENTS(reform(pos.y[*,0]))
           rxv = dblarr(n,3)
           rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
           rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
           rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
           vxb = dblarr(n,3)
           vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
           vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
           vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
           tst = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

           GET_DATA,'EFIT_ALONG_V',DATA=data,dlimit=dlimit
           y2=spl_init(pos.x-tlimit_all[0],tst,/double)
           tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
           data.y = data.y*tst_/abs(tst_)

           ;;Smooth to 4-s resolution
           PRINT,'SMOOTHEfield'
           FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
           IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN

              sRates = 1./(data.x[1:-1]-data.x[0:-2])

              ;;Old-fashioned way

              smoothed         = data.y
              FOR k=0,N_ELEMENTS(data.y)-1 DO BEGIN

                 tmpI          = WHERE(ABS(data.x-data.x[k]) LE fields_smoothWindow_halfLength)
                 smoothed[k]   = MEAN(data.y[tmpI])

              ENDFOR
           ENDIF ELSE BEGIN

              sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
              nBufs  = N_ELEMENTS(strt_i)
              FOR k=0, nBufs-1 DO BEGIN

                 tmpI          = [strt_i[k]:stop_i[k]]
                 tmp           = {x:data.x[tmpI], $
                                  y:data.y[tmpI]}

                 smooth_int    = CEIL(sRates[k]/0.25)
                 smoothed      = SMOOTH(tmp.y,smooth_int)

                 data.y[tmpI]  = smoothed

                 PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
              ENDFOR
           ENDELSE

           ;;Interp to 1-s resolution
           FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                             {TIME:data.x,COMP1:data.y}, $
                             RESULT=datInterp, $
                             /INTERP, $
                             ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                             DELT_T=(1.01)/MIN(sRates), $
                             /TALK

           ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
           ;;    ;; saveStr += ',eField1s'
           eField   = data
           eField1s = datInterp
           ;; ENDIF

           data = {x:tS_1s, $
                   y:datInterp}

           ;; data = {x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
           ;;         y:[TRANSPOSE(TEMPORARY(datInterp)), $
           ;;            TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
           ;; data = {x:[[tS_1s],[tS_1s]], $
           ;;         y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
           ;;            [TEMPORARY(datInterp)]]}

           data = {x:[[tS_1s],[tS_1s]], $
                   y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                      [data.y]]}

           STORE_DATA,'EFIT_ALONG_VSC',DATA=data,dlimit=dlimit
           OPTIONS,'EFIT_ALONG_VSC','yrange',0
           OPTIONS,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C[DC] (mV/m)'
           OPTIONS,'EFIT_ALONG_VSC','colors',[normColorI,normColorI]
           OPTIONS,'EFIT_ALONG_VSC','panel_size',2
           OPTIONS,'EFIT_ALONG_VSC','x_no_interp',1
           OPTIONS,'EFIT_ALONG_VSC','y_no_interp',1

           STORE_DATA,'E_NEAR_B',/delete
           STORE_DATA,'E_ALONG_V',/delete
           STORE_DATA,'EFIT_NEAR_B',/delete
           STORE_DATA,'EFIT_ALONG_V',/delete

           if (N_ELEMENTS(tplot_vars) eq 0) THEN tplot_vars=['EFIT_ALONG_VSC'] else tplot_vars=['EFIT_ALONG_VSC',tplot_vars]

           if (keyword_set(screen_plot)) THEN BEGIN
              loadct2,40
              tplot,tplot_vars,var=['ALT','ILAT','MLT']
           endif

        ENDIF ELSE BEGIN
           PRINT,'No E-field data for this interval! Futile?'
           cant_pFlex = 1
           nEField    = 0
           ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
              ;; saveStr += ',eField1s'
              ;; eField1s = 0L
           ;; ENDIF
           
        ENDELSE

        tmpDatStruct = CREATE_STRUCT(tmpDatStruct,"eField",eField)
        tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,"eField",eField1s)

     ENDIF ELSE BEGIN
        IF (N_ELEMENTS(tplot_vars) NE 0) THEN BEGIN
           tplot_vars = 'dB_fac'
           IF (KEYWORD_SET(use_fac_v)) THEN tplot_vars = 'dB_fac_v'
           IF ~KEYWORD_SET(no_blank_panels) THEN tplot_vars = 'dB_fac'
        ENDIF
     ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Step 3 - Poynting flux
     IF ~cant_pFlex THEN BEGIN ;;If you can't not … well, you know what happens in that case.

        ;;Calc pFlux based on original mag time series
        FA_FIELDS_COMBINE,{TIME:tmpDatStruct.mag.x,COMP1:tmpDatStruct.mag.y}, $
                             {TIME:tmpDatStruct.eField.x,COMP1:tmpDatStruct.eField.y}, $
                             RESULT=tmpEFieldMagInterp, $
                             /INTERP, $
                             ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                             DELT_T=(1.01)/MIN(sRates), $
                             /TALK

        pFluxB = (tmpDatStruct.mag.y*1.e-9) * $
                  (TEMPORARY(tmpEFieldMagInterp)/mu_0) ;Poynting flux along B
        pFluxB[WHERE(~FINITE(pFluxB))] = 0.0
        pFluxB = {x:tmpDatStruct.mag.x,y:pFluxB}

        GET_DATA,'dB_fac_interp',DATA=magData
        GET_DATA,'EFIT_ALONG_VSC',DATA=eFData
        ;; pFluxB = REFORM((magData.y[0,*]*1.e-9)*eFData.y[0,*]/mu_0) ;Poynting flux along B
        pFluxB1s = (magData.y[*,1]*1.e-9)*eFData.y[*,1]/mu_0 ;Poynting flux along B
        pFluxB1s[WHERE(~FINITE(pFluxB1s))] = 0.0

        ;; STORE_DATA,'pFlux',DATA={x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
        ;;                          y:[TRANSPOSE(pFluxB), $
        ;;                             TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
        STORE_DATA,'pFlux',DATA={x:[[tS_1s],[tS_1s]], $
                                 y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                                    [pFluxB1s]]}

        ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
           ;; saveStr += ',eField1s'
        ;; PFluxB1s = pFluxB
        ;; ENDIF

        dLimit = {spec:0, ystyle:1, yrange:[-20., 80.], $
                  ytitle:'Poynting Flux!C!C[DC] (mW/m!U2!N)', $
                  panel_size:3}
        OPTIONS,'pFlux','colors',[normColorI,normColorI]
        STORE_DATA,'pFlux',DLIMITS=dLimit
        OPTIONS,'pFlux','x_no_interp',1
        OPTIONS,'pFlux','y_no_interp',1

        IF (N_ELEMENTS(tplot_vars) EQ 0) THEN tplot_vars=['pFlux'] $
        ELSE tplot_vars=['pFlux',tplot_vars]

        IF (KEYWORD_SET(screen_plot)) THEN BEGIN
           LOADCT2,40
           TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
        ENDIF
     ENDIF
     tmpDatStruct = CREATE_STRUCT(tmpDatStruct,"pFluxB",pFluxB)
     tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,"pFluxB",pFluxB1s)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Step 4 - Electron junk, AND
     ;; Step 5 - Ion flux

     ;;Handle ion adjustment
     IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]

     ;; last_index  = LONG(tmp.index)
     ;; t1          = 0.0D
     ;; t2          = 0.0D
     ;; temp        = GET_FA_EES(t1,INDEX=0.0D)
     ;; temp        = GET_FA_EES(t2,INDEX=DOUBLE(last_index))

     types_2dt = ['je_2d_fs','j_2d_fs','j_2d_fs']
     routs_2dt = ['fa_ees_c','fa_ees_c','fa_ies_c']
     names_2dt = ['JEe','Je','Ji']

     enrgy_2dt = [[energy_electrons],[energy_electrons],[energy_ions]]
     titls_2dt = ['Electron!CEnergy Flux!CmW/(m!U2!N)', $
                  'Electron Flux!C!C#/(cm!U2!N-s)', $
                  'Ion Flux!C#/(cm!U2!N-s)']
     lims_2dt  = [[-1.,6.,0],[-5.e9,1.5e10,0],[-1.e9,6.e9,0]]
     nFlux_2dt = MAKE_ARRAY(N_ELEMENTS(types_2dt),/LONG)

     FOR ll=0,N_ELEMENTS(types_2dt)-1 DO BEGIN

        tmpType = types_2dt[ll]
        tmpRout = routs_2dt[ll]
        tmpName = names_2dt[ll]
        tmpNrg  = enrgy_2dt[*,ll]
        tmpTitl = titls_2dt[ll]
        tmpLims = lims_2dt[*,ll]

        PRINT,"Getting " + tmpName + ' ...'

        GET_2DT,tmpType,tmpRout, $
                NAME=tmpName, $
                T1=t1, $
                T2=t2, $
                ENERGY=tmpNrg
        GET_DATA,tmpName,DATA=tmp

        IF SIZE(tmp,/TYPE) NE 8 THEN BEGIN
           PRINT,tmpName + ' appears to be out of commission ...'
           RETURN
        ENDIF

        keep  = WHERE(FINITE(tmp.y))
        tmp.x = tmp.x[keep]
        tmp.y = tmp.y[keep]
        
        keep  = WHERE(ABS(tmp.y) GT 0.0)
        tmp.x = tmp.x[keep]
        tmp.y = tmp.y[keep]
        
        ;;get timescale monotonic
        time_order = SORT(tmp.x)
        tmp.x = tmp.x[time_order]
        tmp.y = tmp.y[time_order]

        ;;kill dupes
        dupe_i      = WHERE(ABS(tmp.x[1:-1]-tmp.x[0:-2]) LT 0.0001,nDupes, $
                            COMPLEMENT=keep,NCOMPLEMENT=nKeep)
        PRINT,STRCOMPRESS(nDupes,/REMOVE_ALL) + ' ' + tmpName + ' duplicates here'
        tmp.x       = tmp.x[keep]
        tmp.y       = tmp.y[keep]
  
        nFlux_2dt[ll] = N_ELEMENTS(tmp.x)

        IF N_ELEMENTS(tmp.y) GE 5 THEN BEGIN
           tmp.y      = SMOOTH(tmp.y,5)
           doDat      = INTERPOL(tmp.y,tmp.x,tS_1s)
           tmpDat     = {x:tS_1s,y:doDat}

        ENDIF ELSE BEGIN
           PRINT,"Insufficient " + tmpName + " data to do smoothing, so I'll fill a struct with garbaGE for you."
           tmpDat     = {x:[0,0],y:[0,0]}
        ENDELSE

        STORE_DATA,tmpName,DATA=tmpDat

        YLIM,tmpName,tmpLims[0],tmpLims[1],tmpLims[2] ; set y limits
        OPTIONS,tmpName,'ytitle',tmpTitl              ; set y title
        OPTIONS,tmpName,'panel_size',3                ; set panel size
        OPTIONS,tmpName,'x_no_interp',1
        OPTIONS,tmpName,'y_no_interp',1

        tmpDatStruct = CREATE_STRUCT(tmpDatStruct,tmpName+'_time',tmp.x,tmpName,tmp.y)
        tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,tmpName,doDat)

     ENDFOR

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Step 6 - VLF data

     ;;DSP_V5-V8HG or DSP_V5-V8

     prog = GETENV('FASTBIN') + '/showDQIs'
     IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
        IF (sdt_idx GE 10) THEN BEGIN
           sidstr = STRING(sdt_idx, FORMAT='(I2)')
        ENDIF ELSE BEGIN
           sidstr = STRING(sdt_idx, FORMAT='(I1)')
        ENDELSE
        SPAWN, [prog, sidstr], result, /NOSHELL
     ENDIF ELSE BEGIN
        SPAWN, prog, result, /NOSHELL
     ENDELSE
     b = WHERE(STRPOS(result,'DspADC_V5-V8HG') GE 0,ndsphg)
     IF (ndsphg GT 0) THEN BEGIN
        IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN ndsphg = 0
     ENDIF
     b = WHERE((STRPOS(result,'DspADC_V5-V8') GE 0) AND $
               (STRPOS(result,'DspADC_V5-V8HG') LT 0),ndsp)
     IF (ndsp GT 0) THEN BEGIN
        IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN ndsp = 0
     ENDIF

     if (ndsphg GT 0) THEN BEGIN
        data = GET_FA_FIELDS('DspADC_V5-V8HG',/all) 
     ENDIF else BEGIN
        IF (ndsp GT 0) THEN BEGIN
           data = GET_FA_FIELDS('DspADC_V5-V8',/all)
        ENDIF
     ENDELSE
     ndsp = (ndsp GT 0) or (ndsphg GT 0)

     tmp_i = WHERE((data.time GE (t1-tBuf)) AND (data.time LE (t2+tBuf)),nTmp)
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
        dt = data.x[1:*]-data.x[0:*]
        ntimes=N_ELEMENTS(data.x)
        bg = where (dt GT 300, ng)
        if (ng GT 0) THEN BEGIN
           bbb = bg-1
           if (bbb[0] lt 0) THEN bbb[0] = 0
           add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
           flag_dat = fltarr(ng*2)+!values.f_nan
           new_tag = [data.x,add_tag]
           tsort = sort(new_tag-new_tag[0])
           nvec=N_ELEMENTS(data.y)/ntimes
           new_dat = fltarr(N_ELEMENTS(new_tag),nvec)
           for nv = 0,nvec-1 do BEGIN
              new_dat[*,nv] = [data.y[*,nv],flag_dat]
              new_dat[*,nv] = new_dat[tsort,nv]
           endfor
           DATA={x:new_tag[tsort],y:new_dat,v:data.v}
           STORE_DATA,'DSP_V5-V8',DATA=data
        endif

        if (N_ELEMENTS(tplot_vars) eq 0) THEN tplot_vars=['DSP_V5-V8'] else tplot_vars=['DSP_V5-V8',tplot_vars]

        if (keyword_set(screen_plot)) THEN BEGIN
           loadct2,40
           tplot,tplot_vars,var=['ALT','ILAT','MLT']
        endif

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

        data      = {x:data.x, $
                     y:integData}

        PRINT,'SMOOTHDSP'
        FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
        IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN

           sRates = 1./(data.x[1:-1]-data.x[0:-2])

           ;;Old-fashioned way

           smoothed         = data.y
           FOR k=0,N_ELEMENTS(data.y)-1 DO BEGIN

              tmpI          = WHERE(ABS(data.x-data.x[k]) LE DSP_smoothWindow_halfLength)
              smoothed[k]   = MEAN(data.y[tmpI])

           ENDFOR
        ENDIF ELSE BEGIN

           sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
           nBufs  = N_ELEMENTS(strt_i)
           FOR k=0, nBufs-1 DO BEGIN

              tmpI          = [strt_i[k]:stop_i[k]]
              tmp           = {x:data.x[tmpI], $
                               y:data.y[tmpI]}

              smooth_int    = CEIL(sRates[k]/0.25)
              smoothed      = SMOOTH(tmp.y,smooth_int)

              data.y[tmpI]  = smoothed

              PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
           ENDFOR
        ENDELSE

        ;;Interp to 1-s resolution
        FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
                          {TIME:data.x,COMP1:data.y}, $
                          RESULT=datInterp, $
                          /INTERP, $
                          ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                          DELT_T=(1.01)/MIN(sRates), $
                          /TALK

        dsp    = data
        dsp1s  = datInterp

        ;; STORE_DATA,'DSP_integ',DATA={x:data.x,y:integData}
        STORE_DATA,'DSP_integ',DATA={x:tS_1s,y:datInterp}
        dlimit = {ystyle:1, yrange:[0.0,0.05], $
                  ytitle:'ELF Amplitude (V/m)', $
                  panel_size:3}
        STORE_DATA,'DSP_integ', dlimit=dlimit
        OPTIONS,'DSP_integ','x_no_interp',1
        OPTIONS,'DSP_integ','y_no_interp',1


     ENDIF ELSE BEGIN

     ENDELSE
     tmpDatStruct = CREATE_STRUCT(tmpDatStruct,'dsp',dsp)
     tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,'dsp',dsp1s)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; STEP 7 - Clean up and return

     ;; determine tlimit_north and tlimit_south also change plot title

     GET_DATA,'LAT',DATA=data

     if (N_ELEMENTS(data.y) le 0) THEN return

     bb = where (data.y GT 10,nn)
     if (nn GT 0) THEN tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

     bb = where (data.y lt -10,nn)
     if (nn GT 0) THEN tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

     hemisph = getenv('FAST_ORBIT_HEMISPHERE')

     GET_DATA,'ORBIT',DATA=data
     nn = N_ELEMENTS(data.y)/2
     orbit = data.y(nn)
     orbit_lab = strcompress(string(orbit,format="(i5.4)"),/remove_all)
     tplot_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

     ;; force tplot_vars to be all the panels unless no_blank_panels is set

     if ~KEYWORD_SET(no_blank_panels) THEN BEGIN


        ;; DSP

        bdat = where(tplot_vars eq 'DSP_V5-V8',ndat)
        if (ndat eq 0) THEN BEGIN
           t_arr = tlimit_all
           y_arr = fltarr(2,4)
           y_arr[*,*] = !values.f_nan
           v_arr = [0.0,0.128,9.984,16.352]
           STORE_DATA,'DSP_V5-V8', DATA={x:t_arr, y:y_arr, v:v_arr}
           dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
                     ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
                     ztitle: '(V/m)!U2!N/Hz', panel_size:2}
           STORE_DATA,'DSP_V5-V8', dlimit=dlimit
           OPTIONS,'DSP_V5-V8','x_no_interp',1
           OPTIONS,'DSP_V5-V8','y_no_interp',1
        endif

        ;; EFIT_ALONG_VSC

        bdat = where(tplot_vars eq 'EFIT_ALONG_VSC',ndat)
        if (ndat eq 0) THEN BEGIN
           t_arr = tlimit_all
           y_arr = [!values.f_nan,!values.f_nan]
           STORE_DATA,'EFIT_ALONG_VSC', DATA={x:t_arr, y:y_arr}
           dlimit = {spec:0, ystyle:1, yrange:[-1000., 1000.], $
                     ytitle:'EFIT ALONG V!C!C55m (mV/m)', $
                     panel_size:3}
           STORE_DATA,'EFIT_ALONG_V',dlimit=dlimit
           OPTIONS,'EFIT_ALONG_VSC','yrange',[-100.,100.]
           OPTIONS,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
           OPTIONS,'EFIT_ALONG_VSC','panel_size',2
        endif

        ;; dB_fac_v
        ;;CHANGED to dB_fac
        bdat = where(tplot_vars eq 'dB_fac',ndat)
        if (ndat eq 0) THEN BEGIN
           t_arr = tlimit_all
           y_arr = dblarr(2,3)
           y_arr[*,*] = !values.d_nan
           STORE_DATA,'dB_fac', DATA={x:t_arr, y:y_arr}
           OPTIONS,'dB_fac','yrange',[-100,100]
           OPTIONS,'dB_fac','ytitle','dB_fac!C!C(nT))'
           OPTIONS,'dB_fac','panel_size',2
           OPTIONS,'dB_fac','colors',[6,4,2]
           ;; OPTIONS,'dB_fac_v','labels',['v ((BxV)xB)','p (BxV)','b']
           OPTIONS,'dB_fac','labels',['o','e','b']
        endif

        ;; tplot_vars=['SFA_V5-V8','DSP_V5-V8','Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','EFIT_ALONG_VSC','dB_fac_v']

        tplot_vars=['EFIT_ALONG_VSC','dB_fac_interp','pFlux','Je','JEe','DSP_integ','Ji']
     endif

     IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Strangeway_et_al_2005'
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
           ;; (N_ELEMENTS(tlimit_north) GT 0): BEGIN
           ;;    tlimit,tlimit_north
           ;; END
           ;; (N_ELEMENTS(tlimit_south) GT 0): BEGIN
           ;;    tlimit,tlimit_south
           ;; END
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
     PRINT,FORMAT='("Interval",T15,": ",I0)',jj
     PRINT,FORMAT='("N Bfield",T15,": ",I0)',nMag
     PRINT,FORMAT='("N Efield",T15,": ",I0)',nEField
     ;; PRINT,FORMAT='("N Efield ",T25,": ",I0)',nEField
     FOR ll=0,N_ELEMENTS(types_2dt)-1 DO BEGIN
        PRINT,FORMAT='("N ",A0,T15,": ",I0)', $
              names_2dt[ll], $
              nFlux_2dt[ll]
     ENDFOR
     PRINT,FORMAT='("N DSP",T15,": ",I0)',nDSP
     PRINT,''
     PRINT,FORMAT='("N 1-s",T15,": ",I0)',N_ELEMENTS(tS_1s)
     PRINT,''

     ;; IF KEYWORD_SET(save_1s_data) THEN BEGIN
     ;;    saveFilePref = 'strangeway_2005--DC_data--'
     ;;    tmpSaveFile = saveFilePref + '_orb_' + orbString + '_' + itvlString

     ;;    PRINT,"Saving stuff from this interval to " + tmpSaveFile + ' ...'
     ;;    SAVE,mag,eField,fluxes, $
     ;;         mag1s,eField1s,pFluxB1s,fluxes_1s, $
     ;;         FILENAME=outDir+tmpSaveFile

     ;; ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Gather data and STORE
     tmpStruct = ASSEMBLE_STRANGEWAY_2005_STRUCT(tS_1s, $
                                                 orbit, $
                                                 tmpDatStruct, $
                                                 tmp1sStruct, $
                                                 minILAT)

     ;;Clear 'em out
     mag       = !NULL 
     mag1s     = !NULL
     eField    = !NULL
     eField1s  = !NULL
     pFluxB1s  = !NULL 
     dsp       = !NULL 
     dsp1s     = !NULL

     PRINT,"Adding struct for interval " + itvlString + " in orbit " + orbString + ' ...'
     structList.Add,tmpStruct
  ENDFOR

  IF ~KEYWORD_SET(no_hash_update) THEN BEGIN
     IF FILE_TEST(outDir+hashFile) THEN BEGIN
        PRINT,"Restoring hash file ..."
        RESTORE,outDir+hashFile

        CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
           -1: BEGIN
              PRINT,'Adding stuff from orbit ' + orbString + ' ...'
              swHash  = swHash + ORDEREDHASH(orbit,structList)
           END
           ELSE: BEGIN
              PRINT,'Replacing hash entry for orbit ' + orbString + ' ...'
              swHash[orbit] = structList
           END
        ENDCASE

        PRINT,'Saving Strangeway statistics hash ...'
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDIF ELSE BEGIN
        PRINT,'Creating Strangeway statistics hash for orbit ' + orbString + ' ...'
        swHash = ORDEREDHASH(orbit,structList)
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDELSE
  ENDIF

  RETURN

END

