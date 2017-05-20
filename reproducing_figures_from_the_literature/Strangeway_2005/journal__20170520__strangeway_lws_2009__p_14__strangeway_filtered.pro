;;2016/09/28
;;Trying to reproduce Strangeway's AC Poynting flux
  ;;magStuff
PRO JOURNAL__20170520__STRANGEWAY_LWS_2009__P_14__STRANGEWAY_FILTERED, $
   TPLT_VARS=tPlt_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps

; create a summary plot of:
; SFA (AKR)
; DSP (VLF)
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; E fit along V (Southern hemisphere corrected)
; dB_fac_v (dB_fac and dB_SM also stored)

; Returns:
; tPlt_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data

; procedure for making summary plots
; batch_summary,tPlt_vars=tPlt_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all
; loadct2,40  ; load color table
; if (n_elements(tPlt_vars) gt 0) then tplot,tPlt_vars,var=['ALT','ILAT','MLT']
; if (n_elements(tlimit_north) gt 0) then tlimit,tlimit_north  ; northern hemisphere
; if (n_elements(tlimit_south) gt 0) then tlimit,tlimit_south  ; southern hemisphere

; if running interactively
; batch_summary,tPlt_vars=tPlt_vars,/screen_plot,/no_blank_panels

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

  IF N_ELEMENTS(smooth_fluxes) EQ 0 THEN BEGIN
     smooth_fluxes  = 0
  ENDIF

  IF N_ELEMENTS(smooth_fields) EQ 0 THEN BEGIN
     smooth_fields  = 1
  ENDIF

  chastFreqBounds   = [0.5,10]
  highFreqBounds    = [0.125,0.5]
  lowFreqBounds     = [0,0.125]

  freqBounds        = [[lowFreqBounds],[highFreqBounds],[chastFreqBounds]]
  freqSuffs         = '_' + ['LOW','HIGH','INERTIAL'] 

  @tplot_com

  @startup

  @strway_stuff

  ON_ERROR,0

  ;;From UCLA_MAG_DESPIN:
  ;;"   Field-aligned coordinates defined as: 
  ;;"   z-along B, y-east (BxR), x-nominally out"
  ;;    (ind 2)    (ind 1)       (ind 0)

  magInd       = 1
  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255
  mu_0         = DOUBLE(4.0D*!PI*1e-7)
  outPlotName  = 'Strangeway_LWS_2009--page_14__sway_filtered--newfilt'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     outPlotName += '-eFieldFits'
  ENDIF

  t1           = STR_TO_TIME('1998-09-24/23:59:30')
  t2           = STR_TO_TIME('1998-09-25/00:38:00')

  t1ZoomStr    = '1998-09-25/00:00:00'
  t2ZoomStr    = '1998-09-25/00:37:00'

  t1Zoom       = STR_TO_TIME(t1ZoomStr)
  t2Zoom       = STR_TO_TIME(t2ZoomStr)

  nn           = N_ELEMENTS(data_quants)

  if (nn gt 1) then for n = nn-1L,1L,-1L do store_data,data_quants(n).name,/delete

; Step 1 - DC Mag data

  ucla_mag_despin,tw_mat=tw_mat,orbit=orbit,spin_axis=spin_axis,delta_phi=delta_phi

  if (n_elements(orbit) gt 0) then begin

;  if orbit > 9936 return (temporary fix)

     if (orbit gt 9936) then begin

        print,""
        print,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        print,""
        return

     endif

; got mag data, set time limits, delete unused tplot variables, set tPlt_vars

     store_data,'BDATA',/delete
     store_data,'BFIT',/delete 
     store_data,'Bx_sp',/delete
     store_data,'By_sp',/delete
     store_data,'Bz_sp',/delete
     store_data,'Bx_sc',/delete
     store_data,'By_sc',/delete
     store_data,'Bz_sc',/delete
     store_data,'Bx_sp_sm',/delete
     store_data,'By_sp_sm',/delete
     store_data,'Bz_sp_sm',/delete
     store_data,'B_gei',/delete
     store_data,'B_sm',/delete
     store_data,'dB_sc',/delete
     store_data,'dB_gei',/delete
     store_data,'spin_freq',/delete
     store_data,'spin_phase',/delete
     store_data,'TORQ_X',/delete
     store_data,'TORQ_Y',/delete
     store_data,'TORQ_Z',/delete
     store_data,'BX_DEL',/delete
     store_data,'BY_DEL',/delete
     store_data,'BZ_DEL',/delete
     store_data,'BFIX',/delete
     store_data,'TW_ZX',/delete
     store_data,'TW_ZY',/delete
     store_data,'TW_YY',/delete
     store_data,'TW_YX',/delete
     store_data,'O_X',/delete
     store_data,'O_Y',/delete
     store_data,'B_model_old',/delete
     store_data,'Delta_B_model',/delete
     store_data,'despun_to_gei',/delete
     store_data,'gei_to_sm',/delete
     store_data,'gei_to_fac',/delete
     store_data,'gei_to_fac_v',/delete

     get_data,'dB_fac_v',data=data
     ;; t1 = data.x[0]
     ;; t2 = data.x[n_elements(data.x)-1L]
     tlimit_all = [t1,t2]
     tPlt_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     ;;Interp time series
     tS_1s  = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

     tPlt_vars = 'dB_fac_v'

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF

; step 2 - E field


; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
  sdt_idx = get_sdt_run_idx()

  prog = getenv('FASTBIN') + '/showDQIs'
  if ((sdt_idx GE 0) AND (sdt_idx LT 100)) then begin
     if (sdt_idx GE 10) then begin
        sidstr = string(sdt_idx, format='(I2)')
     endif else begin
        sidstr = string(sdt_idx, format='(I1)')
     endelse
     spawn, [prog, sidstr], result, /noshell
  endif else begin
     spawn, prog, result, /noshell
  endelse


  b = where (strpos(result,'V1-V4_S') ge 0,nb4)
  if (nb4 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb4 = 0
  b = where (strpos(result,'V1-V2_S') ge 0,nb2)
  if (nb2 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb2 = 0
  if (nb4 gt 0) then v12=get_fa_fields('V1-V4_S',/DEFAULT) $
  else if (nb2 gt 0) then v12=get_fa_fields('V1-V2_S',/DEFAULT)

  b = where (strpos(result,'V5-V8_S') ge 0,nb5)
  if (nb5 gt 0) then v58=get_fa_fields('V5-V8_S',/DEFAULT)

  got_efield = (nb4 gt 0 or nb2 gt 0) and nb5 gt 0

  if (got_efield) then begin

; despin e field data

     FA_FIELDS_DESPIN,v58,v12,/SHADOW_NOTCH,/MAG_NOTCH,/SINTERP,/BINTERP

  endif

  ;; Step 3 - Poynting flux
  GET_DATA,'dB_fac_v',data=magData
  GET_DATA,eAV_variable,data=eAlongV

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     maxeFitPeriod = MAX(eAlongV.x[1:-1]-eAlongV.x[0:-2]) < 2.8
  ENDIF

  mintime = MIN(ABS(t1-magData.x),ind1)
  mintime = MIN(ABS(t2-magData.x),ind2)

  magB = {x:magData.x[ind1:ind2], $
          y:magData.y[ind1:ind2,2]} 
  magp = {x:magData.x[ind1:ind2], $
          y:magData.y[ind1:ind2,1]}
  magv = {x:magData.x[ind1:ind2], $
          y:magData.y[ind1:ind2,0]} 
  magx = {x:magData.x, $
          y:REFORM(magData.y[*,0])}

  ;; magy = {x:magData.x, $
  ;;         y:REFORM(magData.y[*,2])}

  ;; magz = {x:magData.x, $
  ;;         y:REFORM(magData.y[*,1])}


  ;;magStuff
  dBv = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
        magv, $
        INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
        ONESEC_TS=tS_1s)
  dBB = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
        magB, $
        INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
        ONESEC_TS=tS_1s)
  dBp = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
        magp, $
        INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
        ONESEC_TS=tS_1s)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;E-field stuff

  ;; FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
  ;;                   {TIME:eAlongV.x[WHERE(FINITE(eAlongV.y))], $
  ;;                    COMP1:eAlongV.y[WHERE(FINITE(eAlongV.y))]}, $
  ;;                   RESULT=eAlongVInterp, $
  ;;                   ;; /INTERP, $
  ;;                   /SPLINE, $
  ;;                   SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
  ;;                   DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
  ;;                   maxeFitPeriod : !NULL, $
  ;;                   /TALK

  ;; eAV    = {  $                 ;TIME         : magz.x[tmpI]          , $
  ;;          ;; COMP1        : magx.y[tmpI]          , $
  ;;          ;; COMP2        : magy.y[tmpI]          , $
  ;;          ;; COMP3        : magz.y[tmpI]          , $
  ;;          ;; TIME         : magz.x[tmpI]          , $
  ;;          ;; COMP1        : eAlongVInterp[tmpI]   , $
  ;;          TIME         : magz.x          , $
  ;;          COMP1        : eAlongVInterp   , $
  ;;          NCOMP        : 1               , $
  ;;          DATA_NAME    : 'eAlongVInterp' , $
  ;;          VALID        : 1               , $
  ;;          PROJECT_NAME : 'FAST'          , $
  ;;          UNITS_NAME   : 'mV/m'          , $
  ;;          CALIBRATED   : 1}

  ;; eAVHigh = eAV
  ;; eAVLow  = TEMPORARY(eAV)

  ;; eAVHigh.data_name += '_HIGH'
  ;; eAVLow.data_name  += '_LOW'

  ;; IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN

  ;; ENDIF ELSE BEGIN
  ;;    FA_FIELDS_FILTER,eAVHigh,highFreqBounds, $
  ;;                     ;; DB=FFTdb, $
  ;;                     POLES=highFreqPoles

  ;;    FA_FIELDS_FILTER,eAVLow,lowFreqBounds, $
  ;;                     ;; DB=FFTdb, $
  ;;                     POLES=lowFreqPoles

  ;; ENDELSE

  ;; IF KEYWORD_SET(include_E_near_B) THEN BEGIN

  ;;    FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
  ;;                      {TIME:eNearB.x[WHERE(FINITE(eNearB.y))], $
  ;;                       COMP1:eNearB.y[WHERE(FINITE(eNearB.y))]}, $
  ;;                      RESULT=eNearBInterp, $
  ;;                      ;; /INTERP, $
  ;;                      /SPLINE, $
  ;;                      SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
  ;;                      DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
  ;;                      maxeFitPeriod : !NULL, $
  ;;                      /TALK

  ;;    eNB    = {  TIME         : magz.x[tmpI]          , $
  ;;                ;; COMP1        : magx.y[tmpI]          , $
  ;;                ;; COMP2        : magy.y[tmpI]          , $
  ;;                ;; COMP3        : magz.y[tmpI]          , $
  ;;                ;; COMP1        : eNearBInterp[tmpI]   , $
  ;;                COMP1        : eNearBInterp   , $
  ;;                NCOMP        : 1                     , $
  ;;                DATA_NAME    : 'eNearBStuff'        , $
  ;;                VALID        : 1                     , $
  ;;                PROJECT_NAME : 'FAST'                , $
  ;;                UNITS_NAME   : 'mV/m'                , $
  ;;                CALIBRATED   : 1}

  ;;    eNBHigh = eNB
  ;;    eNBLow  = TEMPORARY(eNB)

  ;;    eNBHigh.data_name += '_HIGH'
  ;;    eNBLow.data_name  += '_LOW'

  ;;    FA_FIELDS_FILTER,eNBHigh,highFreqBounds, $
  ;;                     ;; DB=FFTdb, $
  ;;                     POLES=highFreqPoles

  ;;    FA_FIELDS_FILTER,eNBLow,lowFreqBounds, $
  ;;                     ;; DB=FFTdb, $
  ;;                     POLES=lowFreqPoles

  ;; ENDIF

  ;; eAVHigh = {time:eAlongV.x,comp1:eAlongV.y}
  ;; eAVLow  = {time:eAlongV.x,comp1:eAlongV.y}

  ;; eAlongV = !NULL

  ;;E-field trim
  ;; mintime = MIN(ABS(magv.x[0]-eAlongV.x),ind1)
  ;; mintime = MIN(ABS(magv.x[-1]-eAlongV.x),ind2)
  mintime = MIN(ABS(t1-eAlongV.x),ind1)
  mintime = MIN(ABS(t2-eAlongV.x),ind2)

  ;; IF ind1 EQ ind2 THEN BEGIN
  ;;    PRINT,'No usable eAlongV data here. Skipping interval ...'
  ;;    CONTINUE
  ;; ENDIF

  eAlongV  = {x:eAlongV.x[ind1:ind2], $
              y:eAlongV.y[ind1:ind2]}
  nEAlongV = N_ELEMENTS(eAlongV.x)

  eAV = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
        eAlongV, $
        INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
        ONESEC_TS=tS_1s)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Poynting fluxes!

  IF KEYWORD_SET(full_pFlux) THEN BEGIN


     pFBHigh = dBp.AC*eAV.AC/mu_0                        ;Poynting flux along B
     pFPHigh = (eNB.AC*dBv.AC - $
                1.*dBB.AC*eAV.AC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
     pFVHigh = (-1.)*eNB.AC*dBp.AC/mu_0

     pFBLow = dBp.DC*eAV.DC/mu_0                        ;Poynting flux along B
     pFPLow = (eNB.DC*dBv.DC - $
                1.*dBB.DC*eAV.DC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
     pFVHigh = (-1.)*eNB.AC*dBp.AC/mu_0

     pFVHigh *= 1e-9
     pFVLow  *= 1e-9

  ENDIF ELSE BEGIN

     pFBHigh =       dBp.AC *eAV.AC/mu_0 ;Poynting flux along B
     pFPHigh = (-1.)*dBB.AC*eAV.AC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

     pFBLow =       dBp.DC *eAV.DC/mu_0 ;Poynting flux along B
     pFPLow = (-1.)*dBB.DC*eAV.DC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

  ENDELSE

  pFBHigh *= 1e-9                ;Junk that nano prefix in nT
  pFPHigh *= 1e-9

  pFBLow *= 1e-9                ;Junk that nano prefix in nT
  pFPLow *= 1e-9


  ;; pFBHigh[WHERE(~FINITE(pFBHigh))] = 0.0
  ;; pFPHigh[WHERE(~FINITE(pFPHigh))] = 0.0

  ;; tmp    = {x:dBp.time,y:pFBHigh}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;AC Poynting flux

  tField = dBp.x
  doDat  = pFBHigh
  ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
  ;;    tField = tS_1s
  ;; ENDIF

  ;;Make all downgoing, pre-log
  good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.0,nGood, $
                   COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
  tField   = tField[good_i]
  doDat    = ALOG10(ABS(doDat[good_i]))


  tmp    = {x:tField, $
            y:doDat}

  STORE_DATA,'pFluxHigh',DATA=tmp
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

  IF (n_elements(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxHigh'] $
  ELSE tPlt_vars=['pFluxHigh',tPlt_vars]

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;DC Poynting flux

  tField = dBp.x
  doDat  = pFBLow
  ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
  ;;    tField = tS_1s
  ;; ENDIF

  ;;Make all downgoing, pre-log
  good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.00,nGood, $
                   COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

  tField   = tField[good_i]
  ;; doDat  = ABS(doDat[good_i])
  doDat  = ALOG10(ABS(doDat[good_i]))

  tmp    = {x:tField, $
            y:doDat}

  STORE_DATA,'pFluxLow',DATA=tmp
  ;; YLIM,'pFluxLow',1e-4,1e2,1
  ;; OPTIONS,'pFluxLow','yticks',7
  ;; OPTIONS,'pFluxLow','ytickv',[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2]
  ;; OPTIONS,'pFluxLow','ytickname',['10!U-4!N','10!U-3!N','10!U-2!N', $
  ;;                                 '10!U-1!N','10!U0!N','10!U1!N','10!U2!N']

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

  IF (n_elements(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxLow'] $
  ELSE tPlt_vars=['pFluxLow',tPlt_vars]

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
  ENDIF


; Step 4 - Electron junk

; Electron energy flux

  t           = 0.0D
  tmp         = GET_FA_EES_C(t,/EN)
  IF tmp.valid EQ 0 THEN BEGIN
     PRINT,'Junk'
     RETURN
  ENDIF
  last_index  = LONG(tmp.index)
  t1          = 0.0D
  t2          = 0.0D
  temp        = GET_FA_EES(t1,INDEX=0.0D)
  temp        = GET_FA_EES(t2,INDEX=DOUBLE(last_index))

  GET_2DT,'je_2d_fs','fa_ees_c',name='JEe',t1=t1,t2=t2,energy=energy_electrons
  GET_DATA,'JEe',DATA=tmp

  tFlux    = tmp.x
  doDat    = tmp.y
  IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmp.y = SMOOTH(tmp.y,5)
     doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
     tFlux = tS_1s
  ENDIF
  
  ;;Make all downgoing, pre-log
  good_i   = WHERE(FINITE(doDat) AND doDat GT 0.00,nGood, $
                   COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
  tFlux    = tFlux[good_i]
  doDat    = ALOG10(doDat[good_i])

  STORE_DATA,'JEe',DATA={x:tFlux,y:doDat}
  ylim,'JEe',-5,1,0                                                     ; set y limits
  options,'JEe','ytitle','Downgoing Elec.!CEnergy Flux!CmW/(m!U2!N)'    ; set y title
  options,'JEe','panel_size',3                                          ; set panel size
  OPTIONS,'JEe','yticks',7                                              ; set y-axis labels
  OPTIONS,'JEe','ytickv',[-5,-4,-3,-2,-1,0,1]                           ; set y-axis labels
  OPTIONS,'JEe','ytickname',['10!U-5!N','10!U-4!N','10!U-3!N', $
                             '10!U-2!N','10!U-1!N','10!U0!N','10!U1!N'] ; set y-axis labels

; Step 5 - Ion flux

  IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]
  GET_2DT,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=energy_ions

  GET_DATA,'Ji',DATA=tmp

  tFlux    = tmp.x
  doDat    = (-1.)*tmp.y
  IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmp.y = SMOOTH(tmp.y,5)
     doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
     tFlux = tS_1s
  ENDIF

  ;;Make all upgoing, pre-log
  good_i   = WHERE(FINITE(doDat) AND doDat GT 0.00,nGood, $
                   COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
  tFlux    = tFlux[good_i]
  doDat    = ALOG10(doDat[good_i])

  STORE_DATA,'Ji',DATA={x:tFlux,y:doDat}
  YLIM,'Ji',4,10,0                                               ; set y limits
  options,'Ji','ytitle','Upward Ion!CNumber Flux!C(#/cm!U2!N-s)' ; set y title
  options,'Ji','panel_size',3                     ; set panel size
  OPTIONS,'Ji','yticks',7                         ; set y-axis labels
  OPTIONS,'Ji','ytickv',[4,5,6,7,8,9,10]          ; set y-axis labels
  OPTIONS,'Ji','ytickname',['10!U4!N','10!U5!N','10!U6!N', $
                            '10!U7!N','10!U8!N','10!U9!N','10!U10!N'] ; set y-axis labels
  OPTIONS,'Ji','ynozero',1

; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south also change plot title

  get_data,'LAT',data=data

  if (n_elements(data.y) le 0) then return

  bb = where (data.y gt 10,nn)
  if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

  bb = where (data.y lt -10,nn)
  if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

  hemisph = getenv('FAST_ORBIT_HEMISPHERE')

  get_data,'ORBIT',data=data
  nn = n_elements(data.y)/2
  orbit = data.y(nn)
  orbit_lab = strcompress(string(orbit,format="(i5.4)"),/remove_all)
  tplot_options,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

  tPlt_vars=['dB_fac_v','pFluxHigh','pFluxLow','JEe','Ji']

  IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Strangeway_et_al_2005'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           ;; CGPS_OPEN, './plots/McFadden_et_al_1998--Fig_1.ps',FONT=0,XSIZE=4,YSIZE=7
           POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8
           ;; DEVICE,SET_FONT='Garamond*15'
           ;; !P.FONT = -1
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE
     
     CASE 1 OF
        ;; (n_elements(tlimit_north) gt 0): BEGIN
        ;;    tlimit,tlimit_north
        ;; END
        ;; (n_elements(tlimit_south) gt 0): BEGIN
        ;;    tlimit,tlimit_south
        ;; END
        KEYWORD_SET(plot_north): BEGIN
           tLims = tlimit_north
        END
        KEYWORD_SET(plot_south): BEGIN
           tLims = tlimit_south
        END
        ELSE: BEGIN
           tLims = [t1ZoomStr,t2ZoomStr]
        END        
     ENDCASE

     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tLims


     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        PCLOSE
     ENDIF ELSE BEGIN

     ENDELSE

  ENDIF


  RETURN


END


