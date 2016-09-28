;;09/28/16
;;Trying to reproduce Strangeway's AC Poynting flux
PRO JOURNAL__20160928__STRANGEWAY_LWS_2009__P_14, $
   TPLOT_VARS=tplot_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
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
; tplot_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data

; procedure for making summary plots
; batch_summary,tplot_vars=tplot_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all
; loadct2,40  ; load color table
; if (n_elements(tplot_vars) gt 0) then tplot,tplot_vars,var=['ALT','ILAT','MLT']
; if (n_elements(tlimit_north) gt 0) then tlimit,tlimit_north  ; northern hemisphere
; if (n_elements(tlimit_south) gt 0) then tlimit,tlimit_south  ; southern hemisphere

; if running interactively
; batch_summary,tplot_vars=tplot_vars,/screen_plot,/no_blank_panels

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


  ;;smoothing options
  magSmSeconds = 2 ;Up to 0.5 Hz
  eFSmSeconds  = 2 ;Up to 0.5 Hz
  pFluxLowSmSeconds  = 2
  pFluxHighSmSeconds = 2


  zero_unsmoothables = 0


  IF N_ELEMENTS(smooth_fluxes) EQ 0 THEN BEGIN
     smooth_fluxes  = 0
  ENDIF

  IF N_ELEMENTS(smooth_fields) EQ 0 THEN BEGIN
     smooth_fields  = 1
  ENDIF

  highFreqBounds = [0.125,0.5]
  lowFreqBounds  = [0,0.125]

  ;; highFreqPoles  = [8,8]
  ;; lowFreqPoles   = [8,8]

  @tplot_com

  @startup

  @strway_stuff

  ON_ERROR,0

  ;;From UCLA_MAG_DESPIN:
  ;;"   Field-aligned coordinates defined as: 
  ;;"   z-along B, y-east (BxR), x-nominally out"
  ;;    (ind 2)    (ind 1)       (ind 0)

  magInd = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

  mu_0         = DOUBLE(4.0D*!PI*1e-7)

  outPlotName  = 'Strangeway_LWS_2009--page_14'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     outPlotName += '--eFieldFits'
  ENDIF

  t1           = STR_TO_TIME('1998-09-24/23:59:30')
  t2           = STR_TO_TIME('1998-09-25/00:38:00')

  t1ZoomStr    = '1998-09-25/00:00:00'
  t2ZoomStr    = '1998-09-25/00:37:00'

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

; got mag data, set time limits, delete unused tplot variables, set tplot_vars

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
     tplot_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

     tplot_vars = 'dB_fac_v'

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tplot_vars,var=['ALT','ILAT','MLT']
     endif

     ;;Smooth to 4-s resolution
     ;; PRINT,'SMOOTHmag'
     ;; var_name = tplot_vars
     ;; GET_DATA,var_name,DATA=data
     ;; FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
     ;; IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN STOP

     ;; sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
     ;; nBufs  = N_ELEMENTS(strt_i)
     ;; FOR k=0, nBufs-1 DO BEGIN

     ;;    tmpI          = [strt_i[k]:stop_i[k]]
     ;;    smooth_int    = CEIL(sRates[k]/0.25)

     ;;    FOR l=0,2 DO BEGIN
     ;;       tmp           = {x:data.x[tmpI], $
     ;;                        y:data.y[tmpI,l]}

     ;;       smoothed      = SMOOTH(tmp.y,smooth_int)
           
     ;;       data.y[tmpI,l]  = smoothed

     ;;    ENDFOR

     ;;    PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
     ;; ENDFOR

     ;; ;; OPTIONS,var_name,'labels',['o','e','b']

     ;; data = {x:data.x, $
     ;;         y:REFORM(data.y[*,magInd])}

     ;; ;;Interp to 1-s resolution
     ;; FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
     ;;                   {TIME:data.x,COMP1:data.y}, $
     ;;                   RESULT=datInterp, $
     ;;                   /INTERP, $
     ;;                   ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
     ;;                   DELT_T=(1.01)/MIN(sRates), $
     ;;                   /TALK

     ;; ;; data = {x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
     ;; ;;         y:[TRANSPOSE(TEMPORARY(datInterp)), $
     ;; ;;            TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
     ;; data = {x:[[tS_1s],[tS_1s]], $
     ;;         y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
     ;;            [TEMPORARY(datInterp)]]}

     ;; ;; data = {x:tS_1s, $
     ;; ;;         y:datInterp}

     ;; ;; OPTIONS,'dB_fac_interp','labels',(['o','e','b'])[magInd]

     ;; STORE_DATA,'dB_fac_interp',DATA=data

     ;; dLimit = {spec:0, ystyle:1, yrange:[-600., 800.], $
     ;;           ytitle:'dB Perp.!C!C[DC] (nT)', $
     ;;           panel_size:3}

     ;; OPTIONS,'dB_fac_interp','colors',[normColorI,normColorI]
     ;; OPTIONS,'dB_fac_interp','tplot_routine','mplot'
     ;; STORE_DATA,'dB_fac_interp',DLIMITS=dLimit

  endif

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
  if (nb4 gt 0) then v12=get_fa_fields('V1-V4_S',/all) $
  else if (nb2 gt 0) then v12=get_fa_fields('V1-V2_S',/all)

  b = where (strpos(result,'V5-V8_S') ge 0,nb5)
  if (nb5 gt 0) then v58=get_fa_fields('V5-V8_S',/all)

  got_efield = (nb4 gt 0 or nb2 gt 0) and nb5 gt 0

  if (got_efield) then begin

; despin e field data

     FA_FIELDS_DESPIN,v58,v12,/SHADOW_NOTCH,/SINTERP

     ;; OPTIONS,'EFIT_ALONG_V','yrange',0
     ;; OPTIONS,'EFIT_ALONG_V','ytitle','E along V!C!C[DC] (mV/m)'
     ;; OPTIONS,'EFIT_ALONG_V','colors',[normColorI,normColorI]
     ;; OPTIONS,'EFIT_ALONG_V','panel_size',2

; reset time limits if needed

     ;; get_data,'EFIT_ALONG_V',data=data

     ;; ;;Smooth to 4-s resolution
     ;; PRINT,'SMOOTHEfield'
     ;; FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
     ;; IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN

     ;;    sRates = 1./(data.x[1:-1]-data.x[0:-2])

     ;;    ;;Old-fashioned way

     ;;    smoothed         = data.y
     ;;    FOR k=0,N_ELEMENTS(data.y)-1 DO BEGIN

     ;;       tmpI          = WHERE(ABS(data.x-data.x[k]) LE fields_smoothWindow_halfLength)
     ;;       smoothed[k]   = MEAN(data.y[tmpI])

     ;;    ENDFOR
     ;; ENDIF ELSE BEGIN

     ;;    sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
     ;;    nBufs  = N_ELEMENTS(strt_i)
     ;;    FOR k=0, nBufs-1 DO BEGIN

     ;;       tmpI          = [strt_i[k]:stop_i[k]]
     ;;       tmp           = {x:data.x[tmpI], $
     ;;                        y:data.y[tmpI]}

     ;;       smooth_int    = CEIL(sRates[k]/0.25)
     ;;       smoothed      = SMOOTH(tmp.y,smooth_int)
           
     ;;       data.y[tmpI]  = smoothed

     ;;       PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
     ;;    ENDFOR
     ;; ENDELSE

     ;; ;;Interp to 1-s resolution
     ;; FA_FIELDS_COMBINE,{TIME:tS_1s,COMP1:tS_1s}, $
     ;;                   {TIME:data.x,COMP1:data.y}, $
     ;;                   RESULT=datInterp, $
     ;;                   /INTERP, $
     ;;                   ;; /SVY, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
     ;;                   DELT_T=(1.01)/MIN(sRates), $
     ;;                   /TALK

     ;; ;; data = {x:tS_1s, $
     ;; ;;         y:datInterp}

     ;; ;; data = {x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
     ;; ;;         y:[TRANSPOSE(TEMPORARY(datInterp)), $
     ;; ;;            TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
     ;; data = {x:[[tS_1s],[tS_1s]], $
     ;;         y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
     ;;            [TEMPORARY(datInterp)]]}

     ;; STORE_DATA,'EFIT_ALONG_V',DATA=data

     ;; t1 = data.x[0]
     ;; t2 = data.x[n_elements(data.x)-1L]

     ;; if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
     ;;    if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
     ;;    if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
     ;;    get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
     ;;    get_new_igrf,/no_store_old
     ;; endif

; check for southern hemisphere and fix 
; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

  ;;    get_data,'B_model',data=bm
  ;;    get_data,'fa_vel',data=vel
  ;;    get_data,'fa_pos',data=pos
  ;;    n=n_elements(reform(pos.y[*,0]))
  ;;    rxv = dblarr(n,3)
  ;;    rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
  ;;    rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
  ;;    rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
  ;;    vxb = dblarr(n,3)
  ;;    vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
  ;;    vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
  ;;    vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
  ;;    tst = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

  ;;    get_data,'EFIT_ALONG_V',data=data,dlimit=dlimit
  ;;    y2=spl_init(pos.x-tlimit_all[0],tst,/double)
  ;;    tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
  ;;    data.y = data.y*tst_/abs(tst_)
  ;;    store_data,'EFIT_ALONG_VSC',data=data,dlimit=dlimit
  ;;    options,'EFIT_ALONG_VSC','yrange',0
  ;;    options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C[DC] (mV/m)'
  ;;    OPTIONS,'EFIT_ALONG_VSC','colors',[normColorI,normColorI]
  ;;    options,'EFIT_ALONG_VSC','panel_size',2

  ;;    ;; store_data,'E_NEAR_B',/delete
  ;;    ;; store_data,'E_ALONG_V',/delete
  ;;    store_data,'EFIT_NEAR_B',/delete
  ;;    store_data,'EFIT_ALONG_V',/delete

  ;;    if (n_elements(tplot_vars) eq 0) then tplot_vars=['EFIT_ALONG_VSC'] else tplot_vars=['EFIT_ALONG_VSC',tplot_vars]

  ;;    if (keyword_set(screen_plot)) then begin
  ;;       loadct2,40
  ;;       tplot,tplot_vars,var=['ALT','ILAT','MLT']
  ;;    endif

  ;; endif else if (n_elements(tplot_vars) ne 0) then begin

  ;;    tplot_vars = 'dB_fac'
  ;;    if (keyword_set(use_fac_v)) then tplot_vars = 'dB_fac_v'
  ;;    if ~KEYWORD_SET(no_blank_panels) then tplot_vars = 'dB_fac'

  endif


  ;; Step 3 - Poynting flux
  GET_DATA,'dB_fac_v',data=magData
  GET_DATA,eAV_variable,data=eAlongV

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     maxeFitPeriod = MAX(eAlongV.x[1:-1]-eAlongV.x[0:-2]) < 2.8
  ENDIF


  magx = {x:magData.x, $
          y:REFORM(magData.y[*,0])}

  magy = {x:magData.x, $
          y:REFORM(magData.y[*,2])}

  magz = {x:magData.x, $
          y:REFORM(magData.y[*,magInd])}

  ;;Smooth 'em all (think Metallica)
  PRINT,'SMOOTHMAGX'
  SMOOTH_TSERIES,magx, $
                 magSmSeconds, $
                 ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                 NRECURSE=nRecurse
  PRINT,'SMOOTHMAGY'
  SMOOTH_TSERIES,magy, $
                 magSmSeconds, $
                 ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                 NRECURSE=nRecurse
  PRINT,'SMOOTHMAGZ'
  SMOOTH_TSERIES,magz, $
                 magSmSeconds, $
                 ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                 NRECURSE=nRecurse

  
  IF ~KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     PRINT,'SMOOTHEALONGV'
     SMOOTH_TSERIES,eAlongV, $
                    eFSmSeconds, $
                    ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                    NRECURSE=nRecurse
  ENDIF

  IF KEYWORD_SET(include_E_near_B) THEN BEGIN

     GET_DATA,eNB_variable,DATA=eNearB

     IF ~KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
        PRINT,'SMOOTHENEARB'
        SMOOTH_TSERIES,eNearB, $
                       eFSmSeconds, $
                       ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                       NRECURSE=nRecurse
     ENDIF

  ENDIF

  ;;Now check sorted/dupes
  ;; CHECK_DUPES,magz.x,HAS_DUPES=magHasDupes, $
  ;;             IS_SORTED=magIsSort,OUT_UNIQ_I=magUniq_i,/QUIET
  ;; IF magHasDupes OR ~magIsSort THEN BEGIN
  ;;    PRINT,'Mag has dupes/is not sorted! Sorting ...'
  ;;    magx = {x:magx.x[magUniq_i],y:magx.y[magUniq_i]}
  ;;    magy = {x:magy.x[magUniq_i],y:magy.y[magUniq_i]}
  ;;    magz = {x:magz.x[magUniq_i],y:magz.y[magUniq_i]}
  ;; ENDIF

  ;; CHECK_DUPES,eAlongV.x,HAS_DUPES=eAVHasDupes, $
  ;;             IS_SORTED=eAVIsSort,OUT_UNIQ_I=eAVUniq_i,/QUIET
  ;; IF eAVHasDupes OR ~eAVIsSort THEN BEGIN
  ;;    PRINT,'EAV has dupes/is not sorted! Sorting ...'
  ;;    eAlongV = {x:eAlongV.x[eAVUniq_i],y:eAlongV.y[eAVUniq_i]}

  ;;    IF KEYWORD_SET(include_E_near_B) THEN BEGIN
  ;;       eNearB = {x:eNearB.x[eAVUniq_i],y:eNearB.y[eAVUniq_i]}
  ;;    ENDIF
  ;; ENDIF

  ;;magStuff
  dBp    = {  $                 ;TIME         : magz.x[tmpI]          , $
           ;; COMP1        : magx.y[tmpI]          , $
           ;; COMP2        : magy.y[tmpI]          , $
           ;; COMP3        : magz.y[tmpI]          , $
           ;; TIME         : magz.x[tmpI]          , $
           ;; COMP1        : eAlongVInterp[tmpI]   , $
           TIME         : magz.x          , $
           COMP1        : magz.y   , $
           NCOMP        : 1               , $
           DATA_NAME    : 'dB_perp' , $
           VALID        : 1               , $
           PROJECT_NAME : 'FAST'          , $
           UNITS_NAME   : 'mV/m'          , $
           CALIBRATED   : 1}

  dBpHigh = dBp
  dBpLow  = TEMPORARY(dBp)

  dBpHigh.data_name += '_HIGH'
  dBpLow.data_name  += '_LOW'

  FA_FIELDS_FILTER,dBpHigh,highFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=highFreqPoles

  FA_FIELDS_FILTER,dBpLow,lowFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=lowFreqPoles


  dBv  = { $ ;; TIME         : magx.x[tmpI]          , $
                 ;; COMP1        : magx.y[tmpI]          , $
                 TIME         : magx.x                , $
                 COMP1        : magx.y                , $
                 NCOMP        : 1                     , $
                 DATA_NAME    : 'dB_along_v'          , $
                 VALID        : 1                     , $
                 PROJECT_NAME : 'FAST'                , $
                 UNITS_NAME   : 'nT'                  , $
                 CALIBRATED   : 1}

  dBvHigh = dBv
  dBvLow  = TEMPORARY(dBv)

  dBvHigh.data_name += '_HIGH'
  dBvLow.data_name  += '_LOW'

  FA_FIELDS_FILTER,dBvHigh,highFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=highFreqPoles

  FA_FIELDS_FILTER,dBvLow,lowFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=lowFreqPoles

  dBB  = {  $;; TIME         : magy.x[tmpI]          , $
             ;; COMP1        : magy.y[tmpI]          , $
            TIME         : magy.x                , $
            COMP1        : magy.y                , $
            NCOMP        : 1                     , $
            DATA_NAME    : 'dB_along_B'          , $
            VALID        : 1                     , $
            PROJECT_NAME : 'FAST'                , $
            UNITS_NAME   : 'nT'                  , $
            CALIBRATED   : 1}

  dBBHigh = dBB
  dBBLow  = TEMPORARY(dBB)

  dBBHigh.data_name += '_HIGH'
  dBBLow.data_name  += '_LOW'

  FA_FIELDS_FILTER,dBBHigh,highFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=highFreqPoles

  FA_FIELDS_FILTER,dBBLow,lowFreqBounds, $
                   ;; DB=FFTdb, $
                   POLES=lowFreqPoles


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

  eAVHigh = {time:eAlongV.x,comp1:eAlongV.y}
  eAVLow  = {time:eAlongV.x,comp1:eAlongV.y}

  eAlongV = !NULL
  ;; eAVHigh.data_name += '_HIGH'
  ;; eAVLow.data_name  += '_LOW'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN

  ENDIF ELSE BEGIN
     FA_FIELDS_FILTER,eAVHigh,highFreqBounds, $
                      ;; DB=FFTdb, $
                      POLES=highFreqPoles

     FA_FIELDS_FILTER,eAVLow,lowFreqBounds, $
                      ;; DB=FFTdb, $
                      POLES=lowFreqPoles

  ENDELSE

  ;; eAVHigh = {x:eAVHigh.time,y:eAVHigh.comp1}
  ;; eAVLow  = {x:eAVLow.time,y:eAVLow.comp1}

  FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                    {TIME:eAVHigh.time[WHERE(FINITE(eAVHigh.comp1))], $
                     COMP1:eAVHigh.comp1[WHERE(FINITE(eAVHigh.comp1))]}, $
                    RESULT=eAVHighInterp, $
                    ;; /INTERP, $
                    /SPLINE, $
                    SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                    DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                    maxeFitPeriod : !NULL, $
                    /TALK

  FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                    {TIME:eAVLow.time[WHERE(FINITE(eAVLow.comp1))], $
                     COMP1:eAVLow.comp1[WHERE(FINITE(eAVLow.comp1))]}, $
                    RESULT=eAVLowInterp, $
                    ;; /INTERP, $
                    /SPLINE, $
                    SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                    DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                    maxeFitPeriod : !NULL, $
                    /TALK

  eAVHigh = {time:magz.x,comp1:eAVHighInterp}
  eAVLow  = {time:magz.x,comp1:eAVLowInterp}

  IF KEYWORD_SET(include_E_near_B) THEN BEGIN

     eNBHigh = {time:eNearB.x,comp1:eNearB.y}
     eNBLow  = {time:eNearB.x,comp1:eNearB.y}

     eNearB  = !NULL
     ;; eNBHigh.data_name += '_HIGH'
     ;; eNBLow.data_name  += '_LOW'

     IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN

     ENDIF ELSE BEGIN
        FA_FIELDS_FILTER,eNBHigh,highFreqBounds, $
                         ;; DB=FFTdb, $
                         POLES=highFreqPoles

        FA_FIELDS_FILTER,eNBLow,lowFreqBounds, $
                         ;; DB=FFTdb, $
                         POLES=lowFreqPoles

     ENDELSE

     FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                       {TIME:eNBHigh.time[WHERE(FINITE(eNBHigh.comp1))], $
                        COMP1:eNBHigh.comp1[WHERE(FINITE(eNBHigh.comp1))]}, $
                       RESULT=eNBHighInterp, $
                       ;; /INTERP, $
                       /SPLINE, $
                       SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                       maxeFitPeriod : !NULL, $
                       /TALK

     FA_FIELDS_COMBINE,{TIME:magz.x,COMP1:magz.y}, $
                       {TIME:eNBLow.time[WHERE(FINITE(eNBLow.comp1))], $
                        COMP1:eNBLow.comp1[WHERE(FINITE(eNBLow.comp1))]}, $
                       RESULT=eNBLowInterp, $
                       ;; /INTERP, $
                       /SPLINE, $
                       SVY=KEYWORD_SET(use_eField_fit_variables) ? !NULL : 1, $ ;;Sets delt_t to 0.9 of time step in magz. S'OK
                       DELT_T=KEYWORD_SET(use_eField_fit_variables) ? $
                       maxeFitPeriod : !NULL, $
                       /TALK


     eNBHigh = {time:magz.x,comp1:eNBHighInterp}
     eNBLow  = {time:magz.x,comp1:eNBLowInterp}

  ENDIF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Poynting fluxes!

  IF KEYWORD_SET(full_pFlux) OR KEYWORD_SET(save_lil_package) THEN BEGIN


     pFBHigh = dBpHigh.comp1*eAVHigh.comp1/mu_0                        ;Poynting flux along B
     pFPHigh = (eNBHigh.comp1*dBvHigh.comp1 - $
                1.*dBBHigh.comp1*eAVHigh.comp1)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
     pFVHigh = (-1.)*eNBHigh.comp1*dBpHigh.comp1/mu_0

     pFBLow = dBpLow.comp1*eAVLow.comp1/mu_0                        ;Poynting flux along B
     pFPLow = (eNBLow.comp1*dBvLow.comp1 - $
                1.*dBBLow.comp1*eAVLow.comp1)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
     pFVHigh = (-1.)*eNBHigh.comp1*dBpHigh.comp1/mu_0

     pFVHigh *= 1e-9
     pFVLow  *= 1e-9

  ENDIF ELSE BEGIN

     pFBHigh =       dBpHigh.comp1 *eAVHigh.comp1/mu_0 ;Poynting flux along B
     pFPHigh = (-1.)*dBBHigh.comp1*eAVHigh.comp1/mu_0  ;Poynting flux perp to B and to (Bxv)xB
     ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

     pFBLow =       dBpLow.comp1 *eAVLow.comp1/mu_0 ;Poynting flux along B
     pFPLow = (-1.)*dBBLow.comp1*eAVLow.comp1/mu_0  ;Poynting flux perp to B and to (Bxv)xB
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

  tField = dBpHigh.time
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

  SMOOTH_TSERIES,tmp, $
                 pFluxHighSmSeconds, $
                 ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                 NRECURSE=nRecurse

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

  IF (n_elements(tplot_vars) EQ 0) THEN tplot_vars=['pFluxHigh'] $
  ELSE tplot_vars=['pFluxHigh',tplot_vars]

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;DC Poynting flux

  tField = dBpLow.time
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

  SMOOTH_TSERIES,tmp, $
                 pFluxLowSmSeconds, $
                 ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                 NRECURSE=nRecurse


  STORE_DATA,'pFluxLow',DATA=tmp
  ;; YLIM,'pFluxLow',1e-4,1e2,1
  ;; OPTIONS,'pFluxLow','yticks',7
  ;; OPTIONS,'pFluxLow','ytickv',[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2]
  ;; OPTIONS,'pFluxLow','ytickname',['10!U-4!N','10!U-3!N','10!U-2!N', $
  ;;                                 '10!U-1!N','10!U0!N','10!U1!N','10!U2!N']
  ;; OPTIONS,'pFluxLow','x_no_interp',1
  ;; OPTIONS,'pFluxLow','y_no_interp',1

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

  IF (n_elements(tplot_vars) EQ 0) THEN tplot_vars=['pFluxLow'] $
  ELSE tplot_vars=['pFluxLow',tplot_vars]

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
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
  OPTIONS,'JEe','x_no_interp',1
  OPTIONS,'JEe','y_no_interp',1

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
  options,'Ji','x_no_interp',1
  options,'Ji','y_no_interp',1


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

  tplot_vars=['dB_fac_v','pFluxHigh','pFluxLow','JEe','Ji']

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
     TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tLims


     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        PCLOSE
     ENDIF ELSE BEGIN

     ENDELSE

  ENDIF


  RETURN


END

