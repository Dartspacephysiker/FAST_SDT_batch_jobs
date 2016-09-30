;;09/24/16
;;This is entirely ripped off from Strangeway's batch_summary.pro, gifted to me by that beautiful human, Jack Vernetti.
PRO JOURNAL__20160924__STRANGEWAY_2005__FIGURE_3__ORB_8276, $
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

  @tplot_com

  @startup

  @strway_stuff

  ON_ERROR,0

  normColorI     = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

  mu_0           = DOUBLE(4.0D*!PI*1e-7)

  outPlotName             = 'Strangeway_et_al_2005__ion_outflow--Fig_3'

  IF N_ELEMENTS(use_fac) EQ 0 AND N_ELEMENTS(use_fac_v) EQ 0 THEN use_fac = 1

  t1ZoomStr               = '1998-09-25/00:00:00'
  t2ZoomStr               = '1998-09-25/00:16:00'

  nn = n_elements(data_quants)

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
     t1 = data.x[0]
     t2 = data.x[n_elements(data.x)-1L]
     tlimit_all = [t1,t2]
     tplot_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

     if (keyword_set(use_fac)) then tplot_vars = 'dB_fac'

     if ~KEYWORD_SET(no_blank_panels) AND ~KEYWORD_SET(use_fac) then tplot_vars = 'dB_fac_v'

     if (keyword_set(screen_plot)) then begin
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

     ;; OPTIONS,var_name,'labels',['o','e','b']

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

     ;; data = {x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
     ;;         y:[TRANSPOSE(TEMPORARY(datInterp)), $
     ;;            TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
     data = {x:[[tS_1s],[tS_1s]], $
             y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                [TEMPORARY(datInterp)]]}

     ;; data = {x:tS_1s, $
     ;;         y:datInterp}

     ;; OPTIONS,'dB_fac_interp','labels',(['o','e','b'])[magInd]

     STORE_DATA,'dB_fac_interp',DATA=data

     dLimit = {spec:0, ystyle:1, yrange:[-600., 800.], $
               ytitle:'dB Perp.!C!C[DC] (nT)', $
               panel_size:3}

     OPTIONS,'dB_fac_interp','colors',[normColorI,normColorI]
     OPTIONS,'dB_fac_interp','tplot_routine','mplot'
     STORE_DATA,'dB_fac_interp',DLIMITS=dLimit

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

     OPTIONS,'EFIT_ALONG_V','yrange',0
     OPTIONS,'EFIT_ALONG_V','ytitle','E along V!C!C[DC] (mV/m)'
     OPTIONS,'EFIT_ALONG_V','colors',[normColorI,normColorI]
     OPTIONS,'EFIT_ALONG_V','panel_size',2

; reset time limits if needed

     get_data,'EFIT_ALONG_V',data=data

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

     ;; data = {x:tS_1s, $
     ;;         y:datInterp}

     ;; data = {x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
     ;;         y:[TRANSPOSE(TEMPORARY(datInterp)), $
     ;;            TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
     data = {x:[[tS_1s],[tS_1s]], $
             y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                [TEMPORARY(datInterp)]]}

     STORE_DATA,'EFIT_ALONG_V',DATA=data

     t1 = data.x[0]
     t2 = data.x[n_elements(data.x)-1L]

     if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
        if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
        if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
        get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
        get_new_igrf,/no_store_old
     endif

; check for southern hemisphere and fix 
; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

     get_data,'B_model',data=bm
     get_data,'fa_vel',data=vel
     get_data,'fa_pos',data=pos
     n=n_elements(reform(pos.y[*,0]))
     rxv = dblarr(n,3)
     rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
     rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
     rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
     vxb = dblarr(n,3)
     vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
     vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
     vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
     tst = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

     get_data,'EFIT_ALONG_V',data=data,dlimit=dlimit
     y2=spl_init(pos.x-tlimit_all[0],tst,/double)
     tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
     data.y = data.y*tst_/abs(tst_)
     store_data,'EFIT_ALONG_VSC',data=data,dlimit=dlimit
     options,'EFIT_ALONG_VSC','yrange',0
     options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C[DC] (mV/m)'
     OPTIONS,'EFIT_ALONG_VSC','colors',[normColorI,normColorI]
     options,'EFIT_ALONG_VSC','panel_size',2

     store_data,'E_NEAR_B',/delete
     store_data,'E_ALONG_V',/delete
     store_data,'EFIT_NEAR_B',/delete
     store_data,'EFIT_ALONG_V',/delete

     if (n_elements(tplot_vars) eq 0) then tplot_vars=['EFIT_ALONG_VSC'] else tplot_vars=['EFIT_ALONG_VSC',tplot_vars]

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tplot_vars,var=['ALT','ILAT','MLT']
     endif

  endif else if (n_elements(tplot_vars) ne 0) then begin

     tplot_vars = 'dB_fac'
     if (keyword_set(use_fac_v)) then tplot_vars = 'dB_fac_v'
     if ~KEYWORD_SET(no_blank_panels) then tplot_vars = 'dB_fac'

  endif


; Step 3 - Poynting flux
  GET_DATA,'dB_fac_interp',data=magData
  GET_DATA,'EFIT_ALONG_VSC',data=eFData
  ;; pFluxB = REFORM((magData.y[0,*]*1.e-9)*eFData.y[0,*]/mu_0) ;Poynting flux along B
  pFluxB = (magData.y[*,1]*1.e-9)*eFData.y[*,1]/mu_0 ;Poynting flux along B
  pFluxB[WHERE(~FINITE(pFluxB))] = 0.0

  ;; STORE_DATA,'pFlux',DATA={x:[TRANSPOSE(tS_1s),TRANSPOSE(tS_1s)], $
  ;;                          y:[TRANSPOSE(pFluxB), $
  ;;                             TRANSPOSE(MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.))]}
  STORE_DATA,'pFlux',DATA={x:[[tS_1s],[tS_1s]], $
                           y:[[MAKE_ARRAY(N_ELEMENTS(tS_1s),VALUE=0.)], $
                              [pFluxB]]}
  dLimit = {spec:0, ystyle:1, yrange:[-20., 80.], $
            ytitle:'Poynting Flux!C!C[DC] (mW/m!U2!N)', $
            panel_size:3}
  OPTIONS,'pFlux','colors',[normColorI,normColorI]
  STORE_DATA,'pFlux',DLIMITS=dLimit

  IF (n_elements(tplot_vars) EQ 0) THEN tplot_vars=['pFlux'] $
  ELSE tplot_vars=['pFlux',tplot_vars]

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
  ;; ylim,'JEe',1.e-1,1.e1,1                                               ; set y limits
  ;; options,'JEe','tplot_routine','pmplot'                                  
  ;; options,'JEe','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] 
  ;; options,'JEe','labpos',[4.e0,5.e-1]                                     ; set color label
  ;; options,'JEe','labflag',3                                               ; set color label
  GET_DATA,'JEe',DATA=tmp
  tmp.y = SMOOTH(tmp.y,5)
  doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  STORE_DATA,'JEe',DATA={x:tS_1s,y:doDat}
  ylim,'JEe',-1.,6.,0                                                     ; set y limits
  options,'JEe','ytitle','Electron!CEnergy Flux!CmW/(m!U2!N)'             ; set y title
  options,'JEe','panel_size',3                                            ; set panel size

; Electron flux

  GET_2DT,'j_2d_fs','fa_ees_c',NAME='Je',T1=t1,T2=t2,ENERGY=energy_electrons
  ;; ylim,'Je',1.e7,1.e9,1                                                ; set y limits
  ;; options,'Je','tplot_routine','pmplot'                                ; set 2 color plot
  ;; options,'Je','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] ; set color label
  ;; options,'Je','labflag',3                                               ; set color label
  ;; options,'Je','labpos',[4.e8,5.e7]                                      ; set color label
  GET_DATA,'Je',DATA=tmp
  tmp.y = SMOOTH(tmp.y,5)
  doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  STORE_DATA,'Je',DATA={x:tS_1s,y:doDat}
  ylim,'Je',-5.e9,1.5e10,0                                                  ; set y limits
  options,'Je','ytitle','Electron Flux!C#/(cm!U2!N-s)'                    ; set y title
  options,'Je','panel_size',3                                               ; set panel size

; Step 5 - Ion flux

  IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]
  GET_2DT,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=energy_ions
  ;; ylim,'Ji',1.e5,1.e8,1 	; set y limits
  ;; options,'Ji','tplot_routine','pmplot' 	; set 2 color plot
  ;; options,'Ji','labels',['Downgoing!C Ions','Upgoing!C Ions '] 	; set color label
  ;; options,'Ji','labflag',3 	; set color label
  ;; options,'Ji','labpos',[2.e7,1.e6] 	; set color label
  GET_DATA,'Ji',DATA=tmp
  tmp.y = SMOOTH((-1.)*tmp.y,5)
  doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  STORE_DATA,'Ji',DATA={x:tS_1s,y:doDat}
  ylim,'Ji',-1.e9,6.e9,0                          ; set y limits
  options,'Ji','ytitle','Ion Flux!C#/(cm!U2!N-s)' ; set y title
  options,'Ji','panel_size',3                     ; set panel size


; Step 6 - VLF data


; DSP_V5-V8HG or DSP_V5-V8

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
  b = where (strpos(result,'DspADC_V5-V8HG') ge 0,ndsphg)
  if (ndsphg gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsphg = 0
  b = where ((strpos(result,'DspADC_V5-V8') ge 0) and (strpos(result,'DspADC_V5-V8HG') lt 0),ndsp)
  if (ndsp gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsp = 0

  if (ndsphg gt 0) then dat=get_fa_fields('DspADC_V5-V8HG',/all) else if (ndsp gt 0) then dat=get_fa_fields('DspADC_V5-V8',/all)
  ndsp = (ndsp gt 0) or (ndsphg gt 0)

  if (ndsp) then begin
     data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
     store_data,'DSP_V5-V8', data=data
     dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-14,-4], $
               ytitle:'AC E 55m!C!C(kHz)', ylog:1, $
               ztitle: '(V/m)!U2!N/Hz', panel_size:2}
     store_data,'DSP_V5-V8', dlimit=dlimit
     options,'DSP_V5-V8','x_no_interp',1
     options,'DSP_V5-V8','y_no_interp',1

;  look for big jumps in time - blank these

     get_data,'DSP_V5-V8',data=data
     dt = data.x[1:*]-data.x[0:*]
     ntimes=n_elements(data.x)
     bg = where (dt gt 300, ng)
     if (ng gt 0) then begin
        bbb = bg-1
        if (bbb[0] lt 0) then bbb[0] = 0
        add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
        flag_dat = fltarr(ng*2)+!values.f_nan
        new_tag = [data.x,add_tag]
        tsort = sort(new_tag-new_tag[0])
        nvec=n_elements(data.y)/ntimes
        new_dat = fltarr(n_elements(new_tag),nvec)
        for nv = 0,nvec-1 do begin
           new_dat[*,nv] = [data.y[*,nv],flag_dat]
           new_dat[*,nv] = new_dat[tsort,nv]
        endfor
        data={x:new_tag[tsort],y:new_dat,v:data.v}
        store_data,'DSP_V5-V8',data=data
     endif
     
     if (n_elements(tplot_vars) eq 0) then tplot_vars=['DSP_V5-V8'] else tplot_vars=['DSP_V5-V8',tplot_vars]

     if (keyword_set(screen_plot)) then begin
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


     ;; STORE_DATA,'DSP_integ',DATA={x:data.x,y:integData}
     STORE_DATA,'DSP_integ',DATA={x:tS_1s,y:datInterp}
     dlimit = {ystyle:1, yrange:[0.0,0.05], $
               ytitle:'ELF Amplitude (V/m)', $
               panel_size:3}
     store_data,'DSP_integ', dlimit=dlimit
     options,'DSP_integ','x_no_interp',1
     options,'DSP_integ','y_no_interp',1


  endif else begin

  endelse

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

; force tplot_vars to be all the panels unless no_blank_panels is set

  if ~KEYWORD_SET(no_blank_panels) then begin


; DSP

     bdat = where(tplot_vars eq 'DSP_V5-V8',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = [0.0,0.128,9.984,16.352]
        store_data,'DSP_V5-V8', data={x:t_arr, y:y_arr, v:v_arr}
        dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
                  ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        store_data,'DSP_V5-V8', dlimit=dlimit
        options,'DSP_V5-V8','x_no_interp',1
        options,'DSP_V5-V8','y_no_interp',1
     endif

; EFIT_ALONG_VSC

     bdat = where(tplot_vars eq 'EFIT_ALONG_VSC',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = [!values.f_nan,!values.f_nan]
        store_data,'EFIT_ALONG_VSC', data={x:t_arr, y:y_arr}
        dlimit = {spec:0, ystyle:1, yrange:[-1000., 1000.], $
                  ytitle:'EFIT ALONG V!C!C55m (mV/m)', $
                  panel_size:3}
        store_data,'EFIT_ALONG_V',dlimit=dlimit
        options,'EFIT_ALONG_VSC','yrange',[-100.,100.]
        options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
        options,'EFIT_ALONG_VSC','panel_size',2
     endif

; dB_fac_v
;CHANGED to dB_fac
     bdat = where(tplot_vars eq 'dB_fac',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = dblarr(2,3)
        y_arr[*,*] = !values.d_nan
        store_data,'dB_fac', data={x:t_arr, y:y_arr}
        options,'dB_fac','yrange',[-100,100]
        options,'dB_fac','ytitle','dB_fac!C!C(nT))'
        options,'dB_fac','panel_size',2
        options,'dB_fac','colors',[6,4,2]
        ;; options,'dB_fac_v','labels',['v ((BxV)xB)','p (BxV)','b']
        options,'dB_fac','labels',['o','e','b']
     endif

     ;; tplot_vars=['SFA_V5-V8','DSP_V5-V8','Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','EFIT_ALONG_VSC','dB_fac_v']

     tplot_vars=['EFIT_ALONG_VSC','dB_fac_interp','pFlux','Je','JEe','DSP_integ','Ji']
  endif

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
