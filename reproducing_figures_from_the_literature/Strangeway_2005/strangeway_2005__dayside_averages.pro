;;09/24/16
PRO STRANGEWAY_2005__DAYSIDE_AVERAGES, $
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

  IF N_ELEMENTS(use_fac) EQ 0 AND N_ELEMENTS(use_fac_v) EQ 0 THEN use_fac = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

  mu_0         = DOUBLE(4.0D*!PI*1e-7)

  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_2005/'
  hashFile     = 'Strangeway_et_al_2005__DC_params.sav'
  outPlotName  = 'Strangeway_et_al_2005__ion_outflow--Fig_3'

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  nn           = n_elements(data_quants)

  if (nn gt 1) then for n = nn-1L,1L,-1L do store_data,data_quants(n).name,/delete

; Step 1 - DC Mag data

  ucla_mag_despin,tw_mat=tw_mat,orbit=orbit,spin_axis=spin_axis,delta_phi=delta_phi

  if (n_elements(orbit) gt 0) then begin

     orbString           = STRING(FORMAT='(I0)',orbit)
     outPlotName        += '--' + orbString

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
     options,'dB_fac_interp','x_no_interp',1
     options,'dB_fac_interp','y_no_interp',1

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
     options,'EFIT_ALONG_VSC','x_no_interp',1
     options,'EFIT_ALONG_VSC','y_no_interp',1

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
  options,'pFlux','x_no_interp',1
  options,'pFlux','y_no_interp',1

  IF (n_elements(tplot_vars) EQ 0) THEN tplot_vars=['pFlux'] $
  ELSE tplot_vars=['pFlux',tplot_vars]

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT']
  ENDIF

; Step 4 - Electron junk

; Electron energy flux

  t           = 0.0D
  tmp         = GET_FA_EES(t,/EN)
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
  ylim,'JEe',-1.,6.,0                                         ; set y limits
  options,'JEe','ytitle','Electron!CEnergy Flux!CmW/(m!U2!N)' ; set y title
  options,'JEe','panel_size',3                                ; set panel size
  options,'JEe','x_no_interp',1
  options,'JEe','y_no_interp',1

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
  ylim,'Je',-5.e9,1.5e10,0                               ; set y limits
  options,'Je','ytitle','Electron Flux!C!C#/(cm!U2!N-s)' ; set y title
  options,'Je','panel_size',3                            ; set panel size
  options,'Je','x_no_interp',1
  options,'Je','y_no_interp',1

; Step 5 - Ion flux

  IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]
  GET_2DT,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=energy_ions
  ;; ylim,'Ji',1.e5,1.e8,1      ; set y limits
  ;; options,'Ji','tplot_routine','pmplot'      ; set 2 color plot
  ;; options,'Ji','labels',['Downgoing!C Ions','Upgoing!C Ions ']       ; set color label
  ;; options,'Ji','labflag',3   ; set color label
  ;; options,'Ji','labpos',[2.e7,1.e6]  ; set color label
  GET_DATA,'Ji',DATA=tmp
  tmp.y = SMOOTH((-1.)*tmp.y,5)
  doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  STORE_DATA,'Ji',DATA={x:tS_1s,y:doDat}
  ylim,'Ji',-1.e9,6.e9,0                          ; set y limits
  options,'Ji','ytitle','Ion Flux!C#/(cm!U2!N-s)' ; set y title
  options,'Ji','panel_size',3                     ; set panel size
  options,'Ji','x_no_interp',1
  options,'Ji','y_no_interp',1


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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Gather data and STORE
  GET_FA_ORBIT,tS_1s,/TIME_ARRAY,/DEFINITIVE,/ALL
  GET_DATA,'fa_vel',DATA=vel
  speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  ;;get position of each mag point
  position         = MAKE_ARRAY(N_ELEMENTS(tS_1s),/DOUBLE,VALUE=0.0D)
  speed_point_inds = VALUE_CLOSEST2(vel.x,tS_1s)
  speed_mag_point  = speed[speed_point_inds]
  position[1:-1]   = TOTAL((tS_1s[1:-1]-tS_1s[0:-2])*speed_mag_point,/CUMULATIVE)

  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  GET_DATA,'ALT',DATA=alt
  GET_DATA,'EFIT_ALONG_VSC',DATA=eAVsc
  GET_DATA,'dB_fac_interp',DATA=dB_perp
  GET_DATA,'pFlux',DATA=pFluxB
  GET_DATA,'Je',DATA=Je
  GET_DATA,'JEe',DATA=Jee
  GET_DATA,'Ji',DATA=Ji

  ;;Check time series, if you like
  PRINT,ARRAY_EQUAL(eavsc.x[*,1],db_perp.x[*,1])
  PRINT,ARRAY_EQUAL(eavsc.x[*,1],pfluxb.x[*,1])
  PRINT,ARRAY_EQUAL(db_perp.x[*,1],pfluxb.x[*,1])
  PRINT,ARRAY_EQUAL(je.x,pfluxb.x[*,1])
  PRINT,ARRAY_EQUAL(je.x,jee.x)
  PRINT,ARRAY_EQUAL(je.x,ji.x)
  PRINT,ARRAY_EQUAL(jee.x,ji.x)

  ;;Reduce data
  time        = tS_1s           ;ephem stuff
  mlt         = mlt.y
  ilat        = ilat.y
  alt         = alt.y

  eAlongV     = eAVsc.y[*,1]    ;fields
  dB_perp     = dB_perp.y[*,1]
  pFAlongB    = pFluxB.y[*,1]
  je          = je.y            ;particles
  jee         = jee.y
  ji          = ji.y


  ;;Safety
  eAlongV  [WHERE(~FINITE(eAlongV  ))]  = 0.0
  dB_perp  [WHERE(~FINITE(dB_perp  ))]  = 0.0
  pFAlongB [WHERE(~FINITE(pFAlongB ))]  = 0.0
  je       [WHERE(~FINITE(je       ))]  = 0.0     
  jee      [WHERE(~FINITE(jee      ))]  = 0.0    
  ji       [WHERE(~FINITE(ji       ))]  = 0.0     

  ;;Indices divvying up hemispheres as well as day/nightside
  day_i  = WHERE(mlt GE 6.0 AND mlt LT 18.0 AND (ABS(ilat) GE minILAT),nDay)
  ngt_i  = WHERE(mlt GE 18.0 OR mlt LT 6.0  AND (ABS(ilat) GE minILAT),nNgt)

  all_i       = WHERE(ABS(ilat) GE minILAT,nAll)
  north_i     = WHERE(ilat GE minILAT,nNorth)
  south_i     = WHERE(ilat LE -1.*minILAT,nSouth)

  dayN_i = CGSETINTERSECTION(day_i,north_i)
  ngtN_i = CGSETINTERSECTION(ngt_i,north_i)
  dayS_i = CGSETINTERSECTION(day_i,south_i)
  ngtS_i = CGSETINTERSECTION(ngt_i,south_i)

  ;;Time ranges
  day_tRange  = [MIN(time[day_i]),MAX(time[day_i])]
  ngt_tRange  = [MIN(time[ngt_i]),MAX(time[ngt_i])]

  dayN_tRange = [MIN(time[dayN_i]),MAX(time[dayN_i])]
  ngtN_tRange = [MIN(time[ngtN_i]),MAX(time[ngtN_i])]
  dayS_tRange = [MIN(time[dayS_i]),MAX(time[dayS_i])]
  ngtS_tRange = [MIN(time[ngtS_i]),MAX(time[ngtS_i])]

  dayN_len    = MAX(position[dayN_i])-MIN(position[dayN_i])
  dayS_len    = MAX(position[dayS_i])-MIN(position[dayS_i])

  ngtN_len    = MAX(position[ngtN_i])-MIN(position[ngtN_i])
  ngtS_len    = MAX(position[ngtS_i])-MIN(position[ngtS_i])

  day_len     = dayN_len + dayS_len
  ngt_len     = ngtN_len + ngtS_len

  north_len   = dayN_len + ngtN_len
  south_len   = dayS_len + ngtS_len

  all_len     = north_len+south_len

  GET_DATA,'DSP_integ',DATA=DSP

  ;;Averages
  eAlongVAvg        = MEAN(eAlongV[all_i])
  eAlongVAvg_N      = MEAN(eAlongV[north_i])
  eAlongVAvg_S      = MEAN(eAlongV[south_i])
  eAlongVAvg_day    = MEAN(eAlongV[day_i])
  eAlongVAvg_ngt    = MEAN(eAlongV[ngt_i])
  eAlongVAvg_dayN   = MEAN(eAlongV[dayN_i])
  eAlongVAvg_ngtN   = MEAN(eAlongV[ngtN_i])
  eAlongVAvg_dayS   = MEAN(eAlongV[dayS_i])
  eAlongVAvg_ngtS   = MEAN(eAlongV[ngtS_i])

  dB_perpAvg        = MEAN(dB_perp[all_i])
  dB_perpAvg_N      = MEAN(dB_perp[north_i])
  dB_perpAvg_S      = MEAN(dB_perp[south_i])
  dB_perpAvg_day    = MEAN(dB_perp[day_i])
  dB_perpAvg_ngt    = MEAN(dB_perp[ngt_i])
  dB_perpAvg_dayN   = MEAN(dB_perp[dayN_i])
  dB_perpAvg_ngtN   = MEAN(dB_perp[ngtN_i])
  dB_perpAvg_dayS   = MEAN(dB_perp[dayS_i])
  dB_perpAvg_ngtS   = MEAN(dB_perp[ngtS_i])

  pFAlongBAvg       = MEAN(pFAlongB[all_i])
  pFAlongBAvg_N     = MEAN(pFAlongB[north_i])
  pFAlongBAvg_S     = MEAN(pFAlongB[south_i])
  pFAlongBAvg_day   = MEAN(pFAlongB[day_i])
  pFAlongBAvg_ngt   = MEAN(pFAlongB[ngt_i])
  pFAlongBAvg_dayN  = MEAN(pFAlongB[dayN_i])
  pFAlongBAvg_ngtN  = MEAN(pFAlongB[ngtN_i])
  pFAlongBAvg_dayS  = MEAN(pFAlongB[dayS_i])
  pFAlongBAvg_ngtS  = MEAN(pFAlongB[ngtS_i])

  jeAvg             = MEAN(je[all_i])
  jeAvg_N           = MEAN(je[north_i])
  jeAvg_S           = MEAN(je[south_i])
  jeAvg_day         = MEAN(je[day_i])
  jeAvg_ngt         = MEAN(je[ngt_i])
  jeAvg_dayN        = MEAN(je[dayN_i])
  jeAvg_ngtN        = MEAN(je[ngtN_i])
  jeAvg_dayS        = MEAN(je[dayS_i])
  jeAvg_ngtS        = MEAN(je[ngtS_i])

  JEeAvg            = MEAN(JEe[all_i])
  JEeAvg_N          = MEAN(JEe[north_i])
  JEeAvg_S          = MEAN(JEe[south_i])
  JEeAvg_day        = MEAN(JEe[day_i])
  JEeAvg_ngt        = MEAN(JEe[ngt_i])
  JEeAvg_dayN       = MEAN(JEe[dayN_i])
  JEeAvg_ngtN       = MEAN(JEe[ngtN_i])
  JEeAvg_dayS       = MEAN(JEe[dayS_i])
  JEeAvg_ngtS       = MEAN(JEe[ngtS_i])

  JiAvg             = MEAN(Ji[all_i])
  JiAvg_N           = MEAN(Ji[north_i])
  JiAvg_S           = MEAN(Ji[south_i])
  JiAvg_day         = MEAN(Ji[day_i])
  JiAvg_ngt         = MEAN(Ji[ngt_i])
  JiAvg_dayN        = MEAN(Ji[dayN_i])
  JiAvg_ngtN        = MEAN(Ji[ngtN_i])
  JiAvg_dayS        = MEAN(Ji[dayS_i])
  JiAvg_ngtS        = MEAN(Ji[ngtS_i])

  ;;Integrals
  eAlongVInt        = INT_TABULATED(position[all_i],eAlongV[all_i])/all_len
  eAlongVInt_N      = INT_TABULATED(position[north_i],eAlongV[north_i])/north_len
  eAlongVInt_S      = INT_TABULATED(position[south_i],eAlongV[south_i])/south_len
  eAlongVInt_day    = INT_TABULATED(position[day_i],eAlongV[day_i])/day_len
  eAlongVInt_ngt    = INT_TABULATED(position[ngt_i],eAlongV[ngt_i])/ngt_len
  eAlongVInt_dayN   = INT_TABULATED(position[dayN_i],eAlongV[dayN_i])/dayN_len
  eAlongVInt_ngtN   = INT_TABULATED(position[ngtN_i],eAlongV[ngtN_i])/ngtN_len
  eAlongVInt_dayS   = INT_TABULATED(position[dayS_i],eAlongV[dayS_i])/dayS_len
  eAlongVInt_ngtS   = INT_TABULATED(position[ngtS_i],eAlongV[ngtS_i])/ngtS_len

  dB_perpInt        = INT_TABULATED(position[all_i],dB_perp[all_i])/all_len
  dB_perpInt_N      = INT_TABULATED(position[north_i],dB_perp[north_i])/north_len
  dB_perpInt_S      = INT_TABULATED(position[south_i],dB_perp[south_i])/south_len
  dB_perpInt_day    = INT_TABULATED(position[day_i],dB_perp[day_i])/day_len
  dB_perpInt_ngt    = INT_TABULATED(position[ngt_i],dB_perp[ngt_i])/ngt_len
  dB_perpInt_dayN   = INT_TABULATED(position[dayN_i],dB_perp[dayN_i])/dayN_len
  dB_perpInt_ngtN   = INT_TABULATED(position[ngtN_i],dB_perp[ngtN_i])/ngtN_len
  dB_perpInt_dayS   = INT_TABULATED(position[dayS_i],dB_perp[dayS_i])/dayS_len
  dB_perpInt_ngtS   = INT_TABULATED(position[ngtS_i],dB_perp[ngtS_i])/ngtS_len

  pFAlongBInt       = INT_TABULATED(position[all_i],pFAlongB[all_i])/all_len
  pFAlongBInt_N     = INT_TABULATED(position[north_i],pFAlongB[north_i])/north_len
  pFAlongBInt_S     = INT_TABULATED(position[south_i],pFAlongB[south_i])/south_len
  pFAlongBInt_day   = INT_TABULATED(position[day_i],pFAlongB[day_i])/day_len
  pFAlongBInt_ngt   = INT_TABULATED(position[ngt_i],pFAlongB[ngt_i])/ngt_len
  pFAlongBInt_dayN  = INT_TABULATED(position[dayN_i],pFAlongB[dayN_i])/dayN_len
  pFAlongBInt_ngtN  = INT_TABULATED(position[ngtN_i],pFAlongB[ngtN_i])/ngtN_len
  pFAlongBInt_dayS  = INT_TABULATED(position[dayS_i],pFAlongB[dayS_i])/dayS_len
  pFAlongBInt_ngtS  = INT_TABULATED(position[ngtS_i],pFAlongB[ngtS_i])/ngtS_len

  JeInt             = INT_TABULATED(position[all_i],Je[all_i])/all_len
  JeInt_N           = INT_TABULATED(position[north_i],Je[north_i])/north_len
  JeInt_S           = INT_TABULATED(position[south_i],Je[south_i])/south_len
  JeInt_day         = INT_TABULATED(position[day_i],Je[day_i])/day_len
  JeInt_ngt         = INT_TABULATED(position[ngt_i],Je[ngt_i])/ngt_len
  JeInt_dayN        = INT_TABULATED(position[dayN_i],Je[dayN_i])/dayN_len
  JeInt_ngtN        = INT_TABULATED(position[ngtN_i],Je[ngtN_i])/ngtN_len
  JeInt_dayS        = INT_TABULATED(position[dayS_i],Je[dayS_i])/dayS_len
  JeInt_ngtS        = INT_TABULATED(position[ngtS_i],Je[ngtS_i])/ngtS_len

  JeeInt            = INT_TABULATED(position[all_i],Jee[all_i])/all_len
  JeeInt_N          = INT_TABULATED(position[north_i],Jee[north_i])/north_len
  JeeInt_S          = INT_TABULATED(position[south_i],Jee[south_i])/south_len
  JEeInt_day        = INT_TABULATED(position[day_i],JEe[day_i])/day_len
  JEeInt_ngt        = INT_TABULATED(position[ngt_i],JEe[ngt_i])/ngt_len
  JeeInt_dayN       = INT_TABULATED(position[dayN_i],Jee[dayN_i])/dayN_len
  JeeInt_ngtN       = INT_TABULATED(position[ngtN_i],Jee[ngtN_i])/ngtN_len
  JeeInt_dayS       = INT_TABULATED(position[dayS_i],Jee[dayS_i])/dayS_len
  JeeInt_ngtS       = INT_TABULATED(position[ngtS_i],Jee[ngtS_i])/ngtS_len

  JiInt             = INT_TABULATED(position[all_i],Ji[all_i])/all_len
  JiInt_N           = INT_TABULATED(position[north_i],Ji[north_i])/north_len
  JiInt_S           = INT_TABULATED(position[south_i],Ji[south_i])/south_len
  JiInt_day         = INT_TABULATED(position[day_i],Ji[day_i])/day_len
  JiInt_ngt         = INT_TABULATED(position[ngt_i],Ji[ngt_i])/ngt_len
  JiInt_dayN        = INT_TABULATED(position[dayN_i],Ji[dayN_i])/dayN_len
  JiInt_ngtN        = INT_TABULATED(position[ngtN_i],Ji[ngtN_i])/ngtN_len
  JiInt_dayS        = INT_TABULATED(position[dayS_i],Ji[dayS_i])/dayS_len
  JiInt_ngtS        = INT_TABULATED(position[ngtS_i],Ji[ngtS_i])/ngtS_len


  struct = {orbit:orbit, $
            time:time, $
            avg:{all:{eAlongV:eAlongVAvg, $
                      dB_perp:dB_perpAvg, $
                      pFAlongB:pFAlongBAvg, $
                      je:jeAvg, $
                      Jee:JEeAvg, $
                      Ji:JiAvg}, $
                 North:{both:{eAlongV:eAlongVAvg_N, $
                              dB_perp:dB_perpAvg_N, $
                              pFAlongB:pFAlongBAvg_N, $
                              je:jeAvg_N, $
                              Jee:JEeAvg_N, $
                              Ji:JiAvg_N}, $
                        day:{eAlongV:eAlongVAvg_dayN, $
                             dB_perp:dB_perpAvg_dayN, $
                             pFAlongB:pFAlongBAvg_dayN, $
                             je:jeAvg_dayN, $
                             Jee:JEeAvg_dayN, $
                             Ji:JiAvg_dayN}, $
                        ngt:{eAlongV:eAlongVAvg_ngtN, $
                             dB_perp:dB_perpAvg_ngtN, $
                             pFAlongB:pFAlongBAvg_ngtN, $
                             je:jeAvg_ngtN, $
                             Jee:JEeAvg_ngtN, $
                             Ji:JiAvg_ngtN}}, $
                 South:{both:{eAlongV:eAlongVAvg_S, $
                              dB_perp:dB_perpAvg_S, $
                              pFAlongB:pFAlongBAvg_S, $
                              je:jeAvg_S, $
                              Jee:JEeAvg_S, $
                              Ji:JiAvg_S}, $
                        day:{eAlongV:eAlongVAvg_dayS, $
                             dB_perp:dB_perpAvg_dayS, $
                             pFAlongB:pFAlongBAvg_dayS, $
                             je:jeAvg_dayS, $
                             Jee:JEeAvg_dayS, $
                             Ji:JiAvg_dayS}, $
                        ngt:{eAlongV:eAlongVAvg_ngtS, $
                             dB_perp:dB_perpAvg_ngtS, $
                             pFAlongB:pFAlongBAvg_ngtS, $
                             je:jeAvg_ngtS, $
                             Jee:JEeAvg_ngtS, $
                             Ji:JiAvg_ngtS}}}, $
            int:{all:{eAlongV:eAlongVInt, $
                      dB_perp:dB_perpInt, $
                      pFAlongB:pFAlongBInt, $
                      je:jeInt, $
                      Jee:JEeInt, $
                      Ji:JiInt}, $
                 North:{both:{eAlongV:eAlongVInt_N, $
                              dB_perp:dB_perpInt_N, $
                              pFAlongB:pFAlongBInt_N, $
                              je:jeInt_N, $
                              Jee:JEeInt_N, $
                              Ji:JiInt_N}, $
                        day:{eAlongV:eAlongVInt_dayN, $
                             dB_perp:dB_perpInt_dayN, $
                             pFAlongB:pFAlongBInt_dayN, $
                             je:jeInt_dayN, $
                             Jee:JEeInt_dayN, $
                             Ji:JiInt_dayN}, $
                        ngt:{eAlongV:eAlongVInt_ngtN, $
                             dB_perp:dB_perpInt_ngtN, $
                             pFAlongB:pFAlongBInt_ngtN, $
                             je:jeInt_ngtN, $
                             Jee:JEeInt_ngtN, $
                             Ji:JiInt_ngtN}}, $
                 South:{both:{eAlongV:eAlongVInt_S, $
                              dB_perp:dB_perpInt_S, $
                              pFAlongB:pFAlongBInt_S, $
                              je:jeInt_S, $
                              Jee:JEeInt_S, $
                              Ji:JiInt_S}, $
                        day:{eAlongV:eAlongVInt_dayS, $
                             dB_perp:dB_perpInt_dayS, $
                             pFAlongB:pFAlongBInt_dayS, $
                             je:jeInt_dayS, $
                             Jee:JEeInt_dayS, $
                             Ji:JiInt_dayS}, $
                        ngt:{eAlongV:eAlongVInt_ngtS, $
                             dB_perp:dB_perpInt_ngtS, $
                             pFAlongB:pFAlongBInt_ngtS, $
                             je:jeInt_ngtS, $
                             Jee:JEeInt_ngtS, $
                             Ji:JiInt_ngtS}}}}                 


  IF ~KEYWORD_SET(no_hash_update) THEN BEGIN
     IF FILE_TEST(outDir+hashFile) THEN BEGIN
        PRINT,"Restoring hash file ..."
        RESTORE,outDir+hashFile

        CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
           -1: BEGIN
              PRINT,'Adding stuff from orbit ' + orbString + ' ...'
              swHash  = swHash + HASH(orbit,struct)
           END
           ELSE: BEGIN
              PRINT,'Replacing hash entry for orbit ' + orbString + ' ...'
              swHash[orbit] = struct
           END
        ENDCASE

        PRINT,'Saving Strangeway statistics hash ...'
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDIF ELSE BEGIN
        PRINT,'Creating Strangeway statistics hash for orbit ' + orbString + ' ...'
        swHash = HASH(orbit,struct)
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDELSE
  ENDIF

  RETURN

END
