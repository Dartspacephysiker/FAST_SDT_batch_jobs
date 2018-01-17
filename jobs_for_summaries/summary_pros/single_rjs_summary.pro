PRO SINGLE_RJS_SUMMARY,time1,time2, $
                       TPLT_VARS=tPlt_vars, $
                       EEB_OR_EES=eeb_OR_ees, $
                       ENERGY_ELECTRONS=energy_electrons, $
                       TLIMIT_NORTH=tlimit_north, $
                       TLIMIT_SOUTH=tlimit_south, $
                       TLIMIT_ALL=tlimit_all, $
                       SCREEN_PLOT=screen_plot, $
                       USE_FAC_V=use_fac_v, $
                       USE_FAC_NOT_V=use_fac, $
                       NO_BLANK_PANELS=no_blank_panels, $
                       ADD_KAPPA_PANEL=add_kappa_panel, $
                       ADD_CHARE_PANEL=add_chare_panel, $
                       ADD_NEWELL_PANEL=add_Newell_panel, $
                       NEWELL_2009_INTERP=Newell_2009_interp, $
                       LOG_KAPPAPLOT=log_kappaPlot, $
                       FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                       FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                       KAPPAFIT1DS=kappaFit1Ds, $
                       GAUSSFIT1DS=gaussFit1Ds, $
                       CHI2_THRESHOLD=chi2_thresh, $
                       CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                       HIGHDENSITY_THRESHOLD=highDens_thresh, $
                       LOWDENSITY_THRESHOLD=lowDens_thresh, $
                       DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                       N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                       SAVE_PS=save_ps, $
                       SAVE_PNG=save_png, $
                       EPS=eps, $
                       SAVEKAPPA_BONUSPREF=bonusPref, $
                       GRL=GRL, $
                       PLOTDIR=plotDir


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

@tplot_com

  ctNum = 39
  ctNum = 43                    ;Better; not oceans of green
  IF KEYWORD_SET(screen_plot) AND ~(KEYWORD_SET(save_ps) OR KEYWORD_SET(save_png)) THEN BEGIN
     DEVICE,PSEUDO_COLOR=8      ;fixes color table problem for machines with 24-bit color
     LOADCT2,ctNum
  ENDIF ELSE LOADCT2,ctNum         ; rainbow color map

  IF NOT KEYWORD_SET(energy_ions) THEN energy_ions=[4,1.e4]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN ieb_or_ies = 'ieb' ELSE ieb_or_ies = 'ies'
  burst = (WHERE(STRMATCH(STRUPCASE(eeb_or_ees),'*B')))[0] NE -1

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
        ;; return

     endif

     orbString           = STRING(FORMAT='(I0)',orbit)

;;Handle PNGness or PSness before kicking things off

     IF keyword_set(save_ps) THEN BEGIN

        outPlotName  = 'Strangeway_summary'
        outPlotName += '--' + orbString + (KEYWORD_SET(bonusPref) ? bonusPref : '' )

        IF N_ELEMENTS(Newell_2009_interp) GT 0 THEN BEGIN
           IF Newell_2009_interp EQ 0 THEN BEGIN
              outPlotName += '--not_Newell_interpreted'
           ENDIF
        ENDIF

        t1S = STRMID(TIME_TO_STR(time1,/MSEC),11,11)
        t2S = STRMID(TIME_TO_STR(time2,/MSEC),11,11)

        t1S = t1S.REPLACE(':', '_')
        t1S = t1S.REPLACE('.', '__')
        
        t2S = t2S.REPLACE(':', '_')
        t2S = t2S.REPLACE('.', '__')
        
        outPlotName += '--' + t1S + '_-_' + t2S


        IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Strangeway_et_al_2005'
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN

           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7

        ENDIF ELSE BEGIN

           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+outPlotName,/PORT,FONT=-1, $
                    ENCAPSULATED=eps ;,XSIZE=4,YSIZE=7

              DEVICE,/PALATINO,FONT_SIZE=8

           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE

        ENDELSE

     ENDIF

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
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0] ELSE t1 = time1
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L] ELSE t2 = time2
     tlimit_all = [t1,t2]
     tPlt_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     if (keyword_set(use_fac)) then tPlt_vars = 'dB_fac'

     if (not keyword_set(no_blank_panels)) then tPlt_vars = 'dB_fac_v'

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        wInd = 0
        WINDOW,wInd,XSIZE=700,YSIZE=900
        ;; tplot_options,'region',[0.,0.5,1.0,1.0]
        ;; loadct2,39
        tplot,tPlt_vars,var=['ALT','ILAT','MLT'], $
              WINDOW=wInd, $
              TRANGE=[t1,t2]
     endif

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


  CASE 1 OF
     burst: BEGIN
        v12Str = 'V1-V2_S'
        v14Str = 'V1-V4_S'
        v58Str = 'V5-V8_S'
     END
     ELSE: BEGIN
        v12Str = 'V1-V2_S'
        v14Str = 'V1-V4_S'
        v58Str = 'V5-V8_S'
     END
  ENDCASE


  b = where (strpos(result,v14Str) ge 0,nb4)
  if (nb4 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb4 = 0
  b = where (strpos(result,v12Str) ge 0,nb2)
  if (nb2 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb2 = 0
  if (nb4 gt 0) then v12=get_fa_fields(v14Str,/all) $
  else if (nb2 gt 0) then v12=get_fa_fields(v12Str,/all)

  b = where (strpos(result,v58Str) ge 0,nb5)
  if (nb5 gt 0) then v58=get_fa_fields(v58Str,/all)

  got_efield = (nb4 gt 0 or nb2 gt 0) and nb5 gt 0

  if (got_efield) then begin

; despin e field data

     fa_fields_despin,v58,v12,/shadow_notch,/sinterp

     options,'EFIT_ALONG_V','yrange',0
     options,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
     options,'EFIT_ALONG_V','panel_size',2

; reset time limits if needed

     get_data,'EFIT_ALONG_V',data=data
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L]

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
     ;; y2=spl_init(pos.x-tlimit_all[0],tst,/double)
     ;; tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
     ;; data.y = data.y*tst_/abs(tst_)
     store_data,'EFIT_ALONG_VSC',data=data,dlimit=dlimit
     options,'EFIT_ALONG_VSC','yrange',0
     options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
     options,'EFIT_ALONG_VSC','panel_size',2

     store_data,'E_NEAR_B',/delete
     store_data,'E_ALONG_V',/delete
     store_data,'EFIT_NEAR_B',/delete
     store_data,'EFIT_ALONG_V',/delete

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['EFIT_ALONG_VSC'] else tPlt_vars=['EFIT_ALONG_VSC',tPlt_vars]

     YLIM,'EFIT_ALONG_VSC',MIN(data.y),MAX(data.y),0
     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        ;; loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  endif else if (n_elements(tPlt_vars) ne 0) then begin

     tPlt_vars = 'dB_fac'
     if (keyword_set(use_fac_v)) then tPlt_vars = 'dB_fac_v'
     if (not keyword_set(no_blank_panels)) then tPlt_vars = 'dB_fac_v'

  endif


; Step 3 - Iesa data

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
  b = where (strpos(result,'Iesa Survey') ge 0,nesa)
  if (nesa gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nesa = 0

  if (nesa gt 0) then begin

; ION PITCH ANGLE

     var_name='Iesa_Angle'
     get_pa_spec,'fa_' + ieb_or_ies + '_c',units='eflux',name=var_name,energy=[4.,30000.]
     get_data,var_name, data=data
     data.y = alog10(data.y)
     store_data,var_name, data=data
     options,var_name,'spec',1	
     ;; zlim,var_name,4,9,0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 5 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 9),0
     ;; zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     ylim,var_name,0,360,0
     options,var_name,'ytitle','Ions!C!CAngle (Deg.)'
     options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     options,var_name,'x_no_interp',1
     options,var_name,'y_no_interp',1
     options,var_name,'panel_size',2

     get_data,var_name, data=data
     bb = where (data.v gt 270.,nb)
     if (nb gt 0) then data.v(bb)=data.v(bb)-360.
     nn = n_elements(data.x)
     for n = 0,nn-1L do begin & $
        bs = sort (data.v(n,*)) & $
        data.v(n,*)=data.v(n,bs) & $
        data.y(n,*)=data.y(n,bs) & $
        endfor
        store_data,var_name, data=data	
        options,var_name,'yminor',9
        options,var_name,'yticks',4
        options,var_name,'ytickv',[-90,0,90,180,270]
        ylim,var_name,-90,270,0

        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

; reset time limits if needed

        IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
        IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L]

        if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
           if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
           if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
           get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
           get_new_igrf,/no_store_old
        endif

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;; loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

; ION ENERGY 

        var_name='Iesa_Energy'
        get_en_spec,'fa_' + ieb_or_ies + '_c',name=var_name, units='eflux',/CALIB,RETRACE=1
        get_data,var_name, data=data
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1	
        ;; zlim,var_name,4,9,0
        ;; zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 5 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 9),0
        ylim,var_name,4,30000,1
        options,var_name,'ytitle','Ions!C!CEnergy (eV)'
        options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,var_name,'x_no_interp',1
        options,var_name,'y_no_interp',1
        options,var_name,'panel_size',2

        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;; loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

     endif


; Step 4 - Eesa data

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
  b = where (strpos(result,'Eesa Survey') ge 0,nesa)
  if (nesa gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nesa = 0

  if (nesa gt 0) then begin

; ELECTRON PITCH ANGLE

     var_name='Eesa_Angle'
     get_pa_spec,'fa_' + eeb_or_ees + '_c',units='eflux',name=var_name, energy=[10.,30000.]
     get_data,var_name, data=data 
     data.y = alog10(data.y)
     store_data,var_name, data=data
     options,var_name,'spec',1
     ;; zlim,var_name,4,9,0
     ;; zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 6 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 10),0
     ylim,var_name,0,360,0
     options,var_name,'ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
     options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     options,var_name,'x_no_interp',1
     options,var_name,'y_no_interp',1
     options,var_name,'panel_size',2

     get_data,var_name, data=data
     bb = where (data.v gt 270.,nb)
     if (nb gt 0) then data.v(bb)=data.v(bb)-360.
     nn = n_elements(data.x)
     for n = 0,nn-1L do begin & $
        bs = sort (data.v(n,*)) & $
        data.v(n,*)=data.v(n,bs) & $
        data.y(n,*)=data.y(n,bs) & $
        endfor
        store_data,var_name, data=data
        options,var_name,'yminor',9
        options,var_name,'yticks',4
        options,var_name,'ytickv',[-90,0,90,180,270]
        ylim,var_name,-90,270,0

        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

; reset time limits if needed

        IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
        IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L]

        if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
           if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
           if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
           get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
           get_new_igrf,/no_store_old
        endif

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;; loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

; ELECTRON ENERGY

        var_name='Eesa_Energy'
        get_en_spec,'fa_' + eeb_or_ees + '_c',name=var_name,units='eflux',/CALIB,RETRACE=1
        get_data,var_name, data=data
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1	
        ;; zlim,var_name,4,9,0
        ;; zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 5 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 9),0
        ylim,var_name,5,30000,1
        options,var_name,'ytitle','Electrons!C!CEnergy (eV)'
        options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,var_name,'x_no_interp',1
        options,var_name,'y_no_interp',1
        options,var_name,'panel_size',2

        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then BEGIN 
           ;; loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

     endif


; Step 5 - VLF data


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
     ;; dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $

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
     
     ;;Plot opts
     VLF_zRange = [MIN(data.y[WHERE(FINITE(data.y))] > (-13)), $
                   MAX(data.y[WHERE(FINITE(data.y))]) < (-5)]
     dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], $
               zrange:VLF_zRange, $
               ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
               ztitle: '(V/m)!U2!N/Hz', panel_size:2}

     STORE_DATA,'DSP_V5-V8', dlimit=dlimit

     OPTIONS,'DSP_V5-V8','x_no_interp',1
     OPTIONS,'DSP_V5-V8','y_no_interp',1


     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['DSP_V5-V8'] else tPlt_vars=['DSP_V5-V8',tPlt_vars]

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        ;; loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  endif else begin

  endelse

; Step 5 - AKR data

  IF ~KEYWORD_SET(GRL) THEN BEGIN
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
     b = where (strpos(result,'SfaAve_V5-V8') ge 0,nakr)
     if (nakr gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nakr = 0

     if (nakr gt 0) then begin

        dat = get_fa_fields('SfaAve_V5-V8', /all)
        data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
        store_data,'SFA_V5-V8', data=data
        ;; dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $

;  look for big jumps in time - blank these

        get_data,'SFA_V5-V8',data=data
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
           store_data,'SFA_V5-V8',data=data
        endif

        ;;Plot opts
        GET_DATA,'DSP_V5-V8',DLIMIT=VLF_dlimit

        ;;Put AKR and VLF data on same scale in z direction
        ;; AKR_zRange = [MIN([data.y[WHERE(FINITE(data.y))],VLF_dLimit.zRange[0]]) > (-15), $
        ;;               MAX([data.y[WHERE(FINITE(data.y))],VLF_dLimit.zRange[1]]) < (-6)]
        AKR_zRange = [MIN(data.y[WHERE(FINITE(data.y))]) > (-14), $
                      MAX(data.y[WHERE(FINITE(data.y))]) < (-6)]
        ;; VLF_dlimit.zRange = AKR_zRange
        ;; STORE_DATA,'DSP_V5-V8',DLIMIT=VLF_dlimit

        dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], $
                  zrange:AKR_zRange, $
                  ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        store_data,'SFA_V5-V8', dlimit=dlimit
        options,'SFA_V5-V8','x_no_interp',1
        options,'SFA_V5-V8','y_no_interp',1




        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['SFA_V5-V8'] else tPlt_vars=['SFA_V5-V8',tPlt_vars]

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;; loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

     ENDIF

  ENDIF
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

;;Include chare panel?

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  IF KEYWORD_SET(add_chare_panel) OR KEYWORD_SET(add_kappa_panel) OR KEYWORD_SET(add_Newell_panel) THEN BEGIN
     eAngle       = [360.-30.,30.]
     iAngle       = [135.,225.]
     eAngleChare  = eAngle
     iAngleChari  = iAngle
     t1eeb = 0.D 
     t2eeb = 0.D
     bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
     bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
     t1eeb = time1 > t1eeb
     t2eeb = time2 < t2eeb

     GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Je',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Jee',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'j_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Ji',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     GET_2DT,'je_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Jei',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     ;;Remove_crap
     GET_DATA,'Je',DATA=tmp
     ;; GET_DATA,'Je',DATA=Je_originalsk
     ;; saveStr+='Je_originalsk,'
     keep1                          = WHERE(FINITE(tmp.y) NE 0)
     keep2                          = WHERE(ABS(tmp.y) GT 0.0)
     GET_DATA,'Jee',DATA=tmp
     ;; GET_DATA,'Jee',DATA=Jee_originalsk
     ;; saveStr+='Jee_originalsk,'
     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
     GET_DATA,'Ji',DATA=tmp
     ;; GET_DATA,'Ji',DATA=Ji_originalsk
     ;; saveStr+='Ji_originalsk,'
     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
     GET_DATA,'Jei',DATA=tmp
     ;; GET_DATA,'Jei',DATA=Jei_originalsk
     ;; saveStr+='Jei_originalsk,'
     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
     GET_DATA,'Je',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     je_tmp_time                    = tmp.x[keep2]
     je_tmp_data                    = tmp.y[keep2]
     STORE_DATA,'Je',DATA={x:je_tmp_time,y:je_tmp_data}
     GET_DATA,'Jee',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     jee_tmp_time                   = tmp.x[keep2]
     jee_tmp_data                   = tmp.y[keep2]
     STORE_DATA,'Jee',DATA={x:jee_tmp_time,y:jee_tmp_data}
     GET_DATA,'Ji',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     ji_tmp_time                    = tmp.x[keep2]
     ji_tmp_data                    = tmp.y[keep2]
     STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
     GET_DATA,'Jei',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     jei_tmp_time                   = tmp.x[keep2]
     jei_tmp_data                   = tmp.y[keep2]
     STORE_DATA,'Jei',DATA={x:jei_tmp_time,y:jei_tmp_data}

     GET_DATA,'Je',DATA=Je
     GET_DATA,'Jee',DATA=Jee
     GET_DATA,'Ji',DATA=Ji
     GET_DATA,'Jei',DATA=Jei
  ENDIF

  IF KEYWORD_SET(add_chare_panel) THEN BEGIN
     chare            = Jee.y/Je.y*6.242*1.0e11
     chari            = Jei.y/Ji.y*6.242*1.0e11
     charEBounds      = [MIN(chare[WHERE(chare GT 0)]) + MIN(chari[WHERE(chari GT 0)]), $
                         MAX(chare[WHERE(chare GT 0)]) + MAX(chari[WHERE(chari GT 0)])]
     ;; showLog_charE    = (ALOG10(MAX(chare[WHERE(chare GT 0)]))-ALOG10(MIN(chare[WHERE(chare GT 0)]))) GT 2
     ;; showLog_charE    = (ALOG10(charEBounds[1])-ALOG10(charEBounds[0])) GT 2
     showLog_charE    = 1B
     IF showLog_charE THEN BEGIN
        charEBounds[0] -= (charEBounds[0]*0.1)
        charEBounds[1] += (charEBounds[1]*0.1)
     ENDIF ELSE BEGIN
        charEBounds[0] /= 1.1
        charEBounds[1] *= 1.1
     ENDELSE
     chari_interp = DATA_CUT({x:Jei.x,y:chari},Jee.x,/IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1}, $
     ;;                   {time:Jei.x,comp1:chari,ncomp:1}, $
     ;;                   RESULT=chari_interp, $
     ;;                   /INTERP, $
     ;;                   DELT_T=50., $
     ;;                   /TALK
     ;; chari_interp  = {x:Jee.x,y:chari_interp}
     chartot          = chare+chari_interp
     STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari_interp],[chare],[chartot]]}

     red              = (ctNum EQ 43) ? 235 : 250
     green            = 130
     blue             = 90
     maxwell          = 50
     black            = 10

     YLIM,'charepanel',charEBounds[0],charEBounds[1],showLog_charE
     OPTIONS,'charepanel','tplot_routine','mplot'
     OPTIONS,'charepanel','ytitle','E/q Volts'
     OPTIONS,'charepanel','labels',['Ion','Electron','Total']
     OPTIONS,'charepanel','colors',[red,green,20]
     OPTIONS,'charepanel','labflag',-1
     ;; OPTIONS,'charepanel','yticks',5                                   ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['charepanel'] else tPlt_vars=['charepanel',tPlt_vars]

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        ;; loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  ENDIF

;;Include kappa panel?
  IF KEYWORD_SET(add_kappa_panel) THEN BEGIN

     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN STOP

     kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                                         CHI2_THRESHOLD=chi2_thresh, $
                                                         CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                                         HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                         LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                                         KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                         KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                                         DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                                         N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                                         /DESTROY_INFO_LIST, $
                                                         OUT_GOOD_I=includeK_i, $
                                                         OUT_GOOD_T=includeK_t, $
                                                         OUT_BAD_I=excludeK_i, $
                                                         OUT_BAD_T=excludeK_t)

     gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                                         CHI2_THRESHOLD=chi2_thresh, $
                                                         CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                                         HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                         LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                                         KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                         KAPPA_HIGHTHRESHOLD=100.1, $
                                                         DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                                         N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                                         /DESTROY_INFO_LIST, $
                                                         OUT_GOOD_I=includeG_i, $
                                                         OUT_GOOD_T=includeG_t, $
                                                         OUT_BAD_I=excludeG_i, $
                                                         OUT_BAD_T=excludeG_t)

     PRINT,"This has not been update to use 2D fit parameters (e.g., search for Astruct and see what turns up in this pro)!"
     PRINT,"Gotta fix it"
     STOP
     ;; PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
     ;;                         A=a, $
     ;;                         STRUCT_A=Astruct, $
     ;;                         TIME=kappaTime, $
     ;;                         MATCH_TIMES=kappa2D.SDT[*].time, $
     ;;                         NAMES_A=A_names, $
     ;;                         CHI2=chi2, $
     ;;                         PVAL=pVal, $
     ;;                         FITSTATUS=kappaFitStatus, $
     ;;                         /USE_MPFIT1D

     ;; PARSE_KAPPA_FIT_STRUCTS,gaussFit1Ds, $
     ;;                         A=AGauss, $
     ;;                         STRUCT_A=AStructGauss, $
     ;;                         TIME=GaussTime, $
     ;;                         MATCH_TIMES=kappa2D.SDT[*].time, $
     ;;                         NAMES_A=AGauss_names, $
     ;;                         CHI2=chi2Gauss, $
     ;;                         PVAL=pValGauss, $
     ;;                         FITSTATUS=gaussFit1DStatus, $
     ;;                         /USE_MPFIT1D

     ;; IF ~ARRAY_EQUAL(kappaTime,GaussTime) THEN STOP
     ;; nFits           = N_ELEMENTS(kappa2D.fitMoms.scDens)
     ;; badFits_i       = WHERE(kappaFitStatus NE 0,nBadFits)
     ;; badGaussFit1Ds_i  = WHERE(gaussFit1DStatus NE 0,nBadGaussFit1Ds)
     ;; bothBad_i       = ( (badFits_i[0] EQ -1) AND (badGaussFit1Ds_i[0] EQ -1 ) ) ? !NULL : $
     ;;                   CGSETINTERSECTION(badFits_i,badGaussFit1Ds_i)
     ;; PRINT,""
     ;; PRINT,"****************************************"
     ;; PRINT,'NTotalFits    : ',nFits
     ;; PRINT,''
     ;; PRINT,"NbadFits      : ",nBadFits
     ;; PRINT,"NbadGaussFit1Ds : ",nBadGaussFit1Ds
     ;; PRINT,"NBothBad      : ",N_ELEMENTS(bothBad_i)

     ;; STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
     STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:REFORM(kappa2D.fitParams[2,*])}
     CASE 1 OF
        KEYWORD_SET(log_kappaPlot): BEGIN
           YLIM,'kappa_fit',1.0,100,1
        END
        ELSE: BEGIN
           YLIM,'kappa_fit',1.0,11,0
        END
     ENDCASE
     OPTIONS,'kappa_fit','ytitle',"Kappa!CFit Val"
     OPTIONS,'kappa_fit','psym',2 ;Asterisk


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Four-current panel (it's like four-cheese pizza)

     ;;Get mag current
     get_data,'dB_fac_v',data=db_fac

     jMag                         = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                                             db_fac,vel, $
                                                             /USE_DESPUN, $
                                                             SDTNAME__JMAG=jMagName, $
                                                             ;; INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                                             ;; SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                                             QUIET=quiet)

     ;;Get electron ESA current, ion ESA current
     Je_current                   = (-1.)*Je.y*1.6e-9 ;;in microA/m2
     Ji_current                   =       Ji.y*1.6e-9 ;;in microA/m2

     ;;Get Kappa-predicted current
     kappaStr                       = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
     
     ;;Align to kappa fits
     Je_kappa_interp       = DATA_CUT({x:Je.x,y:Je_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     Ji_kappa_interp       = DATA_CUT({x:Ji.x,y:Ji_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     Jtot_kappa_interp     = DATA_CUT({x:Je.x,y:Je_current+Ji_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chare_kappa_interp    = DATA_CUT({x:Jee.x,y:chare}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chari_kappa_interp    = DATA_CUT({x:Jee.x,y:chari_interp}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chartot_kappa_interp  = DATA_CUT({x:Jee.x,y:chartot}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     jMag_kappa_interp     = DATA_CUT({x:JMag.x,y:jMag.y}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current,ncomp:1}, $
     ;;                   RESULT=Je_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Ji.x,comp1:Ji_current,ncomp:1}, $
     ;;                   RESULT=Ji_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current+Ji_current,ncomp:1}, $
     ;;                   RESULT=Jtot_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chare,ncomp:1}, $
     ;;                   RESULT=chare_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chari_interp,ncomp:1}, $
     ;;                   RESULT=chari_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1}, $
     ;;                   RESULT=chartot_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:jMag.x,comp1:jMag.y,ncomp:1}, $
     ;;                   RESULT=jMag_kappa_interp,/INTERP,DELT_T=50.,/TALK
     
     ;;Align to Je
     Ji_interp   = DATA_CUT({x:Ji.x,y:Ji_current},Je.x, $
                            /IGNORE_NAN,GAP_DIST=3)
     Jmag_interp = DATA_CUT({x:JMag.x,y:jMag.y},Je.x, $
                            /IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1}, $
     ;;                   {time:Ji.x,comp1:Ji_current,ncomp:1}, $
     ;;                   RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:jMag.x,comp1:jMag.y,ncomp:1}, $
     ;;                   RESULT=jMag_interp,/INTERP,DELT_T=50.,/TALK
     
     ;; Jtot_interp = {x:Je.x,y:Je_current+Ji_interp}

     STORE_DATA,'Je',DATA=Je


     setup = {kappaS:Astruct, $
              gaussS:AStructGauss, $
              charE:charE_kappa_interp, $
              charI:charI_kappa_interp, $
              charTot:charTot_kappa_interp, $
              Jtot:Jtot_kappa_interp, $
              JMag:jMag_kappa_interp, $
              Je:Je_kappa_interp, $
              Ji:Ji_kappa_interp}

     SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                                 obs_current,obsName,obsSuff, $
                                 kappaPot,gaussPot, $
                                 potName,potTitleStr, $
                                 USE_JE_CURRENT=use_je_current, $
                                 USE_JMAG_CURRENT=use_jMag_current, $
                                 ;; /BOTH_USE_KAPPA_BULKENERGY, $
                                 ;; /BOTH_USE_MAXWELL_BULKENERGY, $
                                 BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkEnergy, $
                                 BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkEnergy, $
                                 NO_CHARI_FOR_POT=no_charI_for_pot

     kappaDens = KAPPA__SELECT_2DFIT_DENS(kappa2D, $
                                          USE_DATA_DENS=use_data_dens, $
                                          CALC_FITDENS_OVER_ELECTRON_ARANGE=calc_fitDens__aRange, $
                                          ELECTRON_ANGLERANGE=(N_ELEMENTS(calc_fitDens__aRange) EQ 2 ? calc_fitDens__aRange : eAngleChare), $
                                          ;; ELECTRON_ANGLERANGE=eAngleChare, $
                                          FITTYPE__STRING='Kappa')

     gaussDens = KAPPA__SELECT_2DFIT_DENS(gauss2D, $
                                          USE_DATA_DENS=use_data_dens, $
                                          CALC_FITDENS_OVER_ELECTRON_ARANGE=calc_fitDens__aRange, $
                                          ELECTRON_ANGLERANGE=(N_ELEMENTS(calc_fitDens__aRange) EQ 2 ? calc_fitDens__aRange : eAngleChare), $
                                          ;; ELECTRON_ANGLERANGE=eAngleChare, $
                                          FITTYPE__STRING='Maxwell')

     CASE 1 OF
        KEYWORD_SET(SDT_calc__no_model): BEGIN
           GET_2DFIT_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
                                                  kappa_current,gauss_current, $
                                                  ENERGY_ELECTRONS=energy_electrons, $
                                                  ANGLE=eAngleCharE
        END
        ELSE: BEGIN
           GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                            kappaPot,gaussPot,R_B, $
                                            kappa_current,gauss_current,obs_current, $
                                            DENSITY_KAPPA2D=kappaDens, $
                                            DENSITY_GAUSS2D=gaussDens ;, $
        END
     ENDCASE

     
     STORE_DATA,'onecheese',DATA={x:kappaTime, $

                                  y:obs_current}
     STORE_DATA,'fourcheese',DATA={x:jMag.x, $

                                   y:jMag.y}
     ;; STORE_DATA,'toppings',DATA={x:[[kappaStr.time],[kappaStr.time]], $
     ;;                             y:[[gauss_current],[kappa_current]]}
     STORE_DATA,'toppings',DATA={x:kappaStr.time, $
                                 y:kappa_current}
     STORE_DATA,'feta',DATA={x:kappaStr.time, $
                             y:gauss_current}

     
     oneCheeseBounds   = [MIN(obs_current) < MIN(gauss_current) < MIN(kappa_current), $
                        MAX(obs_current) > MAX(gauss_current) > MAX(kappa_current)]
     IF oneCheeseBounds[0] LT 0 THEN BEGIN
        showLog_oneCheese   = 0 
        oneCheeseBounds[0] /= 1.1
        oneCheeseBounds[1] *= 1.1
     ENDIF ELSE BEGIN
        showLog_oneCheese   = (ALOG10(oneCheeseBounds[1])-ALOG10(oneCheeseBounds[0])) GT 2
        oneCheeseBounds[0] -= (oneCheeseBounds[0]*0.1)
        oneCheeseBounds[1] += (oneCheeseBounds[1]*0.1)
     ENDELSE
     OPTIONS,'onecheese','colors',green
     OPTIONS,'onecheese','tplot_routine','mplot'
     OPTIONS,'onecheese','ytitle','Current!C('+CGGREEK('mu')+'A/m!U2!Ns)'
     YLIM,   'onecheese',oneCheeseBounds[0],oneCheeseBounds[1],showLog_oneCheese
     oneCheesePos = (INDGEN(4)+1)/5.

     OPTIONS,'onecheese','labels',obsName
     ;; OPTIONS,'onecheese','labflag',-1
     OPTIONS,'onecheese','labflag',3
     OPTIONS,'onecheese','labpos',oneCheesePos[0]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'onecheese','labpos',-0.25
     ;; OPTIONS,'onecheese','yticks',2                    ; set y-axis labels
     ;; OPTIONS,'onecheese','ytickname',['-2.0','-1.0','0'] ; set y-axis labels
     ;; OPTIONS,'onecheese','ytickv',[-2.0,-1.0,0.0]        ; set y-axis labels

     OPTIONS,'fourcheese','colors',red
     OPTIONS,'fourcheese','labels','Fluxgate mag'
     ;; OPTIONS,'fourcheese','labflag',-1
     OPTIONS,'fourcheese','labflag',3
     OPTIONS,'fourcheese','labpos',oneCheesePos[1]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'fourcheese','labpos',-0.75

     ;; OPTIONS,'toppings','labels' ,['Maxwellian Model','Kappa model']
     ;; OPTIONS,'toppings','psym'   ,1
     ;; OPTIONS,'toppings','colors' ,[20,blue]
     ;; ;; OPTIONS,'toppings','labflag',-1
     ;; OPTIONS,'toppings','labflag',3
     ;; OPTIONS,'toppings','labpos',[oneCheesePos[2],oneCheesePos[3]]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'toppings','labpos',[-1.75,-1.25]

     OPTIONS,'toppings','labels' ,'Kappa model'
     OPTIONS,'toppings','psym'   ,1
     OPTIONS,'toppings','colors' ,blue
     ;; OPTIONS,'toppings','labflag',-1
     OPTIONS,'toppings','labflag',3
     OPTIONS,'toppings','labpos',oneCheesePos[3]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'toppings','labpos',[-1.75,-1.25]

     OPTIONS,'feta','labels' ,'Maxwellian Model'
     OPTIONS,'feta','psym'   ,1
     OPTIONS,'feta','colors' ,maxwell
     ;; OPTIONS,'feta','labflag',-1
     OPTIONS,'feta','labflag',3
     OPTIONS,'feta','labpos',oneCheesePos[2]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'feta','labpos',[-1.75,-1.25]

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['onecheese','kappa_fit'] else tPlt_vars=['onecheese','kappa_fit',tPlt_vars]

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        ;; loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'   ;,PSYM=1
     endif

  ENDIF

  IF KEYWORD_SET(add_Newell_panel) THEN BEGIN

     var_name='Eesa_LC_Energy'
     ;;This already gets called above, but we need to call it again to handle angle restrictions
     GET_EN_SPEC,'fa_' + eeb_or_ees + '_c',name=var_name,units='eflux',/CALIB,RETRACE=1,ANGLE=eAngle
     GET_DATA,var_name,DATA=data

     GET_FA_ORBIT,data.x,/TIME_ARRAY
     GET_DATA,'MLT',DATA=mlt
     mlt       = mlt.y

     GET_DATA,'ILAT',DATA=ilat
     ilat      = ilat.y

     GET_DATA,'ALT',DATA=alt
     alt      = alt.y

     GET_DATA,'ORBIT',DATA=orbit
     orbit          = orbit.y
     sc_pot         = GET_FA_POTENTIAL(t1,t2, $
                                ;; /SPIN, $
                                /REPAIR)
     sc_pot_interp  = DATA_CUT({x:sc_pot.time,y:sc_pot.comp1},data.x) 
     this           = VALUE_CLOSEST2(data.x,jee.x) 
     data           = {x:data.x[this],y:data.y[this,*],v:data.v[this,*]}
     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,data,Jee,Je,mlt,ilat,alt,orbit,events,SC_POT=sc_pot_interp

     var_name = 'newellPanel'
     PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,events,TPLOT_NAME=var_name, $
                                               /NO_STRICT_TYPES, $
                                               CONVERT_TO_NEWELL_INTERP=Newell_2009_interp

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        ;; loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  ENDIF

; force tPlt_vars to be all the panels unless no_blank_panels is set

  if (not keyword_set(no_blank_panels)) then begin


; SFA

     bdat = where(tPlt_vars eq 'SFA_V5-V8',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = [-112.700,12.5031,997.434,2015.75]
        store_data,'SFA_V5-V8', data={x:t_arr, y:y_arr, v:v_arr}
        dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $
                  ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        store_data,'SFA_V5-V8', dlimit=dlimit
        options,'SFA_V5-V8','x_no_interp',1
        options,'SFA_V5-V8','y_no_interp',1
     endif

; DSP

     bdat = where(tPlt_vars eq 'DSP_V5-V8',ndat)
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

; Eesa_Energy

     bdat = where(tPlt_vars eq 'Eesa_Energy',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [34119.7,26091.5,50.9600,5.88000]
        store_data,'Eesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
        options,'Eesa_Energy','spec',1	
        zlim,'Eesa_Energy',4,9,0
        ylim,'Eesa_Energy',5,30000,1
        options,'Eesa_Energy','ytitle','Electrons!C!CEnergy (eV)'
        options,'Eesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,'Eesa_Energy','x_no_interp',1
        options,'Eesa_Energy','y_no_interp',1
        options,'Eesa_Energy','panel_size',2
     endif

; Eesa_Angle

     bdat = where(tPlt_vars eq 'Eesa_Angle',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        store_data,'Eesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
        options,'Eesa_Angle','spec',1	
        zlim,'Eesa_Angle',4,9,0
        ylim,'Eesa_Angle',-90,270,0
        options,'Eesa_Angle','ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
        options,'Eesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,'Eesa_Angle','x_no_interp',1
        options,'Eesa_Angle','y_no_interp',1
        options,'Eesa_Angle','panel_size',2
        options,'Eesa_Angle','yminor',9
        options,'Eesa_Angle','yticks',4
        options,'Eesa_Angle','ytickv',[-90,0,90,180,270]
     endif

; Iesa_Energy

     bdat = where(tPlt_vars eq 'Iesa_Energy',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [26808.3,11827.2,27.7200,4.62000]
        store_data,'Iesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
        options,'Iesa_Energy','spec',1	
        zlim,'Iesa_Energy',4,9,0
        ylim,'Iesa_Energy',4,30000,1
        options,'Iesa_Energy','ytitle','Ions!C!CEnergy (eV)'
        options,'Iesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,'Iesa_Energy','x_no_interp',1
        options,'Iesa_Energy','y_no_interp',1
        options,'Iesa_Energy','panel_size',2
     endif

; Iesa_Angle

     bdat = where(tPlt_vars eq 'Iesa_Angle',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        store_data,'Iesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
        options,'Iesa_Angle','spec',1	
        zlim,'Iesa_Angle',4,9,0
        ylim,'Iesa_Angle',-90,270,0
        options,'Iesa_Angle','ytitle','Ions!C!CAngle (Deg.)'
        options,'Iesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,'Iesa_Angle','x_no_interp',1
        options,'Iesa_Angle','y_no_interp',1
        options,'Iesa_Angle','panel_size',2
        options,'Iesa_Angle','yminor',9
        options,'Iesa_Angle','yticks',4
        options,'Iesa_Angle','ytickv',[-90,0,90,180,270]
     endif

; EFIT_ALONG_VSC

     bdat = where(tPlt_vars eq 'EFIT_ALONG_VSC',ndat)
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

     bdat = where(tPlt_vars eq 'dB_fac_v',ndat)
     if (ndat eq 0) then begin
        t_arr = tlimit_all
        y_arr = dblarr(2,3)
        y_arr[*,*] = !values.d_nan
        store_data,'dB_fac_v', data={x:t_arr, y:y_arr}
        options,'dB_fac_v','yrange',[-100,100]
        options,'dB_fac_v','ytitle','dB_fac_v!C!C(nT))'
        options,'dB_fac_v','panel_size',2
        options,'dB_fac_v','colors',[6,4,2]
        options,'dB_fac_v','labels',['v ((BxV)xB)','p (BxV)','b']
     endif

     tPlt_vars=['Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','SFA_V5-V8','DSP_V5-V8','EFIT_ALONG_VSC','dB_fac_v']

     IF KEYWORD_SET(add_chare_panel)  THEN tPlt_vars = [tPlt_vars[0:3],'charepanel',tPlt_vars[4:7]]

     IF KEYWORD_SET(add_kappa_panel)  THEN tPlt_vars = ['onecheese','kappa_fit',tPlt_vars]

     IF KEYWORD_SET(add_Newell_panel) THEN tPlt_vars = ['newellPanel',tPlt_vars]

  endif

  ;;Help db_fac_v
  GET_DATA,'dB_fac_v',DATA=dat
  dat.y[*,0] = dat.y[*,0] - (dat.y[0,0]-dat.y[0,2])
  dat.y[*,1] = dat.y[*,1] - (dat.y[0,1]-dat.y[0,2])
  STORE_DATA,'dB_fac_v',DATA=dat

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     ;; loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'   ;,PSYM=1
     ENDIF
  endif

  IF keyword_set(save_ps) THEN BEGIN

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

     ;; LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'   ;,PSYM=1
     ENDIF

     CASE 1 OF
        KEYWORD_SET(save_png): BEGIN
           CGPS_CLOSE
        END
        KEYWORD_SET(save_ps): BEGIN
           PCLOSE
        END
        ELSE:
     ENDCASE
  ENDIF

  RETURN
END

