PRO SINGLE_KAPPA_SUMMARY,time1,time2, $
                   TPLOT_VARS=tplot_vars, $
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
                   LOG_KAPPAPLOT=log_kappaPlot, $
                   FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                   FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                   KAPPAFITS=kappaFits, $
                   GAUSSFITS=gaussFits, $
                   CHI2_THRESHOLD=chi2_thresh, $
                   CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                   LOWDENSITY_THRESHOLD=lowDens_thresh, $
                   DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                   N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                   SAVE_PS=save_ps, $
                   SAVE_PNG=save_png, $
                   SAVEKAPPA_BONUSPREF=bonusPref, $
                   PLOTDIR=plotDir

  ;;Some defaults
  red              = 250
  green            = 130
  blue             = 90
  maxwell          = 50
  black            = 10

  kappaColor = blue
  kappaSym   = 2                ;Asterisk

  GaussColor = red
  GaussSym   = 1                ;Cross


; create a summary plot of:
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; dB_fac_v (dB_fac and dB_SM also stored)

; Returns:
; tplot_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data

; Program will use fac_v if E field data are available, other use fac_v
; over-ride with use_fac_v and use_fac keywords

; if no_blank_panels is not set procedure will generate tplot data for all the parameters,
; including missing data, for a uniform plot product

; Step 0 - safety measure - delete all tplot quantities if found

@tplot_com

  IF NOT KEYWORD_SET(energy_ions) THEN energy_ions=[4,1.e4]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN ieb_or_ies = 'ieb' ELSE ieb_or_ies = 'ies'

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

     orbString           = STRING(FORMAT='(I0)',orbit)

;;Handle PNGness or PSness before kicking things off

     IF keyword_set(save_ps) THEN BEGIN

        outPlotName  = 'Kappa_summary'
        outPlotName += '--' + orbString + (KEYWORD_SET(bonusPref) ? bonusPref : '' )


        t1S = STRMID(TIME_TO_STR(time1,/MSEC),11,11)
        t2S = STRMID(TIME_TO_STR(time2,/MSEC),11,11)

        t1S = t1S.REPLACE(':', '_')
        t1S = t1S.REPLACE('.', '__')
        
        t2S = t2S.REPLACE(':', '_')
        t2S = t2S.REPLACE('.', '__')
        
        outPlotName += '--' + t1S + '_-_' + t2S


        IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Kappa_summaries'
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN
              POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO,FONT_SIZE=8

           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE

     ENDIF

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
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0] ELSE t1 = time1
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L] ELSE t2 = time2
     tlimit_all = [t1,t2]
     ;; tplot_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     ;; if (keyword_set(use_fac)) then tplot_vars = 'dB_fac'

     ;; if (not keyword_set(no_blank_panels)) then tplot_vars = 'dB_fac_v'

     ;; if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     ;;    wInd = 0
     ;;    WINDOW,wInd,XSIZE=700,YSIZE=900
     ;;    ;; tplot_options,'region',[0.,0.5,1.0,1.0]
     ;;    loadct2,39
     ;;    tplot,tplot_vars,var=['ALT','ILAT','MLT'], $
     ;;          WINDOW=wInd, $
     ;;          TRANGE=[t1,t2]
     ;; endif

  endif


; Step 3 - Iesa data
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
     zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
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

        IF KEYWORD_SET(include_ion_plots) THEN BEGIN
           if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[tplot_vars,var_name]

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
              wInd = 0
              WINDOW,wInd,XSIZE=700,YSIZE=900
              ;; tplot_options,'region',[0.,0.5,1.0,1.0]
              loadct2,39
              tplot,tplot_vars,var=['ALT','ILAT','MLT'], $
                    WINDOW=wInd, $
                    TRANGE=[t1,t2]
           endif
           ;; if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;;    loadct2,40
           ;;    tplot,tplot_vars,var=['ALT','ILAT','MLT']
           ;; endif

; ION ENERGY 

           var_name='Iesa_Energy'
           get_en_spec,'fa_' + ieb_or_ies + '_c',name=var_name, units='eflux',/CALIB,RETRACE=1
           get_data,var_name, data=data
           data.y = alog10(data.y)
           store_data,var_name, data=data
           options,var_name,'spec',1	
           ;; zlim,var_name,4,9,0
           zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
           ylim,var_name,4,30000,1
           options,var_name,'ytitle','Ions!C!CEnergy (eV)'
           options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
           options,var_name,'x_no_interp',1
           options,var_name,'y_no_interp',1
           options,var_name,'panel_size',2

           if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[tplot_vars,var_name]

           if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
              loadct2,40
              tplot,tplot_vars,var=['ALT','ILAT','MLT']
           endif

        ENDIF
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

     IF KEYWORD_SET(include_electron_pa_spec) THEN BEGIN
        var_name='Eesa_Angle'
        get_pa_spec,'fa_' + eeb_or_ees + '_c',units='eflux',name=var_name, energy=[10.,30000.]
        get_data,var_name, data=data 
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1
        ;; zlim,var_name,4,9,0
        zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
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

           if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[tplot_vars,var_name]

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
              loadct2,40
              tplot,tplot_vars,var=['ALT','ILAT','MLT']
           endif
        ENDIF

; ELECTRON ENERGY

        var_name='Eesa_Energy'
        get_en_spec,'fa_' + eeb_or_ees + '_c',name=var_name,units='eflux',/CALIB,RETRACE=1
        get_data,var_name, data=data
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1	
        ;; zlim,var_name,4,9,0
        zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
        ylim,var_name,5,30000,1
        options,var_name,'ytitle','Electrons!C!CEnergy (eV)'
        options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,var_name,'x_no_interp',1
        options,var_name,'y_no_interp',1
        options,var_name,'panel_size',2

        if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[tplot_vars,var_name]

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then BEGIN 
           loadct2,40
           tplot,tplot_vars,var=['ALT','ILAT','MLT']
        endif

     endif

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
  ;; IF KEYWORD_SET(add_chare_panel) OR KEYWORD_SET(add_kappa_panel) OR KEYWORD_SET(add_Newell_panel) THEN BEGIN
  eAngle       = [360.-30.,30.]
  iAngle       = [135.,225.]
  eAngleChare  = eAngle
  iAngleChari  = iAngle
  t1eeb = 0.D 
  t2eeb = 0.D
  bro   = GET_FA_EEB(t1eeb,/ST)
  bro   = GET_FA_EEB(t2eeb,/EN)
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
  ;; ENDIF

  chare            = Jee.y/Je.y*6.242*1.0e11
  chari            = Jei.y/Ji.y*6.242*1.0e11
  FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1}, $
                    {time:Jei.x,comp1:chari,ncomp:1}, $
                    RESULT=chari_interp, $
                    /INTERP, $
                    DELT_T=50., $
                    /TALK
  ;; chari_interp  = {x:Jee.x,y:chari_interp}
  chartot          = chare+chari_interp

  IF KEYWORD_SET(add_chare_panel) THEN BEGIN
     charEBounds      = [MIN(chare[WHERE(chare GT 0)]) + MIN(chari[WHERE(chari GT 0)]), $
                         MAX(chare[WHERE(chare GT 0)]) + MAX(chari[WHERE(chari GT 0)])]
     ;; showLog_charE    = (ALOG10(MAX(chare[WHERE(chare GT 0)]))-ALOG10(MIN(chare[WHERE(chare GT 0)]))) GT 2
     showLog_charE    = (ALOG10(charEBounds[1])-ALOG10(charEBounds[0])) GT 2
     IF showLog_charE THEN BEGIN
        charEBounds[0] -= (charEBounds[0]*0.1)
        charEBounds[1] += (charEBounds[1]*0.1)
     ENDIF ELSE BEGIN
        charEBounds[0] /= 1.1
        charEBounds[1] *= 1.1
     ENDELSE


     STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari_interp],[chare],[chartot]]}

     YLIM,'charepanel',charEBounds[0],charEBounds[1],showLog_charE
     OPTIONS,'charepanel','tplot_routine','mplot'
     OPTIONS,'charepanel','ytitle','E/q Volts'
     OPTIONS,'charepanel','labels',['Ion','Electron','Total']
     OPTIONS,'charepanel','colors',[red,green,20]
     OPTIONS,'charepanel','labflag',-1
     ;; OPTIONS,'charepanel','yticks',5                                   ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

     if (n_elements(tplot_vars) eq 0) then tplot_vars=['charepanel'] else tplot_vars=[tplot_vars,'charepanel']

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        loadct2,40
        tplot,tplot_vars,var=['ALT','ILAT','MLT']
     endif

  ENDIF

;;Include kappa panel?
  ;; IF KEYWORD_SET(add_kappa_panel) THEN BEGIN

  IF N_ELEMENTS(kappaFits) NE N_ELEMENTS(gaussFits) THEN STOP

  ;;Set these to 1 to pull them out of the following routines
  k2DParms           = 1
  g2DParms           = 1

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
                                                      OUT_FITPARAM_STRUCT=k2DParms, $
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
                                                      OUT_FITPARAM_STRUCT=g2DParms, $
                                                      OUT_GOOD_I=includeG_i, $
                                                      OUT_GOOD_T=includeG_t, $
                                                      OUT_BAD_I=excludeG_i, $
                                                      OUT_BAD_T=excludeG_t)

  PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                          A=a, $
                          STRUCT_A=Astruct, $
                          TIME=kappaTime, $
                          MATCH_TIMES=kappa2D.SDT[*].time, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus, $
                          /USE_MPFIT1D

  PARSE_KAPPA_FIT_STRUCTS,gaussFits, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=GaussTime, $
                          MATCH_TIMES=kappa2D.SDT[*].time, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus, $
                          /USE_MPFIT1D

  IF ~ARRAY_EQUAL(kappaTime,GaussTime) THEN STOP
  nFits           = N_ELEMENTS(kappa2D.fitDens)
  badFits_i       = WHERE(fitStatus NE 0,nBadFits)
  badGaussFits_i  = WHERE(gaussFitStatus NE 0,nBadGaussFits)
  bothBad_i       = ( (badFits_i[0] EQ -1) AND (badGaussFits_i[0] EQ -1 ) ) ? !NULL : $
                    CGSETINTERSECTION(badFits_i,badGaussFits_i)
  PRINT,""
  PRINT,"****************************************"
  PRINT,'NTotalFits    : ',nFits
  PRINT,''
  PRINT,"NbadFits      : ",nBadFits
  PRINT,"NbadGaussFits : ",nBadGaussFits
  PRINT,"NBothBad      : ",N_ELEMENTS(bothBad_i)

  ;; STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
  STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:REFORM(kappa2D.fitParams[2,*])}
  kappaBounds      = [MIN(k2DParms.kappa), $
                     MAX(k2DParms.kappa)]
  showLog_kappa    = (ALOG10(kappaBounds[1])-ALOG10(kappaBounds[0])) GT 1

  CASE 1 OF
     KEYWORD_SET(showLog_kappa): BEGIN
        YLIM,'kappa_fit',1.0,100,1
     END
     ELSE: BEGIN
        YLIM,'kappa_fit',1.0,11,0
     END
  ENDCASE
  OPTIONS,'kappa_fit','ytitle',"Kappa"
  OPTIONS,'kappa_fit','psym',kappaSym  

  ;;And a line to show where the awesome kappa vals are
  STORE_DATA,'kappa_critisk',DATA={x:kappaTime,y:MAKE_ARRAY(N_ELEMENTS(kappaTime),VALUE=2.5)}
  OPTIONS,'kappa_critisk','colors',red

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['kappa_fit'] else tplot_vars=[tplot_vars,'kappa_fit']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
  endif

  showLog_Temp = 1
  showLog_Dens = 1
  showLog_BlkE = 1
  showLog_chi2 = 1
  ;;Now normalized Chi^2
  STORE_DATA,'chi22DK',DATA={x:kappaTime,y:kappa2D.chi2/(kappa2D.dof+kappa2D.nFree)}
  OPTIONS,'chi22DK','psym',kappaSym
  OPTIONS,'chi22DK','colors',kappaColor

  STORE_DATA,'chi22DG',DATA={x:GaussTime,y:gauss2D.chi2/(gauss2D.dof+gauss2D.nFree)}
  OPTIONS,'chi22DG','psym',GaussSym
  OPTIONS,'chi22DG','colors',GaussColor

  OPTIONS,'chi22DK','ytitle',CGGREEK('chi',PS=KEYWORD_SET(save_ps))+'!X!U2!N!Dred!N'
  chi2Bounds      = [MIN([kappa2D.chi2/(kappa2D.dof+kappa2D.nFree),gauss2D.chi2/(gauss2D.dof+gauss2D.nFree)]), $
                     MAX([kappa2D.chi2/(kappa2D.dof+kappa2D.nFree),gauss2D.chi2/(gauss2D.dof+gauss2D.nFree)])]
  ;; showLog_chi2    = (ALOG10(chi2Bounds[1])-ALOG10(chi2Bounds[0])) GT 2
  IF showLog_chi2 THEN BEGIN
     chi2Bounds[0] -= (chi2Bounds[0]*0.1)
     chi2Bounds[1] += (chi2Bounds[1]*0.1)
  ENDIF ELSE BEGIN
     chi2Bounds[0] /= 1.1
     chi2Bounds[1] *= 1.1
  ENDELSE
  YLIM,'chi22DK',chi2Bounds[0],chi2Bounds[1],showLog_chi2

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['chi22DK'] else tplot_vars=[tplot_vars,'chi22DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG'  ;,PSYM='*'
  endif

  ;;Now temperature
  STORE_DATA,'Temp2DK',DATA={x:kappaTime,y:k2DParms.temperature}
  OPTIONS,'Temp2DK','psym',kappaSym
  OPTIONS,'Temp2DK','colors',kappaColor

  STORE_DATA,'Temp2DG',DATA={x:GaussTime,y:g2DParms.temperature}
  OPTIONS,'Temp2DG','psym',GaussSym
  OPTIONS,'Temp2DG','colors',GaussColor

  OPTIONS,'Temp2DK','ytitle','Temperature!C(eV)'
  TempBounds      = [MIN([k2DParms.temperature,g2DParms.temperature]), $
                     MAX([k2DParms.temperature,g2DParms.temperature])]
  showLog_Temp    = (ALOG10(TempBounds[1])-ALOG10(TempBounds[0])) GT 2
     IF showLog_Temp THEN BEGIN
        TempBounds[0] -= (TempBounds[0]*0.1)
        TempBounds[1] += (TempBounds[1]*0.1)
     ENDIF ELSE BEGIN
        TempBounds[0] /= 1.1
        TempBounds[1] *= 1.1
     ENDELSE
  YLIM,'Temp2DK',TempBounds[0],TempBounds[1],showLog_Temp

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['Temp2DK'] else tplot_vars=[tplot_vars,'Temp2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
  endif

  ;;Now density
  STORE_DATA,'Dens2DK',DATA={x:kappaTime,y:k2DParms.N}
  OPTIONS,'Dens2DK','psym',kappaSym
  OPTIONS,'Dens2DK','colors',kappaColor

  STORE_DATA,'Dens2DG',DATA={x:GaussTime,y:g2DParms.N}
  OPTIONS,'Dens2DG','psym',GaussSym
  OPTIONS,'Dens2DG','colors',GaussColor

  OPTIONS,'Dens2DK','ytitle','Density!C(cm!U-3!N)'
  DensBounds      = [MIN([k2DParms.N,g2DParms.N]), $
                     MAX([k2DParms.N,g2DParms.N])]
  ;; showLog_Dens    = (ALOG10(DensBounds[1])-ALOG10(DensBounds[0])) GT 2
  IF showLog_Dens THEN BEGIN
     DensBounds[0] -= (DensBounds[0]*0.1)
     DensBounds[1] += (DensBounds[1]*0.1)
  ENDIF ELSE BEGIN
     DensBounds[0] /= 1.1
     DensBounds[1] *= 1.1
  ENDELSE
  YLIM,'Dens2DK',DensBounds[0],DensBounds[1],showLog_Dens

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['Dens2DK'] else tplot_vars=[tplot_vars,'Dens2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
  endif

  ;;Now Bulk energy
  STORE_DATA,'BlkE2DK',DATA={x:kappaTime,y:k2DParms.bulk_energy}
  OPTIONS,'BlkE2DK','psym',kappaSym
  OPTIONS,'BlkE2DK','colors',kappaColor

  STORE_DATA,'BlkE2DG',DATA={x:GaussTime,y:g2DParms.bulk_energy}
  OPTIONS,'BlkE2DG','psym',GaussSym
  OPTIONS,'BlkE2DG','colors',GaussColor

  OPTIONS,'BlkE2DK','ytitle','Bulk energy!C(eV)'
  BlkEBounds      = [MIN([k2DParms.bulk_energy,g2DParms.bulk_energy]), $
                     MAX([k2DParms.bulk_energy,g2DParms.bulk_energy])]
  ;; showLog_BlkE    = (ALOG10(BlkEBounds[1])-ALOG10(BlkEBounds[0])) GT 2
  IF showLog_BlkE THEN BEGIN
     BlkEBounds[0] -= (BlkEBounds[0]*0.1)
     BlkEBounds[1] += (BlkEBounds[1]*0.1)
  ENDIF ELSE BEGIN
     BlkEBounds[0] /= 1.1
     BlkEBounds[1] *= 1.1
  ENDELSE
  YLIM,'BlkE2DK',BlkEBounds[0],BlkEBounds[1],showLog_BlkE

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['BlkE2DK'] else tplot_vars=[tplot_vars,'BlkE2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG'  ;,PSYM='*'
  endif

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
  Je_current                   = (-1.)*Je.y*1.6e-9    ;;in microA/m2
  Ji_current                   =       Ji.y*1.6e-9    ;;in microA/m2

  ;;Get Kappa-predicted current
  kappaStr                       = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
  
  ;;Align to kappa fits
  FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current,ncomp:1}, $
                    RESULT=Je_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Ji.x,comp1:Ji_current,ncomp:1}, $
                    RESULT=Ji_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current+Ji_current,ncomp:1}, $
                    RESULT=Jtot_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chare,ncomp:1}, $
                    RESULT=chare_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chari_interp,ncomp:1}, $
                    RESULT=chari_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1}, $
                    RESULT=chartot_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:jMag.x,comp1:jMag.y,ncomp:1}, $
                    RESULT=jMag_kappa_interp,/INTERP,DELT_T=50.,/TALK
  
  ;;Align to Je
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:Ji.x,comp1:Ji_current,ncomp:1}, $
                    RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:jMag.x,comp1:jMag.y,ncomp:1}, $
                    RESULT=jMag_interp,/INTERP,DELT_T=50.,/TALK
  
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

  
  ;; oneCheeseBounds   = [MIN(obs_current) < MIN(gauss_current) < MIN(kappa_current) < MIN(jMag.y), $
  ;;                      MAX(obs_current) > MAX(gauss_current) > MAX(kappa_current) < MAX(jMag.y)]
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
  OPTIONS,'onecheese','labflag',3
  OPTIONS,'onecheese','labpos',oneCheesePos[0]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]

  OPTIONS,'fourcheese','colors',maxwell
  OPTIONS,'fourcheese','labels','Fluxgate mag'
  OPTIONS,'fourcheese','labflag',3
  OPTIONS,'fourcheese','labpos',oneCheesePos[1]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]

  OPTIONS,'toppings','labels' ,'Kappa model'
  OPTIONS,'toppings','psym'   ,1
  OPTIONS,'toppings','colors' ,kappaColor
  OPTIONS,'toppings','labflag',3
  OPTIONS,'toppings','labpos',oneCheesePos[3]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]

  OPTIONS,'feta','labels' ,'Maxwellian Model'
  OPTIONS,'feta','psym'   ,1
  OPTIONS,'feta','colors' ,GaussColor
  OPTIONS,'feta','labflag',3
  OPTIONS,'feta','labpos',oneCheesePos[2]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['onecheese'] else tplot_vars=[tplot_vars,'onecheese']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'    ;,PSYM=1
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'        ;,PSYM=1
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG'  ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG'  ;,PSYM='*'
  endif

  ;; ENDIF

  ;; IF KEYWORD_SET(add_Newell_panel) THEN BEGIN

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
                                            CONVERT_TO_NEWELL_INTERP=convert_to_Newell_interp

  if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[tplot_vars,var_name]

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT']
  endif

  ;; ENDIF

; force tplot_vars to be all the panels unless no_blank_panels is set

;;   if (not keyword_set(no_blank_panels)) then begin


;; ; SFA

;;      bdat = where(tplot_vars eq 'SFA_V5-V8',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = [-112.700,12.5031,997.434,2015.75]
;;         store_data,'SFA_V5-V8', data={x:t_arr, y:y_arr, v:v_arr}
;;         dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $
;;                   ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
;;                   ztitle: '(V/m)!U2!N/Hz', panel_size:2}
;;         store_data,'SFA_V5-V8', dlimit=dlimit
;;         options,'SFA_V5-V8','x_no_interp',1
;;         options,'SFA_V5-V8','y_no_interp',1
;;      endif

;; ; DSP

;;      bdat = where(tplot_vars eq 'DSP_V5-V8',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = [0.0,0.128,9.984,16.352]
;;         store_data,'DSP_V5-V8', data={x:t_arr, y:y_arr, v:v_arr}
;;         dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
;;                   ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
;;                   ztitle: '(V/m)!U2!N/Hz', panel_size:2}
;;         store_data,'DSP_V5-V8', dlimit=dlimit
;;         options,'DSP_V5-V8','x_no_interp',1
;;         options,'DSP_V5-V8','y_no_interp',1
;;      endif

;; ; Eesa_Energy

;;      bdat = where(tplot_vars eq 'Eesa_Energy',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = fltarr(2,4)
;;         v_arr[0,*] = [34119.7,26091.5,50.9600,5.88000]
;;         store_data,'Eesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
;;         options,'Eesa_Energy','spec',1	
;;         zlim,'Eesa_Energy',4,9,0
;;         ylim,'Eesa_Energy',5,30000,1
;;         options,'Eesa_Energy','ytitle','Electrons!C!CEnergy (eV)'
;;         options,'Eesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
;;         options,'Eesa_Energy','x_no_interp',1
;;         options,'Eesa_Energy','y_no_interp',1
;;         options,'Eesa_Energy','panel_size',2
;;      endif

;; ; Eesa_Angle

;;      bdat = where(tplot_vars eq 'Eesa_Angle',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = fltarr(2,4)
;;         v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
;;         store_data,'Eesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
;;         options,'Eesa_Angle','spec',1	
;;         zlim,'Eesa_Angle',4,9,0
;;         ylim,'Eesa_Angle',-90,270,0
;;         options,'Eesa_Angle','ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
;;         options,'Eesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
;;         options,'Eesa_Angle','x_no_interp',1
;;         options,'Eesa_Angle','y_no_interp',1
;;         options,'Eesa_Angle','panel_size',2
;;         options,'Eesa_Angle','yminor',9
;;         options,'Eesa_Angle','yticks',4
;;         options,'Eesa_Angle','ytickv',[-90,0,90,180,270]
;;      endif

;; ; Iesa_Energy

;;      bdat = where(tplot_vars eq 'Iesa_Energy',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = fltarr(2,4)
;;         v_arr[0,*] = [26808.3,11827.2,27.7200,4.62000]
;;         store_data,'Iesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
;;         options,'Iesa_Energy','spec',1	
;;         zlim,'Iesa_Energy',4,9,0
;;         ylim,'Iesa_Energy',4,30000,1
;;         options,'Iesa_Energy','ytitle','Ions!C!CEnergy (eV)'
;;         options,'Iesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
;;         options,'Iesa_Energy','x_no_interp',1
;;         options,'Iesa_Energy','y_no_interp',1
;;         options,'Iesa_Energy','panel_size',2
;;      endif

;; ; Iesa_Angle

;;      bdat = where(tplot_vars eq 'Iesa_Angle',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = fltarr(2,4)
;;         y_arr[*,*] = !values.f_nan
;;         v_arr = fltarr(2,4)
;;         v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
;;         store_data,'Iesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
;;         options,'Iesa_Angle','spec',1	
;;         zlim,'Iesa_Angle',4,9,0
;;         ylim,'Iesa_Angle',-90,270,0
;;         options,'Iesa_Angle','ytitle','Ions!C!CAngle (Deg.)'
;;         options,'Iesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
;;         options,'Iesa_Angle','x_no_interp',1
;;         options,'Iesa_Angle','y_no_interp',1
;;         options,'Iesa_Angle','panel_size',2
;;         options,'Iesa_Angle','yminor',9
;;         options,'Iesa_Angle','yticks',4
;;         options,'Iesa_Angle','ytickv',[-90,0,90,180,270]
;;      endif

;; ; EFIT_ALONG_VSC

;;      bdat = where(tplot_vars eq 'EFIT_ALONG_VSC',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = [!values.f_nan,!values.f_nan]
;;         store_data,'EFIT_ALONG_VSC', data={x:t_arr, y:y_arr}
;;         dlimit = {spec:0, ystyle:1, yrange:[-1000., 1000.], $
;;                   ytitle:'EFIT ALONG V!C!C55m (mV/m)', $
;;                   panel_size:3}
;;         store_data,'EFIT_ALONG_V',dlimit=dlimit
;;         options,'EFIT_ALONG_VSC','yrange',[-100.,100.]
;;         options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
;;         options,'EFIT_ALONG_VSC','panel_size',2
;;      endif

;; ; dB_fac_v

;;      bdat = where(tplot_vars eq 'dB_fac_v',ndat)
;;      if (ndat eq 0) then begin
;;         t_arr = tlimit_all
;;         y_arr = dblarr(2,3)
;;         y_arr[*,*] = !values.d_nan
;;         store_data,'dB_fac_v', data={x:t_arr, y:y_arr}
;;         options,'dB_fac_v','yrange',[-100,100]
;;         options,'dB_fac_v','ytitle','dB_fac_v!C!C(nT))'
;;         options,'dB_fac_v','panel_size',2
;;         options,'dB_fac_v','colors',[6,4,2]
;;         options,'dB_fac_v','labels',['v ((BxV)xB)','p (BxV)','b']
;;      endif


;;   endif

     ;; tplot_vars=['Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle']

     IF KEYWORD_SET(add_chare_panel)  THEN tplot_vars = ['charepanel',tplot_vars]

     IF KEYWORD_SET(add_kappa_panel)  THEN tplot_vars = ['onecheese','kappa_fit',tplot_vars]

     IF KEYWORD_SET(add_Newell_panel) THEN tplot_vars = ['newellPanel',tplot_vars]

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tplot_vars,var=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     ;; IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG' ;,PSYM='*'
     ;; ENDIF
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

     LOADCT2,40
     TPLOT,tplot_vars,VAR=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     ;; IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG'  ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG'  ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG' ;,PSYM='*'

     ;; ENDIF

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

; step 2 - E field


; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
;; IF KEYWORD_SET(add_eField) THEN BEGIN
;;   sdt_idx = get_sdt_run_idx()

;;   prog = getenv('FASTBIN') + '/showDQIs'
;;   if ((sdt_idx GE 0) AND (sdt_idx LT 100)) then begin
;;      if (sdt_idx GE 10) then begin
;;         sidstr = string(sdt_idx, format='(I2)')
;;      endif else begin
;;         sidstr = string(sdt_idx, format='(I1)')
;;      endelse
;;      spawn, [prog, sidstr], result, /noshell
;;   endif else begin
;;      spawn, prog, result, /noshell
;;   endelse


;;   b = where (strpos(result,'V1-V4_S') ge 0,nb4)
;;   if (nb4 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb4 = 0
;;   b = where (strpos(result,'V1-V2_S') ge 0,nb2)
;;   if (nb2 gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb2 = 0
;;   if (nb4 gt 0) then v12=get_fa_fields('V1-V4_S',/all) $
;;   else if (nb2 gt 0) then v12=get_fa_fields('V1-V2_S',/all)

;;   b = where (strpos(result,'V5-V8_S') ge 0,nb5)
;;   if (nb5 gt 0) then v58=get_fa_fields('V5-V8_S',/all)

;;   got_efield = (nb4 gt 0 or nb2 gt 0) and nb5 gt 0

;;   if (got_efield) then begin

;; ; despin e field data

;;      fa_fields_despin,v58,v12,/shadow_notch,/sinterp

;;      options,'EFIT_ALONG_V','yrange',0
;;      options,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
;;      options,'EFIT_ALONG_V','panel_size',2

;; ; reset time limits if needed

;;      get_data,'EFIT_ALONG_V',data=data
;;      IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
;;      IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L]

;;      if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
;;         if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
;;         if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
;;         get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
;;         get_new_igrf,/no_store_old
;;      endif

;; ; check for southern hemisphere and fix 
;; ; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

;;      get_data,'B_model',data=bm
;;      get_data,'fa_vel',data=vel
;;      get_data,'fa_pos',data=pos
;;      n=n_elements(reform(pos.y[*,0]))
;;      rxv = dblarr(n,3)
;;      rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
;;      rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
;;      rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
;;      vxb = dblarr(n,3)
;;      vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
;;      vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
;;      vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
;;      tst = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

;;      get_data,'EFIT_ALONG_V',data=data,dlimit=dlimit
;;      y2=spl_init(pos.x-tlimit_all[0],tst,/double)
;;      tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
;;      data.y = data.y*tst_/abs(tst_)
;;      store_data,'EFIT_ALONG_VSC',data=data,dlimit=dlimit
;;      options,'EFIT_ALONG_VSC','yrange',0
;;      options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
;;      options,'EFIT_ALONG_VSC','panel_size',2

;;      store_data,'E_NEAR_B',/delete
;;      store_data,'E_ALONG_V',/delete
;;      store_data,'EFIT_NEAR_B',/delete
;;      store_data,'EFIT_ALONG_V',/delete

;;      if (n_elements(tplot_vars) eq 0) then tplot_vars=['EFIT_ALONG_VSC'] else tplot_vars=[tplot_vars,'EFIT_ALONG_VSC']

;;      if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
;;         loadct2,40
;;         tplot,tplot_vars,var=['ALT','ILAT','MLT']
;;      endif

;;   endif else if (n_elements(tplot_vars) ne 0) then begin

;;      tplot_vars = 'dB_fac'
;;      if (keyword_set(use_fac_v)) then tplot_vars = 'dB_fac_v'
;;      if (not keyword_set(no_blank_panels)) then tplot_vars = 'dB_fac_v'

;;   endif

;; ENDIF

; Step 5 - VLF data


; DSP_V5-V8HG or DSP_V5-V8

;; IF KEYWORD_SET(add_DSP) THEN BEGIN
;;   prog = getenv('FASTBIN') + '/showDQIs'
;;   if ((sdt_idx GE 0) AND (sdt_idx LT 100)) then begin
;;      if (sdt_idx GE 10) then begin
;;         sidstr = string(sdt_idx, format='(I2)')
;;      endif else begin
;;         sidstr = string(sdt_idx, format='(I1)')
;;      endelse
;;      spawn, [prog, sidstr], result, /noshell
;;   endif else begin
;;      spawn, prog, result, /noshell
;;   endelse
;;   b = where (strpos(result,'DspADC_V5-V8HG') ge 0,ndsphg)
;;   if (ndsphg gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsphg = 0
;;   b = where ((strpos(result,'DspADC_V5-V8') ge 0) and (strpos(result,'DspADC_V5-V8HG') lt 0),ndsp)
;;   if (ndsp gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsp = 0

;;   if (ndsphg gt 0) then dat=get_fa_fields('DspADC_V5-V8HG',/all) else if (ndsp gt 0) then dat=get_fa_fields('DspADC_V5-V8',/all)
;;   ndsp = (ndsp gt 0) or (ndsphg gt 0)

;;   if (ndsp) then begin
;;      data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
;;      store_data,'DSP_V5-V8', data=data
;;      ;; dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
;;      dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], $
;;                zrange:[MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))])], $
;;                ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
;;                ztitle: '(V/m)!U2!N/Hz', panel_size:2}
;;      store_data,'DSP_V5-V8', dlimit=dlimit
;;      options,'DSP_V5-V8','x_no_interp',1
;;      options,'DSP_V5-V8','y_no_interp',1

;; ;  look for big jumps in time - blank these

;;      get_data,'DSP_V5-V8',data=data
;;      dt = data.x[1:*]-data.x[0:*]
;;      ntimes=n_elements(data.x)
;;      bg = where (dt gt 300, ng)
;;      if (ng gt 0) then begin
;;         bbb = bg-1
;;         if (bbb[0] lt 0) then bbb[0] = 0
;;         add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
;;         flag_dat = fltarr(ng*2)+!values.f_nan
;;         new_tag = [data.x,add_tag]
;;         tsort = sort(new_tag-new_tag[0])
;;         nvec=n_elements(data.y)/ntimes
;;         new_dat = fltarr(n_elements(new_tag),nvec)
;;         for nv = 0,nvec-1 do begin
;;            new_dat[*,nv] = [data.y[*,nv],flag_dat]
;;            new_dat[*,nv] = new_dat[tsort,nv]
;;         endfor
;;         data={x:new_tag[tsort],y:new_dat,v:data.v}
;;         store_data,'DSP_V5-V8',data=data
;;      endif
     
;;      if (n_elements(tplot_vars) eq 0) then tplot_vars=['DSP_V5-V8'] else tplot_vars=[tplot_vars,'DSP_V5-V8']

;;      if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
;;         loadct2,40
;;         tplot,tplot_vars,var=['ALT','ILAT','MLT']
;;      endif

;;   endif else begin

;;   endelse

;; ENDIF

;; ; Step 5 - AKR data
;; IF KEYWORD_SET(add_SFA) THEN BEGIN

;;   prog = getenv('FASTBIN') + '/showDQIs'
;;   if ((sdt_idx GE 0) AND (sdt_idx LT 100)) then begin
;;      if (sdt_idx GE 10) then begin
;;         sidstr = string(sdt_idx, format='(I2)')
;;      endif else begin
;;         sidstr = string(sdt_idx, format='(I1)')
;;      endelse
;;      spawn, [prog, sidstr], result, /noshell
;;   endif else begin
;;      spawn, prog, result, /noshell
;;   endelse
;;   b = where (strpos(result,'SfaAve_V5-V8') ge 0,nakr)
;;   if (nakr gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nakr = 0

;;   if (nakr gt 0) then begin

;;      dat = get_fa_fields('SfaAve_V5-V8', /all)
;;      data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
;;      store_data,'SFA_V5-V8', data=data
;;      ;; dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $
;;      dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], $
;;                zrange:[MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))])], $
;;                ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
;;                ztitle: '(V/m)!U2!N/Hz', panel_size:2}
;;      store_data,'SFA_V5-V8', dlimit=dlimit
;;      options,'SFA_V5-V8','x_no_interp',1
;;      options,'SFA_V5-V8','y_no_interp',1

;; ;  look for big jumps in time - blank these

;;      get_data,'SFA_V5-V8',data=data
;;      dt = data.x[1:*]-data.x[0:*]
;;      ntimes=n_elements(data.x)
;;      bg = where (dt gt 300, ng)
;;      if (ng gt 0) then begin
;;         bbb = bg-1
;;         if (bbb[0] lt 0) then bbb[0] = 0
;;         add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
;;         flag_dat = fltarr(ng*2)+!values.f_nan
;;         new_tag = [data.x,add_tag]
;;         tsort = sort(new_tag-new_tag[0])
;;         nvec=n_elements(data.y)/ntimes
;;         new_dat = fltarr(n_elements(new_tag),nvec)
;;         for nv = 0,nvec-1 do begin
;;            new_dat[*,nv] = [data.y[*,nv],flag_dat]
;;            new_dat[*,nv] = new_dat[tsort,nv]
;;         endfor
;;         data={x:new_tag[tsort],y:new_dat,v:data.v}
;;         store_data,'SFA_V5-V8',data=data
;;      endif

;;      if (n_elements(tplot_vars) eq 0) then tplot_vars=['SFA_V5-V8'] else tplot_vars=[tplot_vars,'SFA_V5-V8']

;;      if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
;;         loadct2,40
;;         tplot,tplot_vars,var=['ALT','ILAT','MLT']
;;      endif

;;   endif

;; ENDIF
