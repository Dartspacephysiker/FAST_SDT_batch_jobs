PRO SINGLE_KAPPA_SUMMARY,time1,time2, $
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
                         PLOT_1_OVER_KAPPA=plot_1_over_kappa, $
                         ADD_CHARE_PANEL=add_chare_panel, $
                         ADD_NEWELL_PANEL=add_Newell_panel, $
                         ADD_CHI2_LINE=add_chi2_line, $
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
                         CONVERT_DESPECS_TO_NEWELL_INTERP=convert_to_Newell_interp, $
                         SAVE_PS=save_ps, $
                         SAVE_PNG=save_png, $
                         SAVEKAPPA_BONUSPREF=bonusPref, $
                         PLOTDIR=plotDir, $
                         SAVE_FOR_OFFLINE=save_for_offline, $
                         LOAD_FROM_OFFLINE=load_from_offline, $
                         KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                         KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops

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

  saveDir    = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'

; create a summary plot of:
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; dB_fac_v (dB_fac and dB_SM also stored)

; Returns:
; tPlt_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data

; Program will use fac_v if E field data are available, other use fac_v
; over-ride with use_fac_v and use_fac keywords

; if no_blank_panels is not set procedure will generate tplot data for all the parameters,
; including missing data, for a uniform plot product

; Step 0 - safety measure - delete all tplot quantities if found

@tplot_com

  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     saveStr = 'SAVE,'
  ENDIF

  if NOT KEYWORD_SET(energy_ions) THEN energy_ions=[4,1.e4]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN ieb_or_ies = 'ieb' ELSE ieb_or_ies = 'ies'

  nn = n_elements(data_quants)

  if (nn gt 1) then for n = nn-1L,1L,-1L do store_data,data_quants(n).name,/delete

; Step 1 - DC Mag data

  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
     ucla_mag_despin,tw_mat=tw_mat,orbit=orbit,spin_axis=spin_axis,delta_phi=delta_phi
  ENDIF

  if (n_elements(orbit) gt 0) then begin

;  if orbit > 9936 return (temporary fix)

     ;; if (orbit gt 9936) then begin

     ;;    print,""
     ;;    print,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
     ;;    print,""
     ;;    ;; return

     ;; endif

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

        IF N_ELEMENTS(convert_to_Newell_interp) GT 0 THEN BEGIN
           IF convert_to_Newell_interp EQ 0 THEN BEGIN
              outPlotName += '--not_Newell_interpreted'
           ENDIF
        ENDIF

        IF KEYWORD_SET(load_from_offline) THEN BEGIN
           load_from_offFile = saveDir + outPlotName + '.sav'
           PRINT,'Restoring ' + load_from_offFile + ' ...'
           RESTORE,load_from_offFile

           STORE_DATA,"dB_fac_V",DATA=dB_fac_V_off
           STORE_DATA,"Iesa_Angle",DATA=Iesa_Angle_off
           STORE_DATA,"Iesa_Energy",DATA=Iesa_Energy_off
           STORE_DATA,"Eesa_Angle",DATA=Eesa_Angle_off
           STORE_DATA,"Eesa_Energy",DATA=Eesa_Energy_off
           STORE_DATA,"LAT",DATA=LAT_off
           STORE_DATA,"ORBIT",DATA=ORBIT_off
           STORE_DATA,"Je",DATA=Je_off
           STORE_DATA,"Jee",DATA=Jee_off
           STORE_DATA,"Ji",DATA=Ji_off
           STORE_DATA,"Jei",DATA=Jei_off
           STORE_DATA,"jMag",DATA=jMag_off
           STORE_DATA,"fa_vel",DATA=fa_vel_off
           STORE_DATA,"Eesa_LC_Energy",DATA=Eesa_LC_Energy_off
           STORE_DATA,"MLT",DATA=MLT_off
           STORE_DATA,"ILAT",DATA=ILAT_off
           STORE_DATA,"ALT",DATA=ALT_off
           STORE_DATA,"ORBIT",DATA=ORBIT2_off
           STORE_DATA,"sc_pot",DATA=sc_pot_off
        ENDIF

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
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        dB_fac_V_off = data
        saveStr += 'dB_fac_V_off,'
     ENDIF
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0] ELSE t1 = time1
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[n_elements(data.x)-1L] ELSE t2 = time2
     tlimit_all = [t1,t2]
     ;; tPlt_vars = 'dB_fac_v'
     options,'dB_fac_v','panel_size',2
     options,'dB_fac','panel_size',2
     options,'dB_sm','panel_size',2

     ;; if (keyword_set(use_fac)) then tPlt_vars = 'dB_fac'

     ;; if (not keyword_set(no_blank_panels)) then tPlt_vars = 'dB_fac_v'

     ;; if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     ;;    wInd = 0
     ;;    WINDOW,wInd,XSIZE=700,YSIZE=900
     ;;    ;; tplot_options,'region',[0.,0.5,1.0,1.0]
     ;;    loadct2,39
     ;;    tplot,tPlt_vars,var=['ALT','ILAT','MLT'], $
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
     IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
        get_pa_spec,'fa_' + ieb_or_ies + '_c',units='eflux',name=var_name,energy=[4.,30000.]
     ENDIF
     get_data,var_name,data=data
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        Iesa_Angle_off = data
        saveStr += var_name + '_off,'
     ENDIF
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

        IF KEYWORD_SET(include_ion_plots) THEN BEGIN
           if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

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
              tplot,tPlt_vars,var=['ALT','ILAT','MLT'], $
                    WINDOW=wInd, $
                    TRANGE=[t1,t2]
           endif
           ;; if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
           ;;    loadct2,40
           ;;    tplot,tPlt_vars,var=['ALT','ILAT','MLT']
           ;; endif

; ION ENERGY 

           var_name='Iesa_Energy'
           IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
              get_en_spec,'fa_' + ieb_or_ies + '_c',name=var_name, units='eflux',/CALIB,RETRACE=1
           ENDIF
           get_data,var_name, data=data
           IF KEYWORD_SET(save_for_offline) THEN BEGIN
              Iesa_Energy_off = data
              saveStr += var_name + '_off,'
           ENDIF
           data.y = alog10(data.y)
           store_data,var_name, data=data
           options,var_name,'spec',1	
           ;; zlim,var_name,4,9,0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 5 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 9),0
           ylim,var_name,4,30000,1
           options,var_name,'ytitle','Ions!C!CEnergy (eV)'
           options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
           options,var_name,'x_no_interp',1
           options,var_name,'y_no_interp',1
           options,var_name,'panel_size',2

           if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

           if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
              loadct2,40
              tplot,tPlt_vars,var=['ALT','ILAT','MLT']
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
        IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
           get_pa_spec,'fa_' + eeb_or_ees + '_c',units='eflux',name=var_name, energy=[10.,30000.]
        ENDIF
        get_data,var_name, data=data 
        IF KEYWORD_SET(save_for_offline) THEN BEGIN
           Eesa_Angle_off = data
           saveStr       += var_name + '_off,'
        ENDIF
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1
        ;; zlim,var_name,4,9,0
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

           if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

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
              tplot,tPlt_vars,var=['ALT','ILAT','MLT']
           endif
        ENDIF

; ELECTRON ENERGY

        var_name='Eesa_Energy'
        IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
           get_en_spec,'fa_' + eeb_or_ees + '_c',name=var_name,units='eflux',/CALIB,RETRACE=1
        ENDIF
        get_data,var_name, data=data
        IF KEYWORD_SET(save_for_offline) THEN BEGIN
           Eesa_Energy_off = data
           saveStr        += var_name + '_off,'
        ENDIF
        data.y = alog10(data.y)
        store_data,var_name, data=data
        options,var_name,'spec',1	
        ;; zlim,var_name,4,9,0
        zlim,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > 6 ), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < 10),0
        ;; zlim,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
        ylim,var_name,5,30000,1
        options,var_name,'ytitle','Electrons!C!CEnergy (eV)'
        options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        options,var_name,'x_no_interp',1
        options,var_name,'y_no_interp',1
        options,var_name,'panel_size',2

        if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

        if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then BEGIN 
           loadct2,40
           tplot,tPlt_vars,var=['ALT','ILAT','MLT']
        endif

     endif

; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south also change plot title

  get_data,'LAT',data=data
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     LAT_off  = data
     saveStr  += 'LAT_off,'
  ENDIF

  if (n_elements(data.y) le 0) then return

  bb = where (data.y gt 10,nn)
  if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

  bb = where (data.y lt -10,nn)
  if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

  hemisph = getenv('FAST_ORBIT_HEMISPHERE')

  get_data,'ORBIT',data=data
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     ORBIT_off  = data
     saveStr  += 'ORBIT_off,'
  ENDIF
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
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
  t1eeb = time1 > t1eeb
  t2eeb = time2 < t2eeb

  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN     
     GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Je',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Jee',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'j_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Ji',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     GET_2DT,'je_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Jei',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  ENDIF
  ;;Remove_crap
  GET_DATA,'Je',DATA=tmp
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     Je_off  = tmp
     saveStr += 'Je_off,'
  ENDIF

  ;; GET_DATA,'Je',DATA=Je_originalsk
  ;; saveStr+='Je_originalsk,'
  keep1                          = WHERE(FINITE(tmp.y) NE 0)
  keep2                          = WHERE(ABS(tmp.y) GT 0.0)
  GET_DATA,'Jee',DATA=tmp
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     Jee_off  = tmp
     saveStr += 'Jee_off,'
  ENDIF
  ;; GET_DATA,'Jee',DATA=Jee_originalsk
  ;; saveStr+='Jee_originalsk,'
  keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Ji',DATA=tmp
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     Ji_off  = tmp
     saveStr += 'Ji_off,'
  ENDIF
  ;; GET_DATA,'Ji',DATA=Ji_originalsk
  ;; saveStr+='Ji_originalsk,'
  keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Jei',DATA=tmp
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     Jei_off  = tmp
     saveStr += 'Jei_off,'
  ENDIF
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
  chari_interp     = DATA_CUT({x:Jei.x,y:chari},Jee.x,/IGNORE_NAN,GAP_DIST=3)
  ;; FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1}, $
  ;;                   {time:Jei.x,comp1:chari,ncomp:1}, $
  ;;                   RESULT=chari_interp, $
  ;;                   /INTERP, $
  ;;                   DELT_T=50., $
  ;;                   /TALK
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

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['charepanel'] else tPlt_vars=[tPlt_vars,'charepanel']

     if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
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

  IF ~KEYWORD_SET(plot_1_over_kappa) THEN BEGIN
     STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
     STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:REFORM(kappa2D.fitParams[2,*])}
     kappaBounds      = [MIN(k2DParms.kappa), $
                         MAX(k2DParms.kappa)]
     CASE 1 OF
        KEYWORD_SET(showLog_kappa): BEGIN
           YLIM,'kappa_fit',1.0,100,1
        END
        ELSE: BEGIN
           YLIM,'kappa_fit',1.0,11,0
        END
     ENDCASE

  ENDIF ELSE BEGIN
     STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:REFORM(1./kappa2D.fitParams[2,*])}
     kappaBounds      = [MIN(1./k2DParms.kappa), $
                         MAX(1./k2DParms.kappa)]
     showLog_kappa    = (ALOG10(1./kappaBounds[1])-ALOG10(1./kappaBounds[0])) GT 1.5

     CASE 1 OF
        KEYWORD_SET(showLog_kappa): BEGIN
           YLIM,'kappa_fit',0.01,0.7,1
        END
        ELSE: BEGIN
           YLIM,'kappa_fit',0.01,0.7,0
        END
     ENDCASE
  ENDELSE

  OPTIONS,'kappa_fit','ytitle',"Kappa"
  OPTIONS,'kappa_fit','psym',kappaSym  

  ;;And a line to show where the awesome kappa vals are
  STORE_DATA,'kappa_critisk',DATA={x:kappaTime,y:MAKE_ARRAY(N_ELEMENTS(kappaTime),VALUE=2.5)}
  OPTIONS,'kappa_critisk','colors',red

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['kappa_fit'] else tPlt_vars=[tPlt_vars,'kappa_fit']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM=BRO
  endif

  showLog_Temp = 1
  showLog_Dens = 1
  showLog_BlkE = 1
  showLog_chi2 = 0
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

  ;;And a line to show where the awesome chi2 vals are, if desired
  IF KEYWORD_SET(add_chi2_line) THEN BEGIN
     STORE_DATA,'chi2_critisk',DATA={x:kappaTime,y:MAKE_ARRAY(N_ELEMENTS(kappaTime),VALUE=5.0)}
     ;; OPTIONS,'chi2_critisk','colors',red
  ENDIF



  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['chi22DK'] else tPlt_vars=[tPlt_vars,'chi22DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG',PSYM=GaussSym
     IF KEYWORD_SET(add_chi2_line) THEN BEGIN
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi2_critisk'
     ENDIF
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

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['Temp2DK'] else tPlt_vars=[tPlt_vars,'Temp2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
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

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['Dens2DK'] else tPlt_vars=[tPlt_vars,'Dens2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
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

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['BlkE2DK'] else tPlt_vars=[tPlt_vars,'BlkE2DK']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG',PSYM=GaussSym
  endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Four-current panel (it's like four-cheese pizza)

  ;;Get mag current
  get_data,'dB_fac_v',data=db_fac

  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
     jMag  = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                      db_fac,vel, $
                                      /USE_DESPUN, $
                                      SDTNAME__JMAG=jMagName, $
                                      ;; INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                      ;; SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                      QUIET=quiet)
  ENDIF
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     jMag_off = jMag
     saveStr += 'jMag_off,'
     GET_DATA,'fa_vel',DATA=fa_vel_off
     saveStr += 'fa_vel_off,'
  ENDIF
  
  ;;Get electron ESA current, ion ESA current
  Je_current                   = (-1.)*Je.y*1.6e-9    ;;in microA/m2
  Ji_current                   =       Ji.y*1.6e-9    ;;in microA/m2

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
  ;; FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:Ji.x,comp1:Ji_current,ncomp:1}, $
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

  IF KEYWORD_SET(kStats__save_stuff) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(kStats__include_these_startstops): BEGIN
           PRINT,'Save it, save stuff: ' + saveDir + outplotname + $
                 '--for_kStats_analysis--has_startstops.sav'
           startStop_t = kStats__include_these_startstops
           SAVE,setup,kappa2d,gauss2d,orbString, $
                startStop_t, $
                FILENAME=saveDir + outplotname + '--for_kStats_analysis--has_startstops.sav'
           
        END
        ELSE: BEGIN
           PRINT,'Save it, save stuff: ' + saveDir + outplotname + $
                 '--for_kStats_analysis.sav'
           SAVE,setup,kappa2d,gauss2d,orbString, $
                FILENAME=saveDir + outplotname + '--for_kStats_analysis.sav'
        END
     ENDCASE
  ENDIF

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
        GET_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
                                         kappaPot,gaussPot,R_B, $
                                         kappa_current,gauss_current,obs_current, $
                                         DENSITY_KAPPA2D=kappaDens, $
                                         DENSITY_GAUSS2D=gaussDens, $
                                         /MAKE_CURRENTS_POSITIVE, $
                                         QUIET=quiet
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

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['onecheese'] else tPlt_vars=[tPlt_vars,'onecheese']

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM=NO
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings',PSYM=kappaSym
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG',PSYM=GaussSym
     IF KEYWORD_SET(add_chi2_line) THEN BEGIN
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi2_critisk'
     ENDIF
  endif

  ;; ENDIF

  ;; IF KEYWORD_SET(add_Newell_panel) THEN BEGIN

  var_name='Eesa_LC_Energy'
  ;;This already gets called above, but we need to call it again to handle angle restrictions
  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
     GET_EN_SPEC,'fa_' + eeb_or_ees + '_c',name=var_name,units='eflux',/CALIB,RETRACE=1,ANGLE=eAngle
  ENDIF
  GET_DATA,var_name,DATA=data
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     Eesa_LC_Energy_off  = data
     saveStr += var_name+'_off,'
  ENDIF

  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN
     GET_FA_ORBIT,data.x,/TIME_ARRAY
  ENDIF
  GET_DATA,'MLT',DATA=mlt
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     MLT_off  = mlt
     saveStr += 'MLT_off,'
  ENDIF
  mlt       = mlt.y

  GET_DATA,'ILAT',DATA=ilat
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     ILAT_off  = ilat
     saveStr += 'ILAT_off,'
  ENDIF
  ilat      = ilat.y

  GET_DATA,'ALT',DATA=alt
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     ALT_off  = alt
     saveStr += 'ALT_off,'
  ENDIF
  alt      = alt.y

  GET_DATA,'ORBIT',DATA=orbit
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     ORBIT2_off  = orbit
     saveStr += 'orbit2_off,'
  ENDIF
  orbit          = orbit.y
  sc_pot         = GET_FA_POTENTIAL(t1,t2, $
                                    ;; /SPIN, $
                                    /REPAIR)
  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     sc_pot_off  = sc_pot
     saveStr += 'sc_pot_off,'
  ENDIF
  sc_pot_interp  = DATA_CUT({x:sc_pot.time,y:sc_pot.comp1},data.x) 
  this           = VALUE_CLOSEST2(data.x,jee.x) 
  data           = {x:data.x[this],y:data.y[this,*],v:data.v[this,*]}
  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,data,Jee,Je,mlt,ilat,alt,orbit,events, $
                                          SC_POT=sc_pot_interp,/QUIET

  var_name = 'newellPanel'
  PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,events,TPLOT_NAME=var_name, $
                                            /NO_STRICT_TYPES, $
                                            CONVERT_TO_NEWELL_INTERP=convert_to_Newell_interp

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[tPlt_vars,var_name]

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
  endif

  ;; ENDIF

  ;; tPlt_vars=['Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle']

  IF KEYWORD_SET(add_chare_panel)  THEN tPlt_vars = ['charepanel',tPlt_vars]

  IF KEYWORD_SET(add_kappa_panel)  THEN tPlt_vars = ['onecheese','kappa_fit',tPlt_vars]

  IF KEYWORD_SET(add_Newell_panel) THEN tPlt_vars = ['newellPanel',tPlt_vars]

  if (keyword_set(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     ;; IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM=BRO
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings',PSYM=kappaSym
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG',PSYM=GaussSym
     IF KEYWORD_SET(add_chi2_line) THEN BEGIN
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi2_critisk'
     ENDIF
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
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     ;; IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;PSYM=BRO
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings',PSYM=kappaSym
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='kappa_fit',OPLOTVAR='kappa_critisk' ;,PSYM=BRO
     TPLOT_PANEL,VARIABLE='Temp2DK',OPLOTVAR='Temp2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='Dens2DK',OPLOTVAR='Dens2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='BlkE2DK',OPLOTVAR='BlkE2DG',PSYM=GaussSym
     TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi22DG',PSYM=GaussSym
     IF KEYWORD_SET(add_chi2_line) THEN BEGIN
        TPLOT_PANEL,VARIABLE='chi22DK',OPLOTVAR='chi2_critisk'
     ENDIF

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

  IF KEYWORD_SET(save_for_offline) THEN BEGIN
     IF N_ELEMENTS(saveFile) EQ 0 THEN saveFile = outPlotName
     saveStr += saveDir + 'FILENAME="' + outPlotName + '.sav"'
     PRINT,"Execute it: " + saveStr
     this = EXECUTE(saveStr)
     PRINT,"Done!"
  ENDIF

  RETURN
END
