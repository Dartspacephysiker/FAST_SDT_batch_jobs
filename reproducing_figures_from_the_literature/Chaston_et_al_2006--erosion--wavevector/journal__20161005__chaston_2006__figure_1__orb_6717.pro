;;2016/10/05
;;This is entirely ripped off from Strangeway's batch_summary.pro, gifted to me by that beautiful human, Jack Vernetti
PRO JOURNAL__20161005__CHASTON_2006__FIGURE_1__ORB_6717, $
   TPLT_VARS=tPlt_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   USE_db_FAC=use_db_fac, $
   ;; USE_db_FAC_v=use_db_fac_v, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   SAVE_B_AND_J_DATA=save_B_and_J_data, $
   ANCILLARY_PLOTS=ancillary_plots, $
   ADD_TIMEBAR=add_timebar

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

  ;;Set YNOZERO
  !Y.STYLE = (!Y.STYLE) OR 16

  IF N_ELEMENTS(use_db_fac) EQ 0 AND N_ELEMENTS(use_db_fac_v) EQ 0 THEN use_db_fac  = 1

  ;; eeb_or_ees        = 'ees'
  ;; ieb_or_ies        = 'ies'

  eeb_or_ees        = 'eeb'
  ieb_or_ies        = 'ieb'

  outPlotName       = 'Chaston_et_al_2006__ionos_erosion--Fig_1'
  IF KEYWORD_SET(ancillary_plots) THEN BEGIN
     outPlotName   += '--with_ancillaries'
  ENDIF

  t1ZoomStr         = '1998-05-04/06:44:15'
  t2ZoomStr         = '1998-05-04/06:45:01'

  t1Zoom            = STR_TO_TIME(t1ZoomStr)
  t2Zoom            = STR_TO_TIME(t2ZoomStr)

  timesBarStr       = ['1998-05-04/06:44:46','1998-05-04/06:44:56']
  timesBar          = STR_TO_TIME(timesBarStr)

  energy_electrons  = [20.,30000.]
  energy_ions       = [0.,30000.]
  ion_angle         = [270,360]

  EFieldVar         = 'EFIT_ALONG_VSC'
  EFieldVar         = 'E_ALONG_V'
  EFieldSpecVar     = 'E_ALONG_V_16k'

; Step 0 - safety measure - delete all tplot quantities if found

@tplot_com

  nn = n_elements(data_quants)

  if (nn gt 1) then for n = nn-1L,1L,-1L do store_data,data_quants(n).name,/delete

  field  = GET_FA_FIELDS('MagDC',t1Zoom-10,t2Zoom+10,/store)
  magAC = GET_FA_FIELDS('Mag3ac_S',t1Zoom-10,t2Zoom+10,/store)

  magVar = 'MAGDATA'

  GET_FA_ORBIT,t1Zoom-10,t2Zoom+10,/DEFINITIVE
  GET_DATA,'ORBIT',data=orbit
  orbit = orbit.y[0]
  
  GET_DATA,'MagDCcomp3',data=magz
  GET_DATA,'Mag3ac_S',data=magz_AC

  ;;Get model field for subtraction
  GET_FA_ORBIT,magz_AC.x,/DEFINITIVE,/ALL,/TIME_ARRAY
  GET_DATA,'B_model',data=bMod

  ;;Which? Those.
  ;; this  = plot(bmod.x-bmod.x[0],bmod.y[*,0]-bmod.y[0,0]) 
  ;; that  = plot(bmod.x-bmod.x[0],bmod.y[*,1]-bMod.y[0,1],/OVERPLOT,COLOR='RED') 
  ;; those = plot(bmod.x-bmod.x[0],bmod.y[*,2]-bMod.y[0,2],/OVERPLOT,COLOR='GREEN')
  min  = MIN(bmod.x-t1Zoom,ind)
  bOff = bmod.y[*,2]-bMod.y[ind,2]

  FA_FIELDS_COMBINE,{time:magz_AC.x,comp1:magz_AC.y,ncomp:1}, $
                    {time:magz.x,comp1:magz.y}, $
                    RESULT=magzInterp, $
                    /SPLINE

  magz = {x:magz_AC.x, $
          y:magzInterp}

  bro = WHERE(FINITE(magz.y))
  firstB = magz.y[bro[0]]

  ;; bogus = (magz_AC.x-magz_AC.x[0])/(magz_AC.x[-1]-magz_AC.x[0])*(-900)

  magVarData = {x:[[magz_AC.x],[magz_AC.x]], $
                y:[[magz.y-firstB+bOff],[magz.y+magz_AC.y-firstB+bOff]]}
                ;; y:[[magz.y-firstB+bogus],[magz.y+magz_AC.y-firstB+bogus]]}

  store_data,magVar,data=magVarData
  options,magVar,'yrange',[-1200,500]
  options,magVar,'ytitle','B!Dy!N!C!C(nT))'
  options,magVar,'panel_size',2
  options,magVar,'colors',[6,4]
  options,magVar,'labels',['FG','FG+SC']

  t1 = magz.x[0]
  t2 = magz.x[n_elements(magz.x)-1L]
  tlimit_all = [t1,t2]

  tPlt_vars = magVar

  IF KEYWORD_SET(screen_plot) THEN BEGIN
     TPLOT,tPlt_vars,TRANGE=tlimit_all
  ENDIF


; Step 1 - DC Mag data


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

     fa_fields_despin,v58,v12 ;,/shadow_notch,/sinterp

     FA_FIELDS_DESPIN_16K
     ;; options,'EFIT_ALONG_V','yrange',0
     ;; options,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
     ;; options,'EFIT_ALONG_V','panel_size',2

; reset time limits if needed

     ;; get_data,'EFIT_ALONG_V',data=data
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

     ;;Stuff for EField plot
     options,EFieldVar,'yrange',[-400,600]
     options,EFieldVar,'ytitle','E along V!Dsc!N!C!C(mV/m)'
     options,EFieldVar,'panel_size',2

     ;; store_data,'E_NEAR_B',/delete
     ;; store_data,'E_ALONG_V',/delete
     ;; store_data,'EFIT_NEAR_B',/delete
     ;; store_data,'EFIT_ALONG_V',/delete

     t = 0.
     dat=get_fa_fields('V5-V8_S',t,/start)
     if dat.valid eq 0 then begin
        print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
        data_valid=0.0
     endif else begin

        efieldV58=get_fa_fields('V5-V8_S',t1Zoom,t2Zoom)
        efieldV1214=get_fa_fields('V1-V2_S',t1Zoom,t2Zoom)
        if efieldV1214.valid eq 0 then begin
           print,'No V1-V2_S data - trying V1-V4_S'
           efieldV1214=get_fa_fields('V1-V4_S',t1Zoom,t2Zoom)
           if efieldV1214.valid eq 0 AND KEYWORD_SET(burst) then begin
              print,'No V1-V4_S data - trying V1-V2_4k (burst)'
              efieldV1214=get_fa_fields('V1-V2_4k',t1Zoom,t2Zoom)
              if efieldV1214.valid eq 0 then begin
                 print,'No V1-V2_4k data - trying V1-V4_4k (burst)'
                 efieldV1214=get_fa_fields('V1-V4_4k',t1Zoom,t2Zoom)
                 if efieldV1214.valid eq 0 then begin
                    print,'No FAST fields data-get_fa_fields returned invalid data'
                    data_valid=0.0
                 endif
              endif
           endif else begin
              print,'No FAST fields data-get_fa_fields returned invalid data'
              data_valid=0.0
           endelse
        endif 
     endelse

     FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
     
     efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
     
     ;; STORE_DATA,EFieldVar,DATA=efield

     ;; magz={time:magz.x,comp1:magz.y,ncomp:1}
     ;; efield={time:efield.x,comp1:efield.y}
     
     ;; FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
     ;; fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

     Langmuir_2=get_fa_fields('NE2_S',t1Zoom,t2Zoom)
     Langmuir_6=get_fa_fields('NE6_S',t1Zoom,t2Zoom)
     Langmuir_9=get_fa_fields('NE9_S',t1Zoom,t2Zoom)
     Langmuir_data=[0]
     Langmuir_time=[0]
     Langmuir_prob=[0]
     if Langmuir_2.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_2.comp1]
        langmuir_time=[Langmuir_time,Langmuir_2.time]
        langmuir_prob=[Langmuir_prob,replicate(2,n_elements(Langmuir_2.time))]
     endif
     if Langmuir_6.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_6.comp1]
        langmuir_time=[Langmuir_time,Langmuir_6.time]
        langmuir_prob=[Langmuir_prob,replicate(6,n_elements(Langmuir_6.time))]
     endif
     if Langmuir_9.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_9.comp1]
        langmuir_time=[Langmuir_time,Langmuir_9.time]
        langmuir_prob=[Langmuir_prob,replicate(9,n_elements(Langmuir_9.time))]
     endif
     if n_elements(langmuir_data) GT 1 then begin
        langmuir_time=langmuir_time(1:n_elements(Langmuir_time)-1)
        langmuir_data=langmuir_data(1:n_elements(Langmuir_time)-1)
        langmuir_prob=langmuir_prob(1:n_elements(Langmuir_time)-1)
        time_order_langmuir=sort(langmuir_time)
        langmuir={x:langmuir_time(time_order_langmuir),y:langmuir_data(time_order_langmuir)}
        dens_probe={x:langmuir_time(time_order_langmuir),y:langmuir_prob(time_order_langmuir)}
     endif else data_valid=0.0

     langVar = 'LANGMUIR'
     STORE_DATA,langVar,DATA=langmuir
     ylim,langVar,2e1,2e4,1
     options,langVar,'ytitle','Probe Current!C(nA)'
     options,langVar,'panel_size',3
     
     tPlt_vars = [langVar,tPlt_vars]

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[EFieldVar] else tPlt_vars=[EFieldVar,tPlt_vars]

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  endif else if (n_elements(tPlt_vars) ne 0) then begin

     tPlt_vars = 'dB_fac_v'

     if (keyword_set(use_db_fac)) then tPlt_vars = 'dB_fac'
     if ~KEYWORD_SET(no_blank_panels) then tPlt_vars = 'dB_fac_v'

  endif


  GET_DATA,EFieldSpecVar,DATA=eAlongV16K
  GET_DATA,'E_NEAR_B_16k',DATA=eNearB16K

  yVar = eAlongV16K.y
  yVar = SQRT(eAlongV16K.y^2 + eNearB16K.y^2)


  eAlongV16KTmp   = {TIME         :  eAlongV16K.x , $
                     COMP1        :  yVar         , $
                     NCOMP        : 1             , $
                     VALID        : 1             , $
                     DATA_NAME    :'E Along V'    , $
                     PROJECT_NAME : 'FAST'        , $
                     UNITS_NAME   : 'mV/m'        , $
                     CALIBRATED   : 1}

  n_ave = 2
  nPts  = 1024
  slide = 1.0
  ESpecVar = 'EAVSpec'
  ESpecThresh = 1e-6          ;in (mV/m)^2/Hz

  spec = FA_FIELDS_SPEC(eAlongV16KTmp, $
                        /STORE, $
                        T_NAME=ESpecVar, $
                        STRUCTURE=eAVSpec, $
                        NPTS=nPts, $
                        N_AVE=n_ave, $
                        SLIDE=slide)

  eAVSpecLims      = [1.e-5,1.e4]
  ZLIM,ESpecVar,eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
  YLIM,ESpecVar,1.e-2,1.e1,1
  OPTIONS,ESpecVar,'ytitle','Frequency!C(kHz)'
  OPTIONS,ESpecVar,'ztitle','Log ' + eAVSpec.units_name ; z title
  OPTIONS,ESpecVar,'panel_size',2.0

  ;; GET_DATA,ESpecVar,DATA=tmp        
  ;; tmp.y[WHERE(~FINITE(tmp.y) OR (tmp.y LT ESpecThresh) )] = 0.0
  ;; STORE_DATA,ESpecVar,DATA=tmp

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[ESpecVar] else tPlt_vars=[ESpecVar,tPlt_vars]

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

; ION ENERGY 

     var_name='Iesa_Energy'
     get_en_spec,T1=t1Zoom,T2=t2Zoom, $
                 'fa_'+ieb_or_ies+'_c',name=var_name,units='eflux',angle=ion_angle
     ;; data.y = alog10(data.y)
     ;; store_data,var_name, data=data
     options,var_name,'spec',1	
     zlim,var_name,1e6,5e7,0
     ylim,var_name,4,30000,1
     options,var_name,'ytitle','Ions!C!CEnergy (eV)'
     options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     options,var_name,'x_no_interp',1
     options,var_name,'y_no_interp',1
     options,var_name,'panel_size',2

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  endif


; ION PITCH ANGLE

  var_name='Iesa_Angle'
  get_pa_spec,"fa_"+ieb_or_ies+"_c",units='eflux',name=var_name,energy=energy_ions
  get_data,var_name, data=data
  this = where(data.v[0,*] GT 170)
  data = {x:data.x, $
          y:data.y[*,this], $
          v:data.v[*,this]}
  ;; get_data,var_name, data=data
  ;; data.y = alog10(data.y)
  store_data,var_name, data=data
  options,var_name,'spec',1	
  zlim,var_name,4e6,3e7,0
  ylim,var_name,180,360,0
  options,var_name,'ytitle','Ions!C!CAngle (Deg.)'
  options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
  options,var_name,'x_no_interp',1
  options,var_name,'y_no_interp',1
  options,var_name,'panel_size',2

  get_data,var_name, data=data
  bb = where (data.v gt 270.,nb)
  if (nb gt 0) then data.v(bb)=data.v(bb)-360.
  nn = n_elements(data.x)
  for n = 0,nn-1L do begin 
     bs = sort (data.v(n,*))
     data.v(n,*)=data.v(n,bs)
     data.y(n,*)=data.y(n,bs)
  endfor
  ;; store_data,var_name, data=data	
  ;; options,var_name,'yminor',9
  options,var_name,'yticks',1
  options,var_name,'ytickv',[180,270,360]
  options,var_name,'ynozero',1
  options,var_name,'ystyle',16
  ;; ylim,var_name,-90,270,0

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

; reset time limits if needed

  t1 = data.x[0]
  t2 = data.x[n_elements(data.x)-1L]

  if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
     if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
     if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
     get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
     get_new_igrf,/no_store_old
  endif

  if (keyword_set(screen_plot)) then begin
     loadct2,40
     tplot,tPlt_vars,var=['ALT','ILAT','MLT']
  endif

  ;;Get potential
  sc_pot  = GET_FA_POTENTIAL(t1Zoom,t2Zoom, $
                             ;; /SPIN, $
                             /REPAIR)

  sc_pot  = {x:sc_pot.time, $
             y:(-1.)*sc_pot.comp1, $ ;;Reverse sign of pot for use with GET_2DT_TS_POT
             valid:sc_pot.valid} 

  GET_2DT_TS_POT,'j_2d_fs','fa_'+ieb_or_ies,name='Ji_up',t1=t1,t2=t2, $
          energy=energy_ions,angle=ion_angle,sc_pot=sc_pot
  ;; ylim,'Ji_up',1.e5,1.e8,1 	; set y limits
  ;; options,'Ji_up','tplot_routine','pmplot' 	; set 2 color plot
  ;; options,'Ji_up','labels',['Downgoing!C Ions','Upgoing!C Ions '] 	; set color label
  ;; options,'Ji_up','labflag',3 	; set color label
  ;; options,'Ji_up','labpos',[2.e7,1.e6] 	; set color label
  GET_DATA,'Ji_up',DATA=tmp
  ;; tmp.y = SMOOTH((-1.)*tmp.y,5)
  ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  ;; STORE_DATA,'Ji_up',DATA={x:tS_1s,y:doDat}
  ylim,'Ji_up',1.e7,1.e10,1                             ; set y limits
  options,'Ji_up','ytitle','Ion Flux!C#/(cm!U2!N-s)'    ; set y title
  options,'Ji_up','panel_size',2                        ; set panel size

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

; ELECTRON ENERGY

     var_name='Eesa_Energy'
     get_en_spec,T1=t1Zoom,T2=t2Zoom, $
                 'fa_'+eeb_or_ees+'_c',name=var_name,units='eflux'
     get_data,var_name, data=data
     data.y = alog10(data.y)
     store_data,var_name, data=data
     options,var_name,'spec',1	
     zlim,var_name,9,10,0
     ylim,var_name,5,30000,1
     options,var_name,'ytitle','Electrons!C!CEnergy (eV)'
     options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
     options,var_name,'x_no_interp',1
     options,var_name,'y_no_interp',1
     options,var_name,'panel_size',2

     if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

  endif


; ELECTRON PITCH ANGLE

  var_name='Eesa_Angle'
  get_pa_spec,"fa_"+eeb_or_ees+"_c",units='eflux',name=var_name, energy=energy_electrons
  ;; get_data,var_name, data=data 
  ;; data.y = alog10(data.y)
  ;; store_data,var_name, data=data
  options,var_name,'spec',1
  zlim,var_name,5e8,5e9,0
  ylim,var_name,0,360,0
  ;; options,var_name,'ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
  options,var_name,'ytitle','Electrons!C!CAngle (Deg.)'
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

     t1 = data.x[0]
     t2 = data.x[n_elements(data.x)-1L]

     if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
        if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
        if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
        get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
        get_new_igrf,/no_store_old
     endif

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT']
     endif

     sphRadius = 4e-2 ;in meters, from Ergun et al. [2001]
     sphCrossSec = !PI*sphRadius^2
     
     ;;Get EESA current
     GET_2DT_TS_POT,'j_2d_b','fa_eeb',t1=t1Zoom,t2=t2Zoom, $
                    name='Je_tot',energy=[0,energy_electrons[1]],sc_pot=sc_pot
     
     GET_DATA,'Je_tot',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     ;;NOTE: we here decide to make currents field-aligned.
     ;;That is, positive currents are along B; in SH, that means spaceward
     jeTotTmp_time  = tmp.x
     jeTotTmp       = tmp.y*1.6e-9*(-1.) ;;in microA/m2, and flip sign 
     

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IeTotTmp_time  = tmp.x[keep2]
     IeTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IeTotTmp      *= sphCrossSec

     Je_z           = {x:jeTotTmp_time,y:jeTotTmp}
     IeTot_z        = {x:IeTotTmp_time,y:IeTotTmp}
     STORE_DATA,'ESACur',DATA=IeTot_z
     OPTIONS,'ESACur','colors',250

     STORE_DATA,'Je_tot',DATA=Je_z

  IF KEYWORD_SET(save_B_and_J_data) OR KEYWORD_SET(ancillary_plots) THEN BEGIN
     UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

     PRINT,"Getting Ji current density fo' yeh'"
     GET_2DT_TS_POT,'j_2d_b','fa_ieb',t1=t1Zoom,t2=t2Zoom, $
                    name='Ji_tot',energy=[0,energy_ions[1]], $
                    angle=[180,360], $
                    sc_pot=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ies',t1=t1Zoom,t2=t2Zoom, $
                    name='Ji_tot_S',energy=[0,energy_ions[1]], $
                    angle=[180,360], $
                    sc_pot=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ees',t1=t1Zoom,t2=t2Zoom, $
                    name='Je_tot_S',energy=[0,energy_electrons[1]], $
                    sc_pot=sc_pot
     
     ;;First, burst ion data
     GET_DATA,'Ji_tot',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jiTotTmp_time  = tmp.x
     jiTotTmp       = tmp.y*1.6e-9*2. ;;in microA/m2, times 2 since half angle range

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IiTotTmp_time  = tmp.x[keep2]
     IiTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IiTotTmp      *= sphCrossSec

     Ji_z           = {x:jiTotTmp_time,y:jiTotTmp}
     IiTot_z        = {x:IiTotTmp_time,y:IiTotTmp}

     STORE_DATA,'Ji_tot',DATA=Ji_z

     ;;Now survey ESA ion data for patching the burst holes
     GET_DATA,'Ji_tot_S',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jiTotTmp_time  = tmp.x
     jiTotTmp       = tmp.y*1.6e-9*2. ;;in microA/m2, times 2 since half angle range

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IiTotTmp_time  = tmp.x[keep2]
     IiTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IiTotTmp      *= sphCrossSec

     Ji_z_S         = {x:jiTotTmp_time,y:jiTotTmp}
     IiTot_z_S      = {x:IiTotTmp_time,y:IiTotTmp}

     ;;Now electron ESA survey
     GET_DATA,'Je_tot_S',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jeTotTmp_time  = tmp.x
     jeTotTmp       = tmp.y*1.6e-9 ;;in microA/m2

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IeTotTmp_time  = tmp.x[keep2]
     IeTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IeTotTmp      *= sphCrossSec

     Je_z_S         = {x:jeTotTmp_time,y:jeTotTmp}
     IeTot_z        = {x:IeTotTmp_time,y:IeTotTmp}


  ENDIF

  IF KEYWORD_SET(save_B_AND_J_data) THEN BEGIN
     saveDir  = '/SPENCEdata/Research/Satellites/FAST/single_sc_wavevector/'
     saveFile = 'Chaston_et_al_2006--B_and_J.sav'
     saveFile = 'Chaston_et_al_2006--B_and_J--20161022--fixed_currents.sav'
     ;; B_J_file = 'Chaston_et_al_2006--B_and_J.dat'

     GET_DATA,'dB_fac_v',DATA=dB_fac_v
     GET_DATA,'dB_fac',DATA=dB_fac

     PRINT,'Saving ' + saveFile + ' ...'
     SAVE,Je_z,Ji_z, $
          Je_z_S,Ji_z_S, $
          dB_fac_v,dB_fac,FILENAME=saveDir+saveFile

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

; force tPlt_vars to be all the panels unless no_blank_panels is set

  if ~KEYWORD_SET(no_blank_panels) then begin


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

     tPlt_vars=['Iesa_Energy','Iesa_Angle','Ji_up','Eesa_Energy','Eesa_Angle', $
                 EFieldVar,ESpecVar,magVar,langVar]
  endif

  IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Chaston_et_al_2006'
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
           tLims = [t1Zoom,t2Zoom]
        END        
     ENDCASE

     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tLims
     TPLOT_PANEL,VARIABLE=langVar,OPLOTVAR='ESACur'

     IF KEYWORD_SET(add_timebar) THEN BEGIN
        TIMEBAR,timesBar,COLOR=!D.N_COLORS-4
     ENDIF

     IF KEYWORD_SET(ancillary_plots) THEN BEGIN
        options,'Ji_tot','ytitle','Ion Current!C(!4l!XA m!U2!N)' ; set y title
        options,'Ji_tot','panel_size',2                     ; set panel size

        options,'Je_tot','ytitle','e!U-!N Current!C(!4l!XA m!U2!N)' ; set y title
        options,'Je_tot','panel_size',2                     ; set panel size

        ancillary_vars = ['Je_tot','Ji_tot']

        IF ~(KEYWORD_SET(save_ps) OR KEYWORD_SET(save_png)) THEN BEGIN
           WINDOW,1,XSIZE=600,YSIZE=800
        ENDIF

        TPLOT,ancillary_vars,VAR=['ALT','ILAT','MLT'], $
              TRANGE=tLims, $
              WINDOW=(KEYWORD_SET(save_ps) OR KEYWORD_SET(save_png)) ? !NULL : -1

        IF KEYWORD_SET(add_timebar) THEN BEGIN
           TIMEBAR,timesBar,COLOR=!D.N_COLORS-4
        ENDIF

     ENDIF

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        PCLOSE
     ENDIF ELSE BEGIN

     ENDELSE

  ENDIF


  RETURN


END
