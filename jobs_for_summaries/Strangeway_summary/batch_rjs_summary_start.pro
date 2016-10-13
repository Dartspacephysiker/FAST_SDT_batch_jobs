pro batch_rjs_summary_start,tplot_vars=tplot_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all

; create a summary plot of:
; SFA (AKR)
; DSP (VLF)
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; E fit along V (Southern hemisphere corrected)
; dB_fac_v (dB_SM also stored)

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

; Input needed on:
; (a) Northern/southern hemisphere limits
; (b) ESA data limits
; (c) DSP calibration


; Under development - R. J. Strangeway 2/19/08

; Step 1 - DC Mag data
; Temporarily disable for orbits later than 9936

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
  store_data,'dB_fac',/delete
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

endif

; step 2 - E field


prog = getenv('FASTBIN') + '/showDQIs'
spawn, prog, result, /noshell
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

  fa_fields_despin,v58,v12,/shadow_notch,/sinterp

  options,'EFIT_ALONG_V','yrange',0
  options,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
  options,'EFIT_ALONG_V','panel_size',2

; reset time limits if needed

  get_data,'EFIT_ALONG_V',data=data
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
  rxv = vector_cross_product(pos.y,vel.y)
  vxb = vector_cross_product(vel.y,bm.y)
  tst = vector_dot_product(rxv,vxb)

  get_data,'EFIT_ALONG_V',data=data,dlimit=dlimit
  y2=spl_init(pos.x-tlimit_all[0],tst,/double)
  tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
  data.y = data.y*tst_/abs(tst_)
  store_data,'EFIT_ALONG_VSC',data=data,dlimit=dlimit
  options,'EFIT_ALONG_VSC','yrange',0
  options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
  options,'EFIT_ALONG_VSC','panel_size',2

  store_data,'E_NEAR_B',/delete
  store_data,'E_ALONG_V',/delete
  store_data,'EFIT_NEAR_B',/delete
  store_data,'EFIT_ALONG_V',/delete

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['EFIT_ALONG_VSC'] else tplot_vars=['EFIT_ALONG_VSC',tplot_vars]

endif


; Step 3 - Iesa data

prog = getenv('FASTBIN') + '/showDQIs'
spawn, prog, result, /noshell
b = where (strpos(result,'Iesa Survey') ge 0,nesa)
if (nesa gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nesa = 0

if (nesa gt 0) then begin

; ION PITCH ANGLE

  var_name='Iesa_Angle'
  get_pa_spec,"fa_ies_c",units='eflux',name=var_name,energy=[4.,30000.]
  get_data,var_name, data=data
  data.y = alog10(data.y)
  store_data,var_name, data=data
	options,var_name,'spec',1	
	zlim,var_name,4,9,0
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

  if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[var_name,tplot_vars]

; reset time limits if needed

  t1 = data.x[0]
  t2 = data.x[n_elements(data.x)-1L]

  if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
    if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
    if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
    get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
    get_new_igrf,/no_store_old
  endif

; ION ENERGY 

  var_name='Iesa_Energy'
  get_en_spec,'fa_ies_c',name=var_name, units='eflux'
  get_data,var_name, data=data	; Remove acne.
  data.y = alog10(data.y>1.e2)		; Remove acne.
  store_data,var_name, data=data	; Remove acne.
	options,var_name,'spec',1	
	zlim,var_name,4,9,0
	ylim,var_name,4,30000,1
	options,var_name,'ytitle','Ions!C!CEnergy (eV)'
	options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
	options,var_name,'x_no_interp',1
	options,var_name,'y_no_interp',1
	options,var_name,'panel_size',2

  if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[var_name,tplot_vars]

endif


; Step 4 - Eesa data

prog = getenv('FASTBIN') + '/showDQIs'
spawn, prog, result, /noshell
b = where (strpos(result,'Eesa Survey') ge 0,nesa)
if (nesa gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nesa = 0

if (nesa gt 0) then begin

; ELECTRON PITCH ANGLE

  var_name='Eesa_Angle'
  get_pa_spec,"fa_ees_c",units='eflux',name=var_name, energy=[10.,30000.]
  get_data,var_name, data=data 
  data.y = alog10(data.y)
  store_data,var_name, data=data
        options,var_name,'spec',1
        zlim,var_name,4,9,0
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

  if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[var_name,tplot_vars]

; reset time limits if needed

  t1 = data.x[0]
  t2 = data.x[n_elements(data.x)-1L]

  if ((t1 lt tlimit_all[0]) or (t2 gt tlimit_all[1])) then begin
    if (t1 lt tlimit_all[0]) then tlimit_all[0] = t1
    if (t2 gt tlimit_all[1]) then tlimit_all[1] = t2
    get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
    get_new_igrf,/no_store_old
  endif

; ELECTRON ENERGY

  var_name='Eesa_Energy'
  get_en_spec,'fa_ees_c',name=var_name,units='eflux'
  get_data,var_name, data=data
  data.y = alog10(data.y)
  store_data,var_name, data=data
	options,var_name,'spec',1	
	zlim,var_name,4,9,0
	ylim,var_name,5,30000,1
	options,var_name,'ytitle','Electrons!C!CEnergy (eV)'
	options,var_name,'ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
	options,var_name,'x_no_interp',1
	options,var_name,'y_no_interp',1
	options,var_name,'panel_size',2

  if (n_elements(tplot_vars) eq 0) then tplot_vars=[var_name] else tplot_vars=[var_name,tplot_vars]

endif


; Step 5 - VLF data


; DSP_V5-V8HG or DSP_V5-V8

prog = getenv('FASTBIN') + '/showDQIs'
spawn, prog, result, /noshell
b = where (strpos(result,'DspADC_V5-V8HG') ge 0,ndsphg)
if (ndsphg gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsphg = 0
b = where ((strpos(result,'DspADC_V5-V8') ge 0) and (strpos(result,'DspADC_V5-V8HG') lt 0),ndsp)
if (ndsp gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then ndsp = 0

if (ndsphg gt 0) then dat=get_fa_fields('DspADC_V5-V8HG',/all) else if (ndsp gt 0) then dat=get_fa_fields('DspADC_V5-V8',/all)
ndsp = (ndsp gt 0) or (ndsphg gt 0)

if (ndsp) then begin
  data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
  store_data,'DSP_V5-V8', data=data
  dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
          ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
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
endif

; Step 5 - AKR data

prog = getenv('FASTBIN') + '/showDQIs'
spawn, prog, result, /noshell
b = where (strpos(result,'SfaAve_V5-V8') ge 0,nakr)
if (nakr gt 0) then if strpos(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nakr = 0

if (nakr gt 0) then begin

  dat = get_fa_fields('SfaAve_V5-V8', /all)
  data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
  store_data,'SFA_V5-V8', data=data
  dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $
            ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
            ztitle: '(V/m)!U2!N/Hz', panel_size:2}
  store_data,'SFA_V5-V8', dlimit=dlimit
  options,'SFA_V5-V8','x_no_interp',1
  options,'SFA_V5-V8','y_no_interp',1

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

  if (n_elements(tplot_vars) eq 0) then tplot_vars=['SFA_V5-V8'] else tplot_vars=['SFA_V5-V8',tplot_vars]

endif

; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south. also change plot title

get_data,'LAT',data=data

if (n_elements(data.y) le 0) then return

bb = where (data.y gt 10,nn)
if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

bb = where (data.y lt -10,nn)
if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

get_data,'ORBIT',data=data
nn = n_elements(data.y)/2
orbit = data.y(nn)
orbit_lab = strcompress(string(orbit,format="(i5.4)"),/remove_all)
tplot_options,'title','FAST Orbit ' + orbit_lab


return
end

