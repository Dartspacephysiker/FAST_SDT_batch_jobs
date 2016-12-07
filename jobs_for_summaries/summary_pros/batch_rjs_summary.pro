pro batch_rjs_summary,tPlt_vars=tPlt_vars, $
tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all, $
screen_plot=screen_plot,use_fac_v=use_fac_v,use_fac=use_fac,no_blank_panels=no_blank_panels, $
FORCE_PAST_9936=force_past_9936,SKIP_EXISTING=skip_existing,GOT_SKIPPED=got_skipped,PLOTDIR=plotDir, $
burst=burst

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

upper_zlim_elec = 9
upper_zlim_ion  = 9

ees_or_eeb = 'ees'
IF KEYWORD_SET(burst) THEN ees_or_eeb = 'eeb'

@tplot_com

nn = n_elements(data_quants)

if (nn gt 1) then for n = nn-1L,1L,-1L do store_data,data_quants(n).name,/delete

; Step 1 - DC Mag data

ucla_mag_despin,tw_mat=tw_mat,orbit=orbit,spin_axis=spin_axis,delta_phi=delta_phi

if (n_elements(orbit) gt 0) then begin

;  if orbit > 9936 return (temporary fix)

   IF KEYWORD_SET(skip_existing) THEN BEGIN

      hemisph = getenv('FAST_ORBIT_HEMISPHERE')
      out_fname=(KEYWORD_SET(plotDir) ? plotDir : '')+'esummary_orbit_'+STRCOMPRESS(orbit,/REMOVE_ALL)+'_'+hemisph+'.plt.ps'
      IF FILE_TEST(out_fname) THEN BEGIN
         PRINT,"File exists: " + out_fname + '! Skipping ...'
         got_skipped = 1
         RETURN
      ENDIF ELSE got_skipped = 0

   ENDIF

  if (orbit gt 9936) AND ~KEYWORD_SET(force_past_9936) then begin

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
  t1 = data.x[0]
  t2 = data.x[n_elements(data.x)-1L]
  tlimit_all = [t1,t2]
  tPlt_vars = 'dB_fac_v'
  options,'dB_fac_v','panel_size',2
  options,'dB_fac','panel_size',2
  options,'dB_sm','panel_size',2

  if (keyword_set(use_fac)) then tPlt_vars = 'dB_fac'

  if (not keyword_set(no_blank_panels)) then tPlt_vars = 'dB_fac_v'

  if (keyword_set(screen_plot)) then begin
    loadct2,40
    tplot,tPlt_vars,var=['ALT','ILAT','MLT']
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
  ;; options,'EFIT_ALONG_VSC','yrange',0
  options,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
  options,'EFIT_ALONG_VSC','panel_size',2

  store_data,'E_NEAR_B',/delete
  store_data,'E_ALONG_V',/delete
  store_data,'EFIT_NEAR_B',/delete
  store_data,'EFIT_ALONG_V',/delete

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['EFIT_ALONG_VSC'] else tPlt_vars=['EFIT_ALONG_VSC',tPlt_vars]

  if (keyword_set(screen_plot)) then begin
    loadct2,40
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
  get_pa_spec,"fa_ies_c",units='eflux',name=var_name,energy=[4.,30000.]
  get_data,var_name, data=data
  data.y = alog10(data.y)
  store_data,var_name, data=data
	options,var_name,'spec',1	
	zlim,var_name,4,upper_zlim_ion,0
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

; ION ENERGY 

  var_name='Iesa_Energy'
  get_en_spec,'fa_ies_c',name=var_name, units='eflux'
  get_data,var_name, data=data
  data.y = alog10(data.y)
  store_data,var_name, data=data
	options,var_name,'spec',1	
	zlim,var_name,4,upper_zlim_ion,0
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
  get_pa_spec,"fa_" + ees_or_eeb + "_c",units='eflux',name=var_name,energy=[10.,30000.]
  get_data,var_name, data=data 
  data.y = alog10(data.y)
  store_data,var_name, data=data
        options,var_name,'spec',1
        zlim,var_name,4,upper_zlim_elec,0
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

; ELECTRON ENERGY

  var_name='Eesa_Energy'
  get_en_spec,'fa_' + ees_or_eeb + '_c',name=var_name,units='eflux'
  get_data,var_name, data=data
  data.y = alog10(data.y)
  store_data,var_name, data=data
	options,var_name,'spec',1	
	zlim,var_name,4,upper_zlim_elec,0
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
 
  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['DSP_V5-V8'] else tPlt_vars=['DSP_V5-V8',tPlt_vars]

  if (keyword_set(screen_plot)) then begin
    loadct2,40
    tplot,tPlt_vars,var=['ALT','ILAT','MLT']
  endif

endif else begin

endelse

; Step 5 - AKR data

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

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=['SFA_V5-V8'] else tPlt_vars=['SFA_V5-V8',tPlt_vars]

  if (keyword_set(screen_plot)) then begin
    loadct2,40
    tplot,tPlt_vars,var=['ALT','ILAT','MLT']
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
	zlim,'Eesa_Energy',4,upper_zlim_elec,0
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
    zlim,'Eesa_Angle',4,upper_zlim_elec,0
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
	zlim,'Iesa_Energy',4,upper_zlim_elec,0
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
    zlim,'Iesa_Angle',4,upper_zlim_elec,0
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

  tPlt_vars=['SFA_V5-V8','DSP_V5-V8','Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','EFIT_ALONG_VSC','dB_fac_v']

endif

if (keyword_set(screen_plot)) then begin
  loadct2,40
  tplot,tPlt_vars,var=['ALT','ILAT','MLT']
endif


return
end

