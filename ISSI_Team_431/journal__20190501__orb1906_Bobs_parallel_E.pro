;2019/05/01
PRO JOURNAL__20190501__ORB1906_BOBS_PARALLEL_E

  COMPILE_OPT IDL2,STRICTARRSUBS

  screen_plot = 1

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

  ;; V1-V2HG_16k
  ;; V5-V8HG_16k

  get_EField_16kHG = 1
  get__16kHG = 1

  ;; t1 = '1997-02-13/16:49:50'
  ;; t2 = '1997-02-13/16:50:08'
  t1 = '1997-02-13/16:49:56'
  t2 = '1997-02-13/16:50:04'
  tlimit_all = [t1,t2]

  IF get_EField_16kHG THEN BEGIN

     ;; Get the E-field data
     FA_FIELDS_DESPIN_HG,t1=t1,t2=t2

     GET_DATA,'E_NEAR_B_HG',data=ENB
     GET_DATA,'E_ALONG_V_HG',data=EAV

     EFileName = 'ENB_EAV__ORBIT_1906__1997-02-13__16_49_50-16_50_08.sav'
     PRINT,"Saving to "+EFileName
     SAVE,ENB,EAV,FILENAME=EFileName

  ENDIF

  field = get_fa_fields('MagDC',t1,t2,/store)
  get_data,'MagDCcomp1',data=magx
  get_data,'MagDCcomp2',data=magy
  get_data,'MagDCcomp3',data=magz

  UCLA_MAG_DESPIN

  ;; Get despun, detrended survey mag field in spacecraft-based field-aligned coordinates
  get_data,'dB_fac_v',DATA=db_fac_v

  ;; Get despun, detrended survey mag field in "out," "east," "along-B" field-aligned coordinates
  get_data,'dB_fac',DATA=db_fac

  GET_FA_ORBIT,db_fac_v.x,/time_array,/all,struc=struct

  db = CREATE_STRUCT('fac_v'       ,db_fac_v.y    , $
                     'fac'         ,db_fac.y      , $
                     'time'        ,struct.time   , $
                     'orbit'       ,struct.orbit  , $
                     'alt'         ,struct.alt    , $
                     'ilat'        ,struct.ilat   , $
                     'mlt'         ,struct.mlt    , $
                     'ilng'        ,struct.ilng   , $
                     'fa_pos_gei'  ,struct.fa_pos , $
                     'fa_vel_gei'  ,struct.fa_vel , $
                     'b_model_gei' ,struct.b_model, $
                     'bfoot_gei'   ,struct.bfoot  , $
                     'lat_geo'     ,struct.lat    , $
                     'lng_geo'     ,struct.lng    , $
                     'flat_geo'    ,struct.flat   , $
                     'flng_geo'    ,struct.flng   )


  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['E_NEAR_B_HG'] else tPlt_vars=['E_NEAR_B_HG',tPlt_vars]
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['E_ALONG_V_HG'] else tPlt_vars=['E_ALONG_V_HG',tPlt_vars]

  tplot,tPlt_vars,var=['ALT','ILAT','MLT']

  ees_or_eeb = 'eeb'

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
  if (nb gt 0) then data.v[bb]=data.v[bb]-360.
  nn = n_elements(data.x)
  for n = 0,nn-1L do begin & $
    bs = sort (data.v[n,*]) & $
    data.v[n,*]=data.v[n,bs] & $
    data.y[n,*]=data.y[n,bs] & $
  endfor
  store_data,var_name, data=data
  options,var_name,'yminor',9
  options,var_name,'yticks',4
  options,var_name,'ytickv',[-90,0,90,180,270]
  ylim,var_name,-90,270,0

  if (n_elements(tPlt_vars) eq 0) then tPlt_vars=[var_name] else tPlt_vars=[var_name,tPlt_vars]

; reset time limits if needed

  ;; t1 = data.x[0]
  ;; t2 = data.x[n_elements(data.x)-1L]

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

  upper_zlim_elec = 9
  upper_zlim_ion  = 9

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
    tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=tlimit_all
  endif




END
