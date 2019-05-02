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

  inds = where((db_fac.x GE s2t(t1)) AND (db_fac.x LE s2t(t2)))
  db_fac_v = {x: db_fac_v.x[inds], $
              y: db_fac_v.y[inds,*]}
  db_fac   = {x: db_fac.x[inds], $
              y: db_fac.y[inds,*]}

  GET_FA_ORBIT,db_fac_v.x,/time_array,/all,struc=struct


  b = CREATE_STRUCT('time'        , magx.x        , $
                    'x'           , magx.y        , $
                    'y'           , magy.y        , $
                    'z'           , magz.y        , $
                    'orbit'       , struct.orbit  , $
                    'alt'         , struct.alt    , $
                    'ilat'        , struct.ilat   , $
                    'mlt'         , struct.mlt    , $
                    'ilng'        , struct.ilng   , $
                    'fa_pos_gei'  , struct.fa_pos , $
                    'fa_vel_gei'  , struct.fa_vel , $
                    'b_model_gei' , struct.b_model, $
                    'bfoot_gei'   , struct.bfoot  , $
                    'lat_geo'     , struct.lat    , $
                    'lng_geo'     , struct.lng    , $
                    'flat_geo'    , struct.flat   , $
                    'flng_geo'    , struct.flng   )
                    

  db = CREATE_STRUCT('fac_v'       ,db_fac_v.y    , $
                     'fac'         ,db_fac.y      , $
                     'time'        ,struct.time)

  dbFileName = 'dB_fac__ORBIT_1906__1997-02-13__16_49_50-16_50_08.sav'
  SAVE,b,db,FILENAME=dbFileName


  spacecraft_potential=get_fa_fields('V8_S',t1,t2)

  ;;get the spacecraft potential per spin
  spin_period=4.946  ; seconds
  
  ;;get_sample_rate
  v8={x:spacecraft_potential.time,y:spacecraft_potential.comp1}

  v8_dt=abs(v8.x-shift(v8.x,-1))
  v8_dt(0)=v8_dt(1)
  v8_dt(n_elements(v8.x)-1)=v8_dt(n_elements(v8.x)-2)

  ;;get maxima within a 1 spin window
  j_range=where(v8.x LT v8.x(n_elements(v8.x)-1)-spin_period)
  index_max=max(j_range)
  print,index_max
  pot=make_array(n_elements(v8.x),/double)
  for j=0L,index_max do begin
     ;;spin_range=where(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
     spin_range=j+findgen(ceil(spin_period/V8_dt(j)))
     pot(j)=max(abs(v8.y(spin_range)),ind)
     sign=v8.y(spin_range(ind))/abs(v8.y(spin_range(ind)))
     pot(j)=sign*pot(j)
                     ;print,j,pot(j)
  endfor
  pot(index_max+1:n_elements(v8.x)-1)=pot(j_range(index_max))
  sc_pot={x:v8.x,y:pot}
  store_data,'S_Pot',data=sc_pot ;note this is actualy the negative of the sp. potential this corrected in the file output


  if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  if not keyword_set(energy_ions) then energy_ions=[0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

  get_data,'ALT',data=alt
  loss_cone_alt=alt.y(0)*1000.0
  lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI
  get_data,'ILAT',data=ilat
  north_south=abs(ilat.y(0))/ilat.y(0)
  
  if north_south EQ -1 then begin
     e_angle=[180.-lcw,180+lcw] ; for Southern Hemis.
     ;;i_angle=[270.0,90.0]	
     ;;elimnate ram from data
     i_angle=[180.0,360.0]
     i_angle_up=[270.0,360.0]
     
  endif else begin
     e_angle=[360.-lcw,lcw]     ;	for Northern Hemis.
     ;;i_angle=[90.,270.0]
     ;;eliminate ram from data
     i_angle=[0.0,180.0]
     i_angle_up=[90.0,180.0]
     
  endelse

  t1d = s2t(t1)
  t2d = s2t(t2)

  get_2dt_ts,'je_2d_b','fa_eeb',t1=t1d,t2=t2d, $
             name='JEe_tot_b',energy=energy_electrons,/CALIB
  get_2dt_ts,'je_2d_b','fa_eeb',t1=t1d,t2=t2d, $
             name='JEe_b',angle=e_angle,energy=energy_electrons,/CALIB
  get_2dt_ts,'j_2d_b','fa_eeb',t1=t1d,t2=t2d, $
             name='Je_b',energy=energy_electrons,/CALIB
  get_2dt_ts,'j_2d_b','fa_eeb',t1=t1d,t2=t2d, $
             name='Je_lc_b',energy=energy_electrons,angle=e_angle,/CALIB
  
  get_2dt_ts,'je_2d_b','fa_ieb',t1=t1d,t2=t2d, $
             name='JEi_b',energy=energy_ions,/CALIB
  get_2dt_ts,'j_2d_b','fa_ieb',t1=t1d,t2=t2d, $
             name='Ji_b',energy=energy_ions,/CALIB
  get_2dt_ts,'je_2d_b','fa_ieb',t1=t1d,t2=t2d, $
             name='JEi_up_b',energy=energy_ions,angle=i_angle,/CALIB
  get_2dt_ts,'j_2d_b','fa_ieb',t1=t1d,t2=t2d, $
             name='Ji_up_b',energy=energy_ions,angle=i_angle,/CALIB

  get_2dt_ts_pot,'je_2d_b','fa_ees',t1=t1d,t2=t2d, $
                 name='JEe_tot_s',energy=energy_electrons,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'je_2d_b','fa_ees',t1=t1d,t2=t2d, $
                 name='JEe_s',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'j_2d_b','fa_ees',t1=t1d,t2=t2d, $
                 name='Je_s',energy=energy_electrons,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'j_2d_b','fa_ees',t1=t1d,t2=t2d, $
                 name='Je_lc_s',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot,/CALIB
  
  get_2dt_ts_pot,'je_2d_b','fa_ies',t1=t1d,t2=t2d, $
                 name='JEi_s',energy=energy_ions,angle=i_angle,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'j_2d_b','fa_ies',t1=t1d,t2=t2d, $
                 name='Ji_s',energy=energy_ions,angle=i_angle,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'je_2d_b','fa_ies',t1=t1d,t2=t2d, $
                 name='JEi_up_s',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot,/CALIB
  get_2dt_ts_pot,'j_2d_b','fa_ies',t1=t1d,t2=t2d, $
                 name='Ji_up_s',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot,/CALIB


  GET_DATA,'JEe_tot_b',DATA=JEe_tot_b
  GET_DATA,'JEe_b',DATA=JEe_b
  GET_DATA,'Je_b',DATA=Je_b
  GET_DATA,'Je_lc_b',DATA=Je_lc_b

  GET_DATA,'JEi_b',DATA=JEi_b
  GET_DATA,'Ji_b',DATA=Ji_b
  GET_DATA,'JEi_up_b',DATA=JEi_up_b
  GET_DATA,'Ji_up_b',DATA=Ji_up_b

  GET_DATA,'JEe_tot_s',DATA=JEe_tot_s
  GET_DATA,'JEe_s',DATA=JEe_s
  GET_DATA,'Je_s',DATA=Je_s
  GET_DATA,'Je_lc_s',DATA=Je_lc_s

  GET_DATA,'JEi_s',DATA=JEi_s
  GET_DATA,'Ji_s',DATA=Ji_s
  GET_DATA,'JEi_up_s',DATA=JEi_up_s
  GET_DATA,'Ji_up_s',DATA=Ji_up_s

  el_b = CREATE_STRUCT('time'   ,JEe_tot_b.x, $
                       'JE'    ,JEe_tot_b.y, $
                       'JE_lc' ,JEe_b.y, $
                       'J'     ,Je_b.y, $
                       'J_lc'  ,Je_lc_b.y)

  ion_b = CREATE_STRUCT('time'  , JEi_b.x, $
                        'JE',JEi_b.y, $
                        'J',Ji_b.y, $
                        'JE_up',JEi_up_b.y, $
                        'J_up',Ji_up_b.y)

  el_s = CREATE_STRUCT('time'   ,JEe_tot_s.x, $
                       'JE'    ,JEe_tot_s.y, $
                       'JE_lc' ,JEe_s.y, $
                       'J'     ,Je_s.y, $
                       'J_lc'  ,Je_lc_s.y)

  ion_s = CREATE_STRUCT('time'  , JEi_s.x, $
                        'JE',JEi_s.y, $
                        'J',Ji_s.y, $
                        'JE_up',JEi_up_s.y, $
                        'J_up',Ji_up_s.y)

  ptclFileName = 'ptcls__ORBIT_1906__1997-02-13__16_49_50-16_50_08.sav'
  SAVE,el_b,ion_b,el_s,ion_s,FILENAME=ptclFileName


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
