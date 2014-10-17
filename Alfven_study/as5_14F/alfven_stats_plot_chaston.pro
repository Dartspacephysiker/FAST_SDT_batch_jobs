pro alfven_stats_plot_chaston,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
  t1=t1,t2=t2,filterfreq=filterfreq,$
  burst=burst,ucla_mag_despin=ucla_mag_despin

  ;thresholds for inclusion as Alfven waves

  current_threshold=1.0;microA/m^2
  delta_b_threshold=5.0; nT
  delta_E_threshold=10.0 ; mV/m
  esa_j_delta_bj_ratio_threshold=0.02
  electron_eflux_ionos_threshold=0.05;ergs/cm^2/s
  eb_to_alfven_speed=10.0; factor by which the event can differ from model Alfven speed and still be called an Alfven wave
  ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  ;energy ranges

  if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.];use 0.0 for lower bound since the sc_pot is used to set this
  if not keyword_set(energy_ions) then energy_ions=[0.,500.];use 0.0 for lower bound since the sc_pot is used to set this

  t=0
  dat = get_fa_ees(t,/st)
  if dat.valid eq 0 then begin
    print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
    return
  endif

  ; Electron current - line plot

  if keyword_set(burst) then begin
    get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endif else begin
    get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endelse

  ;remove spurious crap

  get_data,'Je',data=tmpj

  keep=where(finite(tmpj.y) NE 0)
  tmpj.x=tmpj.x(keep)
  tmpj.y=tmpj.y(keep)

  keep=where(abs(tmpj.y) GT 0.0)
  tx=tmpj.x(keep)
  ty=tmpj.y(keep)

  ;get timescale monotonic

  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)


  ;throw away the first 10  points since they are often corrupted
  if not keyword_set(burst) then begin
    store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
  endif else begin
    store_data,'Je',data={x:tx,y:ty}
  endelse

  ;eliminate data from latitudes below the Holzworth/Meng auroral oval

  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x

  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)

  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  ;Use the electron data to define the time ranges for this orbit

  get_data,'Je',data=je
  part_res_je=make_array(n_elements(Je.x),/double)
  for j=1,n_elements(Je.x)-1 do begin
    part_res_je(j)=abs(Je.x(j)-Je.x(j-1))
  endfor
  part_res_Je(0)=part_res_Je(1)
  gap=where(part_res_je GT 10.0)
  if gap(0) NE -1 then begin
    separate_start=[0,where(part_res_je GT 10.0)]
    separate_stop=[where(part_res_je GT 10.0),n_elements(Je.x)-1]
  endif else begin
    separate_start=[0]
    separate_stop=[n_elements(Je.x)-1]
  endelse


  ;remove esa burp when switched on
  if not keyword_set(burst) then begin
    turn_on=where(part_res_je GT 300.0)
    if turn_on(0) NE -1 then begin
      turn_on_separate=make_array(n_elements(turn_on),/double)
      for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
      separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
    endif
  endif
  ;identify time indices for each interval

  count=0.0
  for j=0,n_elements(separate_start)-1 do begin
    if (separate_stop(j)-separate_start(j)) GT 10 then begin
      count=count+1
      if count EQ 1.0 then begin
        time_range_indices=transpose([separate_start(j)+1,separate_stop(j)-1])
      endif else begin
        time_range_indices=[time_range_indices,transpose([separate_start(j),separate_stop(j)-1])]
      endelse
    endif
  endfor


  ;identify interval times

  time_ranges=je.x(time_range_indices)
  number_of_intervals=n_elements(time_ranges(*,0))

  print,'number_of_intervals',number_of_intervals

  ;loop over each time interval
  ji_tot=make_array(number_of_intervals,/double)
  ji_up_tot=make_array(number_of_intervals,/double)
  jee_tot=make_array(number_of_intervals,/double)
  Ji_tot_alf=make_array(number_of_intervals,/double)
  Ji_up_tot_alf=make_array(number_of_intervals,/double)
  Jee_tot_alf=make_array(number_of_intervals,/double)


  ;get despun mag data if keyword set
  if keyword_set(ucla_mag_despin) then ucla_mag_despin


  ;begin looping each interval

  for jjj=0,number_of_intervals-1 do begin
    print,'time_range',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))

    je_tmp_time=je.x(time_range_indices(jjj,0):time_range_indices(jjj,1))
    je_tmp_data=je.y(time_range_indices(jjj,0):time_range_indices(jjj,1))

    store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}

    ;get fields quantities
    data_valid=1.0
    dat=get_fa_fields('MagDC',t,/start)
    if dat.valid eq 0 then begin
      print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
      data_valid=0.0
    endif else begin
      if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_ranges(jjj,0),time_ranges(jjj,1),/store)
      dat=get_fa_fields('V5-V8_S',t,/start)
      if dat.valid eq 0 then begin
        print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
        data_valid=0.0
      endif else begin
        spacecraft_potential=get_fa_fields('V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
        efieldV58=get_fa_fields('V5-V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
        efieldV1214=get_fa_fields('V1-V2_S',time_ranges(jjj,0),time_ranges(jjj,1))
        if efieldV1214.valid eq 0 then begin
          print,'No V1-V2 data - trying V1-V4'
          efieldV1214=get_fa_fields('V1-V4_S',time_ranges(jjj,0),time_ranges(jjj,1))
          if efieldV1214.valid eq 0 then begin
            print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
            data_valid=0.0
          endif
        endif
      endelse

      Langmuir_2=get_fa_fields('NE2_S',time_ranges(jjj,0),time_ranges(jjj,1))
      Langmuir_6=get_fa_fields('NE6_S',time_ranges(jjj,0),time_ranges(jjj,1))
      Langmuir_9=get_fa_fields('NE9_S',time_ranges(jjj,0),time_ranges(jjj,1))
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
      if n_elements(langmuir_data) GT 1 then  begin
        langmuir_time=langmuir_time(1:n_elements(Langmuir_time)-1)
        langmuir_data=langmuir_data(1:n_elements(Langmuir_time)-1)
        langmuir_prob=langmuir_prob(1:n_elements(Langmuir_time)-1)
        time_order_langmuir=sort(langmuir_time)
        langmuir={x:langmuir_time(time_order_langmuir),y:langmuir_data(time_order_langmuir)}
        dens_probe={x:langmuir_time(time_order_langmuir),y:langmuir_prob(time_order_langmuir)}
      endif else data_valid=0.0
    endelse

    if data_valid NE 0.0 then begin

      ;get E field and B field on same time scale


      ;;      efields_combine=combinets({x:efieldV1214.time,y:efieldV1214.comp1},{x:efieldV58.time,y:efieldV58.comp1})
      FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk

      ;get magnitude of electric and magnetic field

      ;; for k=0,10,1 do begin
      ;;    print, "This is efieldV1214.comp1["+string(k)+"]: " + string(efieldV1214.comp1[k])
      ;;    print, "This is efieldV58.comp1["+string(k)+"]: " + string(efieldV58.comp1[k])
      ;;    print, "This is efields_combine["+string(k)+"]: " + string(efields_combine[k])
      ;; endfor
      ;; help, efieldV1214,/str
      ;; help, efieldV58,/str
      ;; help,efields_combine
      efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
      if not keyword_set(ucla_mag_despin) then begin
        get_data,'MagDCcomp1',data=magx
        get_data,'MagDCcomp2',data=magy
        get_data,'MagDCcomp3',data=magz
      endif else begin
        get_data,'dB_fac_v',data=db_fac
        mintime=min(abs(time_ranges(jjj,0)-db_fac.x),ind1)
        mintime=min(abs(time_ranges(jjj,1)-db_fac.x),ind2)

        magx={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,0)}
        magy={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,2)}
        magz={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,1)}
      endelse

      store_data,'MagZ',data=magz
      ;magz.y=smooth(magz.y,40)
      store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
      if keyword_set(filterfreq) then begin

        magz=filter(magz,filterfreq,'magfilt','l')
        ;remove end effects of the filter by cutting off the first/last 2s
        sf=magz.x(1)-magz.x(0)
        np=n_elements(magz.x)
        padding=round(2.0/sf)
        magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
        store_data,'MagZ',data=magz
      endif


      ;get mag and efield data on same time scale
      ;SMH Try this to make fa_fields_combine stop crying
      magz={time:magz.x,comp1:magz.y,ncomp:1}
      efield={time:efield.x,comp1:efield.y}


      ;; fields=combinets(magz,efield)
      FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
      fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

      ;I'm hoping this means magz is pared down somewhere else

      ;; dens=combinets(magz,langmuir)
      langmuir={time:langmuir.x,comp1:langmuir.y,ncomp:1}
      FA_FIELDS_COMBINE,magz,langmuir,result=dens,/talk
      dens={time:magz.time,comp1:magz.comp1,comp2:dens,ncomp:2}

      magz={x:magz.time,y:magz.comp1}
      langmuir={x:langmuir.time,y:langmuir.comp1}

      ;get the prootn cyc frequency for smoothing the e field data later

      proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI); in Hz

      ;get_orbit data

      get_fa_orbit,je_tmp_time,/time_array,/all

      ;define loss cone angle

      get_data,'ALT',data=alt
      loss_cone_alt=alt.y(0)*1000.0
      lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI
      get_data,'ILAT',data=ilat
      north_south=abs(ilat.y(0))/ilat.y(0)

      if north_south EQ -1 then begin
        e_angle=[180.-lcw,180+lcw]; for Southern Hemis.
        ;i_angle=[270.0,90.0]
        ;elimnate ram from data
        i_angle=[180.0,360.0]
        i_angle_up=[270.0,360.0]

      endif else begin
        e_angle=[360.-lcw,lcw]; for Northern Hemis.
        ;i_angle=[90.,270.0]
        ;eliminate ram from data
        i_angle=[0.0,180.0]
        i_angle_up=[90.0,180.0]

      endelse


      ;get fields mode

      fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))



      ;get the spacecraft potential per spin

      spin_period=4.946; seconds

      ;get_sample_rate

      v8={x:spacecraft_potential.time,y:spacecraft_potential.comp1}

      v8_dt=abs(v8.x-shift(v8.x,-1))
      v8_dt(0)=v8_dt(1)
      v8_dt(n_elements(v8.x)-1)=v8_dt(n_elements(v8.x)-2)

      ;get maxima within a 1 spin window

      j_range=where(v8.x LT v8.x(n_elements(v8.x)-1)-spin_period)
      index_max=max(j_range)
      print,index_max
      pot=make_array(n_elements(v8.x),/double)
      for j=0L,index_max do begin
        ;spin_range=where(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
        spin_range=j+findgen(ceil(spin_period/V8_dt(j)))
        pot(j)=max(abs(v8.y(spin_range)),ind)
        sign=v8.y(spin_range(ind))/abs(v8.y(spin_range(ind)))
        pot(j)=sign*pot(j)
        ;print,j,pot(j)
      endfor
      pot(index_max+1:n_elements(v8.x)-1)=pot(j_range(index_max))
      sc_pot={x:v8.x,y:pot}
      store_data,'S_Pot',data=sc_pot;note this is actualy the negative of the sp. potential this corrected in the file output





      if keyword_set(burst) then begin

        get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe_tot',energy=energy_electrons
        get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe',angle=e_angle,energy=energy_electrons
        get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je',energy=energy_electrons
        get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je_lc',energy=energy_electrons,angle=e_angle


        get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi',energy=energy_ions
        get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji',energy=energy_ions
        get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi_up',energy=energy_ions,angle=i_angle
        get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji_up',energy=energy_ions,angle=i_angle

      endif else begin

        get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe_tot',energy=energy_electrons,sc_pot=sc_pot
        get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot
        get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je',energy=energy_electrons,sc_pot=sc_pot
        get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je_lc',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot


        get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
        get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
        get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
        get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot


        ;SNIP #3 starting at
        ;if keyword_set(heavy) then begin

        ;SNIP #3 ending at
        ;  get_2dt_pot,'j_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Jh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
        ;
        ;endif

      endelse

      get_data,'Je',data=tmp
      get_data,'Ji',data=tmpi
      ;remove crap
      keep1=where(finite(tmp.y) NE 0 and finite(tmpi.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      keep2=where(abs(tmp.y) GT 0.0 and abs(tmpi.y) GT 0.0)
      je_tmp_time=tmp.x(keep2)
      je_tmp_data=tmp.y(keep2)
      store_data,'Je',data={x:je_tmp_time,y:je_tmp_data}

      get_data,'JEe',data=tmp
      ;remove crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      jee_tmp_time=tmp.x(keep2)
      jee_tmp_data=tmp.y(keep2)
      store_data,'JEe',data={x:jee_tmp_time,y:jee_tmp_data}

      get_data,'JEe_tot',data=tmp
      ;remove crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      jee_tot_tmp_time=tmp.x(keep2)
      jee_tot_tmp_data=tmp.y(keep2)
      store_data,'JEe_tot',data={x:jee_tot_tmp_time,y:jee_tot_tmp_data}

      get_data,'Je_lc',data=tmp
      ;remove_crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      je_lc_tmp_time=tmp.x(keep2)
      je_lc_tmp_data=tmp.y(keep2)
      store_data,'Je_lc',data={x:je_lc_tmp_time,y:je_lc_tmp_data}



      get_data,'Ji',data=tmp
      ;remove crap
      ;keep1=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep2=where(abs(tmp.y) GT 0.0)
      ji_tmp_time=tmp.x(keep2)
      ji_tmp_data=2.0*tmp.y(keep2);the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
      store_data,'Ji',data={x:ji_tmp_time,y:ji_tmp_data}

      get_data,'JEi',data=tmp
      ;remove crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      jEi_tmp_time=tmp.x(keep2)
      jEi_tmp_data=tmp.y(keep2)
      store_data,'JEi',data={x:jEi_tmp_time,y:jEi_tmp_data}

      get_data,'JEi_up',data=tmp
      ;remove crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      jEi_up_tmp_time=tmp.x(keep2)
      jEi_up_tmp_data=tmp.y(keep2)
      store_data,'JEi_up',data={x:jEi_up_tmp_time,y:jEi_up_tmp_data}

      get_data,'Ji_up',data=tmp
      ;remove crap
      ;keep=where(finite(tmp.y) NE 0)
      tmp.x=tmp.x(keep1)
      tmp.y=tmp.y(keep1)
      ;keep=where(abs(tmp.y) GT 0.0)
      ji_up_tmp_time=tmp.x(keep2)
      ji_up_tmp_data=2.0*tmp.y(keep2);the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
      store_data,'Ji_up',data={x:ji_up_tmp_time,y:ji_up_tmp_data}

      ;SNIP SNIP SNIP #2 starting at
      ;if keyword_set(heavy) then begin

      ;SNIP #2 ENDING AT
      ; store_data,'Jh_up',data={x:jh_up_tmp_time,y:jh_up_tmp_data}
      ;
      ;endif

      ;get orbit number for filenames

      get_data,'ORBIT',data=tmp
      orbit=tmp.y(0)
      orbit_num=strcompress(string(tmp.y(0)),/remove_all)

      ;Scale electron energy flux to 100km, pos flux earthward

      get_data,'ILAT',data=tmp
      sgn_flx = tmp.y/abs(tmp.y)
      get_data,'B_model',data=tmp1
      get_data,'BFOOT',data=tmp2
      mag1 = (tmp1.y(*,0)*tmp1.y(*,0)+tmp1.y(*,1)*tmp1.y(*,1)+tmp1.y(*,2)*tmp1.y(*,2))^0.5
      mag2 = (tmp2.y(*,0)*tmp2.y(*,0)+tmp2.y(*,1)*tmp2.y(*,1)+tmp2.y(*,2)*tmp2.y(*,2))^0.5
      ratio = (mag2/mag1)
      jee_ionos_tmp_data = sgn_flx*jee_tmp_data*ratio
      store_data,'JEei',data={x:jee_tmp_time,y:jee_ionos_tmp_data}

      jee_tot_ionos_tmp_data=sgn_flx*jee_tot_tmp_data*ratio
      store_data,'JEei_tot',data={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}

      get_data,'fa_vel',data=vel
      speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0

      old_pos=0.
      position=make_array(n_elements(magz.x),/double)
      speed_mag_point=make_array(n_elements(magz.x),/double)
      for j=0L,n_elements(magz.x)-2 do begin
        speed_point_ind=min(abs(vel.x-magz.x(j)),ind)
        ;print,ind
        speed_mag_point(j)=speed(ind)
        samplingperiod=magz.x(j+1)-magz.x(j)
        ;position=make_array(n_elements(magz.x),/double)
        position(j)=old_pos+speed_mag_point(j)*samplingperiod
        old_pos=position(j)
      endfor

      window,0,xsize=600,ysize=800
      loadct,39
      !p.charsize=1.3
      tplot,['MagZ'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges(jjj,0),time_ranges(jjj,1)]

      ;SNIP SNIP SNIP #4 includes sections
      ;calculate the total ion outflow for this interval
      ;calculate the total electron downflux at the spacecraft altitude over this interval
      ;END SNIP #4
      
      ;calculate the current from mag


      deltaBX=deriv(position,magz.y)
      jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
      sign_jtemp=abs(deltaBx)/deltaBx
      store_data,'jtemp',data={x:magz.x,y:jtemp}
      ;terminate the intervals before the last point

      if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)


      ;SNIP SNIP SNIP #1
      ;starts at start_points=[0]
      ;          stop_points=[0]
      ;;END SNIP #1
      
      ;;SNIP SNIP SNIP #0
      ;;entire loop over start points discarded
      ;;END SNIP #0
      
    endif ; end of data_valid if statement

endfor ;end of number_of_intervals (indexed by jjj) loop

return
end

  