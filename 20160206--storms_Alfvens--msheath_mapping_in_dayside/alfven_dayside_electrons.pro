PRO ALFVEN_DAYSIDE_ELECTRONS,FILENAME=filename, $
                             ENERGY_ELECTRONS=energy_electrons, $
                             ENERGY_IONS=energy_ions, $
                             ANALYSE_NOISE=analyse_noise, $
                             T1=t1, $
                             T2=t2, $
                             FILTERFREQ=filterfreq, $
                             BURST=burst, $
                             HEAVY=heavy, $
                             UCLA_MAG_DESPIN=ucla_mag_despin, $
                             KEEP_ALFVEN_ONLY=keep_alfven_only, $
                             PNG_SUMPLOT=png_sumplot, $
                             PNG_OUREVENTS=png_ourevents, $
                             CONT_IF_FILE_EXISTS=cont_if_file_exists

  plot_dir = '/SPENCEdata/software/sdt/batch_jobs/20160206--storms_Alfvens--msheath_mapping_in_dayside/'

  hemi                = KEYWORD_SET(hemi) ? hemi : 'north'
  inDir               = '/SPENCEdata/Research/Cusp/storms_Alfvens/journals'
  inFile              = 'journal__20160206__times_for_orbs_with_more_than_50_events_on_dayside_during_mainphase__check_for_msheath_electrons__'+hemi+'.sav'

  ;;energy ranges
  if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  if not keyword_set(energy_ions) then energy_ions=[0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; If no data exists, return to main
  t=0
  dat = get_fa_ees(t,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; Electron current - line plot
  if keyword_set(burst) then begin
     get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endif else begin
     get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endelse
  
  ;;remove spurious crap
  get_data,'Je',data=tmpj
  
  keep=where(finite(tmpj.y) NE 0)
  tmpj.x=tmpj.x(keep)
  tmpj.y=tmpj.y(keep)
  
  keep=where(abs(tmpj.y) GT 0.0)
  tx=tmpj.x(keep)
  ty=tmpj.y(keep)
  
  ;;get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)
  
  
  ;;throw away the first 10  points since they are often corrupted
  if not keyword_set(burst) then begin
     store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
  endif else begin
     store_data,'Je',data={x:tx,y:ty}
  endelse
  
  ;;eliminate data from latitudes above the Holzworth/Meng auroral oval 
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) LE auroral_zone(mlt.y,7,/lat)/(!DPI)*180.0 AND abs(ilat.y) GE 50.0 )
  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  ;;Use the electron data to define the time ranges for this orbit	
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
  
  ;;remove esa burp when switched on
  if not keyword_set(burst) then begin
     turn_on=where(part_res_je GT 300.0)
     if turn_on(0) NE -1 then begin
        turn_on_separate=make_array(n_elements(turn_on),/double)
        for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
        separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
     endif
  endif

  ;;identify time indices for each interval
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
  
  ;;identify interval times
  time_ranges=je.x(time_range_indices)
  number_of_intervals=n_elements(time_ranges(*,0))
  
  print,'number_of_intervals',number_of_intervals
  
  ;;get despun mag data if keyword set
  if keyword_set(ucla_mag_despin) then ucla_mag_despin
  
  
  ;;begin looping each interval
  for jjj=0,number_of_intervals-1 do begin
     print,'time_range',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))
     
     ;;get orbit number for filenames		
     get_data,'ORBIT',data=tmp
     orbit=tmp.y(0)
     orbit_num=strcompress(string(tmp.y(0)),/remove_all)
                                ;filename for output file
     curfile = plot_dir + 'batch_output/'+'Dartmouth_as5_dflux_'+strcompress(orbit_num,/remove_all)+'_'+strcompress(jjj,/remove_all)
     IF KEYWORD_SET(burst) THEN BEGIN
        curfile = curfile + '--burst'
     ENDIF
     
     ;;make sure we're not overwriting
     IF file_test(curfile) THEN BEGIN
        IF NOT KEYWORD_SET(cont_if_file_exists) THEN BEGIN
           right_now=strmid(timestamp(),0,13)
           curfile = curfile + "--" + right_now
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(cont_if_file_exists) THEN BEGIN
              PRINT,"Not overwriting file " + curfile + "! Returning..."
              RETURN
           ENDIF
        ENDELSE
     ENDIF
        
     je_tmp_time=je.x(time_range_indices(jjj,0):time_range_indices(jjj,1))
     je_tmp_data=je.y(time_range_indices(jjj,0):time_range_indices(jjj,1))
     
     store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}
     
     ;;get fields quantities
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
              print,'No V1-V2_S data - trying V1-V4_S'
              efieldV1214=get_fa_fields('V1-V4_S',time_ranges(jjj,0),time_ranges(jjj,1))
              if efieldV1214.valid eq 0 AND KEYWORD_SET(burst) then begin
                 print,'No V1-V4_S data - trying V1-V2_4k (burst)'
                 efieldV1214=get_fa_fields('V1-V2_4k',time_ranges(jjj,0),time_ranges(jjj,1))
                 if efieldV1214.valid eq 0 then begin
                    print,'No V1-V2_4k data - trying V1-V4_4k (burst)'
                    efieldV1214=get_fa_fields('V1-V4_4k',time_ranges(jjj,0),time_ranges(jjj,1))
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
     endelse	
     
     
     if data_valid NE 0.0 then begin
        
        ;;get E field and B field on same time scale
;;			efields_combine=combinets({x:efieldV1214.time,y:efieldV1214.comp1},{x:efieldV58.time,y:efieldV58.comp1})
        FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
        
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
        ;;magz.y=smooth(magz.y,40)
        store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
        if keyword_set(filterfreq) then begin
           
           magz=filter(magz,filterfreq,'magfilt','l')
           ;;remove end effects of the filter by cutting off the first/last 2s
           sf=magz.x(1)-magz.x(0)
           np=n_elements(magz.x)
           padding=round(2.0/sf)
           magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
           store_data,'MagZ',data=magz
        endif
        
        
        ;;get mag and efield data on same time scale
        ;;SMH Try this to make fa_fields_combine stop crying                        
        magz={time:magz.x,comp1:magz.y,ncomp:1}
        efield={time:efield.x,comp1:efield.y}
        
        
        ;; fields=combinets(magz,efield)
        FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
        fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

        ;;I'm hoping this means magz is pared down somewhere else

        ;; dens=combinets(magz,langmuir)
        langmuir={time:langmuir.x,comp1:langmuir.y,ncomp:1}
        FA_FIELDS_COMBINE,magz,langmuir,result=dens,/talk
        dens={time:magz.time,comp1:magz.comp1,comp2:dens,ncomp:2}

        magz={x:magz.time,y:magz.comp1}
        langmuir={x:langmuir.time,y:langmuir.comp1}

        ;;get the prootn cyc frequency for smoothing the e field data later
        proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
        ;;get_orbit data
        get_fa_orbit,je_tmp_time,/time_array,/all
        
        ;;define loss cone angle
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
           e_angle=[360.-lcw,lcw] ;	for Northern Hemis.
           ;;i_angle=[90.,270.0]
           ;;eliminate ram from data
           i_angle=[0.0,180.0]
           i_angle_up=[90.0,180.0]
           
        endelse
        
        ;;get fields mode
        fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
        
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

        ;;get moments/integrals of various fluxes
        if keyword_set(burst) then begin

           get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='JEe_tot',energy=energy_electrons
           get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='JEe',angle=e_angle,energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='Je',energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='Je_lc',energy=energy_electrons,angle=e_angle
           
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='JEi',energy=energy_ions
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='Ji',energy=energy_ions
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='JEi_up',energy=energy_ions,angle=i_angle
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                      name='Ji_up',energy=energy_ions,angle=i_angle
           
        endif else begin
           
           get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEe_tot',energy=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEe',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Je',energy=energy_electrons,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Je_lc',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot
           
           get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEi',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Ji',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
           get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEi_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Ji_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           
           if keyword_set(heavy) then begin
              
              get_2dt_pot,'je_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'je_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'je_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='JEh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
              get_2dt_pot,'j_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Jp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'j_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Jo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'j_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
                          name='Jh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           endif
           
        endelse
        
        get_data,'Je',data=tmp
        get_data,'Ji',data=tmpi
        ;;remove crap
        keep1=where(finite(tmp.y) NE 0 and finite(tmpi.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        keep2=where(abs(tmp.y) GT 0.0 and abs(tmpi.y) GT 0.0)
        je_tmp_time=tmp.x(keep2)
        je_tmp_data=tmp.y(keep2)
        store_data,'Je',data={x:je_tmp_time,y:je_tmp_data}
        
        get_data,'JEe',data=tmp
        ;;remove crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        jee_tmp_time=tmp.x(keep2)
        jee_tmp_data=tmp.y(keep2)
        store_data,'JEe',data={x:jee_tmp_time,y:jee_tmp_data}
        
        get_data,'JEe_tot',data=tmp
        ;;remove crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        jee_tot_tmp_time=tmp.x(keep2)
        jee_tot_tmp_data=tmp.y(keep2)
        store_data,'JEe_tot',data={x:jee_tot_tmp_time,y:jee_tot_tmp_data}
        
        get_data,'Je_lc',data=tmp
        ;;remove_crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        je_lc_tmp_time=tmp.x(keep2)
        je_lc_tmp_data=tmp.y(keep2)
        store_data,'Je_lc',data={x:je_lc_tmp_time,y:je_lc_tmp_data}
        
        get_data,'Ji',data=tmp
        ;;remove crap	
        ;;keep1=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep2=where(abs(tmp.y) GT 0.0)
        ji_tmp_time=tmp.x(keep2)
        ji_tmp_data=2.0*tmp.y(keep2) ;;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        store_data,'Ji',data={x:ji_tmp_time,y:ji_tmp_data}
        
        get_data,'JEi',data=tmp
        ;;remove crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        jEi_tmp_time=tmp.x(keep2)
        jEi_tmp_data=tmp.y(keep2)
        store_data,'JEi',data={x:jEi_tmp_time,y:jEi_tmp_data}
        
        get_data,'JEi_up',data=tmp
        ;;remove crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        jEi_up_tmp_time=tmp.x(keep2)
        jEi_up_tmp_data=tmp.y(keep2)
        store_data,'JEi_up',data={x:jEi_up_tmp_time,y:jEi_up_tmp_data}
        
        get_data,'Ji_up',data=tmp
        ;;remove crap
        ;;keep=where(finite(tmp.y) NE 0)
        tmp.x=tmp.x(keep1)
        tmp.y=tmp.y(keep1)
        ;;keep=where(abs(tmp.y) GT 0.0)
        ji_up_tmp_time=tmp.x(keep2)
        ji_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        store_data,'Ji_up',data={x:ji_up_tmp_time,y:ji_up_tmp_data}
        
        
        ;;get ion end electron characteristic energies
        
        chare=(jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11
        chare_tot=(jee_tot_tmp_data/je_tmp_data)*6.242*1.0e11
        charei=(JEi_up_tmp_data/ji_up_tmp_data)*6.242*1.0e11
        store_data,'CharE',data={x:jee_tmp_time,y:chare}
        store_data,'CharE_tot',data={x:jee_tot_tmp_time,y:chare_tot}
        store_data,'CharEi',data={x:jei_up_tmp_time,y:charei}
        
        ;;Scale electron energy flux to 100km, pos flux earthward
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
        
        ;;get position of each mag point
        ;;samplingperiod=magz.x(300)-magz.x(299)
        ;;position=make_array(n_elements(magz.x),/double)
        ;;position=speed(300)*samplingperiod*findgen(n_elements(magz.x))
        ;;speed_mag_point=speed(300)
        
        old_pos=0.
        position=make_array(n_elements(magz.x),/double)
        speed_mag_point=make_array(n_elements(magz.x),/double)
        for j=0L,n_elements(magz.x)-2 do begin
           speed_point_ind=min(abs(vel.x-magz.x(j)),ind)
           ;;print,ind
           speed_mag_point(j)=speed(ind)
           samplingperiod=magz.x(j+1)-magz.x(j)
           ;;position=make_array(n_elements(magz.x),/double)
           position(j)=old_pos+speed_mag_point(j)*samplingperiod
           old_pos=position(j)
        endfor
        
        
        ;;calculate the total ion outflow for this interval
        part_res_ji=make_array(n_elements(ji_up_tmp_time),/double)
        position_ji=make_array(n_elements(Ji_up_tmp_time),/double)
        position_ji(0)=0.0
        for j=1,n_elements(ji_tmp_time)-1 do begin
           part_res_ji(j)=abs(ji_up_tmp_time(j-1)-ji_up_tmp_time(j))
           if part_res_ji(j) EQ 0.0 then part_res_ji(j)=part_res_ji(j-1)
           position_ji(j)=position_ji(j-1)+speed(j)*part_res_Ji(j)
        endfor
        part_res_Ji(0)=part_res_Ji(1)
        ji_tot(jjj)=int_tabulated(position_ji,ji_tmp_data*sqrt(ratio))       ;mapped to ionosphere sqrt due to intergration in x 
        ji_up_tot(jjj)=int_tabulated(position_ji,ji_up_tmp_data*sqrt(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        
        print,'ji_tot',ji_tot(jjj)
        
        ;;calculate the total electron downflux at the spacecraft altitude over this interval
        part_res_je=make_array(n_elements(jee_tmp_data),/double)
        position_je=make_array(n_elements(jee_tmp_time),/double)
        position_je(0)=0.0
        for j=1,n_elements(je_tmp_time)-1 do begin
           part_res_je(j)=abs(jee_tmp_time(j-1)-jee_tmp_time(j))
           if part_res_je(j) EQ 0.0 then part_res_je(j)=part_res_je(j-1)
           position_je(j)=position_je(j-1)+speed(j)*part_res_Je(j)
        endfor
        part_res_Je(0)=part_res_Je(1)
        jee_tot(jjj)=int_tabulated(position_je,jee_tmp_data*sqrt(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        ;;calculate the current from mag
        deltaBX=deriv(position,magz.y)
        jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
        sign_jtemp=abs(deltaBx)/deltaBx
        store_data,'jtemp',data={x:magz.x,y:jtemp}

        ;;terminate the intervals before the last point
        if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)

        
        ;;If we want to save a summary plot
        IF KEYWORD_SET(png_sumplot) THEN BEGIN
           cgPS_Open, as5_dir+'plots/summaries/as5_orbit' + strcompress(orbit_num+'_'+string(jjj),/remove_all) + '.ps', font=1
           loadct,39
           !p.charsize=1.3
           tplot,['Je','CharE','JEei','Ji','JEi','MagZ','jtemp'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges(jjj,0),time_ranges(jjj,1)]
           cgPS_Close, /PNG, /Delete_PS, Width=1000
        ENDIF ELSE BEGIN 
           IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN
              window,0,xsize=600,ysize=800
              loadct,39
              !p.charsize=1.3
              tplot,['Je','CharE','JEei','Ji','JEi','MagZ'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges(jjj,0),time_ranges(jjj,1)]
           ENDIF
        ENDELSE

        start_points=[0]
        stop_points=[0]
        
        ;;get current intervals
        for j=1L,n_elements(sign_jtemp)-2 do begin

           if sign_jtemp(j)+sign_jtemp(j-1) EQ 0.0 then begin
              start_points=[start_points,j]
           endif
           if sign_jtemp(j)+sign_jtemp(j+1) EQ 0.0 then begin
              stop_points=[stop_points,j]
           endif

        endfor

        if sign_jtemp(0)+sign_jtemp(1) NE 0.0 then stop_points=stop_points(1:n_elements(stop_points)-1)

        ;;eliminate single points
        non_single_points=where(stop_points NE start_points)

        start_points=start_points(non_single_points)
        stop_points=stop_points(non_single_points)

        ;;define the current intervals
        ;;in this array 	0-interval start index
        ;;		1-interval stop index
        ;;		2-sign of the current (field-aligned is pos)
        ;;		3-validity of the point-i.e does it satisfy the thresholds
        ;;		4-maximum size of the current in that interval
        ;;		5-maximum size of the current from the Electron esa
        ;;		6-maximum size of the electron energy flux mapped to the ionosphere
        ;;		7-integrated downgoing electron flux over that interval at ionosphere
        ;;		8-maximum characteristic electron energy from that interval
        ;;		9-maximum ion energy flux
        ;;		10-maximum ion flux
        ;;		11-maximum upgoing ion flux
        ;;		12-integrated upgoing ion flux over the interval at the ionosphere
        ;;		13-integrated upgoing only ion flux over the interval at the ionosphere
        ;;		14-maximum characteristic ion energy
        ;;		15-time width of the current filament in s
        ;;		16-width of the current filament at the s/c altitude
        ;;		17-magnetic field amplitude (nT)
        ;;		18-electric field amplitude (mV/m)
        ;;		19-Orbit number
        ;;		20-max current time (based on location of max current
        ;;		21-max current altitude
        ;;		22-max current MLT
        ;;		23- max current ILAT
        ;;		24-average value of B
        ;;		25-average value of E
        ;;		26-field sample rate
        ;;		27-fields mode		 
        ;;		28-maximum upgoing proton flux
        ;;		29-maximum characteristic proton energy
        ;;		30-maximum upgoing oxygen flux
        ;;		31-maximum characteristic oxygen energy
        ;;		32-maximum upgoing helium flux
        ;;		33-maximum characteristic helium energy
        ;;		34-spacecraft potential -1.*V8_S
        ;;		35-langmuir probe number
        ;;		36-max L. current over interval
        ;;		37-min L.current over interval
        ;;		38-median L.current over interval
        ;;		39-maximum characteristic electron energy from total distribution from that interval
        ;;		40-maximum size of the electron energy flux from total distribution mapped to the ionosphere
        ;;		41-integrated downgoing electron flux from total distribution over that interval at ionosphere

        
        current_intervals=make_array(n_elements(start_points),42,/double)
        current_intervals(*,0)=start_points
        current_intervals(*,1)=stop_points
        current_intervals(*,2)=sign_jtemp(start_points)
        current_intervals(*,3)=1
        
        intervalparts_electrons_old=-1
        intervalparts_ions_old=-1
        valid_old=0.0
        for j=0L,n_elements(start_points)-1 do begin
           
           ;;define the interval points 
           intervalfields=(current_intervals(j,0))+findgen(current_intervals(j,1)+1-current_intervals(j,0))
           tempz=magz.y(intervalfields)
           fields_res_interval=magz.x(intervalfields)-magz.x(intervalfields-1)
           ;;help,magz,/st
           ;;print,'current_indices ',current_intervals(j,0),current_intervals(j,1)
           intervalparts_electrons=where(je_tmp_time GE magz.x(current_intervals(j,0)) and je_tmp_time LE magz.x(current_intervals(j,1)))
           intervalparts_ions=where(ji_up_tmp_time GE magz.x(current_intervals(j,0)) and ji_up_tmp_time LE magz.x(current_intervals(j,1)))
           if intervalparts_electrons(0) EQ -1 then begin
              minitime=min(abs(je_tmp_time-magz.x(current_intervals(j,0))),intervalparts_electrons)
           endif
           if intervalparts_ions(0) EQ -1 then begin
              minitime=min(abs(ji_up_tmp_time-magz.x(current_intervals(j,0))),intervalparts_ions)
           endif

           ;;get the current from b and determine if to keep this event
           jmax=max(jtemp(intervalfields),indjmax)
           current_intervals(j,4)=jmax*sign_jtemp(start_points(j))
           if jmax LE current_threshold then begin
              current_intervals(j,3)=0.0
           endif
           
           ;;define the time of the max current
           current_intervals(j,20)=magz.x(intervalfields(indjmax))
           
           
           ;;get the electron current and determine if to keep this event
           sign=-1.*je_tmp_data(intervalparts_electrons)/abs(je_tmp_data(intervalparts_electrons))
           maxJe=max(abs(je_tmp_data(intervalparts_electrons)),ind)
           maxJe=maxJe*sign(ind)*1.6e-9 ;;in microA/m2
           current_intervals(j,5)=maxJe
           if abs(maxJe)/abs(jmax) LE esa_j_delta_bj_ratio_threshold then begin
              current_intervals(j,3)=0.0
           endif
           
           ;;get the electron energy flux and dtermine if to keep this event
           ;;print,'intervalparts_electrons',intervalparts_electrons
           ;;help,jee_tmp_time
           ;;help,je_tmp_time
           ;;print,'jee start stop ',time_to_str(jee_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
           ;;print,'je start stop ',time_to_str(je_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
           
           sign=jee_ionos_tmp_data(intervalparts_electrons)/abs(jee_ionos_tmp_data(intervalparts_electrons)) ;note corrected direction (i.e.-1) from Alfven_stats_4-positive is now really downwards
           maxJEe_ionos=max(abs(jee_ionos_tmp_data(intervalparts_electrons)),ind)
           maxJEe_ionos=maxJEe_ionos*sign(ind)
           
           sign=jee_tot_ionos_tmp_data(intervalparts_electrons)/abs(jee_tot_ionos_tmp_data(intervalparts_electrons))
           maxJEe_tot_ionos=max(abs(jee_tot_ionos_tmp_data(intervalparts_electrons)),ind)
           maxJEe_tot_ionos=maxJEe_tot_ionos*sign(ind)



           current_intervals(j,6)=maxJEe_ionos
           current_intervals(j,40)=maxJEe_tot_ionos
           if abs(maxJEe_ionos) LE electron_eflux_ionos_threshold and abs(maxJEe_tot_ionos-maxJEe_ionos) LE electron_eflux_ionos_threshold then begin ;note change from previously when only downgoing fluxes where considered.
              current_intervals(j,3)=0.0				      
           endif
           
           if keyword_set(heavy) then begin
              
              minitime=min(abs(Jp_up_tmp_time-current_intervals(j,20)),ind_OH)
              current_intervals(j,28)=Jp_up_tmp_data(ind_OH)
              C_Ep=JEp_up_tmp_data(ind_OH)/Jp_up_tmp_data(ind_OH)*6.242*1.0e11
              current_intervals(j,29)=C_Ep
              
              current_intervals(j,30)=Jo_up_tmp_data(ind_OH)
              C_Eo=JEo_up_tmp_data(ind_OH)/Jo_up_tmp_data(ind_OH)*6.242*1.0e11
              current_intervals(j,31)=C_Eo
              
              minitime=min(abs(Jh_up_tmp_time-current_intervals(j,20)),ind_h)
              current_intervals(j,32)=Jh_up_tmp_data(ind_h)
              C_Eh=JEh_up_tmp_data(ind_h)/Jh_up_tmp_data(ind_h)*6.242*1.0e11
              current_intervals(j,33)=C_Eh
              
           endif
           
           ;;get width of current filament in time (s)
           time_width=magz.x(current_intervals(j,1))-magz.x(current_intervals(j,0))
           
           current_intervals(j,15)=time_width
           ;;get width of the current filament at this altitude
           
           width=speed_mag_point(current_intervals(j,0))*abs(magz.x(current_intervals(j,0))-magz.x(current_intervals(j,1)))
           ;;print,'speed',speed_mag_point(current_intervals(j,0))
           current_intervals(j,16)=width
           
           ;;get the integrated electron dflux in ionosphere over this interval
           if intervalparts_electrons(0) NE -1 then begin
              if n_elements(intervalparts_electrons) EQ 1 then begin 
                 
                 current_intervals(j,7)=width*jee_tmp_data(intervalparts_electrons)
                 current_intervals(j,41)=width*jee_tot_tmp_data(intervalparts_electrons)
              endif else begin
                 ;;interpolate particle data to same resolution as the fields data
                 jee_tmp_data_fields_res_interval=interpol(jee_tmp_data(intervalparts_electrons),jee_tmp_time(intervalparts_electrons),magz.x(intervalfields))
                 jee_tot_tmp_data_fields_res_interval=interpol(jee_tot_tmp_data(intervalparts_electrons),jee_tot_tmp_time(intervalparts_electrons),magz.x(intervalfields))
                 current_intervals(j,7)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,jee_tmp_data_fields_res_interval,/double)
                 current_intervals(j,41)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,jee_tot_tmp_data_fields_res_interval,/double)
                 
              endelse
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              current_intervals(j,7)=current_intervals(j,7)*sqrt(ratio(intervalparts_electrons(0)))
              current_intervals(j,41)=current_intervals(j,41)*sqrt(ratio(intervalparts_electrons(0)))
           endif
           
           
           
           ;;get integrated ion outflow mapped to ionosphere over this interval
           if intervalparts_ions(0) NE -1 then begin
              if n_elements(intervalparts_ions) EQ 1 then begin 
                 ;;if intervalparts_ions(0) NE intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) or valid_old EQ 0.0 then begin
                 
                 current_intervals(j,12)=width*ji_tmp_data(intervalparts_ions)
                 current_intervals(j,13)=width*ji_up_tmp_data(intervalparts_ions)
                 ;;endif
              endif else begin
                 ;;if  intervalparts_ions(0) EQ intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) and valid_old EQ 1.0 then intervalparts_ions=intervalparts_ions(1:n_elements(intervalparts_ions)-1)
                 ;;if n_elements(intervalparts_ions) EQ 1 then begin 
                 ;;current_intervals(j,12)=speed(intervalparts_ions)*part_res_ji(intervalparts_ions)*ji_up_tmp_data(intervalparts_ions)/2.0
                 
                 ;;endif else begin
                 
                 
                 ;;interpolate particle data to same resolaution as the fields data
                 ji_tmp_data_fields_res_interval=interpol(ji_tmp_data(intervalparts_ions),ji_tmp_time(intervalparts_ions),magz.x(intervalfields))
                 ji_up_tmp_data_fields_res_interval=interpol(ji_up_tmp_data(intervalparts_ions),ji_up_tmp_time(intervalparts_ions),magz.x(intervalfields))
                 
                 current_intervals(j,12)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,ji_tmp_data_fields_res_interval,/double)
                 current_intervals(j,13)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,ji_up_tmp_data_fields_res_interval,/double)
                 ;;print,'ji_tot_alf',Ji_tot_alf(jjj)
                 
                 
                 ;;endelse
                 
              endelse
              
              ;;map result to ionosphere (sqrt of B since have integrated in x)
              current_intervals(j,12)=current_intervals(j,12)*sqrt(ratio(intervalparts_ions(0)))
              current_intervals(j,13)=current_intervals(j,13)*sqrt(ratio(intervalparts_ions(0)))
           endif
           
           ;;get max electron characteristic energy over this interval
           C_E=max(charE(intervalparts_electrons))
           C_E_tot=max(charE_tot(intervalparts_electrons))

           current_intervals(j,8)=C_E
           current_intervals(j,39)=C_E_tot

           ;;get max upgoing ion energy flux over this interval
           maxJEi=max(abs(jei_up_tmp_data(intervalparts_ions)),ind)
           current_intervals(j,9)=maxJEi
           
           ;;get max ion flux over this interval
           sign_ion=-1.*ji_tmp_data(intervalparts_ions)/abs(ji_tmp_data(intervalparts_ions))
           maxJi=max(abs(ji_tmp_data(intervalparts_ions)),ind)
           maxJi=maxJi*sign_ion(ind)
           current_intervals(j,10)=maxJi
           
           ;;get max upgoing ion flux over this interval
           maxJi_up=max(abs(ji_up_tmp_data(intervalparts_ions)),ind)
           current_intervals(j,11)=maxJi_up
           
           ;;get max characteristic ion energy over this interval
           C_Ei=max(charEi(intervalparts_ions))
           current_intervals(j,14)=C_Ei
           
           
           
           ;;fields sample period
           current_intervals(j,26)=magz.x(intervalfields(indjmax)+1)-magz.x(intervalfields(indjmax))
           
           ;;get mag field amplitude
           db=max(magz.y(intervalfields))-min(magz.y(intervalfields))
           median_db=median(magz.y(intervalfields))
           current_intervals(j,17)=db
           current_intervals(j,24)=median_db
           if db LT delta_b_threshold then current_intervals(j,3)=0.0 ;threshold for reliablity of identification
           
           ;;get elec field amplitude
           ;;smooth to below proton gyro freq.
           smooth_int=ceil((1./proton_cyc_freq(intervalfields(indjmax)))/current_intervals(j,26))
           if smooth_int GT 1.0 and smooth_int LE n_elements(intervalfields)/4.0 then efield_smooth=smooth(fields.comp2(intervalfields),smooth_int) else efield_smooth=fields.comp2(intervalfields)
           
           de=max(efield_smooth)-min(efield_smooth)
           median_de=median(fields.comp2(intervalfields))
           current_intervals(j,18)=de
           current_intervals(j,25)=median_de
           if de LT delta_E_threshold then current_intervals(j,3)=0.0 ;threshold for reliablity of identification
           
           ;;get max and min L. probe currents
           smooth_int=ceil((1./proton_cyc_freq(intervalfields(indjmax)))/current_intervals(j,26))
           if smooth_int GT 1.0 and smooth_int LE n_elements(intervalfields)/4.0 then dens_smooth=smooth(dens.comp2(intervalfields),smooth_int) else dens_smooth=dens.comp2(intervalfields)
           
           dens_max=max(dens_smooth)
           dens_min=min(dens_smooth)
           probe_time=min(abs(dens_probe.x-magz.x(intervalfields(indjmax))),probe_ind)
           
           
           median_dens=median(dens.comp2(intervalfields))
           current_intervals(j,35)=dens_probe.y(probe_ind)
           current_intervals(j,36)=dens_max
           current_intervals(j,37)=dens_min
           current_intervals(j,38)=median_dens

           ;;now get orbit quantities
           get_data,'ORBIT',data=orb
           get_data,'MLT',data=mlt
           get_data,'ALT',data=alt
           get_data,'ILAT',data=ilat

           mintime=min(abs(mlt.x-magz.x(intervalfields(indjmax))),ind)
           
           current_intervals(j,19)=orb.y(ind)
           current_intervals(j,21)=alt.y(ind)	
           current_intervals(j,22)=mlt.y(ind)	
           current_intervals(j,23)=ilat.y(ind)
           
           ;;fields_mode
           mintime=min(abs(fields_mode.time-magz.x(intervalfields(indjmax))),ind)
           current_intervals(j,27)=fields_mode.comp1(13,ind)
           
           ;;sc potential
           mintime=min(abs(sc_pot.x-magz.x(intervalfields(indjmax))),ind)
           current_intervals(j,34)=-1*sc_pot.y(ind)
           
           ;;e over b test
           va=1000.0*alfven_speed_mlt(current_intervals(j,21),current_intervals(j,22))
           e_over_b=(1.0e-3*current_intervals(j,18))/(current_intervals(j,17)*1.0e-9)
           if e_over_b/va LT 1.0/eb_to_alfven_speed then current_intervals(j,3)=0.0
           
           intervalparts_electrons_old=intervalparts_electrons
           intervalparts_ions_old=intervalparts_ions	
           valid_old=current_intervals(j,3)
        endfor
        
     endif

;;remove crap data
     keep=where(current_intervals(*,3) NE 0.0)
     print,'keep',keep
     if keyword_set(keep_alfven_only) then begin
        current_intervals=current_intervals(keep,*)
     endif

     print,'number of intervals',n_elements(keep)
;;if jjj GT 0 or not keyword_set(filename) then
;;filename='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5_14F/'+'Dartmouth_as5_dflux_'+strcompress(orbit_num+'_'+string(jjj)+"_magcal_v"
;;+ string(version)+"_burst",/remove_all)
     if jjj GT 0 or not keyword_set(filename) then filename= curfile

     openw,unit1,filename,/get_lun
	

     ;;Want pngs of each of OUR events?
     IF KEYWORD_SET(png_ourevents) THEN BEGIN
        ;;cur_time = str_to_time(data_chast.time[jj])
        ;;IF cur_time GT time_ranges(jjj,0) AND cur_time LT time_ranges(jjj,1) THEN BEGIN
        fname=plot_dir+'plots/orb_' + strcompress(orbit_num+'_'+string(jjj)+'_'+string(jj),/remove_all) + '--alfven_msheath_elecs--'+strcompress(jj,/remove_all)+'.ps'
        plotstr = "B!Dz!N and J!Dmag!N for Dartmouth event " + str(jj)
        tplot_options,'title',plotstr
        cgPS_Open,fname,font=1
        loadct,39
        !p.charsize = 1.3
;;                      tfirst = magz.x(current_intervals(jj,0))
;;                      tlast = magz.x(current_intervals(jj,1))
        tplot,['MagZ','jtemp'] ,var_label=['ALT','MLT','ILAT'],trange=[magz.x(current_intervals(jj,0)),magz.x(current_intervals(jj,1))]
        cgPS_Close, /PNG,/delete_ps, WIDTH=1000
;;            ENDIF
           ;;; ENDIF ELSE PRINT,$
        ;;  FORMAT='("Chaston event[",I-0,"]: ",A-0," outside range (for jjj=",I-0,")")',$
        ;;  jj,data_chast.time[jj],jjj
     ENDIF
     
     
     
  endfor
  
  return 
end
