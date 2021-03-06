pro exp_density_stats,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
                        t1=t1,t2=t2,filterfreq=filterfreq,$
                        burst=burst,heavy=heavy,ucla_mag_despin=ucla_mag_despin,$
                        save_plot=save_p,save_data=save_d,smooth=smooth,extra_times=extra_t,no_screen=no_s ;spence additions

;09/25/2014 SMH This is a copy of alfven_stats_3.pro in the
;Alfven_study_13F directory. I'm adding some output fields to
;make sure we're identifying current intervals the same way chaston did

;This program identifies Alfven waves and writes varous observable to a file names dflux_'orbit_number'_index.txt' from the size of the field-aligned current greater than som threshold value.
;Analysis of many orbits have shown this to be an effective way to find Alfven waves
; the output quantities are described in the output file and in the txt of this pro.
;to run load the data in sdt using the config alfven_stats_survey and then compile in the dircetory
;/disks/moose/home/ccc/Alfven_stats

;example from idl prompt: Alfven_Stats_3,/heavy,/analyse_noise
;the keyword heavy is set to get heavy ion data from teams
; the keyword analyse noise is set to extract Alfven waves out of noisy b field data

;Intended to be run in batch mode-to do this from terminal prompt type: sdt_batch alfven_stats_batch.txt 

;thresholds for inclusion as Alfven waves

  current_threshold=1.0         ;microA/m^2
  delta_b_threshold=5.0         ; nT
  delta_E_threshold=10.0        ; mV/m
  esa_j_delta_bj_ratio_threshold=0.02
  electron_eflux_ionos_threshold=0.05 ;ergs/cm^2/s
  eb_to_alfven_speed=10.0             ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT


;energy ranges

  if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  if not keyword_set(energy_ions) then energy_ions=[0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

;this pro dteremines the downward energy flux in the loss cone, width in B , current from esas;
;and B (sheet approx from axial only, and char Energy for skaws-only good on short intervals;
; since spacecraft speed changes not taken into account
;the field stuff is done more thoroughly in alfvne_wave_auto.pro

;e.g downflux,'orb8276_dflux',[0.0,100.0],t1=str_to_time('1998-09-24/24:04:46'),t2=str_to_time('1998-09-24/24:06:09')

;threshold=20.0;threshold current for identifying SKAW in microA/m^2


; If no data exists, return to main

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
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,4,/lat)/(!DPI)*180.)

  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

;Use the electron data to define the time ranges for this orbit	
  
  get_data,'Je',data=je
  part_res_je=make_array(n_elements(Je.x),/double)
  for j=1,n_elements(Je.x)-1 do begin
     part_res_je(j)=abs(Je.x(j)-Je.x(j-1))
  endfor
  part_res_Je(0)=part_res_Je(1)
  gap=where(part_res_je GT 10.0) ; & print, "This is gap: " &  print, gap, format='(4(I0))'
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
     endelse	
     
     
     if data_valid NE 0.0 then begin
        
                                ;get E field and B field on same time scale
                                ;SMH FA_FIELDS COMBINE aligns .comp1
                                ;data from 2nd arg with data from 1st arg
        
        print, 'Combining efields V1214 and V58 into efields_combine...'
        FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk

                                ;get magnitude of electric and magnetic field
        
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
        IF KEYWORD_SET(smooth) THEN magz.y=smooth(magz.y,40)
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
        
                                ;SMH make 'em '/talk' so I can see what's happening
        FA_FIELDS_COMBINE,magz,efield,result=efield_aligned,/interp,delt_t=50.,/talk
                                ;I'm hoping this means magz is pared down somewhere else
        magz={x:magz.time,y:magz.comp1}


                                ;get the prootn cyc frequency for smoothing the e field data later
        
                                ;proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI); in Hz
        proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
                                ;get_orbit data
        
        get_fa_orbit,je_tmp_time,/time_array,/all
        
                                ;define loss cone angle
        
        get_data,'ALT',data=alt
        loss_cone_alt=alt.y(0)*1000.0
;			lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI

        get_data,'ILAT',data=ilat
        print, "loss_cone_alt", loss_cone_alt, "ilat.y(0)", ilat.y(0)
        lcw=loss_cone_width(loss_cone_alt,ilat.y(0))*180.0/!DPI
;        lcw=0.69745*180.0/!DPI
        north_south=abs(ilat.y(0))/ilat.y(0)
        
        if north_south EQ -1 then begin
           e_angle=[180.-lcw,180+lcw] ; for Southern Hemis.
                                ;i_angle=[270.0,90.0]	
                                ;elimnate ram from data
           i_angle=[180.0,360.0]
           i_angle_up=[270.0,360.0]
           
        endif else begin
           e_angle=[360.-lcw,lcw] ;	for Northern Hemis.
                                ;i_angle=[90.,270.0]
                                ;eliminate ram from data
           i_angle=[0.0,180.0]
           i_angle_up=[90.0,180.0]
           
        endelse
        
        
                                ;get fields mode
        
        fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
        
        
        
                                ;get the spacecraft potential per spin
        
        spin_period=4.946       ; seconds
        
                                ;get_sample_rate

        v8={x:spacecraft_potential.time,y:spacecraft_potential.comp1}
        
        v8_dt=abs(v8.x-shift(v8.x,-1))
        v8_dt(0)=v8_dt(1)
        v8_dt(n_elements(v8.x)-1)=v8_dt(n_elements(v8.x)-2)

                                ;get maxima within a 1 spin window

        j_range=where(v8.x LT v8.x(n_elements(v8.x)-1)-spin_period)
        index_max=max(j_range)
        print,"index max:", index_max
        pot=make_array(n_elements(v8.x),/double)
        for j=0L,index_max do begin
                                ;spin_range=where(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
           spin_range=j+findgen(ceil(spin_period/V8_dt(j)))
           pot(j)=max(abs(v8.y(spin_range)),ind)
           sign=v8.y(spin_range(ind))/abs(v8.y(spin_range(ind)))
           
                                ;print,j,pot(j)
        endfor
        pot(index_max+1:n_elements(v8.x)-1)=pot(j_range(index_max))
        sc_pot={x:v8.x,y:pot}
        store_data,'S_Pot',data=sc_pot ;note this is actualy the negative of the sp. potential this corrected in the file output
        
        
        
        
        
        if keyword_set(burst) then begin
           
           get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe',angle=e_angle,energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je',energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je_lc',energy=energy_electrons,angle=e_angle
           
           
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi',energy=energy_ions
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji',energy=energy_ions
           get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi_up',energy=energy_ions,angle=i_angle
           get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji_up',energy=energy_ions,angle=i_angle
           
        endif else begin
           
           ;; get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot
           ;; get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je',energy=energy_electrons,sc_pot=sc_pot
           ;; get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je_lc',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot	
           ;; get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
           ;; get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
           ;; get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           ;; get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           get_2dt_ts,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEe',angle=e_angle,energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je',energy=energy_electrons
           get_2dt_ts,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Je_lc',energy=energy_electrons,angle=e_angle	
           get_2dt_ts,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi',energy=energy_ions,angle=i_angle
           get_2dt_ts,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji',energy=energy_ions,angle=i_angle
           get_2dt_ts,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEi_up',energy=energy_ions,angle=i_angle_up
           get_2dt_ts,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Ji_up',energy=energy_ions,angle=i_angle_up
           
           
           
           if keyword_set(heavy) then begin
              
              get_2dt_pot,'je_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'je_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'je_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='JEh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
              get_2dt_pot,'j_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Jp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'j_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Jo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              get_2dt_pot,'j_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1),name='Jh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
           endif
           
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
        ji_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
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
        ji_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        store_data,'Ji_up',data={x:ji_up_tmp_time,y:ji_up_tmp_data}
        
        
        if keyword_set(heavy) then begin
           
           get_data,'JEp_up',data=tmp
                                ;remove crap
           keep1=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
           keep2=where(abs(tmp.y) GT 0.0)
           jEp_up_tmp_time=tmp.x(keep2)
           jEp_up_tmp_data=tmp.y(keep2)
           store_data,'JEp_up',data={x:jEp_up_tmp_time,y:jEp_up_tmp_data}
           
           get_data,'Jp_up',data=tmp
                                ;remove crap
                                ;keep=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
                                ;keep=where(abs(tmp.y) GT 0.0)
           jp_up_tmp_time=tmp.x(keep2)
           jp_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
           store_data,'Jp_up',data={x:jp_up_tmp_time,y:jp_up_tmp_data}
           
           
           get_data,'JEo_up',data=tmp
                                ;remove crap
                                ;keep=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
                                ;keep=where(abs(tmp.y) GT 0.0)
           jEo_up_tmp_time=tmp.x(keep2)
           jEo_up_tmp_data=tmp.y(keep2)
           store_data,'JEo_up',data={x:jEo_up_tmp_time,y:jEo_up_tmp_data}
           
           get_data,'Jo_up',data=tmp
                                ;remove crap
                                ;keep=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
                                ;keep=where(abs(tmp.y) GT 0.0)
           jo_up_tmp_time=tmp.x(keep2)
           jo_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
           store_data,'Jo_up',data={x:jo_up_tmp_time,y:jo_up_tmp_data}
           
           
           get_data,'JEh_up',data=tmp
                                ;remove crap
           keep1=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
           keep2=where(abs(tmp.y) GT 0.0)
           jEh_up_tmp_time=tmp.x(keep2)
           jEh_up_tmp_data=tmp.y(keep2)
           store_data,'JEh_up',data={x:jEh_up_tmp_time,y:jEh_up_tmp_data}
           
           get_data,'Jh_up',data=tmp
                                ;remove crap
                                ;keep=where(finite(tmp.y) NE 0)
           tmp.x=tmp.x(keep1)
           tmp.y=tmp.y(keep1)
                                ;keep=where(abs(tmp.y) GT 0.0)
           jh_up_tmp_time=tmp.x(keep2)
           jh_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
           store_data,'Jh_up',data={x:jh_up_tmp_time,y:jh_up_tmp_data}
           
        endif
                                ;get ion end electron characteristic energies
        
        chare=(jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11
        charei=(JEi_up_tmp_data/ji_up_tmp_data)*6.242*1.0e11
        store_data,'CharE',data={x:jee_tmp_time,y:chare}
        store_data,'CharEi',data={x:jei_up_tmp_time,y:charei}
        
        
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
        
        
        get_data,'fa_vel',data=vel
        speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
        
                                ;get position of each mag point
        
                                ;samplingperiod=magz.x(300)-magz.x(299)
                                ;position=make_array(n_elements(magz.x),/double)
                                ;position=speed(300)*samplingperiod*findgen(n_elements(magz.x))
                                ;speed_mag_point=speed(300)
        
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
        tplot,['Je','CharE','JEei','Ji','JEi','MagZ'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges(jjj,0),time_ranges(jjj,1)]
        IF KEYWORD_SET(save_p) THEN popen,/port,'biz.data' & loadct2,43 & tplot & pclose ; For hard copies use 


                                ;calculate the total ion outflow for this interval

        part_res_ji=make_array(n_elements(ji_up_tmp_time),/double)
        position_ji=make_array(n_elements(Ji_up_tmp_time),/double)
        position_ji(0)=0.0
        for j=1,n_elements(ji_tmp_time)-1 do begin
           part_res_ji(j)=abs(ji_up_tmp_time(j-1)-ji_up_tmp_time(j))
           if part_res_ji(j) EQ 0.0 then part_res_ji(j)=part_res_ji(j-1)
           position_ji(j)=position_ji(j-1)+speed(j)*part_res_Ji(j)
        endfor
        part_res_Ji(0)=part_res_Ji(1)
        ji_tot(jjj)=int_tabulated(position_ji,ji_tmp_data*sqrt(ratio))                       ;mapped to ionosphere sqrt due to intergration in x 
        ji_up_tot(jjj)=int_tabulated(position_ji,ji_up_tmp_data*sqrt(ratio))                 ;mapped to ionosphere sqrt due to intergration in x 
        
        
        print,'ji_tot',ji_tot(jjj)
        
                                ;calculate the total electron downflux at the spacecraft altitude over this interval
        
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
        
                                ;calculate the current from mag

        
        deltaBX=deriv(position,magz.y)
        jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
        sign_jtemp=abs(deltaBx)/deltaBx
        store_data,'jtemp',data={x:magz.x,y:jtemp}

                                ;terminate the intervals before the last point
        if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)

        

        start_points=[0]
        stop_points=[0]

        
                                ;get current intervals
        for j=1L,n_elements(sign_jtemp)-2 do begin

           if sign_jtemp(j)+sign_jtemp(j-1) EQ 0.0 then begin
              start_points=[start_points,j]
           endif
           if sign_jtemp(j)+sign_jtemp(j+1) EQ 0.0 then begin
              stop_points=[stop_points,j]
           endif

        endfor

        if sign_jtemp(0)+sign_jtemp(1) NE 0.0 then stop_points=stop_points(1:n_elements(stop_points)-1)

                                ;eliminate single points

        non_single_points=where(stop_points NE start_points)

        start_points=start_points(non_single_points)
        stop_points=stop_points(non_single_points)

                                ;define the current intervals

        
                                ;in this array 	0-interval start index
                                ;		1-interval stop index
                                ;		2-sign of the current (field-aligned is pos)
                                ;		3-validity of the point-i.e does it satisfy the thresholds
                                ;		4-maximum size of the current in that interval
                                ;		5-maximum size of the current from the Electron esa
                                ;		6-maximum size of the electron energy flux mapped to the ionosphere
                                ;		7-integrated downgoing electron flux over that interval at ionosphere
                                ;		8-maximum characteristic electron energy from that interval
                                ;		9-maximum ion energy flux
                                ;		10-maximum ion flux
                                ;		11-maximum upgoing ion flux
                                ;		12-integrated upgoing ion flux over the interval at the ionosphere
                                ;		13-integrated upgoing only ion flux over the interval at the ionosphere
                                ;		14-maximum characteristic ion energy
                                ;		15-time width of the current filament in s
                                ;		16-width of the current filament at the s/c altitude
                                ;		17-magnetic field amplitude (nT)
                                ;		18-electric field amplitude (mV/m)
                                ;		19-Orbit number
                                ;		20-max current time (based on location of max current
                                ;		21-max current altitude
                                ;		22-max current MLT
                                ;		23- max current ILAT
                                ;		24-average value of B
                                ;		25-average value of E
                                ;		26-field sample rate
                                ;		27-fields mode		 
                                ;		28-maximum upgoing proton flux
                                ;		29-maximum characteristic proton energy
                                ;		30-maximum upgoing oxygen flux
                                ;		31-maximum characteristic oxygen energy
                                ;		32-maximum upgoing helium flux
                                ;		33-maximum characteristic helium energy
                                ;		34-spacecraft potential -1.*V8_S
                                ;SPENCE ADDITIONS 09/25/2014                       
                                ;               35-start time
                                ;               36-stop time
                                ;               37-current interval
        
        IF KEYWORD_SET(extra_t) THEN BEGIN
           current_intervals=make_array(n_elements(start_points),38,/double)
        ENDIF ELSE BEGIN
           current_intervals=make_array(n_elements(start_points),35,/double)
        ENDELSE
        current_intervals(*,0)=start_points
        current_intervals(*,1)=stop_points
        current_intervals(*,2)=sign_jtemp(start_points)
        current_intervals(*,3)=1
        
        intervalparts_electrons_old=-1
        intervalparts_ions_old=-1
        valid_old=0.0
        for j=0L,n_elements(start_points)-1 do begin
           
                                ;define the interval points 
           
           intervalfields=(current_intervals(j,0))+findgen(current_intervals(j,1)+1-current_intervals(j,0))
           tempz=magz.y(intervalfields)
                                ;help,magz,/st
                                ;print,'current_indices ',current_intervals(j,0),current_intervals(j,1)
           intervalparts_electrons=where(je_tmp_time GE magz.x(current_intervals(j,0)) and je_tmp_time LE magz.x(current_intervals(j,1)))
           intervalparts_ions=where(ji_up_tmp_time GE magz.x(current_intervals(j,0)) and ji_up_tmp_time LE magz.x(current_intervals(j,1)))
           if intervalparts_electrons(0) EQ -1 then begin
              minitime=min(abs(je_tmp_time-magz.x(current_intervals(j,0))),intervalparts_electrons)
           endif
           if intervalparts_ions(0) EQ -1 then begin
              minitime=min(abs(ji_up_tmp_time-magz.x(current_intervals(j,0))),intervalparts_ions)
           endif
                                ;get the current from b and determine if to keep this event
           
           jmax=max(jtemp(intervalfields),indjmax)
           current_intervals(j,4)=jmax*sign_jtemp(start_points(j))
           if jmax LE current_threshold then begin
              IF NOT KEYWORD_SET(no_s) THEN current_intervals(j,3)=0.0
           endif
           
                                ;define the time of the max current
           
           
           current_intervals(j,20)=magz.x(intervalfields(indjmax))
           
           
                                ;get the electron current and determine if to keep this event
           
           
           sign=-1.*je_tmp_data(intervalparts_electrons)/abs(je_tmp_data(intervalparts_electrons))
           maxJe=max(abs(je_tmp_data(intervalparts_electrons)),ind)
           maxJe=maxJe*sign(ind)*1.6e-9 ;in microA/m2
           current_intervals(j,5)=maxJe
           if abs(maxJe)/abs(jmax) LE esa_j_delta_bj_ratio_threshold then begin
              IF NOT KEYWORD_SET(no_s) THEN current_intervals(j,3)=0.0
           endif
           
                                ;get the electron energy flux and dtermine if to keep this event
                                ;print,'intervalparts_electrons',intervalparts_electrons
                                ;help,jee_tmp_time
                                ;help,je_tmp_time
                                ;print,'jee start stop ',time_to_str(jee_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
                                ;print,'je start stop ',time_to_str(je_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
           sign=-1.*jee_ionos_tmp_data(intervalparts_electrons)/abs(jee_ionos_tmp_data(intervalparts_electrons))
           maxJEe_ionos=max(abs(jee_ionos_tmp_data(intervalparts_electrons)),ind)
           maxJEe_ionos=maxJEe_ionos*sign(ind)
           current_intervals(j,6)=maxJEe_ionos
           if abs(maxJEe_ionos) LE electron_eflux_ionos_threshold then begin
              IF NOT KEYWORD_SET(no_s) THEN current_intervals(j,3)=0.0
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
           
           
           
           
           
                                ;get the integrated electron dflux in ionosphere over this interval
           
           if intervalparts_electrons(0) NE -1 then begin
              if n_elements(intervalparts_electrons) EQ 1 then begin 
                                ;if intervalparts_electrons(0) NE intervalparts_electrons_old(n_elements(intervalparts_electrons_old)-1) or valid_old EQ 0.0 then begin
                 
                 
                 current_intervals(j,7)=speed(intervalparts_electrons)*part_res_Je(intervalparts_electrons)*jee_tmp_data(intervalparts_electrons)/2.0
                                ;endif
              endif else begin
                                ;if  intervalparts_electrons(0) EQ intervalparts_electrons_old(n_elements(intervalparts_electrons_old)-1) and valid_old EQ 1.0 then intervalparts_electrons=intervalparts_electrons(1:n_elements(intervalparts_electrons)-1)
                                ;if n_elements(intervalparts_electrons) EQ 1 then begin 
                 
                                ;current_intervals(j,7)=speed(intervalparts_electrons)*part_res_Je(intervalparts_electrons)*jee_tmp_data(intervalparts_electrons)/2.0
                                ;endif else begin	
                 
                                ;calculate the integrated contribution to electron dflux over this interval and add it to get total
                                ;print,'intervalparts_electrons ',intervalparts_electrons
                 current_intervals(j,7)=int_tabulated(findgen(n_elements(intervalparts_electrons))*speed(intervalparts_electrons)*part_res_je(intervalparts_electrons),jee_tmp_data(intervalparts_electrons),/double)
                                ;endelse
                 
              endelse
              
                                ;map result to ionosphere (sqrt of B since have integrated in x)
              
              current_intervals(j,7)=current_intervals(j,7)*sqrt(ratio(intervalparts_electrons(0)))
              
           endif
           
                                ;get integrated ion outflow mapped to ionosphere over this interval
           
           if intervalparts_ions(0) NE -1 then begin
              if n_elements(intervalparts_ions) EQ 1 then begin 
                                ;if intervalparts_ions(0) NE intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) or valid_old EQ 0.0 then begin
                 
                 current_intervals(j,12)=speed(intervalparts_ions)*part_res_ji(intervalparts_ions)*ji_tmp_data(intervalparts_ions)/2.0
                 current_intervals(j,13)=speed(intervalparts_ions)*part_res_ji(intervalparts_ions)*ji_up_tmp_data(intervalparts_ions)/2.0
                                ;endif
              endif else begin
                                ;if  intervalparts_ions(0) EQ intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) and valid_old EQ 1.0 then intervalparts_ions=intervalparts_ions(1:n_elements(intervalparts_ions)-1)
                                ;if n_elements(intervalparts_ions) EQ 1 then begin 
                                ;current_intervals(j,12)=speed(intervalparts_ions)*part_res_ji(intervalparts_ions)*ji_up_tmp_data(intervalparts_ions)/2.0
                 
                                ;endif else begin
                 
                 
                                ;calculate the integrated contribution to ion outflow over thsi interval
                 
                 current_intervals(j,12)=int_tabulated(findgen(n_elements(intervalparts_ions))*speed(intervalparts_ions)*part_res_ji(intervalparts_ions),ji_tmp_data(intervalparts_ions),/double)
                 current_intervals(j,13)=int_tabulated(findgen(n_elements(intervalparts_ions))*speed(intervalparts_ions)*part_res_ji(intervalparts_ions),ji_up_tmp_data(intervalparts_ions),/double)
                                ;print,'ji_tot_alf',Ji_tot_alf(jjj)
                 
                 
                                ;endelse
                 
              endelse
              
                                ;map result to ionosphere (sqrt of B since have integrated in x)
              
              current_intervals(j,12)=current_intervals(j,12)*sqrt(ratio(intervalparts_ions(0)))
              current_intervals(j,13)=current_intervals(j,13)*sqrt(ratio(intervalparts_ions(0)))
           endif
           
                                ;get max electron characteristic energy over this interval
           
           C_E=max(charE(intervalparts_electrons))
           current_intervals(j,8)=C_E
           
                                ;get max upgoing ion energy flux over this interval
           
           maxJEi=max(abs(jei_up_tmp_data(intervalparts_ions)),ind)
           current_intervals(j,9)=maxJEi
           
                                ;get max ion flux over this interval
           
           sign_ion=-1.*ji_tmp_data(intervalparts_ions)/abs(ji_tmp_data(intervalparts_ions))
           maxJi=max(abs(ji_tmp_data(intervalparts_ions)),ind)
           maxJi=maxJi*sign_ion(ind)
           current_intervals(j,10)=maxJi
           
                                ;get max upgoing ion flux over this interval
           
           maxJi_up=max(abs(ji_up_tmp_data(intervalparts_ions)),ind)
           current_intervals(j,11)=maxJi_up
           
                                ;get max characteristic ion energy over this interval
           
           C_Ei=max(charEi(intervalparts_ions))
           current_intervals(j,14)=C_Ei
           
                                ;get width of current filament in time (s)
           
           time_width=magz.x(current_intervals(j,1))-magz.x(current_intervals(j,0))
           
           current_intervals(j,15)=time_width
           
                                ;get width of the current filament at this altitude
           
           width=speed_mag_point(current_intervals(j,0))*abs(magz.x(current_intervals(j,0))-magz.x(current_intervals(j,1)))
                                ;print,'speed',speed_mag_point(current_intervals(j,0))
           current_intervals(j,16)=width
           
                                ;fields sample period
           current_intervals(j,26)=magz.x(intervalfields(indjmax)+1)-magz.x(intervalfields(indjmax))
           
                                ;get mag field amplitude
           
           db=max(magz.y(intervalfields))-min(magz.y(intervalfields))
           median_db=median(magz.y(intervalfields))
           current_intervals(j,17)=db
           current_intervals(j,24)=median_db
           if db LT delta_b_threshold then begin
              IF NOT KEYWORD_SET(no_s) THEN current_intervals(j,3)=0.0 ;threshold for reliablity of identification
           endif
           
                                ;get elec field amplitude
                                ;smooth to below proton gyro freq.
           
           smooth_int=ceil((1./proton_cyc_freq(intervalfields(indjmax)))/current_intervals(j,26))
           if smooth_int GT 1.0 and smooth_int LE n_elements(intervalfields)/4.0 then efield_smooth=smooth(efield_aligned(intervalfields),smooth_int) else efield_smooth=efield_aligned(intervalfields)
           
           de=max(efield_smooth)-min(efield_smooth)
           median_de=median(efield_aligned(intervalfields))
           current_intervals(j,18)=de
           current_intervals(j,25)=median_de
           if de LT delta_E_threshold then begin
              IF NOT KEYWORD_SET(no_s) THEN current_intervals(j,3)=0.0 ;threshold for reliablity of identification
           endif
                  ;now get orbit quantities
           
           get_data,'ORBIT',data=orb
           get_data,'MLT',data=mlt
           get_data,'ALT',data=alt
           get_data,'ILAT',data=ilat

           mintime=min(abs(mlt.x-magz.x(intervalfields(indjmax))),ind)
           
           
           
           current_intervals(j,19)=orb.y(ind)
           current_intervals(j,21)=alt.y(ind)	
           current_intervals(j,22)=mlt.y(ind)	
           current_intervals(j,23)=ilat.y(ind)	
           
           
           
           
           
                                ;fields_mode
           mintime=min(abs(fields_mode.time-magz.x(intervalfields(indjmax))),ind)
           current_intervals(j,27)=fields_mode.comp1(13,ind)
           
                                ;sc potential
           
           mintime=min(abs(sc_pot.x-magz.x(intervalfields(indjmax))),ind)
           current_intervals(j,34)=-1*sc_pot.y(ind)
           
                                ;e over b test
           ;; va=1000.0*alfven_speed_mlt(current_intervals(j,21),current_intervals(j,22))
           ;; e_over_b=(1.0e-3*current_intervals(j,18))/(current_intervals(j,17)*1.0e-9)
           ;; if e_over_b/va LT 1.0/eb_to_alfven_speed then current_intervals(j,3)=0.0
           
           intervalparts_electrons_old=intervalparts_electrons
           intervalparts_ions_old=intervalparts_ions	
           valid_old=current_intervals(j,3)

                                ;SPENCE edit
                                ;define total length of interval, as
                                ;well as start/stop time
           IF KEYWORD_SET(extra_t) THEN BEGIN
              current_intervals(j,35)=magz.x(current_intervals(j,0))
              current_intervals(j,36)=magz.x(current_intervals(j,1))
              current_intervals(j,37)=current_intervals(j,36)-current_intervals(j,35)
           ENDIF
        endfor
        
     endif
     
     
     if keyword_set(analyse_noise) then begin

;find intervals that may be longer than an individual interval and qualify for a Alfven event
;this is primarily included since noise may mean that large scale ALfven waves with big current are missed

        streaks=where(current_intervals(*,3) EQ 0.0)
;remove single points and get the start and end point of each streak

        count_reset=0.0
        start_streaks=[0.0]
        stop_streaks=[0.0]
        number_streaks=0.
        for j=0L,n_elements(streaks)-2 do begin
           if streaks(j)-streaks(j+1) EQ -1 then begin
              if count_reset EQ 0.0 then begin
                 start_streaks=[start_streaks,streaks(j)]	
                 count_reset=1.0	
              endif
           endif else begin
              if count_reset EQ 1.0 then begin
                 stop_streaks=[stop_streaks,streaks(j)]
                 number_streaks=number_streaks+1
                 count_reset=0.0
              endif
           endelse
        endfor
        start_streaks=start_streaks(1:n_elements(start_streaks)-1)
        stop_streaks=stop_streaks(1:n_elements(stop_streaks)-1)

        for m=0L,number_streaks-1 do begin
           position_streak=make_array((stop_streaks(m)-start_streaks(m))+1,/double,value=0.0)
           for n=1,(stop_streaks(m)-start_streaks(m)) do begin
              position_streak(n)=position_streak(n-1)+(current_intervals(start_streaks(m)+n,16)+current_intervals(start_streaks(m)+n-1,16))/2.
                                ;position_streak(n)=total(current_intervals(start_streaks(m):start_streaks(m)+n,16))-current_intervals(start_streaks(m),16)
           endfor
                                ;print,'posit_streak ',n_elements(position_streak)
                                ;print,'curret_streak ',n_elements(current_intervals(start_streaks(m):stop_streaks(m),24))
           
           if n_elements(position_streak) GE 3 then begin
              deltaBx_streak=deriv(position_streak,current_intervals(start_streaks(m):stop_streaks(m),24))
           endif else deltaBx_streak=position_streak*1.0e-12
           
           sign_jtemp_streak=abs(deltaBx_streak)/deltaBx_streak
           jtemp_streak=abs(1.0e-3*(deltaBx_streak)/1.26e-6)

           
           first_neg=1.
           first_pos=1.
           start_pos_streak=[0]
           stop_pos_streak=[0]
           start_neg_streak=[0]
           stop_neg_streak=[0]
           for mm=0L,n_elements(sign_jtemp_streak)-1 do begin
              
              if sign_jtemp_streak(mm) LT 0.0 and first_neg EQ 1.0 then begin
                 start_neg_streak=[start_neg_streak,mm]
                 first_neg=0.0
              endif
              if sign_jtemp_streak(mm) GT 0.0 and first_neg EQ 0.0 then begin
                 stop_neg_streak=[stop_neg_streak,mm-1]
                 first_neg=1.0
              endif
              
              if sign_jtemp_streak(mm) GT 0.0 and first_pos EQ 1.0 then begin
                 start_pos_streak=[start_pos_streak,mm]
                 first_pos=0.0
              endif
              if sign_jtemp_streak(mm) LT 0.0 and first_pos EQ 0.0 then begin
                 stop_pos_streak=[stop_pos_streak,mm-1]
                 first_pos=1.0
              endif
              
           endfor

           if sign_jtemp_streak(n_elements(sign_jtemp_streak)-1) LT 0.0 then stop_neg_streak=[stop_neg_streak,n_elements(sign_jtemp_streak)-1]
           if sign_jtemp_streak(n_elements(sign_jtemp_streak)-1) GT 0.0 then stop_pos_streak=[stop_pos_streak,n_elements(sign_jtemp_streak)-1]

           if n_elements(start_pos_streak) GT 1 then start_pos_streak=start_pos_streak(1:n_elements(start_pos_streak)-1) else start_pos_streak=-1
           if n_elements(stop_pos_streak) GT 1 then stop_pos_streak=stop_pos_streak(1:n_elements(stop_pos_streak)-1) else stop_pos_streak=-1
           if n_elements(start_neg_streak) GT 1 then start_neg_streak=start_neg_streak(1:n_elements(start_neg_streak)-1) else start_neg_streak=-1
           if n_elements(stop_neg_streak) GT 1 then stop_neg_streak=stop_neg_streak(1:n_elements(stop_neg_streak)-1) else stop_neg_streak=-1
           if start_pos_streak(0) NE -1 then begin	
              for mmm=0L,n_elements(start_pos_streak)-1 do begin
                                ;print,mmm,'start-stop',start_pos_streak(mmm),stop_pos_streak(mmm)
                 interval=start_streaks(m)+start_pos_streak(mmm)+findgen(stop_pos_streak(mmm)-start_pos_streak(mmm)+1)
                 jmax_b_pos_streak=max(abs(jtemp_streak(start_pos_streak(mmm):stop_pos_streak(mmm))),indj_streak_max)
                 if jmax_b_pos_streak GT current_threshold then begin
                    jmax_esa_pos_streak=max(abs(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),5)),indj_esa_streak_max)
                    if jmax_esa_pos_streak/jmax_b_pos_streak GT esa_j_delta_bj_ratio_threshold then begin
                       jemax_pos_streak=max(abs(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),6)),indje_esa_streak_max)
                       if jemax_pos_streak GT electron_eflux_ionos_threshold then begin
                          db=max(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),24))-min(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),24))
                          de=max(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),25))-min(current_intervals(streaks(start_pos_streak(mmm):stop_pos_streak(mmm)),25))
                          if db GT delta_b_threshold and de GT delta_E_threshold then begin
                             e_over_b=abs((1.0e-3*de)/(db*1.0e-9))
                             ;; va=1000.0*alfven_speed_mlt(current_intervals(j,21),current_intervals(j,22))
                             ;; if e_over_b/va LT 1.0/eb_to_alfven_speed then begin
                             
                             integrated_downgoing_eflux=total(current_intervals(interval,7))
                             integrated_iflux=total(current_intervals(interval,12))
                             integrated_upgoing_iflux=total(current_intervals(interval,13))
                             C_E_max_pos_streak=max(current_intervals(interval,8))
                             JEi_max_pos_streak=max(current_intervals(interval,9))
                             Ji_max_pos_streak=max(current_intervals(interval,10))
                             Ji_up_max_pos_streak=max(current_intervals(interval,11))
                             C_Ei_max_pos_streak=max(current_intervals(interval,14))
                             time_width_max_pos_streak=total(current_intervals(interval,15))
                             width_max_pos_streak=total(current_intervals(interval,16))
                             orbit_pos_streak=current_intervals(start_streaks(m)+start_pos_streak(mmm),19)
                             time_pos_streak=current_intervals(interval(indj_streak_max),20)
                             
                                ;print,'start ',time_to_str(current_intervals(interval,20),/ms)
                                ;print,'stop ',time_to_str(current_intervals(start_streaks(m)+stop_pos_streak(mmm),20),/ms)
                                ;print,'maxj ',time_to_str(current_intervals(interval(indj_streak_max),20),/ms)
                                ;print,'time ',time_to_str(current_intervals(interval,20),/ms)
                                ;print,'times ',current_intervals(interval,16)
                                ;print,'dbs ',current_intervals(interval,24)
                                ;print,'index',current_intervals(interval,1)-current_intervals(interval,0)
                             dt=magz.x(current_intervals(interval,1))-magz.x(current_intervals(interval,0))
                                ;print,'delta_t',dt
                                ;!p.multi=[0,1,3]
                             time_temp=current_intervals(interval,20)-current_intervals(interval(0),20)
                             data_temp=current_intervals(interval,24)
                                ;plot,time_temp,data_temp;,yrange=[6700.0,6800.0]
                                ;plot,time_temp,jtemp_streak(start_pos_streak(mmm):stop_pos_streak(mmm))
                                ;print,'sign',sign_jtemp_streak(start_pos_streak(mmm):stop_pos_streak(mmm))
                                ;pos_temp=position_streak(start_pos_streak(mmm):stop_pos_streak(mmm))-position_streak(start_pos_streak(mmm))
                                ;plot,time_temp,abs(1.0e-3*deriv(pos_temp,current_intervals(interval,24)/1.26e-6))
                                ;print,'position',position_streak(start_pos_streak(mmm)-1:stop_pos_streak(mmm))-position_streak(start_pos_streak(mmm))
                             
                                ;print,'Max current index',interval(indj_streak_max)
                                ;print,'max current index',start_streaks(m)+start_pos_streak(mmm)+indj_streak_max
                                ;print,'position',time_temp*7000.
                                ;return
                                ;current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max),0)=jtemp_pos_streak
                                ;current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max),1)=jtemp_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,2)=1.
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,3)=1.
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,4)=jmax_b_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,5)=jmax_esa_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,6)=jemax_pos_streak			
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,7)=integrated_downgoing_eflux			
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,8)=C_E_max_pos_streak			
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,9)=JEi_max_pos_streak			
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,10)=Ji_max_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,11)=Ji_up_max_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,12)=integrated_iflux
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,13)=integrated_upgoing_iflux
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,14)=C_Ei_max_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,15)=time_width_max_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,16)=width_max_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,17)=db
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,18)=de
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,19)=orbit_pos_streak
                             current_intervals(start_streaks(m)+start_pos_streak(mmm)+indj_streak_max,20)=time_pos_streak
;;						endif
                          endif
                       endif
                    endif
                    
                 endif	

              endfor
           endif
                                ;print,'start_neg_streak ',start_neg_streak
           if start_neg_streak(0) NE -1 then begin
              for mmm=0L,n_elements(start_neg_streak)-1 do begin
                 interval=start_streaks(m)+start_neg_streak(mmm)+findgen(stop_neg_streak(mmm)-start_neg_streak(mmm)+1)
                 jmax_b_neg_streak=max(abs(jtemp_streak(start_neg_streak(mmm):stop_neg_streak(mmm))),indj_streak_max)
                 if jmax_b_neg_streak GT current_threshold then begin
                    jmax_esa_neg_streak=max(abs(current_intervals(interval,5)),indj_esa_streak_max)
                    if jmax_esa_neg_streak/jmax_b_neg_streak GT esa_j_delta_bj_ratio_threshold then begin
                       jemax_neg_streak=max(abs(current_intervals(interval,6)),indje_esa_streak_max)
                       if jemax_neg_streak GT electron_eflux_ionos_threshold then begin
                          db=max(current_intervals(interval,24))-min(current_intervals(interval,24))
                          de=max(current_intervals(interval,25))-min(current_intervals(interval,25))
                          if db GT delta_b_threshold and de GT delta_E_threshold then begin
                             e_over_b=abs((1.0e-3*de)/(db*1.0e-9))
                             ;; va=1000.0*alfven_speed_mlt(current_intervals(j,21),current_intervals(j,22))
                             ;; if e_over_b/va LT 1.0/eb_to_alfven_speed then begin
                             
                             
                             integrated_downgoing_eflux=total(current_intervals(interval,7))
                             integrated_iflux=total(current_intervals(interval,12))
                             integrated_upgoing_iflux=total(current_intervals(interval,13))
                             C_E_max_neg_streak=max(current_intervals(interval,8))
                             JEi_max_neg_streak=max(current_intervals(interval,9))
                             Ji_max_neg_streak=max(current_intervals(interval,10))
                             Ji_up_max_neg_streak=max(current_intervals(interval,11))
                             C_Ei_max_neg_streak=max(current_intervals(interval,14))
                             time_width_max_neg_streak=total(current_intervals(interval,15))
                             width_max_neg_streak=total(current_intervals(interval,16))
                             orbit_neg_streak=current_intervals(start_streaks(m)+start_neg_streak(mmm),19)
                             time_neg_streak=current_intervals(interval(indj_streak_max),20)
                             
                                ;print,current_intervals(streaks(start_neg_streak(mmm)),20)
                             
                                ;current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,0)=jtemp_neg_streak
                                ;current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,1)=jtemp_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,2)=1.
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,3)=1.
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,4)=jmax_b_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,5)=jmax_esa_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,6)=jemax_neg_streak			
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,7)=integrated_downgoing_eflux			
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,8)=C_E_max_neg_streak			
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,9)=JEi_max_neg_streak			
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,10)=Ji_max_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,11)=Ji_up_max_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,12)=integrated_iflux
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,13)=integrated_upgoing_iflux
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,14)=C_Ei_max_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,15)=time_width_max_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,16)=width_max_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,17)=db
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,18)=de
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,19)=orbit_neg_streak
                             current_intervals(start_streaks(m)+start_neg_streak(mmm)+indj_streak_max,20)=time_neg_streak
;;						endif
                          endif
                       endif
                    endif
                    
                 endif	

              endfor
           endif
        endfor

     endif
;remove crap data


     keep=where(current_intervals(*,3) NE 0.0)
     print,'keep',keep
     current_intervals=current_intervals(keep,*)

     print,'number of intervals',n_elements(keep)
     if jjj GT 0 or not keyword_set(filename) then $
        filename='/SPENCEdata/software/sdt/batch_jobs/Alfven_study_14F/output_alfven_stats/Dartmouth_dflux_'+$
                 strcompress(orbit_num+'_'+string(jjj),/remove_all)
     ;if jjj GT 0 or not keyword_set(filename) then filename='/home/spencerh/biz.data'
     IF KEYWORD_SET(analyse_noise) THEN filename += '_analysenoise'
     IF KEYWORD_SET(smooth) THEN filename += '_smooth'
     IF KEYWORD_SET(extra_t) THEN filename += '_extratimes'
     IF KEYWORD_SET(no_s) THEN filename += '_noscreen'
     IF KEYWORD_SET(ucla_mag_despin) THEN filename += '_magdespin'

     print,filename,jjj
     openw,unit1,filename,/get_lun
     printf,unit1,n_elements(keep),29

     printf,unit1,' Column No.  	1-Orbit number'
     printf,unit1,'			2-max current time (based on location of max current'
     printf,unit1,'			3-max current altitude'
     printf,unit1,'			4-max current MLT'
     printf,unit1,'			5-max current ILAT'			
     printf,unit1,'			6-maximum size of the delta B current in that interval'
     printf,unit1,'			7-maximum size of the current from the Electron esa at s/c alt.'
     printf,unit1,'			8-maximum size of the electron energy flux mapped to the ionosphere-positive is downwards'
     printf,unit1,'			9-integrated downgoing electron flux over that interval at ionosphere'
     printf,unit1,'			10-maximum characteristic electron energy from that interval'
     printf,unit1,'			11-maximum ion energy flux at the s/c altitude'
     printf,unit1,'			12-maximum ion flux at the s/c altitude'
     printf,unit1,'			13-maximum upgoing ion flux at the s/c altitude'
     printf,unit1,'			14-integrated ion flux over the interval at ionosphere'
     printf,unit1,'			15-integrated upgoing only ion flux over the interval at ionosphere'
     printf,unit1,'			16-maximum characteristic ion energy'
     printf,unit1,'			17-width of the current fiament in time (s)'
     printf,unit1,'			18-width of the current filament in m at the s/c altitude'
     printf,unit1,'			19-magnetic field amplitude (nT)'
     printf,unit1,'			20-electric field amplitude (mV/m)'
     printf,unit1,'			21-fields mode'				
     printf,unit1,'			22-fields sample period'
     printf,unit1,'			23-maximum upgoing proton flux'
     printf,unit1,'			24-maximum upgoing proton characteristic energy'
     printf,unit1,'			25-maximum upgoing oxygen flux'
     printf,unit1,'			26-maximum upgoing oxygen characteristic energy'
     printf,unit1,'			27-maximum upgoing helium flux'
     printf,unit1,'			28-maximum upgoing helium characteristic energy'
     printf,unit1,'			29-spacecraft potential'
     IF KEYWORD_SET(extra_t) THEN BEGIN
        printf,unit1,'			   30-current interval start time'
        printf,unit1,'			   31-current interval stop time'
        printf,unit1,'			   32-current interval duration'
     ENDIF
     printf,unit1,'total electron dflux at ionosphere',Jee_tot(jjj)
     printf,unit1,'total Alfven electron dflux at ionosphere',total(current_intervals(*,7))
     printf,unit1,'total ion outflow at ionosphere',Ji_tot(jjj)
     printf,unit1,'total Alfven ion outflow at ionosphere',total(current_intervals(*,12))
     printf,unit1,'total upward only ion outflow at ionosphere',Ji_up_tot(jjj)
     printf,unit1,'total upward only Alfven ion outflow at ionosphere',total(current_intervals(*,13))
     

     IF KEYWORD_SET(extra_t) THEN BEGIN
        for jj=0L,n_elements(keep)-1 do begin
           printf,unit1,format='(I9,A24,27G13.6,2A24,G13.6)',current_intervals(jj,19),time_to_str(current_intervals(jj,20),/ms),$
                  current_intervals(jj,21),current_intervals(jj,22),current_intervals(jj,23),current_intervals(jj,4),$
                  current_intervals(jj,5),current_intervals(jj,6),current_intervals(jj,7),current_intervals(jj,8),$
                  current_intervals(jj,9),current_intervals(jj,10),current_intervals(jj,11),current_intervals(jj,12),$
                  current_intervals(jj,13),current_intervals(jj,14),current_intervals(jj,15),current_intervals(jj,16),$
                  current_intervals(jj,17),current_intervals(jj,18),current_intervals(jj,26),current_intervals(jj,27),$
                  current_intervals(jj,28),current_intervals(jj,29),current_intervals(jj,30),current_intervals(jj,31),$
                  current_intervals(jj,32),current_intervals(jj,33),current_intervals(jj,34),time_to_str(current_intervals(jj,35)),$
                  time_to_str(current_intervals(jj,36)),current_intervals(jj,37)
        endfor
     ENDIF ELSE BEGIN
        for jj=0L,n_elements(keep)-1 do begin
           printf,unit1,format='(I9,A24,27G13.6)',current_intervals(jj,19),time_to_str(current_intervals(jj,20),/ms),$
                  current_intervals(jj,21),current_intervals(jj,22),current_intervals(jj,23),current_intervals(jj,4),$
                  current_intervals(jj,5),current_intervals(jj,6),current_intervals(jj,7),current_intervals(jj,8),$
                  current_intervals(jj,9),current_intervals(jj,10),current_intervals(jj,11),current_intervals(jj,12),$
                  current_intervals(jj,13),current_intervals(jj,14),current_intervals(jj,15),current_intervals(jj,16),$
                  current_intervals(jj,17),current_intervals(jj,18),current_intervals(jj,26),current_intervals(jj,27),$
                  current_intervals(jj,28),current_intervals(jj,29),current_intervals(jj,30),current_intervals(jj,31),$
                  current_intervals(jj,32),current_intervals(jj,33),current_intervals(jj,34)
        endfor

     ENDELSE
     free_lun,unit1


  endfor

  IF KEYWORD_SET(save_d) THEN BEGIN
     outdataf = "orbit" + strcompress(orbit_num+'_'+string(jjj),/remove_all) + ".dat"
     save, /variables, filename=outdataf
  ENDIF
  
  return 
end
