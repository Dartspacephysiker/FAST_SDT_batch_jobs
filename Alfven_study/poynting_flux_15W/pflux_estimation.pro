;**
;PFLUX ESTIMATES
;**

;NOTE: Where the comment ";get magnitude of electric and magnetic field" is located (currently line 208), the electric field is converted to its magnitude and we lose any component information.
;We can probably do better than that, especially if we wanted signed Poynting flux info.

pro pflux_estimation,EVENTNUM=eventNum,ORBNUM=orbNum,ALFEVENTS=alfEvents,BURST=burst

  @startup       ;run necessary sdt startup

  defOrb = 1000  ;default orbit if none provided

  ;; load DB file
  dbDir = '/SPENCEdata/Research/Cusp/ACE_FAST/scripts_for_processing_Dartmouth_data/'
  dbFile = 'Dartdb_02282015--500-14999--maximus.sav'
  restore,dbDir + dbFile

  ;;handle orbit or event number
  IF KEYWORD_SET(orbNum) THEN BEGIN
     events_i = where(maximus.orbit EQ orbNum)
     nEvents = n_elements(events_i)
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(eventNum) THEN BEGIN
        events_i = eventNum
        nEvents = 1.
     ENDIF ELSE BEGIN 
        print,"No orbnum or eventnum provided! Setting orbNum=" + strcompress(defOrb,/remove_all) + "..."
        orbNum = defOrb;
        events_i = where(maximus.orbit EQ orbNum)
        nEvents = n_elements(events_i)
     ENDELSE
  ENDELSE

  ;;create Alfven event objects
  IF events_i[0] NE -1 THEN BEGIN
     alfEvents = OBJARR(nEvents)
     FOR i=0, nEvents-1 DO BEGIN
        alfEvents(i) = OBJ_NEW('alfEvent')
     ENDFOR
  ENDIF ELSE BEGIN
     PRINT, "No events for the orbit number provided, or, if you instead provided an event number, there is no such event."
     PRINT, "Exiting..."
     RETURN
  ENDELSE
  
  ;;energy ranges
  ;; if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.];use 0.0 for lower bound since the sc_pot is used to set this
  ;; if not keyword_set(energy_ions) then energy_ions=[0.,500.]            ;use 0.0 for lower bound since the sc_pot is used to set this

  ;tstart=maximus.start_time(events_i[0])
  tstart=0
  
  dat = get_fa_ees(tstart,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; Electron current - line plot
  get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons

  ;;remove spurious crap
  get_data,'Je',data=tmpj
  
  npoints = N_ELEMENTS(tmpj.x)

  keep=where(finite(tmpj.y) NE 0)
  tmpj.x=tmpj.x(keep)
  tmpj.y=tmpj.y(keep)
  
  keep=where(abs(tmpj.y) GT 0.0)
  tx=tmpj.x(keep)
  ty=tmpj.y(keep)

  nkeep = N_ELEMENTS(keep)

  IF nkeep NE npoints THEN print, "Junking garbage Je data: Losing " + strcompress(npoints - nkeep,/remove_all) + " data points of " + strcompress(npoints,/remove_all) + " total."
  
  ;;get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)
    
  ;;throw away the first 10  points since they are often corrupted
  store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  ;;Use the electron data to define the time ranges for this orbit	
  ;; get_data,'Je',data=je
  ;; part_res_je=make_array(n_elements(Je.x),/double)
  ;; for j=1,n_elements(Je.x)-1 do begin
  ;;    part_res_je(j)=abs(Je.x(j)-Je.x(j-1))
  ;; endfor
  ;; part_res_Je(0)=part_res_Je(1)
  ;; gap=where(part_res_je GT 10.0)
  ;; if gap(0) NE -1 then begin
  ;;    separate_start=[0,where(part_res_je GT 10.0)]
  ;;    separate_stop=[where(part_res_je GT 10.0),n_elements(Je.x)-1]
  ;; endif else begin
  ;;    separate_start=[0]
  ;;    separate_stop=[n_elements(Je.x)-1]
  ;; endelse
  
  ;;remove esa burp when switched on
  ;; turn_on=where(part_res_je GT 300.0)
  ;; if turn_on(0) NE -1 then begin
  ;;    turn_on_separate=make_array(n_elements(turn_on),/double)
  ;;    for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
  ;;    separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
  ;; endif
  
  ;;identify time indices for each interval
  ;; count=0.0
  ;; for j=0,n_elements(separate_start)-1 do begin
  ;;    if (separate_stop(j)-separate_start(j)) GT 10 then begin
  ;;       count=count+1
  ;;       if count EQ 1.0 then begin
  ;;          time_range_indices=transpose([separate_start(j)+1,separate_stop(j)-1])
  ;;       endif else begin
  ;;          time_range_indices=[time_range_indices,transpose([separate_start(j),separate_stop(j)-1])]
  ;;       endelse
  ;;    endif
  ;; endfor
  
  ;; ;;identify interval times
  ;; time_range=je.x(time_range_indices)
  ;; number_of_intervals=n_elements(time_range(*,0))

  time_range=[[str_to_time(maximus.start_time(events_i[0]))],[str_to_time(maximus.stop_time(events_i[-1]))]]
  
  ;; print,'number_of_intervals',number_of_intervals
  
  ;;loop over each time interval
  ;; ji_tot=make_array(number_of_intervals,/double)
  ;; ji_up_tot=make_array(number_of_intervals,/double)
  ;; jee_tot=make_array(number_of_intervals,/double)
  ;; Ji_tot_alf=make_array(number_of_intervals,/double)
  ;; Ji_up_tot_alf=make_array(number_of_intervals,/double)
  ;; Jee_tot_alf=make_array(number_of_intervals,/double)
  
  ;;get despun mag data if keyword set
  if keyword_set(ucla_mag_despin) then ucla_mag_despin



  ;;begin looping each event
  for jjj=0,nEvents-1 do begin
     print,format='("time_range ",A0,TR4,A0)',time_to_str(time_range(jjj,0),/msec),time_to_str(time_range(jjj,1),/msec)
     
     ;; times for this event
     t1 = str_to_time(maximus.start_time(events_i[jjj]))
     t2 = str_to_time(maximus.stop_time(events_i[jjj]))

     ;;get indices for time range
     ;;this might not work
     time_range_indices = value_locate(je.x,time_range)

     ;;get orbit number for filenames		
     get_data,'ORBIT',data=tmp
     orbit=tmp.y(0)
     orbit_num=strcompress(string(tmp.y(0)),/remove_all)
     
     je_tmp_time=je.x(time_range_indices(jjj,0):time_range_indices(jjj,1))
     je_tmp_data=je.y(time_range_indices(jjj,0):time_range_indices(jjj,1))
     
     store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}
     
     ;;get fields quantities
     data_valid=1.0
     dat=get_fa_fields('MagDC',tstart,/start)
     if dat.valid eq 0 then begin
        print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
        data_valid=0.0
     endif else begin
        if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_range(jjj,0),time_range(jjj,1),/store)
        dat=get_fa_fields('V5-V8_S',tstart,/start)
        if dat.valid eq 0 then begin
           print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
           data_valid=0.0
        endif else begin
           spacecraft_potential=get_fa_fields('V8_S',time_range(jjj,0),time_range(jjj,1))
           efieldV58=get_fa_fields('V5-V8_S',time_range(jjj,0),time_range(jjj,1))
           efieldV1214=get_fa_fields('V1-V2_S',time_range(jjj,0),time_range(jjj,1))
           if efieldV1214.valid eq 0 then begin
              print,'No V1-V2 data - trying V1-V4'
              efieldV1214=get_fa_fields('V1-V4_S',time_range(jjj,0),time_range(jjj,1))
              if efieldV1214.valid eq 0 then begin
                 print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
                 data_valid=0.0
              endif 
           endif 
        endelse
     endelse	
     
     
     if data_valid NE 0.0 then begin
        
        ;;get E field and B field on same time scale
        FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
        
        ;;get magnitude of electric and magnetic field
        efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
        ;; if not keyword_set(ucla_mag_despin) then begin   
           get_data,'MagDCcomp1',data=magx
           get_data,'MagDCcomp2',data=magy
           get_data,'MagDCcomp3',data=magz
        ;; endif else begin
        ;;    get_data,'dB_fac_v',data=db_fac
        ;;    mintime=min(abs(time_range(jjj,0)-db_fac.x),ind1)
        ;;    mintime=min(abs(time_range(jjj,1)-db_fac.x),ind2)
           
        ;;    magx={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,0)}
        ;;    magy={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,2)}
        ;;    magz={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,1)}
        ;; endelse
        
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
        fields_mode=get_fa_fields('DataHdr_1032',time_range(jjj,0),time_range(jjj,1))
        
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

        ;; get moments/integrals of various fluxes
        ;; get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='JEe_tot',energy=energy_electrons,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='JEe',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='Je',energy=energy_electrons,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='Je_lc',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot
        
        ;; get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='JEi',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='Ji',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='JEi_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
        ;; get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_range(jjj,0),t2=time_range(jjj,1), $
        ;;                name='Ji_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
        
        ;; get_data,'Je',data=tmp
        ;; get_data,'Ji',data=tmpi
        ;; ;;remove crap
        ;; keep1=where(finite(tmp.y) NE 0 and finite(tmpi.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; keep2=where(abs(tmp.y) GT 0.0 and abs(tmpi.y) GT 0.0)
        ;; je_tmp_time=tmp.x(keep2)
        ;; je_tmp_data=tmp.y(keep2)
        ;; store_data,'Je',data={x:je_tmp_time,y:je_tmp_data}
        
        ;; get_data,'JEe',data=tmp
        ;; ;;remove crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; jee_tmp_time=tmp.x(keep2)
        ;; jee_tmp_data=tmp.y(keep2)
        ;; store_data,'JEe',data={x:jee_tmp_time,y:jee_tmp_data}
        
        ;; get_data,'JEe_tot',data=tmp
        ;; ;;remove crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; jee_tot_tmp_time=tmp.x(keep2)
        ;; jee_tot_tmp_data=tmp.y(keep2)
        ;; store_data,'JEe_tot',data={x:jee_tot_tmp_time,y:jee_tot_tmp_data}
        
        ;; get_data,'Je_lc',data=tmp
        ;; ;;remove_crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; je_lc_tmp_time=tmp.x(keep2)
        ;; je_lc_tmp_data=tmp.y(keep2)
        ;; store_data,'Je_lc',data={x:je_lc_tmp_time,y:je_lc_tmp_data}
        
        ;; get_data,'Ji',data=tmp
        ;; ;;remove crap	
        ;; ;;keep1=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep2=where(abs(tmp.y) GT 0.0)
        ;; ji_tmp_time=tmp.x(keep2)
        ;; ji_tmp_data=2.0*tmp.y(keep2) ;;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        ;; store_data,'Ji',data={x:ji_tmp_time,y:ji_tmp_data}
        
        ;; get_data,'JEi',data=tmp
        ;; ;;remove crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; jEi_tmp_time=tmp.x(keep2)
        ;; jEi_tmp_data=tmp.y(keep2)
        ;; store_data,'JEi',data={x:jEi_tmp_time,y:jEi_tmp_data}
        
        ;; get_data,'JEi_up',data=tmp
        ;; ;;remove crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; jEi_up_tmp_time=tmp.x(keep2)
        ;; jEi_up_tmp_data=tmp.y(keep2)
        ;; store_data,'JEi_up',data={x:jEi_up_tmp_time,y:jEi_up_tmp_data}
        
        ;; get_data,'Ji_up',data=tmp
        ;; ;;remove crap
        ;; ;;keep=where(finite(tmp.y) NE 0)
        ;; tmp.x=tmp.x(keep1)
        ;; tmp.y=tmp.y(keep1)
        ;; ;;keep=where(abs(tmp.y) GT 0.0)
        ;; ji_up_tmp_time=tmp.x(keep2)
        ;; ji_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
        ;; store_data,'Ji_up',data={x:ji_up_tmp_time,y:ji_up_tmp_data}


        get_data,'fa_vel',data=vel
        speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
        
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

        ;;calculate the current from mag
        deltaBX=deriv(position,magz.y)
        jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
        sign_jtemp=abs(deltaBx)/deltaBx
        store_data,'jtemp',data={x:magz.x,y:jtemp}

        ;;terminate the events before the last point
        if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)

        start_points=[0]
        stop_points=[0]
        
        ;;get current events
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

        current_events=make_array(n_elements(start_points),42,/double)
        current_events(*,0)=start_points
        current_events(*,1)=stop_points
        current_events(*,2)=sign_jtemp(start_points)
        current_events(*,3)=1
        
        eventparts_electrons_old=-1
        eventparts_ions_old=-1
        valid_old=0.0
        for j=0L,n_elements(start_points)-1 do begin
           
           ;;define the event points 
           eventfields=(current_events(j,0))+findgen(current_events(j,1)+1-current_events(j,0))
           tempz=magz.y(eventfields)
           fields_res_event=magz.x(eventfields)-magz.x(eventfields-1)

           ;;get the current from b and determine if to keep this event
           jmax=max(jtemp(eventfields),indjmax)
           current_events(j,4)=jmax*sign_jtemp(start_points(j))
           if jmax LE current_threshold then begin
              current_events(j,3)=0.0
           endif
           
           ;;define the time of the max current
           current_events(j,20)=magz.x(eventfields(indjmax))

           ;;get mag field amplitude
           db=max(magz.y(eventfields))-min(magz.y(eventfields))
           median_db=median(magz.y(eventfields))
           current_events(j,17)=db
           current_events(j,24)=median_db
           if db LT delta_b_threshold then current_events(j,3)=0.0 ;threshold for reliablity of identification

           ;;get elec field amplitude
           ;;smooth to below proton gyro freq.
           smooth_int=ceil((1./proton_cyc_freq(eventfields(indjmax)))/current_events(j,26))
           if smooth_int GT 1.0 and smooth_int LE n_elements(eventfields)/4.0 then efield_smooth=smooth(fields.comp2(eventfields),smooth_int) else efield_smooth=fields.comp2(eventfields)
           
           de=max(efield_smooth)-min(efield_smooth)
           median_de=median(fields.comp2(eventfields))
           current_events(j,18)=de
           current_events(j,25)=median_de
           if de LT delta_E_threshold then current_events(j,3)=0.0 ;threshold for reliablity of identification

           ;;now get orbit quantities
           get_data,'ORBIT',data=orb
           get_data,'MLT',data=mlt
           get_data,'ALT',data=alt
           get_data,'ILAT',data=ilat

           mintime=min(abs(mlt.x-magz.x(eventfields(indjmax))),ind)
           
           current_events(j,19)=orb.y(ind)
           current_events(j,21)=alt.y(ind)	
           current_events(j,22)=mlt.y(ind)	
           current_events(j,23)=ilat.y(ind)	     				
           
           ;;fields_mode
           mintime=min(abs(fields_mode.time-magz.x(eventfields(indjmax))),ind)
           current_events(j,27)=fields_mode.comp1(13,ind)
           
           ;;sc potential
           mintime=min(abs(sc_pot.x-magz.x(eventfields(indjmax))),ind)
           current_events(j,34)=-1*sc_pot.y(ind)
           
        endfor
        
     endif

  endfor

end