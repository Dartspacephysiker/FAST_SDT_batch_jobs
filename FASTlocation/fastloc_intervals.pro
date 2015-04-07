pro fastloc_intervals,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
                   t1=t1,t2=t2,filterfreq=filterfreq,$
                   burst=burst,heavy=heavy,ucla_mag_despin=ucla_mag_despin,keep_alfven_only=keep_alfven_only, $
                   png_sumplot=png_sumplot,png_ourevents=png_ourevents, $
                   SKIP_IF_FILE_EXISTS=skip_if_file_exists

  fastloc_dir = '/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'

  list_of_attempted_repeats_file='list_of_attempted_repeats--fastloc_intervals.txt'

;  IF NOT KEYWORD_SET(skip_if_file_exists) THEN skip_if_file_exists=1


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
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
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
  
  ;;begin looping each interval
  for jjj=0,number_of_intervals-1 do begin
     print,'time_range',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))
     
     ;;get orbit number for filenames		
     get_data,'ORBIT',data=tmp
     orbit=tmp.y(0)
     orbStr=strcompress(string(tmp.y(0)),/remove_all)
                                ;filename for output file
     curfile = fastloc_dir + 'batch_output__intervals/'+'Dartmouth_fastloc_intervals_'+strcompress(orbStr,/remove_all)+'_'+strcompress(jjj,/remove_all)
     IF KEYWORD_SET(burst) THEN BEGIN
        curfile = curfile + '--burst'
     ENDIF
     
     ;;make sure we're not overwriting
     IF file_test(curfile) THEN BEGIN
        right_now=strmid(timestamp(),0,13)
        IF NOT KEYWORD_SET(skip_if_file_exists) THEN BEGIN
           curfile = curfile + "--" + right_now
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(skip_if_file_exists) THEN BEGIN
              OPENU,replun,fastloc_dir + 'batch_output__intervals/' + list_of_attempted_repeats_file,/APPEND,/GET_LUN
              PRINTF,replun,FORMAT='(A24,T30,A50)',right_now,curfile
              free_lun,replun
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
        
        ;;get_orbit data
        get_fa_orbit,je_tmp_time,/time_array,/all

        ;Use the following line if you don't want to line the times up with je_tmp_time
        ;; get_fa_orbit,time_ranges(jjj,0),time_ranges(jjj,1),DELTA_T=5,/ALL,DRAG_PROP=1
        ;drag_prop is slightly more accurate, but slower. Takes stock of atmosph. drag
        ; turn it off if you want...

        ;;define loss cone angle
        get_data,'ALT',data=alt
        loss_cone_alt=alt.y(0)*1000.0
        lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI
        get_data,'ILAT',data=ilat
        north_south=abs(ilat.y(0))/ilat.y(0)
        
        fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
        
        ;;get the spacecraft potential per spin
        spin_period=4.946       ; seconds
        
        get_data,'fa_vel',data=vel
        speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
        
        ;; old_pos=0.
        ;; position=make_array(n_elements(magz.x),/double)
        ;; speed_mag_point=make_array(n_elements(magz.x),/double)
        ;; for j=0L,n_elements(magz.x)-2 do begin
        ;;    speed_point_ind=min(abs(vel.x-magz.x(j)),ind)
        ;;    ;;print,ind
        ;;    speed_mag_point(j)=speed(ind)
        ;;    samplingperiod=magz.x(j+1)-magz.x(j)
        ;;    ;;position=make_array(n_elements(magz.x),/double)
        ;;    position(j)=old_pos+speed_mag_point(j)*samplingperiod
        ;;    old_pos=position(j)
        ;; endfor        

        ;;now get orbit quantities
        get_data,'ORBIT',data=orb
        get_data,'MLT',data=mlt
        get_data,'ALT',data=alt
        get_data,'ILAT',data=ilat

        ;;fields_mode
        ;;get fields mode nearest to each je_tmp_time point
        fieldsmode_arr = MAKE_ARRAY(n_elements(je_tmp_time),/DOUBLE)
        FOR i=0,N_ELEMENTS(je_tmp_time) -1 DO BEGIN
           near = Min(Abs(fields_mode.time-je_tmp_time[i]), index)
           IF near LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = !Values.F_NAN
        ENDFOR

     endif

     
;;if jjj GT 0 or not keyword_set(filename) then
;;filename='/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'+'Dartmouth_fastloc_intervals'+strcompress(orbStr+'_'+string(jjj)+"_magcal_v"
;;+ string(version)+"_burst",/remove_all)
     if jjj GT 0 or not keyword_set(filename) then filename= curfile

     print,filename,jjj
     openw,unit1,filename,/get_lun
     printf,unit1,FORMAT='("time range: ",A24,T40,A24)',time_to_str(time_ranges(jjj,0),/ms),time_to_str(time_ranges(jjj,1),/ms)

     printf,unit1,' Column No.  	1-Orbit number'
     printf,unit1,'                     2-time'
     printf,unit1,'                     3-altitude'
     printf,unit1,'                     4-MLT'
     printf,unit1,'                     5-ILAT'			
     printf,unit1,'                     6-fields mode'

     for jj=0L,n_elements(je_tmp_time)-1 do begin
        printf,unit1,format='(I9,A24,4G13.6)',orbit,time_to_str(je_tmp_time[jj],/ms),alt.y[jj],mlt.y[jj],ilat.y[jj],fieldsmode_arr[jj]
     ENDFOR
  ENDFOR  
  
free_lun,unit1


  return 
end
