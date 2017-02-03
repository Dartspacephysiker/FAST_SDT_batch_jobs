PRO journal_figuring_out_teams__jp_vs_jp2d_comparison__looks_good__01052015
  
  ;; 01/05/2015
  ;; With data from orbit 1676 (the one referenced in the TEAMS instrument paper) loaded into SDT you
  ;;should find that while get_3dt (returns 'jp') and get_2dt (returns 'jp2d') return different numbers
  ;;of points originally (402 and 576, respectively), when they are winnowed based on finiteness and
  ;;non-zeroness (neonym!) as well as lat/long (Holzworth/Meng Auroral Oval), jp and jp2d have an equal
  ;;number of samples, 356.
  ;;Go ahead and try it with some other orbits, s'il vous plaît
  
  exp_teams_dir = '/SPENCEdata/software/sdt/batch_jobs/explore_TEAMS/'

  ;;; BEGIN RIPOFF ALFVEN_STATS_5
  ;;; We're doing this in order to divide an orbit up into intervals the way Chaston does
  
  ;; data = get_fa_tsop_hdr(time) ;'Tms_HO_Survey_Packet_Hdr'
  ;; data = get_fa_tsah_hdr(time) ;'Tms_He_Survey_Packet_Hdr'

  ;; Get the orbit data
  IF get_sdt_timespan(t1,t2) THEN BEGIN
     print,'timespan is from ',time_to_str(t1),' to ',time_to_str(t2)
  ENDIF ELSE BEGIN
     print,' could not get timespan...'
  ENDELSE
  orbit_file=fa_almanac_dir()+'/orbit/predicted'
  get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'ORBIT',data=tmp
  orbit=tmp.y[0]
  orbit_num=strcompress(string(tmp.y[0]),/remove_all)

  ;; If no data exists, return to main
  t=0
  dat = get_fa_tsp(t,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; proton current - spectrogram
  if not keyword_set(energy_ions) then energy_ions=[0.,500.] ;use 0.0 for lower bound since the sc_pot is used to set this

  ;;  if keyword_set(burst) then begin
  ;;     get_2dt_ts,'j_3d','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  ;;  endif else begin
  ;; to my great confusion, these produce a different number of data points
     get_3dt,'j_3d','fa_tsp',name='Jp',t=t
     get_2dt,'j_3d','fa_tsp',name='Jp2d'
  ;;  endelse
  
  ;; get the data into IDL
  get_data,'Jp',data=tmpj
  get_data,'Jp2d',data=tmpj2d
  ;;  for i=0,10 do print,time_to_str(tmpj.x(i))time_to_str(tmpj2d.x(i))
  ;;  for i=0,10 do print,str(tmpj.y(i,0))+", "+str(tmpj2d.y(i,0))

  ;; 1. Do this stuff for Jp from get_3dt (will repeat for Jp2d below)
  ;; remove spurious crap
  ;; have to be finite
  keep=where(finite(tmpj.y(*,0)) NE 0 AND finite(tmpj.y(*,1)) NE 0 AND finite(tmpj.y(*,2)) NE 0)
  tx=tmpj.x(keep)
  ty=[[tmpj.y(keep,0)],[tmpj.y(keep,1)],[tmpj.y(keep,2)]]
  print,"Original Jp data has " + strcompress(n_elements(tmpj.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj.x)-n_elements(keep)) + " by removing junk data"
  ;; and have to be greater than zero
  keep=where(abs(ty(*,0)) GT 0.0 AND abs(ty(*,1)) GT 0.0 AND abs(ty(*,2)) GT 0.0)
  tx=tx(keep)
;  ty=ty(keep)
  ty=ty(keep,*)
  
  ;; get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
  ;; ty=ty(time_order)
  ty=ty(time_order,*)
  
  ;; throw away the first 10  points since they are often corrupted
  if not keyword_set(burst) then begin
     store_data,'Jp',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1,*)}
  endif else begin
     store_data,'Jp',data={x:tx,y:ty}
  endelse
  
  ;; eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Jp',data=jp
  get_fa_orbit,/time_array,jp.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
  store_data,'Jp',data={x:jp.x(keep),y:jp.y(keep,*)}

  ;; Use the proton data to define the time ranges for this orbit	
  ;; just looking at the difference between consecutive time stamps
  max_t_gap=10.0
  get_data,'Jp',data=jp
  part_res_jp=make_array(n_elements(Jp.x),/double)
  for j=1,n_elements(Jp.x)-1 do begin
     part_res_jp(j)=abs(Jp.x(j)-Jp.x(j-1))
  endfor
  part_res_Jp(0)=part_res_Jp(1)
  gap=where(part_res_jp GT max_t_gap)
  if gap(0) NE -1 then begin
     separate_start=[0,where(part_res_jp GT max_t_gap)]
     separate_stop=[where(part_res_jp GT max_t_gap),n_elements(Jp.x)-1]
  endif else begin
     separate_start=[0]
     separate_stop=[n_elements(Jp.x)-1]
  endelse
  
  ;; remove esa burp when switched on
  if not keyword_set(burst) then begin
     turn_on=where(part_res_jp GT 300.0)
     if turn_on(0) NE -1 then begin
        turn_on_separate=make_array(n_elements(turn_on),/double)
        for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
        separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
     endif
  endif

  ;; identify time indices for each interval
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
  
  ;; identify interval times
  time_ranges=jp.x(time_range_indices)
  number_of_intervals=n_elements(time_ranges(*,0))
  
  print,'number_of_intervals',number_of_intervals

  ;; 2. Do this stuff for Jp2d from get_2dt
  ;; remove spurious crap
  ;; have to be finite
  keep2d=where(finite(tmpj2d.y(*,0)) NE 0 AND finite(tmpj2d.y(*,1)) NE 0 AND finite(tmpj2d.y(*,2)) NE 0)
  tx2d=tmpj2d.x(keep2d)
  ty2d=[[tmpj2d.y(keep2d,0)],[tmpj2d.y(keep2d,1)],[tmpj2d.y(keep2d,2)]]
  print,"Original Jp2d data has " + strcompress(n_elements(tmpj2d.x)) + " elements, but we're losing " $
        + strcompress(n_elements(tmpj2d.x)-n_elements(keep2d)) + " by removing junk data"
  ;; and have to be greater than zero
  keep2d=where(abs(ty2d(*,0)) GT 0.0 AND abs(ty2d(*,1)) GT 0.0 AND abs(ty2d(*,2)) GT 0.0)
  tx2d=tx2d(keep2d)
;  ty2d=ty2d(keep2d)
  ty2d=ty2d(keep2d,*)
  
  ;; get timescale monotonic
  time_order=sort(tx2d)
  tx2d=tx2d(time_order)
  ;; ty2d=ty2d(time_order)
  ty2d=ty2d(time_order,*)
  
  ;; throw away the first 10  points since they are often corrupted
  if not keyword_set(burst) then begin
     store_data,'Jp2d',data={x:tx2d(10:n_elements(tx2d)-1),y:ty2d(10:n_elements(tx2d)-1,*)}
  endif else begin
     store_data,'Jp2d',data={x:tx2d,y:ty2d}
  endelse
  
  ;; eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Jp2d',data=jp2d
  get_fa_orbit,/time_array,jp2d.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep2d=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
  store_data,'Jp2d',data={x:jp2d.x(keep2d),y:jp2d.y(keep2d,*)}

  ;; Use the proton data to define the time ranges for this orbit	
  ;; just looking at the difference between consecutive time stamps
  max_t_gap2d=10.0
  get_data,'Jp2d',data=jp2d
  part_res_jp2d=make_array(n_elements(Jp2d.x),/double)
  for j=1,n_elements(Jp2d.x)-1 do begin
     part_res_jp2d(j)=abs(Jp2d.x(j)-Jp2d.x(j-1))
  endfor
  part_res_Jp2d(0)=part_res_Jp2d(1)
  gap2d=where(part_res_jp2d GT max_t_gap2d)
  if gap2d(0) NE -1 then begin
     separate_2d_start=[0,where(part_res_jp2d GT max_t_gap2d)]
     separate_2d_stop=[where(part_res_jp2d GT max_t_gap2d),n_elements(Jp2d.x)-1]
  endif else begin
     separate_2d_start=[0]
     separate_2d_stop=[n_elements(Jp2d.x)-1]
  endelse
  
  ;; remove esa burp when switched on
  if not keyword_set(burst) then begin
     turn_on_2d=where(part_res_jp2d GT 300.0)
     if turn_on_2d(0) NE -1 then begin
        turn_on_2d_separate=make_array(n_elements(turn_on_2d),/double)
        for j=0,n_elements(turn_on_2d)-1 do turn_on_2d_separate(j)=where(separate_2d_start EQ turn_on_2d(j))
        separate_2d_start(turn_on_2d_separate+1)=separate_2d_start(turn_on_2d_separate+1)+5
     endif
  endif

  ;; identify time indices_2d for each interval
  count_2d=0.0
  for j=0,n_elements(separate_2d_start)-1 do begin
     if (separate_2d_stop(j)-separate_2d_start(j)) GT 10 then begin
        count_2d=count_2d+1
        if count_2d EQ 1.0 then begin
           time_range_indices_2d=transpose([separate_2d_start(j)+1,separate_2d_stop(j)-1])
        endif else begin
           time_range_indices_2d=[time_range_indices_2d,transpose([separate_2d_start(j),separate_2d_stop(j)-1])]
        endelse
     endif
  endfor
  
  ;; identify interval times
  time_ranges_2d=jp2d.x(time_range_indices_2d)
  number_of_intervals_2d=n_elements(time_ranges_2d(*,0))
  
  print,'number_of_intervals_2d',number_of_intervals_2d

  ;;after all junk removal, how about one last comparison of the two?
  get_data,'Jp',data=jp
  get_data,'Jp2d',data=jp2d
  help,jp,jp2d,/str
;  for i=0,10 do print,time_to_str(jp.x(i))+", "+time_to_str(jp2d.x(i))
;  for i=0,10 do print,str(jp.y(i,0))+", "+str(jp2d.y(i,0))
  print,"Places where jp and jp2d don't match:   " + str(where(jp.y NE jp2d.y))
  print,'2D time intervals: '
  print,time_to_str(time_ranges_2d)
  print,'time intervals   : '
  print,time_to_str(time_ranges)

END