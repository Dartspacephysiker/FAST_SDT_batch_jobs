PRO JOURNAL_figuring_out_teams__20150320


  @startup

  ;; List all current DQDs, along with their start and stop times
  dqds = get_dqds(start_times=startme,end_times=endme)
  FOR i=1,n_elements(startme)-1 DO BEGIN 
     print,format='(A0,T30,A0,T60,A0)',dqds[i],time_to_str(startme[i]),time_to_str(endme[i]) 
  ENDFOR
  
  ;use this to get the first bit of TEAMS HiMass data
  ;For orbit 1536, this should 
  t=0
  himass = get_fa_th_3d(t,/start)
  ;; print,time_to_str(t)
  ;; 1997-01-10/11:07:13 ;for orbit 1536, this is true

  ;; Now, to advance...
  ;; himass = get_fa_th_3d(t,/ad)
  ;; himass = get_fa_th_3d(t,/ad)
  ;; himass = get_fa_th_3d(t,/ad)
  ;; print,time_to_str(t)
  ;; ;;1997-01-10/11:11:14 ;;Beginning of data in the FAST TEAMS paper (Klumpar et al. [2001])

  ;; Time range for reproduction of TEAMS paper Figure 4b.
  t1str = '1997-01-10/11:11:14.910'
  t1 = str_to_time(t1str)
  hm_t1 = get_fa_th_3d(t1str)

  t2str = '1997-01-10/11:20:39.527'
  t2 = str_to_time(t2str)
  hm_t2 = get_fa_th_3d(t2)

  ;;Do some thangs
  print,time_to_str(hm_t1.time,/ms)
  print,time_to_str(hm_t1.end_time,/ms)

  ;; Get the orbit data
  IF get_sdt_timespan(t1,t2) THEN BEGIN
     print,'timespan is from ',time_to_str(t1),' to ',time_to_str(t2)
  ENDIF ELSE BEGIN
     print,' could not get timespan...'
  ENDELSE
  orbit_file=fa_almanac_dir()+'/orbit/predicted'
  get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'ORBIT',data=tmp
  orbStr=tmp.y(0)
  orbNum=strcompress(string(tmp.y(0)),/remove_all)


END