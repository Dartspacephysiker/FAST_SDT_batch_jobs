;I'm trying to figure out how we could get various ion populations during storms
;I've never understood the TEAMS stuff
;Using orbit 1536 on tadrith

;The procedure JOURNAL__20150320__figuring_out_teams has some more basic stuff, but my defined goal
;here is generating TEAMS-related plots

PRO JOURNAL__20150624__figuring_out_teams

  @startup

  ;use this to get the first bit of TEAMS HiMass data
  ;For orbit 1536, this should 
  t=0
  himass = get_fa_th_3d(t,/start)
;
  ;; Now, to advance...
 
  ;; himass = get_fa_th_3d(t,/ad)
 
  ;; Time range for reproduction of TEAMS paper Figure 4b.
  t1str = '1997-01-10/11:11:14.910'
  t1 = str_to_time(t1str)
  hm_t1 = get_fa_th_3d(t1str)

  t2str = '1997-01-10/11:20:39.527'
  t2 = str_to_time(t2str)
  hm_t2 = get_fa_th_3d(t2)

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

  get_tms_hm_spec, 'fa_th_3d', gap_time = 120,NAME="hm_spec",NO_DATA=no_data 
  IF ~no_data THEN GET_DATA,"hm_spec",data=tmp_hm ELSE PRINT,"No data from tms_hm_spec!"

  FOR i=0,n_elements(tmp_hm.x)-1 DO PRINT,FORMAT='(A0,T25,A0)',time_to_str(tmp_hm.x(i)),time_to_str(tmp_hm.x1(i))


  dc=contour(tmp_hm.y,tmp_hm.x-tmp_hm.x(0),indgen(64))
  p=plot(indgen(64),tmp_hm.y(0,*))

  ;; fluxdat = conv_units(tmp_hm, 'flux') ; doesn't work?

  get_pa_spec, 'fa_tsp_eq', energy = [1, 1000], gap_time = 30 ,name='bidness',NO_DATA=no_data
  IF ~no_data THEN get_data,'bidness',data=pa_spec ELSE PRINT,"No data from get_pa_spec!"
;
  p=plot(indgen(16),pa_spec.y(300,*))
  dc_pa=contour(pa_spec.y,pa_spec.x-pa_spec.x(0),indgen(16))

END