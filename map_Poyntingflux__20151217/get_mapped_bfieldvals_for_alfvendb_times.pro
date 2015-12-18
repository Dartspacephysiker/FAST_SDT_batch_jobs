;2015/12/18 We really really want to map the Poynting flux to the ionosphere
PRO GET_MAPPED_BFIELDVALS_FOR_ALFVENDB_TIMES

  @startup

  IF get_sdt_timespan(t1,t2) THEN BEGIN
     print,'timespan is from ',time_to_str(t1),' to ',time_to_str(t2)
  ENDIF ELSE BEGIN
     print,' could not get timespan...'
  ENDELSE
  ;; orbit_file=fa_almanac_dir()+'/orbit/predicted'
  orbit_file=fa_almanac_dir()+'/orbit/definitive' ;Is this better because it's what actually happened instead of what was predicted?
  get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'ORBIT',data=tmp
  orbit=tmp.y(0)
  orbit_num=strcompress(string(tmp.y(0)),/remove_all)


  ;;Scale electron energy flux to 100km, pos flux earthward
  get_data,'ILAT',data=tmp
  sgn_flx = tmp.y/abs(tmp.y)
  get_data,'B_model',data=tmp1
  get_data,'BFOOT',data=tmp2
  mag1 = (tmp1.y(*,0)*tmp1.y(*,0)+tmp1.y(*,1)*tmp1.y(*,1)+tmp1.y(*,2)*tmp1.y(*,2))^0.5
  mag2 = (tmp2.y(*,0)*tmp2.y(*,0)+tmp2.y(*,1)*tmp2.y(*,1)+tmp2.y(*,2)*tmp2.y(*,2))^0.5
  ratio = (mag2/mag1)


END