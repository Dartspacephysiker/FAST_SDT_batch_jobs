;2015/12/18 We really really want to map the Poynting flux to the ionosphere
PRO GET_MAPPED_BFIELDVALS_FOR_ALFVENDB_TIMES

  @startup

  thisDir    = '/SPENCEdata/software/sdt/batch_jobs/map_Poyntingflux__20151217/'
  alfFile    = 'alfTimes_and_alfOrbits--20151014_DB.sav'

  outDir     = thisDir + 'output/'

  ;;Get the Alfven DB stuff
  restore,thisDir+alfFile

  ;; orbit_file = fa_almanac_dir()+'/orbit/definitive' ;Is this better because it's what actually happened instead of what was predicted?

  t=0
  dat = get_fa_ees(t,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; Electron current - line plot
  get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons

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
  ;;And now some orbit stuff
  ;; get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x,/all
  get_data,'ORBIT',data=tmp

  orbit         = tmp.y[0]
  orbit_num     = strcompress(string(tmp.y[0]),/remove_all)
                
  these_i       = where(alfOrbits EQ tmp.y[0]) ;relevant indices
                
  IF these_i[0] NE -1 THEN BEGIN
     
     times_i       = MAKE_ARRAY(N_ELEMENTS(these_i),/LONG)
     times         = MAKE_ARRAY(N_ELEMENTS(these_i),/DOUBLE)
     ratios        = MAKE_ARRAY(N_ELEMENTS(these_i),/DOUBLE)
     
     FOR j=0,N_ELEMENTS(these_i)-1 DO BEGIN
        tempMin    = MIN(ABS(tmp.x-cdbTime[these_i[j]]),tempMin_i)
        times[j]   = tmp.x[tempMin_i]
        times_i[j] = tempMin_i
     ENDFOR
     
     ;;Scale electron energy flux to 100km, pos flux earthward
     get_data,'ILAT',data=tmp
     get_data,'B_model',data=tmp1
     get_data,'BFOOT',data=tmp2
     ;; mag1 = (tmp1.y(*,0)*tmp1.y(*,0)+tmp1.y(*,1)*tmp1.y(*,1)+tmp1.y(*,2)*tmp1.y(*,2))^0.5
     ;; mag2 = (tmp2.y(*,0)*tmp2.y(*,0)+tmp2.y(*,1)*tmp2.y(*,1)+tmp2.y(*,2)*tmp2.y(*,2))^0.5
     mag1 = (tmp1.y(times_i,0)*tmp1.y(times_i,0)+tmp1.y(times_i,1)*tmp1.y(times_i,1)+tmp1.y(times_i,2)*tmp1.y(times_i,2))^0.5
     mag2 = (tmp2.y(times_i,0)*tmp2.y(times_i,0)+tmp2.y(times_i,1)*tmp2.y(times_i,1)+tmp2.y(times_i,2)*tmp2.y(times_i,2))^0.5
     ratio = (mag2/mag1)
     
     save,times,ratio,mag1,mag2,FILENAME=outDir+'mapping_ratio--orb_'+orbit_num
     

  ENDIF ELSE BEGIN
     PRINT,'No data for orbit ' + orbit_num + '!!!'
  ENDELSE

END