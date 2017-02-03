;2016/01/07 We really really want to map the Poynting flux to the ionosphere, with the new despun DB!
PRO GET_MAPPED_BFIELDVALS_FOR_ALFVENDB_TIMES__UCLA_DESPIN_201512

  @startup

  thisDir    = '/SPENCEdata/software/sdt/batch_jobs/map_Poyntingflux__20151217/'
  alfFile    = 'alfTimes_and_alfOrbits--20160107_despun_DB.sav'

  outDir     = thisDir + 'output__despundb_20160107/'

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
  tmpj.x=tmpj.x[keep]
  tmpj.y=tmpj.y[keep]
  
  keep=where(abs(tmpj.y) GT 0.0)
  tx=tmpj.x[keep]
  ty=tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order=sort(tx)
  tx=tx[time_order]
  ty=ty[time_order]
  ;;And now some orbit stuff
  ;; get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x,/all
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GE 50.0 )

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
  time_ranges      = je.x(time_range_indices)

  get_fa_orbit,time_ranges(0,0),time_ranges(0,1)
  get_data,'ORBIT',data=tmp

  orbInd           = N_ELEMENTS(tmp.y) GT 1 ? CEIL(N_ELEMENTS(tmp.y)/2.) : N_ELEMENTS(tmp.y)/2
  unOrbs           = tmp.y[UNIQ(tmp.y)]
  IF N_ELEMENTS(unOrbs) GT 1 THEN BEGIN
     PRINT,FORMAT='("Look out! This orbit could be anything: ",(I0,TR5))',unOrbs
  ENDIF

  orbit            = tmp.y[orbInd]
  orbit_num        = strcompress(string(tmp.y[orbInd]),/remove_all)
                
  these_i          = where(alfOrbits EQ tmp.y[orbInd]) ;relevant indices
                
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
     mag1          = (tmp1.y[times_i,0]*tmp1.y[times_i,0]+tmp1.y[times_i,1]*tmp1.y[times_i,1]+tmp1.y[times_i,2]*tmp1.y[times_i,2])^0.5
     mag2          = (tmp2.y[times_i,0]*tmp2.y[times_i,0]+tmp2.y[times_i,1]*tmp2.y[times_i,1]+tmp2.y[times_i,2]*tmp2.y[times_i,2])^0.5
     ratio         = (mag2/mag1)
     
     save,times,ratio,mag1,mag2,FILENAME=outDir+'mapping_ratio--orb_'+orbit_num
     

  ENDIF ELSE BEGIN
     PRINT,'No data for orbit ' + orbit_num + '!!!'
  ENDELSE

END