;2015/04/07 This pro gets ALL location info (MLT, ILAT, altitude, velocity) at 5-s intervals for
;every period that FAST was above the auroral oval. The results are spit into an ASCII file.
;the first orbit we did was 500, and the last was 14979
PRO fastlocation,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
                   t1=t1,t2=t2,filterfreq=filterfreq,$
                   burst=burst,heavy=heavy,ucla_mag_despin=ucla_mag_despin,keep_alfven_only=keep_alfven_only, $
                   png_sumplot=png_sumplot,png_ourevents=png_ourevents, $
                   SKIP_IF_FILE_EXISTS=skip_if_file_exists

  fastloc_dir = '/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'

  list_of_attempted_repeats_file='list_of_attempted_repeats--fastlocation.txt'
;  IF NOT KEYWORD_SET(skip_if_file_exists) THEN skip_if_file_exists=1


  ;; If no data exists, return to main
  garbage = GET_SDT_TIMESPAN(t_start,t_stop,DQD='Fast_Orbit_Data')
  
  get_fa_orbit,t_start,t_stop,DELTA_T=5,/ALL,DRAG_PROP=1
  print,FORMAT='("time range: ",A24,T40,A24)',time_to_str(t_start,/ms),time_to_str(t_stop,/ms)

  ;;remove spurious crap
  get_data,'MLT',data=tmp
  
  keep=where(finite(tmp.y) NE 0)
  tmp.x=tmp.x(keep)
  tmp.y=tmp.y(keep)
  
  keep=where(abs(tmp.y) GT 0.0)
  tx=tmp.x(keep)
  ty=tmp.y(keep)
  
  ;;get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)
  store_data,'MLT',data={x:tx,y:ty}
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'MLT',data=mlt
  get_fa_orbit,/time_array,mlt.x
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
  store_data,'MLT',data={x:MLT.x(keep),y:MLT.y(keep)}

  get_data,'MLT',data=mlt
  time_ranges=[MLT.x(0),MLT.x(-1)]
  print,FORMAT='("time range: ",A24,T40,A24)',time_to_str(time_ranges(0),/ms),time_to_str(time_ranges(1),/ms)
     
  ;;get orbit number for filenames		
  get_data,'ORBIT',data=tmp
  orbit=tmp.y(1)
  orbStr=strcompress(string(tmp.y(1)),/remove_all)
  print,'Orbit ' + orbStr
  ;filename for output file
  curfile = fastloc_dir + 'batch_output__intervals/'+'Dartmouth_fastlocation_'+strcompress(orbStr,/remove_all)
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
           OPENU,replun,fastloc_dir + 'batch_output__whole_orb/' + list_of_attempted_repeats_file,/APPEND,/GET_LUN
           PRINTF,replun,FORMAT='(A24,T30,A50)',right_now,curfile
           free_lun,replun
           PRINT,"Not overwriting file " + curfile + "! Returning..."
           RETURN
        ENDIF
     ENDELSE
  ENDIF
        

  ;Use the following line if you don't want to line the times up with je_tmp_time
  get_fa_orbit,time_ranges(0),time_ranges(1),DELTA_T=5,/ALL,DRAG_PROP=1
  ;drag_prop is slightly more accurate, but slower. Takes stock of atmosph. drag
  ; turn it off if you want...
  get_data,'fa_vel',data=vel
  speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
  
  ;;now get orbit quantities
  get_data,'ORBIT',data=orb
  get_data,'MLT',data=mlt
  get_data,'ALT',data=alt
  get_data,'ILAT',data=ilat

  nPoints = N_ELEMENTS(mlt.x)
  print,"nPoints: ",strcompress(nPoints,/remove_all)

  north_south=abs(ilat.y(0))/ilat.y(0)

  ;;fields_mode
  ;;get fields mode nearest to each recorded time measurement
  fields_mode=get_fa_fields('DataHdr_1032',time_ranges(0),time_ranges(1))
  fieldsmode_arr = MAKE_ARRAY(nPoints,/DOUBLE)
  FOR i=0,nPoints -1 DO BEGIN
     near = Min(Abs(fields_mode.time-mlt.x[i]), index)
     ;; IF near LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = !Values.F_NAN
     IF near LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = -9999
  ENDFOR

     

     
;;if jjj GT 0 or not keyword_set(filename) then
;;filename='/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'+'Dartmouth_fastlocation'+strcompress(orbStr+'_'+string(jjj)+"_magcal_v"
;;+ string(version)+"_burst",/remove_all)
     if not keyword_set(filename) then filename= curfile

     print,filename
     openw,unit1,filename,/get_lun
     printf,unit1,FORMAT='("time range: ",A24,T40,A24)',time_to_str(time_ranges(0),/ms),time_to_str(time_ranges(1),/ms)

     printf,unit1,' Column No.  	1-Orbit number'
     printf,unit1,'                     2-time'
     printf,unit1,'                     3-altitude'
     printf,unit1,'                     4-MLT'
     printf,unit1,'                     5-ILAT'			
     printf,unit1,'                     6-fields mode'

     FOR jj=0L,nPoints-1 DO BEGIN
        printf,unit1,format='(I9,A24,4G13.6)',orbit,time_to_str(mlt.x[jj],/ms),alt.y[jj],mlt.y[jj],ilat.y[jj],fieldsmode_arr[jj]
     ENDFOR

  
free_lun,unit1


  RETURN 
END
