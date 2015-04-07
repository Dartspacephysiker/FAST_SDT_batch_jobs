;2015/04/07 This pro uses intervals as defined by Alfven_Stats_5, but interpolates data to 5-s resolution
pro fastlocation_2,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
                   t1=t1,t2=t2,filterfreq=filterfreq,$
                   burst=burst,heavy=heavy,ucla_mag_despin=ucla_mag_despin,keep_alfven_only=keep_alfven_only, $
                   png_sumplot=png_sumplot,png_ourevents=png_ourevents, $
                   SKIP_IF_FILE_EXISTS=skip_if_file_exists

  fastloc_dir = '/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'

;  IF NOT KEYWORD_SET(skip_if_file_exists) THEN skip_if_file_exists=1


  ;; If no data exists, return to main
  GET_SDT_TIMESPAN,t_start,t_stop
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
  
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  print,'time_range',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))
     
  ;;get orbit number for filenames		
  get_data,'ORBIT',data=tmp
  orbit=tmp.y(0)
  orbStr=strcompress(string(tmp.y(0)),/remove_all)
  ;filename for output file
  curfile = fastloc_dir + 'batch_output__intervals/'+'Dartmouth_fastlocation_2_'+strcompress(orbStr,/remove_all)+'_'+strcompress(jjj,/remove_all)
  IF KEYWORD_SET(burst) THEN BEGIN
     curfile = curfile + '--burst'
  ENDIF
  
  ;;make sure we're not overwriting
  IF file_test(curfile) THEN BEGIN
     IF NOT KEYWORD_SET(skip_if_file_exists) THEN BEGIN
        right_now=strmid(timestamp(),0,13)
        curfile = curfile + "--" + right_now
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(skip_if_file_exists) THEN BEGIN
           PRINT,"Not overwriting file " + curfile + "! Returning..."
           RETURN
        ENDIF
     ENDELSE
  ENDIF
        

  ;Use the following line if you don't want to line the times up with je_tmp_time
  get_fa_orbit,time_ranges(jjj,0),time_ranges(jjj,1),DELTA_T=5,/ALL,DRAG_PROP=1
  ;drag_prop is slightly more accurate, but slower. Takes stock of atmosph. drag
  ; turn it off if you want...
  nPoints = N_ELEMENTS(alt.x)
  
  fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
  
  get_data,'fa_vel',data=vel
  speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
  
  ;;now get orbit quantities
  get_data,'ORBIT',data=orb
  get_data,'MLT',data=mlt
  get_data,'ALT',data=alt
  get_data,'ILAT',data=ilat

  north_south=abs(ilat.y(0))/ilat.y(0)

  ;;fields_mode
  ;;get fields mode nearest to each je_tmp_time point
  fieldsmode_arr = MAKE_ARRAY(nPoints,/DOUBLE)
  FOR i=0,nPoints -1 DO BEGIN
     near = Min(Abs(fields_mode.time-je_tmp_time[i]), index)
     IF near LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = !Values.F_NAN
  ENDFOR

     

     
;;if jjj GT 0 or not keyword_set(filename) then
;;filename='/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'+'Dartmouth_fastlocation_2'+strcompress(orbStr+'_'+string(jjj)+"_magcal_v"
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

     for jj=0L,nPoints-1 do begin
        printf,unit1,format='(I9,A24,4G13.6)',orbit,time_to_str(je_tmp_time[jj],/ms),alt.y[jj],mlt.y[jj],ilat.y[jj],fieldsmode_arr[jj]
     ENDFOR

  
free_lun,unit1


  return 
end
