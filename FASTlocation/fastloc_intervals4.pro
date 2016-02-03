;2016/02/03 We need resolution that is finer than 5 seconds, and we also can skip the text output, for the love
PRO fastloc_intervals4,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions, $
                       t1=t1,t2=t2,filterfreq=filterfreq,$
                       burst=burst,heavy=heavy,ucla_mag_despin=ucla_mag_despin, $
                       BELOW_AURORAL_OVAL=below_auroral_oval, ONLY_BELOW_AURORAL_OVAL=only_below_auroral_oval, $
                       SKIP_IF_FILE_EXISTS=skip_if_file_exists

  delta_t                        = 2.5
  fastloc_dir                    = '/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'
  ;; fastloc_dir                    = '/SPENCEdata/software/sdt/batch_jobs/FASTlocation/'

  list_of_attempted_repeats_file='list_of_attempted_repeats--fastloc_intervals4.txt'
  diagnosticFile                =fastloc_dir+'fastloc_intervals4--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
                                 '--diagnostic--returnvalues_from_get_fa_orbit.txt'

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
     store_data,'Je',data={x:tx[10:n_elements(tx)-1],y:ty[10:n_elements(tx)-1]}
  endif else begin
     store_data,'Je',data={x:tx,y:ty}
  endelse
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x
  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  IF KEYWORD_SET(only_below_auroral_oval) THEN BEGIN
     keep=where(abs(ilat.y) LE auroral_zone(mlt.y,7,/lat)/(!DPI)*180.0 AND abs(ilat.y) GE 50.0 )
     belowAurOvalStr='--only_below_aur_oval'
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(below_auroral_oval) THEN BEGIN
        keep=where(abs(ilat.y) GE 50.0 )
        belowAurOvalStr='--below_aur_oval'
     ENDIF ELSE BEGIN
        keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
        belowAurOvalStr=''
     ENDELSE
  ENDELSE

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
     curfile = fastloc_dir + 'batch_output__intervals--20160201/'+'Dartmouth_fastloc_intervals4_'+ $
               strcompress(orbStr,/remove_all)+'_'+strcompress(jjj,/remove_all)+belowAurOvalStr+'.sav'
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
        if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_ranges(jjj,0),time_ranges(jjj,1))
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
     
     
     IF data_valid NE 0.0 THEN BEGIN
        
        ;;get_orbit data
        ;Use the following line if you don't want to line the times up with je_tmp_time
        get_fa_orbit,time_ranges(jjj,0),time_ranges(jjj,1),DELTA_T=delta_t,/ALL,DRAG_PROP=1,STATUS=orb_status
        OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
        PRINTF,diagLun,FORMAT='(A0,T20,I0,T30,I0)',GET_TODAY_STRING(/DO_YYYYMMDD_FMT),orbit,orb_status
        CLOSE,diagLun
        FREE_LUN,diagLun
        IF orb_status NE 0 THEN BEGIN
           PRINT,'Bad orbit data for orb ' + STRCOMPRESS(orbit,/REMOVE_ALL) + '. Exiting ...'
           RETURN
        END
        ;drag_prop is slightly more accurate, but slower. Takes stock of atmosph. drag
        ; turn it off if you want...
        fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
        
        get_data,'fa_vel',data=vel
        speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
        
        ;;now get orbit quantities
        get_data,'ORBIT',data=orb
        get_data,'MLT',data=mlt
        get_data,'ALT',data=alt
        get_data,'ILAT',data=ilat

        nPoints = N_ELEMENTS(alt.x)
        
        north_south=abs(ilat.y(0))/ilat.y(0)

        fieldsmode_arr = MAKE_ARRAY(nPoints,/DOUBLE)
        sample_t_arr = MAKE_ARRAY(nPoints,/DOUBLE)
        FOR i=0,nPoints -1 DO BEGIN

           ;;fields_mode
           ;;get fields mode nearest to each je_tmp_time point
           ;; near = Min(Abs(fields_mode.time-je_tmp_time[i]), index)
           nearFM = Min(Abs(fields_mode.time-mlt.x[i]), index)
           ;; IF nearFM LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = !Values.F_NAN
           IF nearFM LE 20 THEN fieldsmode_arr[i] = fields_mode.comp1[index] ELSE fieldsmode_arr[i] = -9999

           ;;sample period
           nearSR = Min(Abs(field.time-mlt.x[i]), index)
           IF nearSR LE 20 THEN sample_t_arr[i] = ( field.time[index] - field.time[index - 1] ) ELSE sample_t_arr[i] = -9999

        ENDFOR

     ENDIF

     
     IF jjj GT 0 OR NOT keyword_set(filename) THEN filename = curfile

     IF nPoints GT 0 THEN BEGIN

        print,filename,jjj

        ;; IF N_ELEMENTS(je_tmp_time) NE nPoints THEN BEGIN
        ;;    OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
        ;;    PRINTF,diagLun,'Je_tmp_time has ' + STRCOMPRESS(N_ELEMENTS(je_tmp_time),/REMOVE_ALL) + $
        ;;           ' elements, but there are supposedly ' + STRCOMPRESS(nPoints,/REMOVE_ALL) + ' points!'
        ;;    CLOSE,diagLun
        ;;    FREE_LUN,diagLun
        ;; ENDIF

        IF N_ELEMENTS(mlt.y) NE nPoints THEN BEGIN
           OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
           PRINTF,diagLun,'mlt.y has ' + STRCOMPRESS(N_ELEMENTS(mlt.y),/REMOVE_ALL) + $
                 ' elements, but there are supposedly ' + STRCOMPRESS(nPoints,/REMOVE_ALL) + ' points!'
           CLOSE,diagLun
           FREE_LUN,diagLun
        ENDIF

        IF N_ELEMENTS(ilat.y) NE nPoints THEN BEGIN
           OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
           PRINTF,diagLun,'ilat.y has ' + STRCOMPRESS(N_ELEMENTS(ilat.y),/REMOVE_ALL) + $
                 ' elements, but there are supposedly ' + STRCOMPRESS(nPoints,/REMOVE_ALL) + ' points!'
           CLOSE,diagLun
           FREE_LUN,diagLun
        ENDIF

        IF N_ELEMENTS(fieldsmode_arr) NE nPoints THEN BEGIN
           OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
           PRINTF,diagLun,'fieldsmode_arr has ' + STRCOMPRESS(N_ELEMENTS(fieldsmode_arr),/REMOVE_ALL) + $
                 ' elements, but there are supposedly ' + STRCOMPRESS(nPoints,/REMOVE_ALL) + ' points!'
           CLOSE,diagLun
           FREE_LUN,diagLun
        ENDIF

        IF N_ELEMENTS(sample_t_arr) NE nPoints THEN BEGIN
           OPENW,diagLun,diagnosticFile,/GET_LUN,/APPEND
           PRINTF,diagLun,'sample_t_arr has ' + STRCOMPRESS(N_ELEMENTS(sample_t_arr),/REMOVE_ALL) + $
                 ' elements, but there are supposedly ' + STRCOMPRESS(nPoints,/REMOVE_ALL) + ' points!'
           CLOSE,diagLun
           FREE_LUN,diagLun
        ENDIF

        fastLoc_intervals={ORBIT:REPLICATE(orbit,nPoints), $
                           ;; TIME:TIME_TO_STR(je_tmp_time,/ms), $
                           TIME:TIME_TO_STR(ilat.x,/ms), $
                           MLT:mlt.y, $
                           ILAT:ilat.y, $
                           FIELDS_MODE:fieldsmode_arr, $
                           SAMPLE_T:sample_t_arr, $
                           INTERVAL:REPLICATE(jjj,nPoints), $
                           INTERVAL_START:REPLICATE(TIME_TO_STR(time_ranges[jjj,0],/ms),nPoints), $
                           INTERVAL_STOP:REPLICATE(TIME_TO_STR(time_ranges[jjj,1],/ms),nPoints)}

        
        save,fastLoc_intervals,FILENAME=filename

        ;; openw,unit1,filename,/get_lun
        ;; printf,unit1,FORMAT='("time range: ",A24,T40,A24)',time_to_str(time_ranges(jjj,0),/ms),time_to_str(time_ranges(jjj,1),/ms)
        
        ;; printf,unit1,' Column No.  	   1-Orbit number'
        ;; printf,unit1,'                     2-time'
        ;; printf,unit1,'                     3-altitude'
        ;; printf,unit1,'                     4-MLT'
        ;; printf,unit1,'                     5-ILAT'			
        ;; printf,unit1,'                     6-fields mode'
        ;; printf,unit1,'                     7-fields sample period'
        
        ;; FOR jj=0L,nPoints-1 DO BEGIN
        ;;    printf,unit1,format='(I9,A24,5G13.6)',orbit,time_to_str(je_tmp_time[jj],/ms),alt.y[jj],mlt.y[jj],ilat.y[jj],fieldsmode_arr[jj],sample_t_arr[jj]
        ;; ENDFOR
        
        ;; free_lun,unit1
        
     ENDIF
     
  ENDFOR  
  
  RETURN 
END
