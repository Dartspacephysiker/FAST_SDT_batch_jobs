;**
;PFLUX ESTIMATES
;**

pro pflux_estimation,EVENTNUM=eventNum,ORBNUM=orbNum

  @startup       ;run necessary sdt startup

  IF KEYWORD_SET(orbNum) THEN BEGIN
     events_i = where(maximus.orbit EQ orbNum)
     nEvents = n_elements(events_i)
  ENDIF

  IF KEYWORD_SET(eventNum) THEN BEGIN
     events_i = eventNum
     nEvents = 1.
  ENDIF

  alfEvents = OBJARR(nEvents)

  FOR i=0, nEvents-1 DO BEGIN
     alfEvents(i) = OBJ_NEW('alfEvent')
  ENDFOR
  
    ;; load relevant data
    ;; orbit=maximus.

  current_threshold=1.0    ;microA/m^2
  delta_b_threshold=5.0    ; nT
  delta_E_threshold=10.0   ; mV/m
  esa_j_delta_bj_ratio_threshold=0.02
  electron_eflux_ionos_threshold=0.05 ;ergs/cm^2/s
  eb_to_alfven_speed=10.0             ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
                           ;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  ;;energy ranges
  if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  if not keyword_set(energy_ions) then energy_ions=[0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

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
  
  ;;loop over each time interval
  ji_tot=make_array(number_of_intervals,/double)
  ji_up_tot=make_array(number_of_intervals,/double)
  jee_tot=make_array(number_of_intervals,/double)
  Ji_tot_alf=make_array(number_of_intervals,/double)
  Ji_up_tot_alf=make_array(number_of_intervals,/double)
  Jee_tot_alf=make_array(number_of_intervals,/double)
  
  ;;get despun mag data if keyword set
  if keyword_set(ucla_mag_despin) then ucla_mag_despin


end