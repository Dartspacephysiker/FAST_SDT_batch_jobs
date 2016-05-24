;**
;LIST_EVENTS
;2015/03/19
;The idea with this pro is to load dbFile and list all Alfvén events associated with the orbit
;currently loaded by SDT. That's all--start small, and work your way to the top.
;**

PRO list_events,EVENTNUM=eventNum,ORBNUM=orbNum,ALFEVENTS=alfEvents,BURST=burst, $
                PRINT_TIMES=print_times

  @startup       ;run necessary sdt startup

  tstart=0
  
  dat = get_fa_ees(tstart,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; Electron current - line plot
  get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons

  ;;remove spurious crap
  get_data,'Je',data=tmp
  ;;use this one for getting all fast orbit stuff corresponding to times in tmp.x
  ;;get_fa_orbit,/time_array,tmp.x 

  npoints = N_ELEMENTS(tmp.x)

  keep=where(finite(tmp.y) NE 0)
  tmp.x=tmp.x(keep)
  tmp.y=tmp.y(keep)
  
  keep=where(abs(tmp.y) GT 0.0)
  tx=tmp.x(keep)
  ty=tmp.y(keep)

  nkeep = N_ELEMENTS(keep)

  IF nkeep NE npoints THEN print, "Junking garbage Je data: Losing " + strcompress(npoints - nkeep,/remove_all) + " data points of " + strcompress(npoints,/remove_all) + " total."
  
  ;;get timescale monotonic
  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)
    
  ;;throw away the first 10  points since they are often corrupted
  store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
  ;;;;;;;;;;;;;;;;;;;;;;;;End cleanup of tmp data
  
  ;get orbit info
  get_data,'Je',data=jee
  get_fa_orbit,jee.x[0],jee.x[-1]
  ;; load DB file
  dbDir = '/SPENCEdata/Research/Satellites/FAST/OMNI_FAST/scripts_for_processing_Dartmouth_data/'
  dbFile = 'Dartdb_02282015--500-14999--maximus.sav'
  restore,dbDir + dbFile

  ;;get orbit number for filenames		
  get_data,'ORBIT',data=tmp
  orbNum=tmp.y(0)
  orbStr=strcompress(string(tmp.y(0)),/remove_all)
  print,"Orbit " + orbStr
     
  ;;Get all events associated with this orbit number
  events_i = where(maximus.orbit EQ orbNum)
  nEvents = n_elements(events_i)

  ;;create Alfven event objects
  IF events_i[0] NE -1 THEN BEGIN
     alfEvents = OBJARR(nEvents)
     FOR i=0, nEvents-1 DO BEGIN
        alfEvents(i) = OBJ_NEW('alfEvent')
     ENDFOR
  ENDIF ELSE BEGIN
     PRINT, "No events for the orbit number provided, or, if you instead provided an event number, there is no such event."
     PRINT, "Exiting..."
     RETURN
  ENDELSE
  

  IF keyword_set(ucla_mag_despin) THEN ucla_mag_despin

  ;;begin looping each event
  IF KEYWORD_SET(print_times) THEN BEGIN
     print,"Event    Start Time                 Stop Time                  Duration"

     FOR jjj=0,nEvents-1 DO BEGIN
        ;; times for the event
        t1Str = maximus.start_time(events_i[jjj])
        t2Str = maximus.stop_time(events_i[jjj])
        t1 = str_to_time(t1Str)
        t2 = str_to_time(t2Str)
        dur=t2-t1

        print,format='(I0,T10,A0,T37,A0,T64,F0.3)',orbNum,t1Str,t2Str,dur
        
     ENDFOR
     
     RETURN
  ENDIF


  ;;What happens if I get all field data for this orbit at once?
  ;;get fields quantities
  start_t = maximus.start_time(events_i[0])
  stop_t = maximus.stop_time(events_i[-1])
  data_valid=1.0

  dat=get_fa_fields('MagDC',start_t,stop_t)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     data_valid=0.0
  ENDIF ELSE BEGIN
     IF NOT keyword_set(ucla_mag_despin) THEN field=get_fa_fields('MagDC',start_t,stop_t,/store)
     dat=get_fa_fields('V5-V8_S',tstart,/start)
     IF dat.valid EQ 0 THEN BEGIN
        print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
        data_valid=0.0
     ENDIF ELSE BEGIN
        spacecraft_potential=get_fa_fields('V8_S',start_t,stop_t)
        efieldV58=get_fa_fields('V5-V8_S',start_t,stop_t)
        efieldV1214=get_fa_fields('V1-V2_S',start_t,stop_t)
        IF efieldV1214.valid EQ 0 THEN BEGIN
           print,'No V1-V2 data - trying V1-V4'
           efieldV1214=get_fa_fields('V1-V4_S',start_t,stop_t)
           IF efieldV1214.valid EQ 0 THEN BEGIN
              print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
              data_valid=0.0
           ENDIF 
        ENDIF 
     ENDELSE
  ENDELSE	

  IF data_valid NE 0.0 THEN BEGIN
     
     ;;get E field and B field on same time scale
     FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
     
     ;;get magnitude of electric and magnetic field
     efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
     ;; if not keyword_set(ucla_mag_despin) then begin   
     get_data,'MagDCcomp1',data=magx
     get_data,'MagDCcomp2',data=magy
     get_data,'MagDCcomp3',data=magz
     ;; endif else begin
     ;;    get_data,'dB_fac_v',data=db_fac
     ;;    mintime=min(abs(time_range(jjj,0)-db_fac.x),ind1)
     ;;    mintime=min(abs(time_range(jjj,1)-db_fac.x),ind2)
     
     ;;    magx={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,0)}
     ;;    magy={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,2)}
     ;;    magz={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,1)}
     ;; endelse
     
     store_data,'MagZ',data=magz
     ;;magz.y=smooth(magz.y,40)
     store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
     if keyword_set(filterfreq) then begin
        
        magz=filter(magz,filterfreq,'magfilt','l')
        ;;remove end effects of the filter by cutting off the first/last 2s
        sf=magz.x(1)-magz.x(0)
        np=n_elements(magz.x)
        padding=round(2.0/sf)
        magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
        store_data,'MagZ',data=magz
     endif
     
     
     ;;SMH Try this to make fa_fields_combine stop crying                        
     magz={time:magz.x,comp1:magz.y,ncomp:1}
     efield={time:efield.x,comp1:efield.y}
     
     ;; fields=combinets(magz,efield)
     FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
     fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

     ;; dens=combinets(magz,langmuir)
     langmuir={time:langmuir.x,comp1:langmuir.y,ncomp:1}
     FA_FIELDS_COMBINE,magz,langmuir,result=dens,/talk
     dens={time:magz.time,comp1:magz.comp1,comp2:dens,ncomp:2}

     magz={x:magz.time,y:magz.comp1}
     langmuir={x:langmuir.time,y:langmuir.comp1}

     ;;get the prootn cyc frequency for smoothing the e field data later
     proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz

  ENDIF

  FOR jjj=0,nEvents-1 DO BEGIN

     ;; times for the event
     t1Str = maximus.start_time(events_i[jjj])
     t2Str = maximus.stop_time(events_i[jjj])
     t1 = str_to_time(t1Str)
     t2 = str_to_time(t2Str)

     ;;get indices for event time range
     ;;this might not work
     event_indices = value_locate(je.x,[t1,t2])

     je_tmp_time=je.x(event_indices(jjj,0):event_indices(jjj,1))
     je_tmp_data=je.y(event_indices(jjj,0):event_indices(jjj,1))
     
     store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}
     
     ;;get fields quantities
     data_valid=1.0
     dat=get_fa_fields('MagDC',tstart,/start)
     if dat.valid eq 0 then begin
        print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
        data_valid=0.0
     endif else begin
        if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_range(jjj,0),time_range(jjj,1),/store)
        dat=get_fa_fields('V5-V8_S',tstart,/start)
        if dat.valid eq 0 then begin
           print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
           data_valid=0.0
        endif else begin
           spacecraft_potential=get_fa_fields('V8_S',time_range(jjj,0),time_range(jjj,1))
           efieldV58=get_fa_fields('V5-V8_S',time_range(jjj,0),time_range(jjj,1))
           efieldV1214=get_fa_fields('V1-V2_S',time_range(jjj,0),time_range(jjj,1))
           if efieldV1214.valid eq 0 then begin
              print,'No V1-V2 data - trying V1-V4'
              efieldV1214=get_fa_fields('V1-V4_S',time_range(jjj,0),time_range(jjj,1))
              if efieldV1214.valid eq 0 then begin
                 print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
                 data_valid=0.0
              endif 
           endif 
        endelse
     endelse	

  ENDFOR

END