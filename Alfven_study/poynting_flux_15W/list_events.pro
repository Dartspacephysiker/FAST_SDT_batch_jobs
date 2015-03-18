;**
;LIST_EVENTS
;2015/03/19
;The idea with this pro is to load dbFile and list all Alfvén events associated with the orbit
;currently loaded by SDT. That's all--start small, and work your way to the top.
;**

;NOTE: Where the comment ";get magnitude of electric and magnetic field" is located (currently line 208), the electric field is converted to its magnitude and we lose any component information.
;We can probably do better than that, especially if we wanted signed Poynting flux info.

PRO list_events,EVENTNUM=eventNum,ORBNUM=orbNum,ALFEVENTS=alfEvents,BURST=burst

  @startup       ;run necessary sdt startup

  tstart=0
  
  dat = get_fa_ees(tstart,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     return
  endif

  ;; Electron current - line plot
  get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons

  get_data,'Je',data=je
  ;;use this one for getting all fast orbit stuff corresponding to times in je.x
  ;;get_fa_orbit,/time_array,je.x 

  get_fa_orbit,je.x[0],je.x[-1]
  ;; load DB file
  dbDir = '/SPENCEdata/Research/Cusp/ACE_FAST/scripts_for_processing_Dartmouth_data/'
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

END