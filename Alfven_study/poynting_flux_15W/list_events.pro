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

  ;; load DB file
  dbDir = '/SPENCEdata/Research/Cusp/ACE_FAST/scripts_for_processing_Dartmouth_data/'
  dbFile = 'Dartdb_02282015--500-14999--maximus.sav'
  restore,dbDir + dbFile

  ;;get orbit number for filenames		
  get_data,'ORBIT',data=tmp
  orbit=tmp.y(0)
  orbit_num=strcompress(string(tmp.y(0)),/remove_all)
     
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
  FOR jjj=0,nEvents-1 DO BEGIN
     print,format='("time_range ",A0,TR4,A0)',time_to_str(time_ranges(jjj,0),/msec),time_to_str(time_ranges(jjj,1),/msec)
     
     ;; times for this event
     t1 = str_to_time(maximus.start_time(events_i[jjj]))
     t2 = str_to_time(maximus.stop_time(events_i[jjj]))

  ENDFOR

END