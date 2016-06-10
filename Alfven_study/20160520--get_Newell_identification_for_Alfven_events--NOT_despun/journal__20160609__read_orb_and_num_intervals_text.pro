;;06/10/16
PRO JOURNAL__20160609__READ_ORB_AND_NUM_INTERVALS_TEXT

  COMPILE_OPT IDL2

  file  = 'orb_and_num_intervals.txt'
  outFile = 'orb_and_num_intervals--0-16361.sav'

  startOrb = 0
  stopOrb  = 16361

  tmplt = ASCII_TEMPLATE(file)

  orbInterval = READ_ASCII(file,TEMPLATE=tmplt)

  intervalArr = MAKE_ARRAY(stopOrb-startOrb+1,/LONG,VALUE=0)
  orbArr      = LINDGEN(stopOrb-startOrb+1)+startOrb
  FOR i=0,N_ELEMENTS(orbArr)-1 DO BEGIN
     curOrb   = orbArr[i]
     tmp_i    = WHERE(orbInterval.orbit EQ curOrb)
     IF tmp_i[0] NE -1 THEN BEGIN
        IF N_ELEMENTS(tmp_i) EQ 1 THEN BEGIN
           intervalArr[i] = orbInterval.n_intervals[tmp_i]
        ENDIF ELSE BEGIN
           PRINT,'HUH?'
        ENDELSE
     ENDIF ELSE BEGIN
        PRINT,'No hits for orb' + STRCOMPRESS(curOrb,/REMOVE_ALL)
     ENDELSE
  ENDFOR

  ;; oAndI      = {orb:orbArr, $
  ;;               n_int:intervalArr}

  PRINT,'Saving ' + outFile
  SAVE,intervalArr,FILENAME=outFile

END
