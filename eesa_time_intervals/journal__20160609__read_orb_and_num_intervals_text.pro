;;06/10/16
;;
;;But how to make orb_and_num_intervals.txt, you ask? Like THIS:
;;ls -1t ../saves_output_etc/eesa_time_intervals/je_time_ind_dir__noDupes/ > orb_and_num_intervals.txt
;;Then do some editing. Try, for example, regexp find and replace with this line:
;;je_and_cleaned_time_range_indices--noDupes--orbit_\([0-9]\{3,5\}\)--\([0-9]+\)_intervals.sav
;;
;;2017/02/08 Here's a helper:
;; nNoHit = N_ELEMENTS(noHitOrbs)
;; FOR k=0,nNoHit-1 DO BEGIN & $
;;    PRINT,FORMAT='(I0,"/",I0,": Orb ",I0)',K,nNoHit,noHitOrbs[k] & $
;;    IF (WHERE(eSpec.orbit EQ noHitOrbs[k],nHit_eSpec))[0] NE -1 THEN BEGIN & $
;;    PRINT,"ORB " + STRCOMPRESS(noHitOrbs[k],/REMOVE_ALL) + ": But eSpecDB has " + STRCOMPRESS(nHit_eSpec,/REMOVE_ALL) + ' entries for this orbit!'
;; ENDIF & $
;; ENDFOR
PRO JOURNAL__20160609__READ_ORB_AND_NUM_INTERVALS_TEXT,NOHITORBS=noHitOrbs

  COMPILE_OPT IDL2

  ;;inFile
  file         = 'orb_and_num_intervals.txt'
  ;; file         = 'orb_and_num_intervals__noDupes.txt'

  eesa_dir     = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/eesa_time_intervals/'
  ;;Outfiles
  ;; outFile   = 'orb_and_num_intervals--noDupes--0-16361.sav'
  outFile      = 'orb_and_num_intervals--noDupes--500-25007.sav'
  ;; outFile      = 'orb_and_num_intervals__0-50000.sav'

  ;; startOrb     = 0
  ;; stopOrb      = 50000
  ;; stopOrb      = 16361
  startOrb     = 500
  stopOrb      = 25007

  tmplt        = ASCII_TEMPLATE(file)

  orbInterval  = READ_ASCII(file,TEMPLATE=tmplt)

  intervalArr  = MAKE_ARRAY(stopOrb-startOrb+1,/LONG,VALUE=0)
  orbArr       = LINDGEN(stopOrb-startOrb+1)+startOrb

  noHitOrbs    = !NULL
  FOR i=0,N_ELEMENTS(orbArr)-1 DO BEGIN
     curOrb                = orbArr[i]

     tmp_i                 = WHERE(orbInterval.orbit EQ curOrb)
     IF tmp_i[0] NE -1 THEN BEGIN
        IF N_ELEMENTS(tmp_i) EQ 1 THEN BEGIN
           intervalArr[i]  = orbInterval.n_intervals[tmp_i]
        ENDIF ELSE BEGIN
           PRINT,'HUH?'
        ENDELSE
     ENDIF ELSE BEGIN
        PRINT,'No hits for orb' + STRCOMPRESS(curOrb,/REMOVE_ALL)
        noHitOrbs = [noHitOrbs,curOrb]
     ENDELSE
  ENDFOR

  ;; oAndI                 = {orb:orbArr, $
  ;;               n_int:intervalArr}

  intervalStartOrb = startOrb
  intervalStopOrb  = stopOrb
  PRINT,'Saving ' + outFile
  SAVE,intervalArr,intervalStartOrb,FILENAME=eesa_dir+outFile

END
