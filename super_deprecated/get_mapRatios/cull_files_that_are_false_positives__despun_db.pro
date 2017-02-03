;2016/01/09;Do all files actually have Alfvén events?
PRO CULL_FILES_THAT_ARE_FALSE_POSITIVES__DESPUN_DB

  ;; the place where output lives
  outputDir = '/SPENCEdata/software/sdt/batch_jobs/map_Poyntingflux__20151217/output__despundb_20160107/'

  load_maximus_and_cdbtime,maximus,/DO_DESPUNDB

  ;; uniqOrbs = maximus.orbit[UNIQ(maximus.orbit)]

  minOrb     = 502
  maxOrb     = 16361

  orbs       = INDGEN(maxOrb-minOrb+1) + 500
  ;; FOR i=0,N_ELEMENTS(uniqOrbs)-1 DO BEGIN

  ;;    these_i = WHERE(maximus.orbit EQ uniqOrbs[i])
     
  FOR i=0,N_ELEMENTS(orbs)-1 DO BEGIN

     these_i = WHERE(maximus.orbit EQ orbs[i])

     IF these_i[0] EQ -1 THEN BEGIN
        PRINT,'Bad orbit: ' + STRCOMPRESS(orbs[i],/REMOVE_ALL)
        badFile = outputDir + 'mapping_ratio--orb_'+STRCOMPRESS(orbs[i],/REMOVE_ALL)
        IF FILE_TEST(badFile) THEN BEGIN
           PRINT,"There are no Alfven events for orbit " + STRCOMPRESS(orbs[i],/REMOVE_ALL) + '!!!'
           PRINT,'Marking this terrible person of a file...'
           SPAWN,'mv ' + badFile + ' ' + badFile + '--bad'
        ENDIF
     ENDIF
  ENDFOR
  
END