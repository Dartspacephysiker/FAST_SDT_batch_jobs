;RESTORE,'~/Research/database/FAST/dartdb/electron_Newell_db/Alfven_eSpec--NOT_despun--Newell_et_al_2009--orbits_500-4371.sav'
;newFile = 'Alfven_eSpec--NOT_despun--Newell_et_al_2009--orbits_4372-5451.sav'
PRO COMBINE_ALF_SPECIDENT_AND_STATS_WITH_NEW_FILE,alf_specIdent_hash_update,alf_eSpec_stats_update,alf_specIdent_previousFile,alf_specIdent_previousDir,SAVE_NEW=save_new

  IF N_PARAMS() LT 3 THEN BEGIN
     PRINT,'Must provide a previous filename!'
  ENDIF

  IF N_PARAMS() LT 4 THEN BEGIN
     PRINT,'Using default alf_eSpec directory...'
     alf_eSpec_DBdir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  ENDIF

  despunStr       = '--NOT_despun'

  IF FILE_TEST(alf_eSpec_DBdir+alf_specIdent_previousFile) THEN BEGIN
     RESTORE,alf_eSpec_DBdir+alf_specIdent_previousFile

     first_orb       = MIN((alf_specIdent_hash_update.KEYS()).TOARRAY()) < MIN((alf_specIdent_hash.KEYS()).TOARRAY())
     last_orb        = MAX((alf_specIdent_hash_update.KEYS()).TOARRAY()) > MAX((alf_specIdent_hash.KEYS()).TOARRAY())

     outFilePref     = STRING(FORMAT='("Alfven_eSpec",A0,"--Newell_et_al_2009--orbits_",I0,"-",I0)', $
                              despunStr, $
                              first_orb, $
                              last_orb)

     IF N_ELEMENTS(alf_eSpec_stats) NE 0 THEN BEGIN
        alf_eSpec_stats_update  = {orbit:[alf_eSpec_stats_update.orbit,alf_eSpec_stats.orbit], $
                                   interval:[alf_eSpec_stats_update.interval,alf_eSpec_stats.interval], $  
                                   time:[alf_eSpec_stats_update.time,alf_eSpec_stats.time], $
                                   idx:[alf_eSpec_stats_update.idx,alf_eSpec_stats.idx], $
                                   nHits:[alf_eSpec_stats_update.nHits,alf_eSpec_stats.nHits], $
                                   hit_nSpectra:[alf_eSpec_stats_update.hit_nSpectra,alf_eSpec_stats.hit_nSpectra]}
        ;; ident:[alf_eSpec_stats_update.ident,alf_eSpec_stats.ident]} ;, $

        alf_specIdent_hash_update = alf_specIdent_hash_update + alf_specIdent_hash


     ENDIF ELSE BEGIN
        PRINT,'What the????'
        STOP
     ENDELSE
  ENDIF

  
  IF KEYWORD_SET(save_new) THEN BEGIN
     PRINT,'Output directory: ' + alf_eSpec_DBDir
     PRINT,'Saving to ' + outFilePref+'.sav ...'
     IF FILE_TEST(alf_eSpec_DBDir+outFilePref+'.sav') THEN BEGIN
        PRINT,'About to overwrite ' + outFilePref+'.sav!!!'
        PRINT,'You need to OK this or change the filename'
        STOP
     ENDIF
     alf_specIdent_hash = alf_specIdent_hash_update
     alf_eSpec_stats    = alf_eSpec_stats_update
     SAVE,alf_specIdent_hash,alf_eSpec_stats,FILENAME=alf_eSpec_DBDir+outFilePref+'.sav'
  ENDIF

END
