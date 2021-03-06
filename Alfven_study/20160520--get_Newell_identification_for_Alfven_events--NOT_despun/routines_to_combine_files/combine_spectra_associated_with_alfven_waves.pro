PRO COMBINE_SPECTRA_ASSOCIATED_WITH_ALFVEN_WAVES,alf_specIdent_hash,alf_eSpec_stats

  alf_eSpec_dir   = '/home/spencerh/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/batch_output/'
  alf_eSpec_fPref = 'Dartdb--Alfven--Newell_identification_of_electron_spectra--Orbit_'

  despunStr       = '--NOT_despun'
  ;; first_orb       = 500
  ;; last_orb        = 4371
  first_orb       = 4372
  last_orb        = 5451

  outFileDir      = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  outFilePref     = STRING(FORMAT='("Alfven_eSpec",A0,"--Newell_et_al_2009--orbits_",I0,"-",I0)', $
                           despunStr, $
                           first_orb, $
                           last_orb)

  FOR iOrb=first_orb,last_orb DO BEGIN
     inFile   = STRING(FORMAT='(A0,I0,A0)',alf_eSpec_fPref,iOrb,'.sav')
     IF FILE_TEST(alf_eSpec_dir+inFile) THEN BEGIN
        alf_eSpec = !NULL       ;so that we don't get the same one twice
        RESTORE,alf_eSpec_dir+inFile
        IF N_ELEMENTS(alf_eSpec) NE 0 THEN BEGIN
           PRINT,STRING(FORMAT='(I0)',iOrb)

           CAT_ALF_ESPEC,alf_eSpec_stats,alf_specIdent_hash,alf_eSpec
        ENDIF ELSE BEGIN
           PRINT,'What the????'
        ENDELSE
     ENDIF
  ENDFOR

  
  PRINT,'Output directory: ' + outFileDir
  PRINT,'Saving to ' + outFilePref+'.sav ...'
  save,alf_specIdent_hash,alf_eSpec_stats,FILENAME=outFileDir+outFilePref+'.sav'

END
