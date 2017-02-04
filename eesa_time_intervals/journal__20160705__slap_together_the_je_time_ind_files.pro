;;07/05/16
PRO JOURNAL__20160705__SLAP_TOGETHER_THE_JE_TIME_IND_FILES

  COMPILE_OPT IDL2

  noDupesVersion   = 1

  ;;For skipping the "get interval times" bit
  eesa_dir         = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/eesa_time_intervals/'

  orbSaveInterval  = 1000

  CASE 1 OF
     KEYWORD_SET(noDupesVersion): BEGIN
        ;;No dupes version
        PRINT,"Doing the 'no dupes version' of je_time_ind stuff ..."

        firstOrb            = 500
        outFile             = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__500-25007--noDupes.sav'

        indDir              = eesa_dir + 'je_time_ind_dir__noDupes/'
        indFilePref         = "je_and_cleaned_time_range_indices--noDupes--orbit_"
        intervalArrFile     = "orb_and_num_intervals--noDupes--500-25007.sav" ;;Use it to figure out which file to restore
     END
     ELSE: BEGIN
        ;;Outfile stuff
        ;; outFile          = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds.sav'
        outFile             = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__0-50000.sav'

        firstOrb            = 0
        indDir              = eesa_dir + 'je_time_ind_dir/'
        indFilePref         = "je_and_cleaned_time_range_indices--orbit_"

        ;; intervalArrFile  = "orb_and_num_intervals--0-16361.sav" ;;Use it to figure out which file to restore
        ;; intervalArrFile  = "orb_and_num_intervals--0-16361.sav" ;;Use it to figure out which file to restore
        intervalArrFile     = "orb_and_num_intervals__0-50000.sav" ;;Use it to figure out which file to restore


     END
  ENDCASE

  ;;Restore the file that has the master list of orbs 
  RESTORE,eesa_dir+intervalArrFile 

  nOrbs             = N_ELEMENTS(intervalArr)

  IF ~KEYWORD_SET(force_replacement) THEN BEGIN

     PRINT,"Making sure none of these files exist before doing the orbit thing ..."
     lastOrb        = 0
     FOR iOrb=firstOrb,nOrbs-1 DO BEGIN

        IF ( ( ( iOrb + 1) MOD orbSaveInterval) EQ 0 ) AND (iOrb GT 0) THEN BEGIN
           suff     = STRING(FORMAT='("--orbs_",I0,"-",I0)',lastOrb,iOrb)

           PRINT,outFile+suff + ' ...'

           IF FILE_TEST(eesa_dir+outFile+suff) THEN BEGIN
              PRINT,"FILE EXISTS : " + eesa_dir+outFile+suff
              SPAWN,'ls -laht ' + eesa_dir+outFile+suff,shellOutput
              PRINT,shellOutput
              PRINT,"Please set keyword FORCE_REPLACEMENT if you really want me to commit this crime"
              STOP
           ENDIF
           
           lastOrb  = iOrb+1
        ENDIF
     ENDFOR

     PRINT,"Done checking! Proceeding to make new sets of je_time_ind files ..."
  ENDIF

  je_tRanges              = MAKE_ARRAY(nOrbs,1,2,/DOUBLE,VALUE=0.0D)
  je_tRange_inds          = MAKE_ARRAY(nOrbs,1,2,/LONG,VALUE=0L)
  
  je_hash                 = HASH()
  je_tRange_hash          = HASH()
  je_tRange_inds_hash     = HASH()
  lastOrb                 = 0
  gotOrbs                 = 0  
  orbCnt                  = 0
  FOR iOrb=firstOrb,nOrbs-1 DO BEGIN
     number_of_intervals  = intervalArr[orbCnt++]
     PRINT,'orbit, nIntervals',iOrb,number_of_intervals
     ;; print,'number_of_intervals',number_of_intervals

     indFile              = STRING(FORMAT='(A0,I0,"--",I0,"_intervals.sav")', $
                                   indFilePref,iOrb,number_of_intervals)

     ;;Skip if we got nothin'
     ;; IF number_of_intervals EQ 0 THEN BEGIN
     ;;    ;; je_hash           = je_hash + HASH(iOrb,{x:0,y:0})
     ;;    CONTINUE
     ;; ENDIF 
     IF number_of_intervals GT 0 THEN BEGIN

        ;;This file gives us je,iOrb,time_range_indices, and time_range
        PRINT,'Restoring indFile ' + indFile + ' ...'
        RESTORE,indDir+indFile

        ;;Now cat Je and 

        ;; je_tRanges[iOrb,*,*]     = REFORM(time_ranges,1,1,2)
        ;; je_tRange_inds[iOrb,*,*] = REFORM(time_range_indices,1,1,2)
        
        je_tRange_hash       = je_tRange_hash + HASH(iOrb,time_ranges)
        je_tRange_inds_hash  = je_tRange_inds_hash + HASH(iOrb,time_range_indices)
        
        je_hash              = je_hash + HASH(iOrb,je)


        gotOrbs++

     ENDIF 

     IF ( ( ( iOrb + 1) MOD orbSaveInterval) EQ 0 ) AND (iOrb GT 0) AND (gotOrbs GT 0)  THEN BEGIN
        suff = STRING(FORMAT='("--orbs_",I0,"-",I0)',lastOrb,iOrb)
        PRINT,'Saving je stuff to ' + outFile+suff + ' ...'

        IF FILE_TEST(eesa_dir+outFile+suff) AND ~KEYWORD_SET(force_replacement) THEN BEGIN
           PRINT,"FILE EXISTS : " + eesa_dir+outFile+suff
           SPAWN,'ls -laht ' + eesa_dir+outFile+suff,shellOutput
           PRINT,shellOutput
           PRINT,"Please set keyword FORCE_REPLACEMENT if you really want me to commit this crime"
           STOP
        ENDIF

        SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=eesa_dir+outFile+suff
        je_tRange_hash      = !NULL
        je_tRange_inds_hash = !NULL
        je_hash             = !NULL
        je_tRange_hash      = HASH()
        je_tRange_inds_hash = HASH()
        je_hash             = HASH()

        gotOrbs             = 0
        lastOrb             = iOrb+1
     ENDIF


  ENDFOR

  ;; suff = STRING(FORMAT='("--orbs_",I0,"-",I0)',iOrb-orbSaveInterval,iOrb)
  suff = STRING(FORMAT='("--orbs_",I0,"-",I0)',lastOrb,iOrb)
  PRINT,'Saving FINAL je stuff to ' + outFile+suff + ' ...'
  SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=eesa_dir+outFile+suff
  ;; je_tRange_hash      = !NULL
  ;; je_tRange_inds_hash = !NULL
  ;; je_hash             = !NULL
  ;; je_tRange_hash      = HASH()
  ;; je_tRange_inds_hash = HASH()
  ;; je_hash             = HASH()

  ;; PRINT,'Saving je stuff to ' + outFile + ' ...'
  ;; SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=eesa_dir+outFile


END

