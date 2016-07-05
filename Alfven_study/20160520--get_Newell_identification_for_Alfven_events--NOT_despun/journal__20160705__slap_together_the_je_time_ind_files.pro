;;07/05/16
PRO JOURNAL__20160705__SLAP_TOGETHER_THE_JE_TIME_IND_FILES

  COMPILE_OPT IDL2

  ;;Outfile stuff
  outFile                 = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds.sav'

  ;;For skipping the "get interval times" bit
  as5_dir                 = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  indDir                  = as5_dir + 'je_time_ind_dir/'
  indFilePref             = "je_and_cleaned_time_range_indices--orbit_"

  intervalArrDir          = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/20160520--get_Newell_identification/"
  intervalArrFile         = "orb_and_num_intervals--0-16361.sav" ;;Use it to figure out which file to restore

  RESTORE,intervalArrDir+intervalArrFile 

  nOrbs                   = N_ELEMENTS(intervalArr)

  je_tRanges              = MAKE_ARRAY(nOrbs,1,2,/DOUBLE,VALUE=0.0D)
  je_tRange_inds          = MAKE_ARRAY(nOrbs,1,2,/LONG,VALUE=0L)
  
  je_hash                 = HASH()
  je_tRange_hash          = HASH()
  je_tRange_inds_hash     = HASH()
  lastOrb                 = 0
  FOR iOrb=0,nOrbs-1 DO BEGIN
     number_of_intervals  = intervalArr[iOrb]
     PRINT,'orbit, nIntervals',iOrb,number_of_intervals
     ;; print,'number_of_intervals',number_of_intervals

     indFile              = STRING(FORMAT='(A0,I0,"--",I0,"_intervals.sav")', $
                                   indFilePref,iOrb,number_of_intervals)

     ;;Skip if we got nothin'
     IF number_of_intervals EQ 0 THEN BEGIN
        ;; je_hash           = je_hash + HASH(iOrb,{x:0,y:0})
        CONTINUE
     ENDIF

     ;;This file gives us je,iOrb,time_range_indices, and time_range
     PRINT,'Restoring indFile ' + indFile + ' ...'
     RESTORE,indDir+indFile

     ;;Now cat Je and 

     ;; je_tRanges[iOrb,*,*]     = REFORM(time_ranges,1,1,2)
     ;; je_tRange_inds[iOrb,*,*] = REFORM(time_range_indices,1,1,2)
     
     je_tRange_hash       = je_tRange_hash + HASH(iOrb,time_ranges)
     je_tRange_inds_hash  = je_tRange_inds_hash + HASH(iOrb,time_range_indices)
     
     je_hash              = je_hash + HASH(iOrb,je)

     IF ( ( ( iOrb + 1) MOD 1000) EQ 0 ) AND (iOrb GT 0) THEN BEGIN
        suff = STRING(FORMAT='("--orbs_",I0,"-",I0)',lastOrb,iOrb)
        PRINT,'Saving je stuff to ' + outFile+suff + ' ...'
        SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=intervalArrDir+outFile+suff
        je_tRange_hash      = !NULL
        je_tRange_inds_hash = !NULL
        je_hash             = !NULL
        je_tRange_hash      = HASH()
        je_tRange_inds_hash = HASH()
        je_hash             = HASH()

        lastOrb             = iOrb+1
     ENDIF


  ENDFOR

  suff = STRING(FORMAT='("--orbs_",I0,"-",I0)',iOrb-1000,iOrb)
  PRINT,'Saving je stuff to ' + outFile+suff + ' ...'
  SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=intervalArrDir+outFile+suff
  ;; je_tRange_hash      = !NULL
  ;; je_tRange_inds_hash = !NULL
  ;; je_hash             = !NULL
  ;; je_tRange_hash      = HASH()
  ;; je_tRange_inds_hash = HASH()
  ;; je_hash             = HASH()

  ;; PRINT,'Saving je stuff to ' + outFile + ' ...'
  ;; SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,FILENAME=intervalArrDir+outFile


END
