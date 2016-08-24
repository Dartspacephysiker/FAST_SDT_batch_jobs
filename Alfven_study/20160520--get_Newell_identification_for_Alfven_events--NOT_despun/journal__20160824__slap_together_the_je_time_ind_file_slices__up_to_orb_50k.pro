;;2016/08/24
PRO JOURNAL__20160824__SLAP_TOGETHER_THE_JE_TIME_IND_FILE_SLICES__UP_TO_ORB_50K

  COMPILE_OPT IDL2

  ;;Needs to be written
  PRINT,"STOP. This isn't ready to be used. You need to figure out how you get to this journal first, because you have no slices."
  STOP

  ;;Outfile stuff
  pref                          = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds'
  inFile_pref                   = pref+'.sav'
  outFile                       = pref+'--'+GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--orbs_500-50000.sav'

  ;;For skipping the "get interval times" bit
  as5_dir                       = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'

  intervalArrDir                = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/20160520--get_Newell_identification/"

  nOrbs                         = 50000
  nSteps                        = nOrbs/1000-1 ;;minus one for glitch handled below

  by_the_thousands_front        = [INDGEN(8,/LONG)*1000, $
                                   (INDGEN(6,/LONG)+9)*1000,15362]
  by_the_thousands_back         = [(INDGEN(8,/LONG)+1)*1000-1, $
                                   (INDGEN(6,/LONG)+10)*1000-1,16362]
  by_the_thousands_back[7]      = 8999

  by_the_thousands_front        = LONG([0,1,2,3,4,5,6,7,9,10,12,13,14,15,15362])
  by_the_thousands_front[0:-2]  = by_the_thousands_front[0:-2]*1000

  by_the_thousands_back         = LONG([0,1,2,3,4,5,6,8,9,11,12,13,14,15,16362])
  by_the_thousands_back[0:-2]   = (by_the_thousands_back[0:-2]+1)*1000-1

  je_hash_final                 = HASH()
  je_tRange_hash_final          = HASH()
  je_tRange_inds_hash_final     = HASH()
  lastOrb                       = 0

  FOR iOrb=0,nSteps-1 DO BEGIN
     front                      = by_the_thousands_front[iOrb]
     back                       = by_the_thousands_back[iOrb]

     suff                       = STRING(FORMAT='("--orbs_",I0,"-",I0)',front,back)
     PRINT,'Restoring je stuff: ' + inFile_pref+suff + ' ...'
     RESTORE,intervalArrDir+inFile_pref+suff

     ;;Now cat Je and co.
     je_tRange_hash_final       = je_tRange_hash_final + je_tRange_hash
     je_tRange_inds_hash_final  = je_tRange_inds_hash_final + je_tRange_inds_hash
     
     je_hash_final              = je_hash_final + je_hash

     ;;Now y'all be safe
     je_tRange_hash             = !NULL
     je_tRange_inds_hash        = !NULL
     je_hash                    = !NULL

  ENDFOR

  je_tRange_hash                = je_tRange_hash_final
  je_tRange_inds_hash           = je_tRange_inds_hash_final
  je_hash                       = je_hash_final

  PRINT,'Saving je stuff to ' + outFile + ' ...'
  ;; SAVE,je_tRange_hash_final,je_tRange_inds_hash_final,je_hash_final,FILENAME=intervalArrDir+outFile
  je_keys = (je_trange_hash.Keys()).ToArray()
  je_keys = je_keys[SORT(je_keys)]
  SAVE,je_tRange_hash,je_tRange_inds_hash,je_hash,je_keys,FILENAME=intervalArrDir+outFile

  ;;A check
  ;; je_keys2 = (je_trange_inds_hash.Keys()).ToArray()
  ;; je_keys2 = je_keys2[SORT(je_keys2)]
  ;; PRINT,ARRAY_EQUAL(je_keys,je_keys2)

  ;; je_keys3 = (je_hash.Keys()).ToArray()
  ;; je_keys3 = je_keys3[SORT(je_keys3)]
  ;; PRINT,ARRAY_EQUAL(je_keys,je_keys2)array_equal(je_keys,je_keys3)
END
