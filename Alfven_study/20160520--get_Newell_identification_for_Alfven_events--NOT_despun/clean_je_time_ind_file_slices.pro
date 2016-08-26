;;08/26/16
PRO CLEAN_JE_TIME_IND_FILE_SLICES

  COMPILE_OPT IDL2

  
  PRINT,"Not ready yet. Still need to figure out best way to clean. Why should there be dupes, anyway?"
  STOP

  dbDir  = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Alfven_study/' + $
           '20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  dbPref = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__0-50000.sav--orbs_'

  orbRanges = [ $
              [    0,  999], $
              [ 1000, 1999], $
              [ 2000, 2999], $
              [ 3000, 3999], $
              [ 4000, 4999], $
              [ 5000, 5999], $
              [ 6000, 6999], $
              [ 7000, 8999], $
              [ 9000, 9999], $
              [10000,11999], $
              [12000,12999], $
              [13000,13999], $
              [14000,14999], $
              [15000,15999], $
              [16000,16999], $
              [17000,17999], $
              [18000,19999], $
              [20000,20999], $
              [21000,21999], $
              [22000,22999], $
              [23000,23999], $
              [24000,24999], $
              [25000,25999], $
              [26000,28999], $
              [29000,29999], $
              [30000,30999], $
              [31000,31999], $
              [32000,33999], $
              [34000,36999], $
              [37000,37999], $
              [38000,38999], $
              [39000,39999], $
              [40000,40999], $
              [41000,41999], $
              [42000,42999], $
              [43000,43999], $
              [44000,44999], $
              [45000,48999], $
              [49000,49999], $
              [49001,50001] $
              ]


  nRanges    = N_ELEMENTS(orbRanges[0,*])

  FOR iRange=0,nRanges-1 DO BEGIN
     oA      = orbRanges[*,iRange]
     orbSuff = STRCOMPRESS(oA[0],/REMOVE_ALL) + '-' + STRCOMPRESS(oA[1],/REMOVE_ALL) 

     PRINT,'orbRange ' + orbSuff

     IF FILE_TEST(dbDir+dbPref+orbSuff) THEN BEGIN
        RESTORE,dbDir+dbPref+orbSuff
     ENDIF ELSE BEGIN
        PRINT,"Can't find " + dbPref+orbSuff + '!'
        STOP
     ENDELSE

     IF N_ELEMENTS(je_keys) EQ 0 THEN BEGIN
        PRINT,"Generating je_keys ..."
        je_keys = je_hash.Keys()
        PRINT,'Saving keys to file ...'
        SAVE,je_hash,je_keys,je_tRange_hash,je_tRange_inds_hash,FILENAME=dbDir+dbPref+orbSuff
     ENDIF

     IF N_ELEMENTS(je_monotonicity) GT 0 THEN BEGIN
        PRINT,'This range has been checked, and it is ' + $
              (KEYWORD_SET(je_monotonicity) ? '' : 'NOT ' ) + 'monotonic'

        je_hash             = !NULL
        je_trange_hash      = !NULL
        je_trange_inds_hash = !NULL
        je_keys             = !NULL
        je_monotonicity     = !NULL


        CONTINUE
     ENDIF

     je_mono_unique_hash   = HASH()
     FOREACH tmpOrb,je_keys DO BEGIN

        ;; tmpJe              = (je_hash[tmpOrb]).x
        ;; CHECK_DUPES,(je_hash[tmpOrb]).x, $
        ;;             HAS_DUPES=hasDupes, $
        ;;             IS_SORTED=isSorted, $
        ;;             OUT_UNIQ_I=uniq_i
        ;;             /QUIET

        uniq_i   = UNIQ((je_hash[tmpOrb]).x,SORT((je_hash[tmpOrb]).x))

        nNow     = N_ELEMENTS((je_hash[tmpOrb].x))
        IF N_ELEMENTS(uniq_i) NE nNow THEN BEGIN
           PRINT,'Removing ' + STRCOMPRESS(nNow - N_ELEMENTS(uniq_i),/REMOVE_ALL ) + ' dupes ...'

           jeTmp       = je_hash[tmpOrb]

        ENDIF

        ;; je_mono_unique_hash = je_mono_unique_hash + HASH(tmpOrb,BYTE( (hasDupes)*2)+(~isSorted) )
        ;; je_mono_unique_hash = je_mono_unique_hash + HASH(tmpOrb, $
        ;;                                                  {dupes:BYTE(hasDupes), $
        ;;                                                   sorted:BYTE(isSorted)})
        ;; je_mono_unique_hash = je_mono_unique_hash + HASH(tmpOrb, $
        ;;                                                  BYTE([hasDupes,isSorted]))
                    
     ENDFOREACH

     unsort_i         = je_mono_unique_hash.Where(1)
     dupes_i          = je_mono_unique_hash.Where(2)
     dupes_unsort_i   = je_mono_unique_hash.Where(3)
     je_monotonicity  = ( ( N_ELEMENTS(unsort_i) + N_ELEMENTS(dupes_i) + $
                            N_ELEMENTS(dupes_unsort_i) ) EQ 0 ) ? 1 : 0

     ;; PRINT,'Saving file with monotonicity info ...'
     ;; STOP
     ;; SAVE,je_hash,je_keys, $
     ;;      je_tRange_hash,je_tRange_inds_hash, $
     ;;      je_mono_unique_hash, $
     ;;      je_monotonicity, $
     ;;      FILENAME=dbDir+dbPref+orbSuff

     PRINT,'Saving file with monotonicity info ...'
     STOP
     SAVE,je_hash,je_keys, $
          je_tRange_hash,je_tRange_inds_hash, $
          je_mono_unique_hash, $
          je_monotonicity, $
          FILENAME=dbDir+dbPref+orbSuff
     
     unsort_i            = !NULL
     dupes_i             = !NULL
     dupes_unsort_i      = !NULL
     je_mono_unique_hash = !NULL
     je_hash             = !NULL
     je_trange_hash      = !NULL
     je_trange_inds_hash = !NULL
     je_keys             = !NULL
     je_monotonicity     = !NULL

  ENDFOR



  ;; reportDir  = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/' + $
  ;;              '20160520--get_Newell_identification_for_Alfven_events--NOT_despun'
  ;; reportFile = 'REPORT--monotonicity_of_je_time_inds'

  ;; OPENW,rLun,reportDir+reportFile,/GET_LUN

  ;; FOR iOrb=startOrb,stopOrb DO BEGIN

     ;; e = LOAD_JE_AND_JE_TIMES_FOR_ORB(iOrb, $
     ;;                                  JE_OUT=je, $
     ;;                                  TIME_RANGES_OUT=time_ranges, $
     ;;                                  TIME_RANGE_INDICES_OUT=time_range_indices, $
     ;;                                  NINTERVALS_OUT=number_of_intervals, $
     ;;                                  QUIET=quiet)

     ;; ;;Get out if no data
     ;; IF e EQ -1 THEN BEGIN
     ;;    tmpReport = 'no data'
     ;; ENDIF ELSE BEGIN

     ;;    CHECK_DUPES,je.x,HAS_DUPES=hasDupes, $
     ;;                IS_SORTED=isSorted, $
     ;;                OUT_UNIQ_I=out_uniq_i, $
     ;;                /QUIET
     ;;    CASE isSorted OF
     ;;       0: BEGIN
     ;;          tmpReport           = 'Unsorted'
              
     ;;       END
     ;;       1: BEGIN
     ;;          tmpReport           = 'Sorted'
     ;;       END
     ;;    ENDCASE

     ;;    CASE hasDupes OF
     ;;       0: 
     ;;       1: tmpReport += ',hasDupes'
     ;;    ENDCASE


     ;; ENDELSE

     ;; PRINTF,rLun,FORMAT='(I0,T10,)',tmpReport
     
     ;; je = !NULL

  ;; ENDFOR


  ;; CLOSE,rLun
  ;; FREE_LUN,rLun


END
