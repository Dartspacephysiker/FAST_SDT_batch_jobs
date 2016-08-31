;;08/31/16
PRO JOURNAL__20160830__HOW_MANY_ENTRIES_IN_JE_TIME_IND_FILE_RELATIVE_TO_DIFF_EFLUX_FILE, $
   orbit, $
   CLEAN_JE_DUPES=clean_je_dupes, $
   DIFF_EFLUX_DIR=diff_eFlux_dir

  COMPILE_OPT IDL2

  diffDir       = N_ELEMENTS(diff_eFlux_dir) GT 0 ? diff_eFlux_dir : $
                  'home/spencerh/software/sdt/batch_jobs/' + $
                  'saves_output_etc/do_the_Newell_2009/batch_output__just_electrons/'

  tmpDir        = '/home/spencerh/Desktop/orbstuff/'
  diffDir       = tmpDir

  diffFilePref  = 'Dartdb--e-_spectra__all_angles_energies--ees--Orbit_'

  ;;...And defaults
  cleanDupes    = N_ELEMENTS(clean_je_dupes) GT 0 ? clean_je_dupes : 1
  orb           = N_ELEMENTS(orbit)          GT 0 ? orbit          : 9858
  orbStr        = STRCOMPRESS(orb,/REMOVE_ALL)


  ;;See what we got
  e             = load_je_and_je_times_for_orb( $
                  orb, $
                  /RETURN_STRUCT, $
                  CLEAN_DUPES=cleanDupes)

  ;;Check for disagreement between n intervals reported above and 
  ;;n intervals reported by diff_eFlux dir
  
  nTotJe        = N_ELEMENTS(e.je.x)
  nUniqJe       = N_ELEMENTS(UNIQ(e.je.x,SORT(e.je.x)))

  nTotDiffE     = 0
  nUniqDiffE    = 0
  nMatchJe      = 0
  pad           = 0.1 ;sec
  FOR tmpInterval=0,e.number_of_intervals-1 DO BEGIN

     tmpFile    = diffDir + diffFilePref + orbStr + '_' + $
                  STRCOMPRESS(tmpInterval,/REMOVE_ALL) + '.sav'

     ;; CASE FILE_TEST(tmpFile) OF
     ;; 0: BEGIN
     IF ~FILE_TEST(tmpFile) THEN BEGIN
        PRINT,"Someone is lying. "
        PRINT,"File doesn't exist: " + tmpFile
        PRINT,"Maybe it's your directory (" + diffDir + ")?" 
        RETURN
        ;; STOP
     ENDIF
     ;;    END
     ;;    ELSE: 
     ;; ENDCASE

     RESTORE,tmpFile

     tmpInds     = WHERE(e.je.x GE (diff_eFlux.time[ 0]-pad) AND $
                         e.je.x LE (diff_eFlux.time[-1]+pad),nHere)
     
     nTotDiffE  += N_ELEMENTS(diff_eFlux.time)
     nUniqDiffE += N_ELEMENTS(UNIQ(diff_eFlux.time,SORT(diff_eFlux.time)))
     nMatchJe   += nHere

  ENDFOR

  PRINT,FORMAT='("N Total       Je    ",T26,":",T28,I0)',nTotJe
  PRINT,FORMAT='("N Unique      Je    ",T26,":",T28,I0)',nUniqJe
  PRINT,''
  PRINT,FORMAT='("N total    diffE    ",T26,":",T28,I0)',nTotDiffE
  PRINT,FORMAT='("N Unique   diffE    ",T26,":",T28,I0)',nUniqDiffE
  PRINT,''
  PRINT,FORMAT='("N matching diffE/Je ",T26,":",T28,I0)',nMatchJe
  PRINT,FORMAT='("N loner       Je    ",T26,":",T28,I0)',nTotJe-nMatchJe

  ;; HELP,time_to_str(e.je.x[],/ms)

END
