;;01/24/17
PRO DOWNGOING_IONS__V1__STITCH_FILES, $
   JUST_CHECK_IF_INDS_ARE_SAMMEN=just_check_if_inds_are_sammen

  COMPILE_OPT IDL2

  PRINT,"Do yourself a favor and mosey on over to JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB, where the REAL stitching happens."
  RETURN

  startOrb       = 500
  stopOrb        = 14361

  Newell_saveDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/'
  outNewellDir   = Newell_saveDir + 'downgoing_ions__v1_output/'
  outFile_pref   = 'downgoing_ions__v1--orbit_'

  nNotSammen     = 0
  notSammenArr   = !NULL
  nMissing       = 0
  missingArr     = !NULL
  nSameEphem     = 0
  nChecked       = 0
  FOR orbit_num=startOrb,stopOrb DO BEGIN

     orbStr = STRCOMPRESS(orbit_num,/REMOVE_ALL)
     this   = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                           /USE_DUPELESS_FILES, $
                                           NINTERVALS_OUT=number_of_intervals)

     IF this EQ -1 THEN BEGIN
        PRINT,"Can't get ESA time ranges/time range indices for orbit " + orbStr + '! Continue ...'
        nMissing++
        missingArr = [missingArr,orbit_num]
        CONTINUE
     ENDIF

     PRINT,'orb, nIntervals: ',orbit_num,number_of_intervals

     FOR jjj=0,number_of_intervals-1 DO BEGIN
        sameEphem = 0
        notSammen = 0
        
        ;;shutup
        out_newell_file = outFile_pref + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

        RESTORE,outNewellDir+out_newell_file

        ;;Here's what you get, no money down:
        ;; tmpJEi_down_lowE,tmpJi_down_lowE, $
        ;; tmpJEi_down_lowE_lc,tmpJi_down_lowE_lc, $
        ;; tmpJEi_down_lowE_lc_ram,tmpJi_down_lowE_lc_ram, $
        ;; tmpJEi_down_highE,tmpJi_down_highE, $
        ;; tmpJEi_down_highE_lc,tmpJi_down_highE_lc, $
        ;; tmpJEi_down_highE_lc_ram,tmpJi_down_highE_lc_ram, $
        ;; JEi_down_lowE_i,Ji_down_lowE_i, $
        ;; JEi_down_lowE_lc_i,Ji_down_lowE_lc_i, $
        ;; JEi_down_lowE_lc_ram_i,Ji_down_lowE_lc_ram_i, $
        ;; JEi_down_highE_i,Ji_down_highE_i, $
        ;; JEi_down_highE_lc_i,Ji_down_highE_lc_i, $
        ;; JEi_down_highE_lc_ram_i,Ji_down_highE_lc_ram_i, $
        ;; mlt,ilat,alt,orbit,ratio, $
        ;; iSpec_down,iSpec_down_lc_ram

        IF KEYWORD_SET(just_check_if_inds_are_sammen) THEN BEGIN

           IF ~ARRAY_EQUAL(JEi_down_lowE_i,Ji_down_lowE_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowE_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_i,Ji_down_lowE_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_lowE_lc_i,Ji_down_lowE_lc_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowE_lc_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_lc_i,Ji_down_lowE_lc_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_lowE_lc_ram_i,Ji_down_lowE_lc_ram_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowE_lc_ram_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_lc_ram_i,Ji_down_lowE_lc_ram_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_highE_i,Ji_down_highE_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_highE_sammen", $
                    ARRAY_EQUAL(JEi_down_highE_i,Ji_down_highE_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_highE_lc_i,Ji_down_highE_lc_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_highE_lc_sammen", $
                    ARRAY_EQUAL(JEi_down_highE_lc_i,Ji_down_highE_lc_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_highE_lc_ram_i,Ji_down_highE_lc_ram_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_highE_lc_ram_sammen", $
                    ARRAY_EQUAL(JEi_down_highE_lc_ram_i,Ji_down_highE_lc_ram_i)
           ENDIF

           IF ~ARRAY_EQUAL(JEi_down_lowE_i,Ji_down_highE_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowHighE_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_i,Ji_down_highE_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_lowE_lc_i,Ji_down_highE_lc_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowHighE_lc_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_lc_i,Ji_down_highE_lc_i)
           ENDIF
           IF ~ARRAY_EQUAL(JEi_down_lowE_lc_ram_i,Ji_down_highE_lc_ram_i) THEN BEGIN
              notSammen = 1
              PRINT,FORMAT='(A0,T25,I0)',"down_lowHighE_lc_ram_sammen", $
                    ARRAY_EQUAL(JEi_down_lowE_lc_ram_i,Ji_down_highE_lc_ram_i)
           ENDIF
           PRINT,''

           nNotSammen  += notSammen
           notSammenArr = [[notSammenArr],[orbit_num,jjj]]
  
           sameEphem    = ~notSammen AND (N_ELEMENTS(mlt) EQ N_ELEMENTS(tmpJei_down_lowE))
           nSameEphem  += sameEphem
           
           nChecked++

        ENDIF
        

     ENDFOR

  ENDFOR

  IF KEYWORD_SET(just_check_if_inds_are_sammen) THEN BEGIN
     PRINT,FORMAT='(A0,T25,":  ",I0)',"N not sammen",nNotSammen
  ENDIF

  PRINT,FORMAT='(A0,T25,":  ",I0)',"N Missing",nMissing
  PRINT,missingArr
  PRINT,''
  PRINT,FORMAT='(A0,T25,":  ",I0)',"nChecked",nChecked
  PRINT,FORMAT='(A0,T25,":  ",I0)',"nSameEphem",nSameEphem
END
