;2018/08/01
PRO JOURNAL__20181207__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS__AGU, $
   SOUTH=south, $
   NORTH=north, $
   NIGHT=night, $
   MINMLT=minMLT, $
   MAXMLT=maxMLT, $
   MAXILAT=maxILAT, $
   RESTORE_LAST_FILE=restore_last_file, $
   USE_V3_STRANGEWAY=use_v3_strangeway, $
   V3__USE_ELEC_LB30EV_HASHFILE=use_elec_lb30eV_hashFile, $
   V3__USE_ELEC_LB50EV_HASHFILE=use_elec_lb50eV_hashFile, $
   V3__HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
   V3__AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass, $
   OUT_STATS=this, $
   OUT_PLOTINFO=plotInfo, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; USE KEYWORDS
  ;; south = 1
  ;; north = 1

  simple_60_assumption = 1

  ;; night = 0

  ;; USE KEYWORDS
  ;; use_v3_strangeway = 1
  ;; use_elec_lb30eV_hashFile = 1

  CASE 1 OF
     KEYWORD_SET(south): BEGIN

        CASE 1 OF
           KEYWORD_SET(use_v3_strangeway): BEGIN

              CASE 1 OF
                 KEYWORD_SET(use_elec_lb30eV_hashFile): BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-30eVLBforelec-SOUTH.sav'
                 END
                 KEYWORD_SET(use_elec_lb50eV_hashFile): BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-50eVLBforelec-SOUTH.sav'
                 END
                 ELSE: BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s.sav'
                 END
              ENDCASE

           skip_these_orbs =  [8260, $
                               8261, $
                               8262, $
                               8263, $
                               8264, $
                               8265, $
                               8266, $
                               8267, $
                               8268, $
                               8269, $
                               8270, $
                               8271, $
                               8272, $
                               8273, $
                               8274, $
                               8275, $
                               8276, $
                               8277, $
                               8278, $
                               8279, $
                               8280, $
                               8281, $
                               8282, $
                               8283, $
                               8284, $
                               8285, $
                               8286, $
                               8287, $
                               8288, $
                               8289, $
                               8290, $
                               8291, $
                               8292]


           END
           ELSE: BEGIN
              userDef_hashFile = 'Strangeway_et_al_2005__v2-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-SOUTH.sav'
           END
        ENDCASE

        ;; These minILATs only apply to dayside
        minILATs = [[9443,70], $
                    [9444,74], $
                    [9452,70], $
                    [9454,70], $
                    [9457,70], $
                    [9463,76], $
                    [9465,74], $
                    [9472,68], $
                    [9474,72], $
                    [9475,68], $
                    [9476,67], $
                    [9477,70], $
                    [9478,62], $
                    [9479,63], $
                    [9480,60], $
                    [9483,72], $
                    [9485,65], $
                    [9486,74], $
                    [9487,65], $
                    [9488,65], $
                    [9489,66], $
                    [9490,67], $
                    [9493,70], $
                    [9495,71], $
                    [9497,75], $
                    [9498,72], $
                    [9504,73], $
                    [9506,74], $
                    [9517,75]]

     END
     KEYWORD_SET(north): BEGIN

        CASE 1 OF
           KEYWORD_SET(use_v3_strangeway): BEGIN

              CASE 1 OF
                 KEYWORD_SET(use_elec_lb30eV_hashFile): BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-30eVLBforelec.sav'
                 END
                 KEYWORD_SET(use_elec_lb50eV_hashFile): BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-50eVLBforelec.sav'
                 END
                 ELSE: BEGIN
                    userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s.sav'
                 END
              ENDCASE

              skip_these_orbs = [9443, $
                                 9444, $
                                 9452, $
                                 9454, $
                                 9457, $
                                 9463, $
                                 9465, $
                                 9472, $
                                 9474, $
                                 9475, $
                                 9476, $
                                 9477, $
                                 9478, $
                                 9479, $
                                 9480, $
                                 9483, $
                                 9485, $
                                 9486, $
                                 9487, $
                                 9488, $
                                 9489, $
                                 9490, $
                                 9493, $
                                 9495, $
                                 9497, $
                                 9498, $
                                 9504, $
                                 9506, $
                                 9517]

           END
           ELSE: BEGIN
              userDef_hashFile = 'Strangeway_et_al_2005__v2-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s.sav'
           END
        ENDCASE

        ;; These minILATs only apply to dayside
        minILATs = [[8260,70], $
                    [8261,74], $
                    [8262,70], $
                    [8263,70], $
                    [8264,70], $
                    [8265,76], $
                    [8266,74], $
                    [8267,68], $
                    [8268,72], $
                    [8269,68], $
                    [8270,67], $
                    [8271,70], $
                    [8272,62], $
                    [8273,63], $
                    [8274,60], $
                    [8275,72], $
                    [8276,65], $
                    [8277,74], $
                    [8278,65], $
                    [8279,65], $
                    [8280,66], $
                    [8281,67], $
                    [8282,70], $
                    [8283,71], $
                    [8284,75], $
                    [8285,72], $
                    [8286,73], $
                    [8287,74], $
                    [8288,74], $
                    [8289,74], $
                    [8290,74], $
                    [8291,74], $
                    [8292,75]]
     END
  ENDCASE

  IF KEYWORD_SET(simple_60_assumption) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(south): BEGIN
           PRINT,"Simple assumption that ILAT lower limit for ion upflow is 65 deg ..."
           minILATs[1,*] = 60
        END
        KEYWORD_SET(north): BEGIN
           PRINT,"Simple assumption that ILAT lower limit for ion upflow is 60 deg ..."
           minILATs[1,*] = 60
        END
     ENDCASE
  ENDIF

  CASE 1 OF
     KEYWORD_SET(use_v3_strangeway): BEGIN
        func = 'EXTRACT_STRANGEWAY_STATS__V3'
     END
     ELSE: BEGIN
        func = 'EXTRACT_STRANGEWAY_STATS__V2'
     END
  ENDCASE

  yesDay = (~KEYWORD_SET(night)) AND N_ELEMENTS(minMLT) EQ 0 AND N_ELEMENTS(maxMLT) EQ 0

  this = CALL_FUNCTION(func,/AVERAGES, $
                       SOUTH=south, $
                       NORTH=north, $
                       SKIP_THESE_ORBS=skip_these_orbs, $
                       DAY=yesDay, $
                       NIGHT=KEYWORD_SET(night), $
                       MINMLT=minMLT, $
                       MAXMLT=maxMLT, $
                       USERDEF_HASHFILE=userDef_hashFile, $
                       MINILAT=minILATs, $
                       MAXILAT=maxILAT, $
                       RESTORE_LAST_FILE=restore_last_file, $
                       OUT_PLOTINFO=plotInfo, $
                       NO_PLOTS=no_plots, $
                       HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
                       AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass)


END

