;2018/08/01
PRO JOURNAL__20180801__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS, $
   SOUTH=south, $
   NORTH=north, $
   RESTORE_LAST_FILE=restore_last_file, $
   USE_V3_STRANGEWAY=use_v3_strangeway, $
   OUT_STATS=this, $
   OUT_PLOTINFO=plotInfo, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; south = 1
  ;; north = 0

  simple_60_assumption = 1

  night_instead = 0

  ;; use_v3_strangeway = 1

  CASE 1 OF
     KEYWORD_SET(south): BEGIN

        CASE 1 OF
           KEYWORD_SET(use_v3_strangeway): BEGIN
              ;; PRINT,"Can't!"
              ;; STOP
           userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s.sav'

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
              userDef_hashFile = 'Strangeway_et_al_2005__v3-threshEFlux5e5-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s.sav'

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
     PRINT,"Simple assumption that ILAT lower limit for ion upflow is 60 deg ..."
     minILATs[1,*] = 60
  ENDIF

  CASE 1 OF
     KEYWORD_SET(use_v3_strangeway): BEGIN
        func = 'EXTRACT_STRANGEWAY_STATS__V3'
     END
     ELSE: BEGIN
        func = 'EXTRACT_STRANGEWAY_STATS__V2'
     END
  ENDCASE

  this = CALL_FUNCTION(func,/AVERAGES, $
                       SOUTH=south, $
                       NORTH=north, $
                       SKIP_THESE_ORBS=skip_these_orbs, $
                       DAY=~KEYWORD_SET(night_instead), $
                       NIGHT=KEYWORD_SET(night_instead), $
                       USERDEF_HASHFILE=userDef_hashFile, $
                       MINILAT=minILATs, $
                       RESTORE_LAST_FILE=restore_last_file, $
                       OUT_PLOTINFO=plotInfo, $
                       NO_PLOTS=no_plots)


END
