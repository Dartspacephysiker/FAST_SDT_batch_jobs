;2018/09/18
PRO JOURNAL__20181207__N_OG_S_HALVKULENE__AGU, $
   NIGHT=night, $
   MINMLT=minMLT, $
   MAXMLT=maxMLT, $
   JUST_NORTH=just_north, $
   RESTORE_LAST_FILE=restore_last_file, $
   USE_V3_STRANGEWAY=use_v3_strangeway, $
   V3__HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
   V3__AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass, $
   JUST_USE_LOCALE_NOT_OUTFLOW_FOR_AVGS_OF_OTHER_QUANTS=just_use_locale_not_outflow_for_avgs_of_other_quants

  COMPILE_OPT IDL2,STRICTARRSUBS

  no_plots = 1

  use_v3_strangeway = 1

  IF ~KEYWORD_SET(just_north) THEN BEGIN

     JOURNAL__20181207__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS__AGU, $
        ;; SOUTH=south, $
        ;; NORTH=north, $
        /SOUTH, $
        NIGHT=night, $
        MINMLT=minMLT, $
        MAXMLT=maxMLT, $
        RESTORE_LAST_FILE=restore_last_file, $
        USE_V3_STRANGEWAY=use_v3_strangeway, $
        OUT_STATS=thisS, $
        OUT_PLOTINFO=plotInfoS, $
        NO_PLOTS=no_plots, $
        V3__HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
        V3__AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass, $
        JUST_USE_LOCALE_NOT_OUTFLOW_FOR_AVGS_OF_OTHER_QUANTS=just_use_locale_not_outflow_for_avgs_of_other_quants

  ENDIF
  
  JOURNAL__20181207__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS__AGU, $
     /NORTH, $
     NIGHT=night, $
     MINMLT=minMLT, $
     MAXMLT=maxMLT, $
     RESTORE_LAST_FILE=restore_last_file, $
     USE_V3_STRANGEWAY=use_v3_strangeway, $
     OUT_STATS=thisN, $
     OUT_PLOTINFO=plotInfoN, $
     NO_PLOTS=no_plots, $
     V3__HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
     V3__AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass, $
     JUST_USE_LOCALE_NOT_OUTFLOW_FOR_AVGS_OF_OTHER_QUANTS=just_use_locale_not_outflow_for_avgs_of_other_quants

  IF ~KEYWORD_SET(just_north) THEN BEGIN

     CASE 1 OF
        KEYWORD_SET(use_v3_strangeway): BEGIN
           this = { $
                  orbit       : [thisN.orbit,thisS.orbit]           , $
                  EAlongVAC   : [thisN.EAlongVAC,thisS.EAlongVAC]   , $
                  dB_perpAC   : [thisN.dB_perpAC,thisS.dB_perpAC]   , $
                  pFAlongBDC  : [thisN.pFAlongBDC,thisS.pFAlongBDC] , $
                  pFAlongPDC  : [thisN.pFAlongPDC,thisS.pFAlongPDC] , $
                  pFAlongBAC  : [thisN.pFAlongBAC,thisS.pFAlongBAC] , $
                  pFAlongPAC  : [thisN.pFAlongPAC,thisS.pFAlongPAC] , $
                  DSPDC       : [thisN.DSPDC,thisS.DSPDC]           , $
                  Je          : [thisN.Je,thisS.Je]                 , $
                  Jee         : [thisN.Jee,thisS.Jee]               , $
                  Ji          : [thisN.Ji,thisS.Ji]                 , $
                  dense       : [thisN.dense,thisS.dense]           , $
                  pFBDCAC     : [thisN.pFBDCAC,thisS.pFBDCAC]       , $
                  densPFBAC   : [thisN.densPFBAC,thisS.densPFBAC]   }
        END
        ELSE: BEGIN
           this = { $
                  orbit       : [thisN.orbit,thisS.orbit]           , $
                  EAlongVAC   : [thisN.EAlongVAC,thisS.EAlongVAC]   , $
                  dB_perpAC   : [thisN.dB_perpAC,thisS.dB_perpAC]   , $
                  pFAlongBDC  : [thisN.pFAlongBDC,thisS.pFAlongBDC] , $
                  pFAlongPDC  : [thisN.pFAlongPDC,thisS.pFAlongPDC] , $
                  pFAlongBAC  : [thisN.pFAlongBAC,thisS.pFAlongBAC] , $
                  pFAlongPAC  : [thisN.pFAlongPAC,thisS.pFAlongPAC] , $
                  DSPDC       : [thisN.DSPDC,thisS.DSPDC]           , $
                  Je          : [thisN.Je,thisS.Je]                 , $
                  Jee         : [thisN.Jee,thisS.Jee]               , $
                  Ji          : [thisN.Ji,thisS.Ji]}


        END
     ENDCASE

     STR_ELEMENT,plotInfoN,"yData",this.Ji,/ADD_REPLACE

     pSymMagnitudes = [MAKE_ARRAY(N_ELEMENTS(thisN.orbit),VALUE=0B,/BYTE), $
                       MAKE_ARRAY(N_ELEMENTS(thisS.orbit),VALUE=1B,/BYTE)]

     pSymRGBTable   = MAKE_ARRAY(3,256,VALUE=0B,/BYTE)

     pSymRGBTable[*,0] = BYTE([255,0,0])
     pSymRGBTable[*,1] = BYTE([0,0,255])

  ENDIF ELSE BEGIN 

     this = thisN

     ;; pSymMagnitudes = MAKE_ARRAY(N_ELEMENTS(thisN.orbit),VALUE=0B,/BYTE)

     ;; pSymRGBTable   = MAKE_ARRAY(3,256,VALUE=0B,/BYTE)

     ;; pSymRGBTable[*,0] = BYTE([255,0,0])
     ;; pSymRGBTable[*,1] = BYTE([0,0,255])

  ENDELSE

  PLOT_STRANGEWAY_STATS, $
     this, $
     PLOTINFO=plotInfoN, $
     OUT_PLOTARR=plotArr, $
     SQUARE_WINDOW=square_window, $
     SAVE_PLOTS=save_plots, $
     PLOTDIR=plotDir, $
     PSYMMAGNITUDES=pSymMagnitudes, $
     PSYMRGBTABLE=pSymRGBTable


END

