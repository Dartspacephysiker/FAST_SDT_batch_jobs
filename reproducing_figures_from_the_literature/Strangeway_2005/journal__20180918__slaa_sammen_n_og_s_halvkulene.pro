;2018/09/18
PRO JOURNAL__20180918__SLAA_SAMMEN_N_OG_S_HALVKULENE

  COMPILE_OPT IDL2,STRICTARRSUBS

  no_plots = 1

  use_v3_strangeway = 1
  restore_last_file = 0

  JOURNAL__20180801__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS, $
     ;; SOUTH=south, $
     ;; NORTH=north, $
     /SOUTH, $
     ;; /NORTH, $
     RESTORE_LAST_FILE=restore_last_file, $
     ;; /RESTORE_LAST_FILE, $
     USE_V3_STRANGEWAY=use_v3_strangeway, $
     OUT_STATS=thisS, $
     OUT_PLOTINFO=plotInfoS, $
     NO_PLOTS=no_plots

  JOURNAL__20180801__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS, $
     ;; SOUTH=south, $
     ;; NORTH=north, $
     ;; /SOUTH, $
     /NORTH, $
     RESTORE_LAST_FILE=restore_last_file, $
     ;; /RESTORE_LAST_FILE, $
     USE_V3_STRANGEWAY=use_v3_strangeway, $
     OUT_STATS=thisN, $
     OUT_PLOTINFO=plotInfoN, $
     NO_PLOTS=no_plots

  CASE 1 OF
     KEYWORD_SET(use_v3_strangeway): BEGIN
        this = { $
               orbit       : [thisN.orbit,thisS.orbit]           , $
               EAlongVAC   : [ThisN.EAlongVAC,ThisS.EAlongVAC]   , $
               dB_perpAC   : [thisN.dB_perpAC,thisS.dB_perpAC]   , $
               pFAlongBDC  : [thisN.pFAlongBDC,thisS.pFAlongBDC] , $
               pFAlongPDC  : [thisN.pFAlongPDC,thisS.pFAlongPDC] , $
               pFAlongBAC  : [thisN.pFAlongBAC,thisS.pFAlongBAC] , $
               pFAlongPAC  : [thisN.pFAlongPAC,thisS.pFAlongPAC] , $
               DSPDC       : [THISN.DSPDC,THISS.DSPDC]           , $
               Je          : [ThisN.Je,ThisS.Je]                 , $
               Jee         : [ThisN.Jee,ThisS.Jee]               , $
               Ji          : [ThisN.Ji,ThisS.Ji]                 , $
               dense       : [ThisN.dense,ThisS.dense]           }
     END
     ELSE: BEGIN
        this = { $
               orbit       : [thisN.orbit,thisS.orbit]           , $
               EAlongVAC   : [ThisN.EAlongVAC,ThisS.EAlongVAC]   , $
               dB_perpAC   : [thisN.dB_perpAC,thisS.dB_perpAC]   , $
               pFAlongBDC  : [thisN.pFAlongBDC,thisS.pFAlongBDC] , $
               pFAlongPDC  : [thisN.pFAlongPDC,thisS.pFAlongPDC] , $
               pFAlongBAC  : [thisN.pFAlongBAC,thisS.pFAlongBAC] , $
               pFAlongPAC  : [thisN.pFAlongPAC,thisS.pFAlongPAC] , $
               DSPDC       : [THISN.DSPDC,THISS.DSPDC]           , $
               Je          : [ThisN.Je,ThisS.Je]                 , $
               Jee         : [ThisN.Jee,ThisS.Jee]               , $
               Ji          : [ThisN.Ji,ThisS.Ji]}


     END
  ENDCASE

  STR_ELEMENT,plotInfoN,"yData",this.Ji,/ADD_REPLACE

  pSymMagnitudes = [MAKE_ARRAY(N_ELEMENTS(thisN.orbit),VALUE=0B,/BYTE), $
                    MAKE_ARRAY(N_ELEMENTS(thisS.orbit),VALUE=1B,/BYTE)]

  pSymRGBTable   = MAKE_ARRAY(3,256,VALUE=0B,/BYTE)

  pSymRGBTable[*,0] = BYTE([255,0,0])
  pSymRGBTable[*,1] = BYTE([0,0,255])

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
