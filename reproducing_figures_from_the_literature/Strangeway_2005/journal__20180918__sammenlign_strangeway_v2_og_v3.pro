;2018/09/18
PRO JOURNAL__20180918__SAMMENLIGN_STRANGEWAY_V2_OG_V3

  COMPILE_OPT IDL2,STRICTARRSUBS

  thisOrb = 8276

  ;; Can change which database to use here! There are currently three (2018/07/27)
  @strangeway_2005__defaults__v3.pro

  outDir3      = outDir
  hashFile3    = hashFile
  indivOrbPref3 = indivOrbPref

  RESTORE,outDir3+hashFile3

  swHash3 = TEMPORARY(swHash)

  keys3    = swHash3.Keys()
  key3     = (swHash3.Keys())[0]
  struct3  = swHash3[key3,0]

  keys3 = keys3.ToArray()

  @strangeway_2005__defaults__v2.pro

  outDir2      = outDir
  hashFile2    = hashFile
  indivOrbPref2 = indivOrbPref

  RESTORE,outDir2+hashFile2

  swHash2 = TEMPORARY(swHash)

  keys2    = swHash2.Keys()
  key2     = (swHash2.Keys())[0]
  struct2  = swHash2[key2,0]

  keys2 = keys2.ToArray()

  s3    = swHash3[thisOrb,0]
  s2    = swHash2[thisOrb,0]

  no_plots = 1

  JOURNAL__20180801__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS, $
     ;; SOUTH=south, $
     ;; NORTH=north, $
     ;; /SOUTH, $
     /NORTH, $
     ;; RESTORE_LAST_FILE=restore_last_file, $
     /RESTORE_LAST_FILE, $
     /USE_V3_STRANGEWAY, $
     OUT_STATS=this3, $
     OUT_PLOTINFO=plotInfo3, $
     NO_PLOTS=no_plots

  JOURNAL__20180801__EXTRACT_STRANGEWAY_STATS__SOUTH_AND_NORTH_CUSTOM_MINILAT_BOUNDS, $
     ;; SOUTH=south, $
     ;; NORTH=north, $
     ;; /SOUTH, $
     /NORTH, $
     /RESTORE_LAST_FILE, $
     ;; USE_V3_STRANGEWAY=use_v3_strangeway, $
     OUT_STATS=this2, $
     OUT_PLOTINFO=plotInfo2, $
     NO_PLOTS=no_plots

  this = { $
         orbit       : [this2.orbit,this3.orbit]           , $
         EAlongVAC   : [this2.EAlongVAC,this3.EAlongVAC]   , $
         dB_perpAC   : [this2.dB_perpAC,this3.dB_perpAC]   , $
         pFAlongBDC  : [this2.pFAlongBDC,this3.pFAlongBDC] , $
         pFAlongPDC  : [this2.pFAlongPDC,this3.pFAlongPDC] , $
         pFAlongBAC  : [this2.pFAlongBAC,this3.pFAlongBAC] , $
         pFAlongPAC  : [this2.pFAlongPAC,this3.pFAlongPAC] , $
         DSPDC       : [this2.DSPDC,this3.DSPDC]           , $
         Je          : [this2.Je,this3.Je]                 , $
         Jee         : [this2.Jee,this3.Jee]               , $
         Ji          : [this2.Ji,this3.Ji]}

  STR_ELEMENT,plotInfo2,"yData",this.Ji,/ADD_REPLACE

  pSymMagnitudes = [MAKE_ARRAY(N_ELEMENTS(this2.orbit),VALUE=0B,/BYTE), $
                    MAKE_ARRAY(N_ELEMENTS(this3.orbit),VALUE=1B,/BYTE)]

  pSymRGBTable   = MAKE_ARRAY(3,256,VALUE=0B,/BYTE)

  pSymRGBTable[*,0] = BYTE([255,0,0])
  pSymRGBTable[*,1] = BYTE([0,0,255])

  PLOT_STRANGEWAY_STATS, $
     this, $
     PLOTINFO=plotInfo2, $
     OUT_PLOTARR=plotArr, $
     SQUARE_WINDOW=square_window, $
     SAVE_PLOTS=save_plots, $
     PLOTDIR=plotDir, $
     PSYMMAGNITUDES=pSymMagnitudes, $
     PSYMRGBTABLE=pSymRGBTable

  STOP

END
