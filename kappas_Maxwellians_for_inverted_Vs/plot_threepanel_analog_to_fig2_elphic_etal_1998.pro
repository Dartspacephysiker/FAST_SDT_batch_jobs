;;2017/03/04
PRO PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,jvPlotData, $
   ;; T1=t1, $
   ;; T2=t2, $
   ORIGINAL_PLOTIDEE=orig_plotIdee, $
   SAVEPLOT=savePlot, $
   SPNAME=spName, $
   ORIGINATING_ROUTINE=routName, $
   PLOTDIR=plotDir;; , $
   ;; ERROR_BAR_FACTOR=errorBarFac
  
  COMPILE_OPT IDL2

  ;;Temperature plot
  ;; TErr = TRANSPOSE([[curPotList[0].Terr < curPotList[0].T[3,*]],[curPotList[0].Terr]])
  ;; this = ERRORPLOT(curPotList[0].time-curPotList[0].time[0], $
  ;;                  curPotList[0].T[3,*], $
  ;;                  TErr, $
  ;;                  LINESTYLE='', $
  ;;                  SYMBOL='x', $
  ;;                  /YLOG, $
  ;;                  YRANGE=[1.,1e3])

  ;; errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.
  nPoints         = N_ELEMENTS(jvPlotData.time)

  rgbTable        = 4
  nColors         = 256
  transpose       = 1
  hammerCT        = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  curTitle        = 'j!D||!N($\mu$A m!U-2!N)'

  potRange        = [1,3e4]
  potTitle        = '$\Phi$ (V)'

  errSym          = '.'
  errSym_size     = 3.0
  errSym_fill     = 0
  errSym_capSize  = 0.05

  ;;J-V plot options
  logPotTmp       = ROUND_TO_NTH_DECIMAL_PLACE(ALOG10(potRange[1]))
  jv_tickValues   = (10L)^(LINDGEN(FIX(logPotTmp))+1)
  jv_tickNames    = !NULL
  FOR k=0,logPotTmp-1 DO $
     jv_tickNames = [jv_tickNames,STRING(FORMAT='("10!U",I0,"!N")',k+1)]
  ;; jv_tickNames      = STRING(FORMAT='('+STRCOMPRESS(N_ELEMENTS(logPotTmp-1),/REMOVE_ALL)+'())'
  jvSym           = '*'
  jvSymSize       = 2.0
  jvSymThick      = 2.0
  jvSymTransp     = 70
  jvSymFilled     = 1
  jv_xTitle       = curTitle
  jv_yTitle       = potTitle

  ;;These all pertain to grid creation
  xGridStyle      = ':'
  yGridStyle      = ':'
  xTickLen        = 1
  yTickLen        = 1
  xSubTickLen     = 0.01
  ySubTickLen     = 0.01
  
  winDim          = [900,600]

  p1pos           = [0.10,0.08,0.46,0.50]
  p2pos           = [0.10,0.53,0.46,0.94]
  p3pos           = [0.54,0.08,0.95,0.94]
  cbpos           = [0.10,0.97,0.95,0.99]

  orig_p1Pos      = [0.1,0.1,0.95,0.8]
  orig_cbPos      = [0.1,0.96,0.95,0.98]

  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     pDirSuff = '/cur_and_pot_analysis'
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
  ENDIF

  timeTitle       = 'Seconds since ' + TIME_TO_STR(jvPlotData.time[0])

  jRange      = MINMAX(jvPlotData.cur)

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window    = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

     sPlot     = SCATTERPLOT(jvPlotData.cur, $
                             jvPlotData.pot, $
                             XRANGE=jRange, $
                             YRANGE=potRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=jvPlotData.tMag, $
                             SYMBOL='*', $
                             SYM_SIZE=2.0, $
                             SYM_THICK=2.0, $
                             SYM_TRANSPARENCY=70, $
                             XTITLE=curTitle, $
                             YTITLE=potTitle, $
                             XGRIDSTYLE=xGridStyle, $
                             YGRIDSTYLE=yGridStyle, $
                             XTICKLEN=xTickLen, $
                             YTICKLEN=yTickLen, $
                             XSUBTICKLEN=xSubTickLen, $
                             YSUBTICKLEN=ySubTickLen, $
                             /CURRENT, $
                             POSITION=orig_p1Pos, $
                             BUFFER=savePlot)

     ;;And a colorbar thing
     nTMarks     = 5
     tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
     tickValues  = tMag[safe_i[tInds]]
     tickTimes   = time[safe_i[tInds]]
     tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
     tMagRange   = [tMag[safe_i[0]],tMag[safe_i[-1]]]
     cb          = COLORBAR(RGB_TABLE=rgbTable, $
                            TICKNAME=tickName, $
                            TICKVALUES=tickValues, $
                            POSITION=orig_cbPos, $
                            RANGE=tMagRange, $
                            ;; TEXT_ORIENTATION=180, $
                            /NORMAL)
     STOP

  ENDIF

  window1      = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

  CTInds       = BYTSCL(jvPlotData.tMag)

  tRange            = [0,jvPlotdata.tDiff[-1]]
  errJRange         = jRange

  CASE 1 OF
     KEYWORD_SET(sans_errorBars): BEGIN

        ;;The old, error bar–less way

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;First plot

        ;; plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
        ;;                               jvPlotData.cur[inds], $
        ;;                               tmpErr, $
        ;;                               XRANGE=tRange, $
        ;;                               ;; YRANGE=jRange, $
        ;;                               YRANGE=errJRange, $
        ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].jvPlotData.time[inds]), $
        ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                               /CURRENT, $
        ;;                               POSITION=p1pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        plot_2            = SCATTERPLOT(jvPlotData.tDiff, $
                                        jvPlotData.pot, $
                                        ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
                                        YTITLE=potTitle, $
                                        RGB_TABLE=hammerCT, $
                                        MAGNITUDE=jvPlotData.tMag, $
                                        ;; LINESTYLE='', $
                                        SYMBOL='.', $
                                        SYM_SIZE=3.0, $
                                        /SYM_FILLED, $
                                        /CURRENT, $
                                        POSITION=p2pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot
        plot_3      = SCATTERPLOT(jvPlotData.cur, $
                                  jvPlotData.pot, $
                                  XRANGE=jRange, $
                                  YRANGE=potRange, $
                                  /YLOG, $
                                  RGB_TABLE=hammerCT, $
                                  MAGNITUDE=jvPlotData.tMag, $
                                  SYMBOL=jvSym, $
                                  SYM_SIZE=jvSymSize, $
                                  SYM_THICK=jvSymThick, $
                                  SYM_TRANSPARENCY=jvSymTransp, $
                                  SYM_FILLED=jvSymFilled, $
                                  YTICKVALUES=jv_tickValues, $
                                  YTICKNAME=jv_tickNames, $
                                  XTITLE='j!D||!N($\mu$A m!U-2!N)', $
                                  YTITLE='$\Phi$ (V)', $
                                  XGRIDSTYLE=':', $
                                  YGRIDSTYLE=':', $
                                  XTICKLEN=1, $
                                  YTICKLEN=1, $
                                  XSUBTICKLEN=0.01, $
                                  YSUBTICKLEN=0.01, $
                                  /CURRENT, $
                                  POSITION=p3pos)

     END
     ELSE: BEGIN

        ;;Initialize things
        inds              = [0,1]
        tmpCurErr         = jvPlotData.curErr[inds]

        plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.cur[inds], $
                                      tmpCurErr, $
                                      XRANGE=tRange, $
                                      ;; YRANGE=jRange, $
                                      YRANGE=errJRange, $
                                      XTITLE=timeTitle, $
                                      YTITLE=curTitle, $
                                      RGB_TABLE=hammerCT, $
                                      ;; ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                      VERT_COLORS=CTInds[inds], $
                                      LINESTYLE='', $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      XGRIDSTYLE=xGridStyle, $
                                      YGRIDSTYLE=yGridStyle, $
                                      XTICKLEN=xTickLen, $
                                      YTICKLEN=yTickLen, $
                                      XSUBTICKLEN=xSubTickLen, $
                                      YSUBTICKLEN=ySubTickLen, $
                                      /CURRENT, $
                                      POSITION=p1pos, $
                                      BUFFER=savePlot)

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,k+1]
           ;; tmpCurErr      = curErr[*,inds]
           tmpCurErr      = jvPlotData.curErr[inds]

           plot_1         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.cur[inds], $
                                      tmpCurErr, $
                                      ;; RGB_TABLE=hammerCT, $
                                      VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      LINESTYLE='', $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      /CURRENT, $
                                      ;; POSITION=p1pos, $
                                      /OVERPLOT)
        ENDFOR

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        inds              = [0,1]
        tmpPotErr         = jvPlotData.potErr[inds]

        errPotRange       = MINMAX(jvPlotData.pot)
        plot_2            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.pot[inds], $
                                      tmpPotErr, $
                                      XRANGE=tRange, $
                                      /YLOG, $
                                      YRANGE=potRange, $
                                      YTITLE=potTitle, $
                                      RGB_TABLE=hammerCT, $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                      VERT_COLORS=CTInds[inds], $
                                      LINESTYLE='', $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      XGRIDSTYLE=xGridStyle, $
                                      YGRIDSTYLE=yGridStyle, $
                                      XTICKLEN=xTickLen, $
                                      YTICKLEN=yTickLen, $
                                      XSUBTICKLEN=xSubTickLen, $
                                      YSUBTICKLEN=ySubTickLen, $
                                      /CURRENT, $
                                      POSITION=p2pos, $
                                      XSHOWTEXT=0B)
        ;; plot_2.xshowtext  = 0B

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,k+1]
           tmpPotErr      = jvPlotData.potErr[inds]

           plot_2         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.pot[inds], $
                                      tmpPotErr, $
                                      /YLOG, $
                                      VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      LINESTYLE='', $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      /CURRENT, $
                                      /OVERPLOT)
        ENDFOR

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot

        inds              = [0,1]
        ;; tmpCurErr         = curErr[*,inds]
        ;; tmpPotErr         = potErr[*,inds]
        tmpCurErr         = jvPlotData.curErr[inds]
        tmpPotErr         = jvPlotData.potErr[inds]

        plot_3      = ERRORPLOT(jvPlotData.cur[inds], $
                                jvPlotData.pot[inds], $
                                tmpCurErr, $
                                tmpPotErr, $
                                XRANGE=jRange, $
                                YRANGE=potRange, $
                                /YLOG, $
                                LINESTYLE='', $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                VERT_COLORS=CTInds[inds], $
                                YTICKVALUES=jv_tickValues, $
                                YTICKNAME=jv_tickNames, $
                                XTITLE=jv_xTitle, $
                                YTITLE=jv_yTitle, $
                                XGRIDSTYLE=xGridStyle, $
                                YGRIDSTYLE=yGridStyle, $
                                XTICKLEN=xTickLen, $
                                YTICKLEN=yTickLen, $
                                XSUBTICKLEN=xSubTickLen, $
                                YSUBTICKLEN=ySubTickLen, $
                                /CURRENT, $
                                POSITION=p3pos)

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,k+1]
           tmpCurErr      = jvPlotData.curErr[inds]
           tmpPotErr      = jvPlotData.potErr[inds]

           plot_3         = ERRORPLOT((jvPlotData.cur[inds]), $
                                      jvPlotData.pot[inds], $
                                      tmpCurErr, $
                                      tmpPotErr, $
                                      ;; RGB_TABLE=hammerCT, $
                                      LINESTYLE='', $
                                      VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                      ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                      ERRORBAR_CAPSIZE=errSym_capSize, $
                                      SYMBOL=errSym, $
                                      SYM_SIZE=errSym_size, $
                                      SYM_FILLED=errSym_fill, $
                                      /CURRENT, $
                                      /OVERPLOT)
        ENDFOR

     END
  ENDCASE

  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
  tickValues  = jvPlotData.tMag[tInds]
  tickTimes   = jvPlotData.time[tInds]
  tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
  tMagRange   = [jvPlotData.tMag[0],jvPlotData.tMag[-1]]
  cb          = COLORBAR(RGB_TABLE=hammerCT, $
                         TICKNAME=tickName, $
                         TICKVALUES=tickValues, $
                         POSITION=cbpos, $
                         RANGE=tMagRange, $
                         /NORMAL)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-believeIt.png'
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

END
