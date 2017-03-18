;2017/03/17
PRO PLOT_TEMPERATURE_AND_DENSITY_TSERIES, $
   jvPlotData, $
   ORIGINAL_PLOTIDEE=orig_plotIdee, $
   SAVEPLOT=savePlot, $
   SPNAME=spName, $
   ORIGINATING_ROUTINE=routName, $
   PLOTDIR=plotDir, $
   OUT_WINDOW=window1, $
   OVERPLOTALL=overplotAll, $
   OVERPLOT_WINDOW=overplot_window

  COMPILE_OPT IDL2,STRICTARRSUBS


  ;; errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.
  nPoints          = N_ELEMENTS(jvPlotData.time)

  rgbTable         = 4
  nColors          = 256
  transpose        = 1
  hammerCT         = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  tDownTitle         = 'T, downgoing e!U-!N (eV)'

  ;; nDownRange       = MINMAX(jvPlotData.nDown)
  nDownRange       = [1e-3,1e0]
  nDownTitle       = 'N (cm!U-3!N)'

  errSym           = '.'
  errSym_size      = 3.0
  errSym_fill      = 0
  errSym_capSize   = 0.05

  ;;These all pertain to grid creation
  xGridStyle       = ':'
  yGridStyle       = ':'
  xTickLen         = 1
  yTickLen         = 1
  xSubTickLen      = 0.01
  ySubTickLen      = 0.01
  
  winDim           = [900,600]

  p1pos            = [0.10,0.08,0.95,0.50]
  p2pos            = [0.10,0.53,0.95,0.94]
  ;; p3pos            = [0.54,0.08,0.95,0.94]
  cbpos            = [0.10,0.97,0.95,0.99]

  orig_p1Pos       = [0.1,0.1,0.95,0.8]
  orig_cbPos       = [0.1,0.96,0.95,0.98]

  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     pDirSuff      = '/cur_and_pot_analysis'
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
  ENDIF

  timeTitle        = 'Seconds since ' + TIME_TO_STR(jvPlotData.time[0])

  tDownRange       = MINMAX(jvPlotData.tDown)

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window        = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

     sPlot         = SCATTERPLOT(jvPlotData.tDown, $
                             jvPlotData.nDown, $
                             XRANGE=tDownRange, $
                             YRANGE=nDownRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=jvPlotData.tMag, $
                             SYMBOL='*', $
                             SYM_SIZE=2.0, $
                             SYM_THICK=2.0, $
                             SYM_TRANSPARENCY=70, $
                             XTITLE=tDownTitle, $
                             YTITLE=nDownTitle, $
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
     nTMarks       = 5
     tInds         = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
     tickValues    = tMag[safe_i[tInds]]
     tickTimes     = time[safe_i[tInds]]
     tickName      = STRMID(TIME_TO_STR(tickTimes),11,15)
     tMagRange     = [tMag[safe_i[0]],tMag[safe_i[-1]]]
     cb            = COLORBAR(RGB_TABLE=rgbTable, $
                            TICKNAME=tickName, $
                            TICKVALUES=tickValues, $
                            POSITION=orig_cbPos, $
                            RANGE=tMagRange, $
                            ;; TEXT_ORIENTATION=180, $
                            /NORMAL)
     STOP

  ENDIF

  IF ~KEYWORD_SET(overplotAll) THEN BEGIN
     window1          = WINDOW(DIMENSIONS=winDim, $
                               BUFFER=savePlot)
  ENDIF
  
  CTInds           = BYTSCL(jvPlotData.tMag)

  tRange           = [0,jvPlotdata.tDiff[-1]]
  errTDownRange    = TDownRange

  CASE 1 OF
     KEYWORD_SET(sans_errorBars): BEGIN

        ;;The old, error bar–less way

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;First plot

        ;; plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
        ;;                               jvPlotData.tDown[inds], $
        ;;                               tmpErr, $
        ;;                               XRANGE=tRange, $
        ;;                               ;; YRANGE=TDownRange, $
        ;;                               YRANGE=errTDownRange, $
        ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].jvPlotData.time[inds]), $
        ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                               /CURRENT, $
        ;;                               POSITION=p1pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        plot_2            = SCATTERPLOT(jvPlotData.tDiff, $
                                        jvPlotData.nDown, $
                                        ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
                                        YTITLE=nDownTitle, $
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
        ;; plot_3      = SCATTERPLOT(jvPlotData.cur, $
        ;;                           jvPlotData.nDown, $
        ;;                           XRANGE=TDownRange, $
        ;;                           YRANGE=nDownRange, $
        ;;                           /YLOG, $
        ;;                           RGB_TABLE=hammerCT, $
        ;;                           MAGNITUDE=jvPlotData.tMag, $
        ;;                           SYMBOL=jvSym, $
        ;;                           SYM_SIZE=jvSymSize, $
        ;;                           SYM_THICK=jvSymThick, $
        ;;                           SYM_TRANSPARENCY=jvSymTransp, $
        ;;                           SYM_FILLED=jvSymFilled, $
        ;;                           YTICKVALUES=jv_tickValues, $
        ;;                           YTICKNAME=jv_tickNames, $
        ;;                           XTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                           YTITLE='$\Phi$ (V)', $
        ;;                           XGRIDSTYLE=':', $
        ;;                           YGRIDSTYLE=':', $
        ;;                           XTICKLEN=1, $
        ;;                           YTICKLEN=1, $
        ;;                           XSUBTICKLEN=0.01, $
        ;;                           YSUBTICKLEN=0.01, $
        ;;                           /CURRENT, $
        ;;                           POSITION=p3pos)

     END
     ELSE: BEGIN

        ;;Initialize things
        inds              = [0,1]
        tmpTDownErr         = jvPlotData.tDownErr[inds]

        plot_1            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.tDown[inds], $
                                      tmpTDownErr, $
                                      XRANGE=tRange, $
                                      ;; YRANGE=TDownRange, $
                                      YRANGE=errTDownRange, $
                                      XTITLE=timeTitle, $
                                      YTITLE=tDownTitle, $
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
                                      OVERPLOT=KEYWORD_SET(overplotAll), $
                                      POSITION=p1pos, $
                                      BUFFER=savePlot)

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,k+1]
           ;; tmpTDownErr      = tDownErr[*,inds]
           tmpTDownErr      = jvPlotData.tDownErr[inds]

           plot_1         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.tDown[inds], $
                                      tmpTDownErr, $
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
        tmpNDownErr         = jvPlotData.nDownErr[inds]

        errNDownRange       = MINMAX(jvPlotData.nDown)
        plot_2            = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.nDown[inds], $
                                      tmpNDownErr, $
                                      XRANGE=tRange, $
                                      /YLOG, $
                                      YRANGE=nDownRange, $
                                      YTITLE=nDownTitle, $
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
                                      OVERPLOT=KEYWORD_SET(overplotAll), $
                                      POSITION=p2pos, $
                                      XSHOWTEXT=0B)
        ;; plot_2.xshowtext  = 0B

        ;;Now add all the other symbols
        FOR k=2,nPoints-1,2 DO BEGIN

           inds           = [k,k+1]
           tmpNDownErr      = jvPlotData.nDownErr[inds]

           plot_2         = ERRORPLOT((jvPlotData.tDiff[inds]), $
                                      jvPlotData.nDown[inds], $
                                      tmpNDownErr, $
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

        ;; inds              = [0,1]
        ;; ;; tmpTDownErr         = tDownErr[*,inds]
        ;; ;; tmpNDownErr         = nDownErr[*,inds]
        ;; tmpTDownErr         = jvPlotData.tDownErr[inds]
        ;; tmpNDownErr         = jvPlotData.nDownErr[inds]

        ;; plot_3      = ERRORPLOT(jvPlotData.tDown[inds], $
        ;;                         jvPlotData.nDown[inds], $
        ;;                         tmpTDownErr, $
        ;;                         tmpNDownErr, $
        ;;                         XRANGE=TDownRange, $
        ;;                         YRANGE=nDownRange, $
        ;;                         /YLOG, $
        ;;                         LINESTYLE='', $
        ;;                         ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
        ;;                         ERRORBAR_CAPSIZE=errSym_capSize, $
        ;;                         SYMBOL=errSym, $
        ;;                         SYM_SIZE=errSym_size, $
        ;;                         SYM_FILLED=errSym_fill, $
        ;;                         VERT_COLORS=CTInds[inds], $
        ;;                         YTICKVALUES=jv_tickValues, $
        ;;                         YTICKNAME=jv_tickNames, $
        ;;                         XTITLE=jv_xTitle, $
        ;;                         YTITLE=jv_yTitle, $
        ;;                         XGRIDSTYLE=xGridStyle, $
        ;;                         YGRIDSTYLE=yGridStyle, $
        ;;                         XTICKLEN=xTickLen, $
        ;;                         YTICKLEN=yTickLen, $
        ;;                         XSUBTICKLEN=xSubTickLen, $
        ;;                         YSUBTICKLEN=ySubTickLen, $
        ;;                         /CURRENT, $
        ;;                         POSITION=p3pos)

        ;; ;;Now add all the other symbols
        ;; FOR k=2,nPoints-1,2 DO BEGIN

        ;;    inds           = [k,k+1]
        ;;    tmpTDownErr      = jvPlotData.tDownErr[inds]
        ;;    tmpNDownErr      = jvPlotData.nDownErr[inds]

        ;;    plot_3         = ERRORPLOT((jvPlotData.tDown[inds]), $
        ;;                               jvPlotData.nDown[inds], $
        ;;                               tmpTDownErr, $
        ;;                               tmpNDownErr, $
        ;;                               ;; RGB_TABLE=hammerCT, $
        ;;                               LINESTYLE='', $
        ;;                               VERT_COLORS=hammerCT[*,CTInds[inds]], $
        ;;                               ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
        ;;                               ERRORBAR_CAPSIZE=errSym_capSize, $
        ;;                               SYMBOL=errSym, $
        ;;                               SYM_SIZE=errSym_size, $
        ;;                               SYM_FILLED=errSym_fill, $
        ;;                               /CURRENT, $
        ;;                               /OVERPLOT)
        ;; ENDFOR

     END
  ENDCASE

  ;;And a colorbar thing
  IF ~KEYWORD_SET(overplotAll) THEN BEGIN
     nTMarks     = 5
     tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
     IF tInds[-1] EQ nPoints THEN tInds[-1] -= 1
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

  ENDIF



END
