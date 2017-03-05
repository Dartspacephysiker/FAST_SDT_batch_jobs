;;2017/03/04
PRO PLOT_J_VS_POTBAR,jvPlotData, $
                     J_ON_YAXIS=j_on_yAxis, $
                     SAVEPLOT=savePlot, $
                     SPNAME=spName, $
                     INTERACTIVE_OVERPLOT=interactive_overplot, $
                     ORIGINATING_ROUTINE=routName, $
                     PLOTDIR=plotDir

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(savePlot) THEN BEGIN
     interactive_overplot = 0B
  ENDIF

  rgbTable        = 4
  nColors         = 256
  transpose       = 1
  hammerCT        = COLORTABLE(rgbTable,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  curTitle        = 'j!D||!N($\mu$A m!U-2!N)'

  time            = jvPlotData.time
  tDiff           = jvPlotData.tDiff
  tMag            = jvPlotData.tMag

  curData         = jvPlotData.cur
  potData         = jvPlotData.phiBar
  curDataErr      = jvPlotData.curErr
  potDataErr      = jvPlotData.phiBarErr

  NDown           = jvPlotData.NDown
  NDownErr        = jvPlotData.NDownErr

  TDown           = jvPlotData.TDown
  TDownErr        = jvPlotData.TDownErr

  only_neg_current = 1
  IF KEYWORD_SET(only_neg_current) THEN BEGIN

     this = WHERE(curData LT 0,nThis)
     IF nThis EQ 0 THEN STOP
     
     time         = time[this]
     tDiff        = tDiff[this]
     tMag         = tMag[this]

     curData      = ABS(curData[this])
     potData      = potData[this]
     curDataErr   = ABS(curDataErr[this])
     potDataErr   = potDataErr[this]

     NDown        = NDown[this]
     NDownErr     = NDownErr[this]

     TDown        = TDown[this]
     TDownErr     = TDownErr[this]

  ENDIF

  nPoints         = N_ELEMENTS(time)
  tDiff          -= tDiff[0]
  tRange          = [tDiff[0],tDiff[-1]]


  IF KEYWORD_SET(j_on_yAxis) THEN BEGIN
     curData      = ABS(curData)
  ENDIF

  potRange        = [0.1,1e2]
  ;; potRange        = MINMAX(potData)
  potTitle        = '$\Phi$/k!DB!NT'

  ;; jRange            = KEYWORD_SET(j_on_yAxis) ? MINMAX(ABS(cur)) : MINMAX(cur)
  ;; jRange           = ROUND(MINMAX(curData))
  ;; errJRange        = jRange
  jRange          = [0.08,12]

  errSym          = '.'
  errSym_size     = 4.0
  errSym_fill     = 0
  errSym_capSize  = 0.08

  ;;J-V plot options
  ;; IF ~KEYWORD_SET(j_on_yAxis) AND KEYWORD_SET(potRange) THEN BEGIN
  ;;    logPotTmp       = ROUND_TO_NTH_DECIMAL_PLACE(ALOG10(potRange[1]))
  ;;    jv_tickValues   = (10L)^(LINDGEN(FIX(logPotTmp))+1)
  ;;    jv_tickNames    = !NULL
  ;;    FOR k=0,logPotTmp-1 DO $
  ;;       jv_tickNames = [jv_tickNames,STRING(FORMAT='("10!U",I0,"!N")',k+1)]
  ;;    ;; jv_tickNames      = STRING(FORMAT='('+STRCOMPRESS(N_ELEMENTS(logPotTmp-1),/REMOVE_ALL)+'())'
  ;; ENDIF
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

  pTpos           = [0.10,0.08,0.96,0.50]
  pNpos           = [0.10,0.53,0.96,0.94]

  orig_p1Pos      = [0.1,0.1,0.95,0.8]
  orig_cbPos      = [0.1,0.96,0.95,0.98]

  CTInds          = BYTSCL(tMag)


  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     pDirSuff = '/cur_and_pot_analysis'
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
  ENDIF

  timeTitle       = 'Seconds since ' + TIME_TO_STR(time[0])

  IF KEYWORD_SET(interactive_overplot) THEN BEGIN

     window2      = WINDOW(DIMENSIONS=[800,600])

     TempRange    = MINMAX(TDown)
     ;; TempRange    = [1.,1e3]
     TempTitle    = 'Temperature (eV)'
     TempXStyle   = 1

     DensRange    = MINMAX(NDown)
     DensRange    = [0.01,1]
     DensTitle    = 'Density (cm!U-3!N)'
     DensXStyle   = 1

     ;; TDownErr         = TRANSPOSE([[TDownErr < TDown],[TDownErr]])
     ;; plot_T0      = ERRORPLOT(TDiff, $
     ;;                          TDown, $
     ;;                          TErr, $
     ;;                          XTITLE=timeTitle, $
     ;;                          LINESTYLE='', $
     ;;                          SYMBOL='x', $
     ;;                          /YLOG, $
     ;;                          YRANGE=TempRange)


     inds              = [0,1]
     tmpTErr           = TDownErr[inds]

     plot_T0           = ERRORPLOT((tDiff[inds]), $
                                   TDown[inds], $
                                   tmpTErr, $
                                   XRANGE=tRange, $
                                   XSTYLE=TempXStyle, $
                                   YRANGE=TempRange, $
                                   XTITLE=timeTitle, $
                                   YTITLE=TempTitle, $
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
                                   POSITION=pTpos, $
                                   BUFFER=savePlot)

     ;;Now add all the other symbols
     FOR k=3,nPoints-1,2 DO BEGIN

        inds           = [k-1,k]
        ;; tmpCurErr      = curErr[*,inds]
        tmpTErr        = TDownErr[inds]

        plot_T1        = ERRORPLOT(tDiff[inds], $
                                   TDown[inds], $
                                   tmpTErr, $
                                   ;; RGB_TABLE=hammerCT, $
                                   YRANGE=TempRange, $
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

     inds              = [0,1]
     tmpNErr           = NDownErr[inds]

     plot_N0           = ERRORPLOT((tDiff[inds]), $
                                   NDown[inds], $
                                   tmpNErr, $
                                   XRANGE=tRange, $
                                   XSTYLE=DensXStyle, $
                                   YRANGE=DensRange, $
                                   XTITLE=timeTitle, $
                                   YTITLE=DensTitle, $
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
                                   POSITION=pNpos, $
                                   BUFFER=savePlot, $
                                   XSHOWTEXT=0B)

     plot_N0.xshowtext  = 0B


     ;;Now add all the other symbols
     FOR k=3,nPoints-1,2 DO BEGIN

        inds           = [k-1,k]
        ;; tmpCurErr      = curErr[*,inds]
        tmpNErr        = NDownErr[inds]

        plot_N1        = ERRORPLOT(tDiff[inds], $
                                   NDown[inds], $
                                   tmpNErr, $
                                   ;; RGB_TABLE=hammerCT, $
                                   YRANGE=DensRange, $
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

  ENDIF

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window    = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)
     sPlot     = SCATTERPLOT(curData, $
                             potData, $
                             XRANGE=jRange, $
                             YRANGE=potRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=tMag, $
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

  CASE 1 OF
     KEYWORD_SET(sans_errorBars): BEGIN

        ;;The old, error bar–less way

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;First plot

        ;; plot_1            = ERRORPLOT((tDiff[inds]), $
        ;;                               cur[inds], $
        ;;                               tmpErr, $
        ;;                               XRANGE=tRange, $
        ;;                               ;; YRANGE=jRange, $
        ;;                               YRANGE=jRange, $
        ;;                               XTITLE='Seconds since ' + TIME_TO_STR(time[inds]), $
        ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
        ;;                               /CURRENT, $
        ;;                               POSITION=p1pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Second plot

        plot_2            = SCATTERPLOT(ABS(tDiff), $
                                        potData, $
                                        ;; XTITLE='Seconds since ' + TIME_TO_STR(time[safe_i[0]]), $
                                        YTITLE=potTitle, $
                                        RGB_TABLE=hammerCT, $
                                        MAGNITUDE=tMag, $
                                        ;; LINESTYLE='', $
                                        SYMBOL='.', $
                                        SYM_SIZE=3.0, $
                                        /SYM_FILLED, $
                                        /CURRENT, $
                                        POSITION=p2pos)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Third plot
        plot_3      = SCATTERPLOT(curData, $
                                  potData, $
                                  XRANGE=jRange, $
                                  YRANGE=potRange, $
                                  /YLOG, $
                                  RGB_TABLE=hammerCT, $
                                  MAGNITUDE=tMag, $
                                  SYMBOL=jvSym, $
                                  SYM_SIZE=jvSymSize, $
                                  SYM_THICK=jvSymThick, $
                                  SYM_TRANSPARENCY=jvSymTransp, $
                                  SYM_FILLED=jvSymFilled, $
                                  YTICKVALUES=jv_tickValues, $
                                  YTICKNAME=jv_tickNames, $
                                  XTITLE=curTitle, $
                                  YTITLE=potTitle, $
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
        tmpCurErr         = curDataErr[inds]

        plot_1            = ERRORPLOT((tDiff[inds]), $
                                      curData[inds], $
                                      tmpCurErr, $
                                      XRANGE=tRange, $
                                      YRANGE=jRange, $
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
        FOR k=3,nPoints-1,2 DO BEGIN

           inds           = [k-1,k]
           ;; tmpCurErr      = curErr[*,inds]
           tmpCurErr      = curDataErr[inds]

           plot_1         = ERRORPLOT(tDiff[inds], $
                                      curData[inds], $
                                      tmpCurErr, $
                                      YRANGE=jRange, $
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
        tmpPotErr         = potDataErr[inds]

        ;; errPotRange       = potRange
        plot_2            = ERRORPLOT(tDiff[inds], $
                                      potData[inds], $
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
        plot_2.xshowtext  = 0B

        ;;Now add all the other symbols
        FOR k=3,nPoints-1,2 DO BEGIN

           inds           = [k-1,k]
           tmpPotErr      = potDataErr[inds]

           plot_2         = ERRORPLOT(tDiff[inds], $
                                      potData[inds], $
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
        tmpCurErr         = curDataErr[inds]
        tmpPotErr         = potDataErr[inds]

        IF KEYWORD_SET(j_on_yAxis) THEN BEGIN

           plot_3      = ERRORPLOT(potData[inds], $
                                   curData[inds], $
                                   tmpPotErr, $
                                   tmpCurErr, $
                                   XRANGE=potRange, $
                                   YRANGE=jRange, $
                                   /XLOG, $
                                   /YLOG, $
                                   LINESTYLE='', $
                                   ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                   ERRORBAR_CAPSIZE=errSym_capSize, $
                                   SYMBOL=errSym, $
                                   SYM_SIZE=errSym_size, $
                                   SYM_FILLED=errSym_fill, $
                                   VERT_COLORS=CTInds[inds], $
                                   ;; XTICKVALUES=jv_tickValues, $
                                   ;; XTICKNAME=jv_tickNames, $
                                   YTITLE=jv_xTitle, $
                                   XTITLE=jv_yTitle, $
                                   YGRIDSTYLE=xGridStyle, $
                                   XGRIDSTYLE=yGridStyle, $
                                   YTICKLEN=xTickLen, $
                                   XTICKLEN=yTickLen, $
                                   YSUBTICKLEN=xSubTickLen, $
                                   XSUBTICKLEN=ySubTickLen, $
                                   /CURRENT, $
                                   POSITION=p3pos)

           ;; Now add all the other symbols
           FOR k=3,nPoints-1,2 DO BEGIN

              inds           = [k-1,k]
              tmpCurErr      = curDataErr[inds]
              tmpPotErr      = potDataErr[inds]

              plot_3         = ERRORPLOT(potData[inds], $
                                         curData[inds], $
                                         tmpPotErr, $
                                         tmpCurErr, $
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

        ENDIF ELSE BEGIN

           plot_3      = ERRORPLOT(curData[inds], $
                                   potData[inds], $
                                   tmpCurErr, $
                                   tmpPotErr, $
                                   XRANGE=jRange, $
                                   YRANGE=potRange, $
                                   ;; /XLOG, $
                                   /YLOG, $
                                   LINESTYLE='', $
                                   ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                   ERRORBAR_CAPSIZE=errSym_capSize, $
                                   SYMBOL=errSym, $
                                   SYM_SIZE=errSym_size, $
                                   SYM_FILLED=errSym_fill, $
                                   VERT_COLORS=CTInds[inds], $
                                   ;; XTICKVALUES=jv_tickValues, $
                                   ;; XTICKNAME=jv_tickNames, $
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
           FOR k=3,nPoints-1,2 DO BEGIN

              inds           = [k-1,k]
              tmpCurErr      = curDataErr[inds]
              tmpPotErr      = potDataErr[inds]

              plot_3         = ERRORPLOT(curData[inds], $
                                         potData[inds], $
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

        ENDELSE

     END
  ENDCASE

  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*nPoints)/(nTMarks-1)
  IF tInds[-1] EQ nPoints THEN tInds[-1] -= 1
  tickValues  = tMag[tInds]
  tickTimes   = time[tInds]
  tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
  tMagRange   = [tMag[0],tMag[-1]]
  cb          = COLORBAR(RGB_TABLE=hammerCT, $
                         TICKNAME=tickName, $
                         TICKVALUES=tickValues, $
                         POSITION=cbpos, $
                         RANGE=tMagRange, $
                         /NORMAL)

  IF KEYWORD_SET(interactive_overplot) THEN BEGIN

     magicNumber = 100
     plotInter = MAKE_ARRAY(magicNumber,/OBJ)
     ;; colors    = GENERATE_LIST_OF_RANDOM_COLORS(magicNumber)
     ;; lStyle    = GENERATE_LIST_OF_RANDOM_LINESTYLES(magicNumber)
     colors    = ['black','purple','blue','brown','red','green']
     lStyle    = ['-','--',':','-:','__']
     nCoulouir = N_ELEMENTS(colors)
     nLStyle   = N_ELEMENTS(lStyle)
     pInd      = 0

     pot = 10.D^(LINDGEN(181)/20.-1.)
     
     running    = 1B
     maxTries   = 10B
     WHILE running DO BEGIN

        go    = 0B
        tries = 0B
        WHILE ~go DO BEGIN
           
           PRINT,(tries EQ 0 ? "Enter kappa, temperature (eV),density (cm^-3),magRatio" : "I'm serious")
           
           ;; kappa = ''
           ;; T     = ''
           ;; dens  = ''
           ;; R_B   = ''
           READ,kappa,T,dens,R_B

           cont = 0B
           PRINT,"K, I got ..."
           PRINT,FORMAT='(A0,T15,":",F0.2)',"kappa",kappa
           PRINT,FORMAT='(A0,T15,":",F0.2)',"Temp",T
           PRINT,FORMAT='(A0,T15,":",F0.2)',"Density",dens
           PRINT,FORMAT='(A0,T15,":",F0.2)',"R_B",R_B
           PRINT,''
           PRINT,"Look OK? (y/n/q)"
           WHILE ~cont DO BEGIN

              read = ''
              READ,read
              CASE 1 OF
                 STRMATCH(read,'y*',/FOLD_CASE) OR STRMATCH(read,'1',/FOLD_CASE): BEGIN

                    cont            = 1B
                    go              = 1B
                 END
                 STRMATCH(read,'n*',/FOLD_CASE) OR STRMATCH(read,'0',/FOLD_CASE): BEGIN
                    
                    cont            = 1B
                    running         = 0B

                 END
                 STRMATCH(read,'q*',/FOLD_CASE) : BEGIN

                    cont            = 1B
                    running         = 0B
                    go              = 1B
                 END
                 ELSE: BEGIN

                    IF tries GT maxTries THEN BEGIN
                       PRINT,"Can't understand a word comin' out of his mouth ..."
                       cont         = 1B
                       running      = 0B
                       go           = 1B
                    ENDIF ELSE BEGIN
                       PRINT,"Huh?"
                       tries++
                    ENDELSE

                 END
              ENDCASE

           ENDWHILE

        ENDWHILE

        IF ~running THEN BREAK

        IF kappa LT 10. THEN BEGIN
           theory = KNIGHT_RELATION__DORS_KLETZING_11(kappa,T,dens,pot,R_B, $
                                                      IN_POTBAR=in_potBar, $
                                                      OUT_POTBAR=potBar, $
                                                      /NO_MULT_BY_CHARGE)

           name = STRING(FORMAT='(F0.2,",",F0.2,",",F0.3,",",G0.2)',kappa,T,dens,R_B)
        ENDIF ELSE BEGIN
           PRINT,"Yes, a Maxwellian, I see!"

           theory = KNIGHT_RELATION__DORS_KLETZING_4(T,dens,pot,R_B, $
                                                     IN_POTBAR=in_potBar, $
                                                     OUT_POTBAR=potBar, $
                                                     /NO_MULT_BY_CHARGE)

           name = STRING(FORMAT='(A0,",",F0.2,",",F0.3,",",G0.2)',"Maxw",T,dens,R_B)

        ENDELSE
        
        plotInter[pInd] = PLOT(potBar, $
                               theory*1D6, $
                               NAME=name, $
                               LINESTYLE=lStyle[pInd MOD nLStyle], $
                               THICK=3.0, $
                               COLOR=colors[pInd MOD nCoulouir], $
                               /CURRENT, $
                               /OVERPLOT)
                         
        IF pInd EQ 0 THEN BEGIN
           leg = LEGEND(TARGET=plotInter[0])
        ENDIF ELSE BEGIN
           leg.Delete
           leg = LEGEND(TARGET=plotInter[0:pInd])
        ENDELSE
        cont = 0
        WHILE ~cont DO BEGIN
           
           PRINT,"Do another? (y/n/q/[d]elete all plots)"

           read = ''
           READ,read
           CASE 1 OF
              STRMATCH(read,'d*',/FOLD_CASE) OR STRMATCH(read,'2',/FOLD_CASE): BEGIN

                 cont            = 0B

                 IF ISA(leg) THEN leg.Delete

                 FOR k=0,pInd DO BEGIN
                    IF ISA(plotInter[k]) THEN plotInter[k].Delete
                 ENDFOR
                 
                 plotInter = MAKE_ARRAY(magicNumber,/OBJ)
                 ;; colors    = GENERATE_LIST_OF_RANDOM_COLORS(magicNumber)
                 ;; lStyle    = GENERATE_LIST_OF_RANDOM_LINESTYLES(magicNumber)
                 pInd      = 0
                 wasJustReset = 1B
              END
              STRMATCH(read,'y*',/FOLD_CASE) OR STRMATCH(read,'1',/FOLD_CASE): BEGIN

                 ;; plotInter.Delete
                 cont            = 1B
                 IF KEYWORD_SET(wasJustReset) THEN BEGIN
                    wasJustReset = 0B
                 ENDIF ELSE BEGIN
                    pInd++
                 ENDELSE
              END
              STRMATCH(read,'n*',/FOLD_CASE) OR STRMATCH(read,'0',/FOLD_CASE): BEGIN
                 
                 cont            = 1B
                 running         = 0B

              END
              STRMATCH(read,'q*',/FOLD_CASE) : BEGIN

                 cont            = 1B
                 running         = 0B

              END
              ELSE: BEGIN

                 IF tries GT maxTries THEN BEGIN
                    PRINT,"Can't understand a word comin' out of his mouth ..."
                    cont         = 1B
                    running      = 0B
                 ENDIF ELSE BEGIN
                    PRINT,"Huh?"
                    tries++
                 ENDELSE

              END
           ENDCASE
        ENDWHILE

     ENDWHILE

  ENDIF

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-believeIt.png'
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF


END
