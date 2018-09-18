;;09/27/16
PRO PLOT_STRANGEWAY_STATS, $
   stats, $
   PLOTINFO=plotInfo, $
   OUT_PLOTARR=plotArr, $
   SQUARE_WINDOW=square_window, $
   SAVE_PLOTS=save_plots, $
   PLOTDIR=plotDir, $
   PSYMMAGNITUDES=pSymMagnitudes, $
   PSYMRGBTABLE=pSymRGBTable

   COMPILE_OPT IDL2

   sqWinDims = [800,800]
   winDims   = [800,600]

   ;;plot output stuff
   IF KEYWORD_SET(save_plots) THEN BEGIN

      IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
         SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotInfo.plotDirSuff
      ENDIF

   ENDIF

   xQuants     = plotInfo.xQuants

   nBad        = 0
   verbotenArr = !NULL
   FOR jDawg=0,N_ELEMENTS(plotInfo.verboten)-1 DO BEGIN

      bad = (WHERE(plotInfo.xQuants EQ plotInfo.verboten[jDawg]))[0] NE -1

      IF bad THEN BEGIN
         nBad++
         verbotenArr = [verbotenArr,plotInfo.navn_verboten[jDawg]]
      ENDIF
   ENDFOR

   IF nBad GT 0 THEN BEGIN
      PRINT,"Can't handle the following plots:"
      FOR jDawg=0,nBad-1 DO PRINT,verbotenArr[jDawg]
      PRINT,''
      PRINT,'Returning ...'
      RETURN
   ENDIF

   nPlots   = N_ELEMENTS(plotInfo.xQuants)*2 ;one extra for linear regression
   plotArr  = MAKE_ARRAY(nPlots,/OBJ)

   xTitle   = plotInfo.xTitle

   xRange   = plotInfo.xRange

   yTitle   = plotInfo.yTitle
   yData    = plotInfo.yData
   yRange   = plotInfo.yRange

   nNegY    = N_ELEMENTS(WHERE(yData LT 0))
   nYData   = N_ELEMENTS(yData)
   IF nNegY GE nYData/2 THEN BEGIN
      PRINT,FORMAT='(A0,A0,A0,F0.2,A0)', $
            "Have you got the sign wrong? Your quantity is ", $
            yTitle, $
            ', and ', $
            FLOAT(nNegY)/nYData*100., $
            "% of them are neg."
      ;; STOP
      yData *= -1.D

   ENDIF

   plotSN = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + plotInfo.canonPref + $
            plotInfo.plots_prefix + plotInfo.plotNames

   ;; IF KEYWORD_SET(square_window) THEN BEGIN
   windowArr = MAKE_ARRAY(N_ELEMENTS(xQuants),/OBJ)
   ;; ENDIF

   PRINT,'xQuant',' ','Title'
   FOR k=0,nPlots-1,2 DO BEGIN

      datI   = xQuants[k/2]

      xDat   = stats.(datI)
      sDat   = SORT(xDat)
      xDat   = xDat[sDat]
      yDat   = yData[sDat]

      IF KEYWORD_SET(pSymMagnitudes) THEN $
         IF N_ELEMENTS(pSymMagnitudes) EQ N_ELEMENTS(sDat) THEN BEGIN
         magnitude = pSymMagnitudes[sDat]
      ENDIF

      inds   = WHERE((xDat GT 0) AND (yDat GT 0),nInds)

      PRINT,k,' ',(TAG_NAMES(stats))[datI]

      IF nInds LE 1 THEN BEGIN
         PRINT,'No good data for these plots! Outta sight!'
         IF (WHERE(xDat GT 0))[0] EQ -1 THEN PRINT,'(xData is yunk)'
         IF (WHERE(yDat GT 0))[0] EQ -1 THEN PRINT,'(yData is yunk)'
         CONTINUE
      ENDIF

      windowArr[k/2] = WINDOW(DIMENSIONS=KEYWORD_SET(square_window) ? sqWinDims : winDims)

      params = LINFIT(ALOG10(xDat[inds]),ALOG10(yDat[inds]),YFIT=yFitter)
      corr   = LINCORR(ALOG10(xDat[inds]),ALOG10(yDat[inds]),T_STAT=t_stat)

      xFit   = 10.^((INDGEN(10))/10.*(ALOG10(xRange[1,datI])-ALOG10(xRange[0,datI]))+$
                    ALOG10(xRange[0,datI]))
      yFit   = 10.^(params[1] * ALOG10(xFit) + params[0])

      yFitter = 10.^(params[1] * ALOG10(xDat[inds]) + params[0])

      plotArr[k] = SCATTERPLOT(xDat, $
                               yDat, $
                               XTITLE=xTitle[datI], $
                               YTITLE=yTitle, $
                               XLOG=1, $
                               YLOG=1, $
                               ;; LINESTYLE='', $
                               MAGNITUDE=magnitude, $
                               RGB_TABLE=pSymRGBTable, $
                               SYMBOL='o', $
                               /SYM_FILLED, $
                               XRANGE=xRange[*,datI], $
                               YRANGE=yRange, $
                               FONT_SIZE=18, $
                               CURRENT=windowArr[k/2])

      plotArr[k+1] = PLOT(xFit,yFit, $
                          /OVERPLOT, $
                          CURRENT=windowArr[k/2])

      slopeString  = STRING(FORMAT='(A-10,T15,F7.3)',"slope  =",params[1])
      corrString   = STRING(FORMAT='(A-10,T15,F7.3)',"r      =",corr[0])
      tString      = STRING(FORMAT='(A-10,T15,F7.3)',"t-test =",t_stat)

      slopeText    = TEXT(0.2,0.80, $
                          slopeString, $
                          /NORMAL, $
                          FONT_NAME='Courier', $
                          FONT_SIZE=18, $
                          TARGET=plotArr[k])
      corrText     = TEXT(0.2,0.75, $
                          corrString, $
                          /NORMAL, $
                          FONT_NAME='Courier', $
                          FONT_SIZE=18, $
                          TARGET=plotArr[k])
      tText        = TEXT(0.2,0.70, $
                          tString, $
                          /NORMAL, $
                          FONT_NAME='Courier', $
                          FONT_SIZE=18, $
                          TARGET=plotArr[k])

      IF KEYWORD_SET(save_plots) THEN BEGIN
         PRINT,"Saving " + plotSN[datI] + '.png' + ' ...'
         windowArr[k/2].Save,plotDir+plotSN[datI]+'.png'

         windowArr[k/2].Close
      ENDIF

   ENDFOR

END
