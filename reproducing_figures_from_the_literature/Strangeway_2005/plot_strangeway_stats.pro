;;09/27/16
PRO PLOT_STRANGEWAY_STATS, $
   stats, $
   X_QUANTITIES=xQuants, $
   OUT_PLOTARR=plotArr
      

   COMPILE_OPT IDL2

  reqOrb   = (WHERE(xQuants EQ 0))[0] NE -1
  reqItvl  = (WHERE(xQuants EQ 1))[0] NE -1
  reqEav   = (WHERE(xQuants EQ 2))[0] NE -1
  reqdB    = (WHERE(xQuants EQ 3))[0] NE -1

  IF reqOrb  OR $
     reqItvl OR $
     reqEav  OR $
     reqdB      $
     THEN BEGIN
     PRINT,"Can't handle the following plots:"
     PRINT,"Orbit    (ind 0)"
     PRINT,"Interval (ind 1)"
     PRINT,"EalongV  (ind 2)"
     PRINT,"dB       (ind 3)"
     PRINT,''
     PRINT,'Returning ...'
     RETURN
  ENDIF

  nPlots   = N_ELEMENTS(xQuants)*2 ;one extra for linear regression
  plotArr  = MAKE_ARRAY(nPlots,/OBJ)

  xTitle   = ["", $
              "", $
              "$\Delta$B [DC] (nT)", $
              "E along V$_{sc}$ [DC] (mV/m)", $
              "Poynting Flux [DC] (mW/m^2)", $
              "Average Electron Flux (#/cm$^2$/s)", $
              "Average Electron Energy Flux (mW/m$^2$)", $
              "Ion Flux (#/cm!U2!N/s)", $
              "Average ELF amplitude (V/m)"]

  xRange   = [[0.,0.], $
              [0.,0.], $
              [0.,0.], $
              [0.,0.], $
              [1e-1,1e2], $
              [1e7,1e10], $
              [1e-2,1e0], $
              [1e6,1e10], $
              [1e-3,1e-1]]

  yTitle   = "Ion Flux (#/cm!U2!N/s)"
  yData    = (-1.)*stats.ji
  yRange   = [1e6,1e10]

  FOR k=0,nPlots-1,2 DO BEGIN
     datI   = xQuants[k/2]

     xDat   = stats.(datI)
     sDat   = SORT(xDat)
     xDat   = xDat[sDat]
     yDat   = yData[sDat]

     inds   = WHERE((xDat GT 0) AND (yDat GT 0),nInds)

     IF nInds LE 1 THEN BEGIN
        PRINT,'No good data for these plots! Outta sight!'
        CONTINUE
     ENDIF

     params = LINFIT(ALOG10(xDat[inds]),ALOG10(yDat[inds]),YFIT=yFitter)
     corr   = LINCORR(ALOG10(xDat[inds]),ALOG10(yDat[inds]),T_STAT=t_stat)

     ;; yFit = 10.^(params[1] * ALOG10(xDat[inds]) + params[0])

     xFit   = 10.^((INDGEN(10))/10.*(ALOG10(xRange[1,datI])-ALOG10(xRange[0,datI]))+$
                   ALOG10(xRange[0,datI]))
     yFit   = 10.^(params[1] * ALOG10(xFit) + params[0])

     yFitter = 10.^(params[1] * ALOG10(xDat[inds]) + params[0])

     ;; tTest   = TM_TEST(yDat[inds],yFitter,/UNEQUAL)
     ;; tTest   = TM_TEST(ALOG10(yDat[inds]),ALOG10(yFitter),/UNEQUAL)

     ;; tTest[0] = t_stat

     plotArr[k] = PLOT(xDat, $
                       yDat, $
                       XTITLE=xTitle[datI], $
                       YTITLE=yTitle, $
                       XLOG=1, $
                       YLOG=1, $
                       LINESTYLE='', $
                       SYMBOL='o', $
                       /SYM_FILLED, $
                       XRANGE=xRange[*,datI], $
                       YRANGE=yRange)

     plotArr[k+1] = PLOT(xFit,yFit, $
                         /OVERPLOT)

     slopeString  = STRING(FORMAT='(A-10,T15,F7.3)',"slope  =",params[1])
     corrString   = STRING(FORMAT='(A-10,T15,F7.3)',"r      =",corr[0])
     tString      = STRING(FORMAT='(A-10,T15,F7.3)',"t-test =",t_stat)

     slopeText    = TEXT(0.2,0.80, $
                         slopeString, $
                         /NORMAL, $
                         FONT_NAME='Courier', $
                         TARGET=plotArr[k])
     corrText     = TEXT(0.2,0.75, $
                         corrString, $
                         /NORMAL, $
                         FONT_NAME='Courier', $
                         TARGET=plotArr[k])
     tText        = TEXT(0.2,0.70, $
                         tString, $
                         /NORMAL, $
                         FONT_NAME='Courier', $
                         TARGET=plotArr[k])

  ENDFOR

  ;; that = PLOT(stats.pfalongb,(-1.)*stats.ji, $
  ;;             XTITLE=xTitle[2], $
  ;;             YTITLE=yTitle, $
  ;;             XLOG=1, $
  ;;             YLOG=1, $
  ;;             LINESTYLE='', $
  ;;             SYMBOL='o', $
  ;;             /SYM_FILLED, $
  ;;             XRANGE=[1e-1,1e2], $
  ;;             YRANGE=[1e6,1e10])

END
