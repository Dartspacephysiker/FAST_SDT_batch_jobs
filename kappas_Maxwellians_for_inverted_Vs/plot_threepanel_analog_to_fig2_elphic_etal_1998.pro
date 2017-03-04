;;2017/03/04
PRO PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,curPotList, $
   T1=t1, $
   T2=t2, $
   ORIGINAL_PLOTIDEE=orig_plotIdee, $
   SAVEPLOT=savePlot, $
   SPNAME=spName, $
   ORIGINATING_ROUTINE=routName, $
   PLOTDIR=plotDir, $
   ERROR_BAR_FACTOR=errorBarFac
  
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

  errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.
  rgbTable        = 4
  hammerCT        = COLORTABLE(4,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  curTitle        = 'j!D||!N($\mu$A m!U-2!N)'

  potRange        = [1,3e4]
  potTitle        = '$\Phi$ (V)'

  errSym          = '.'
  errSym_size     = 3.0
  errSym_fill     = 0
  errSym_capSize  = 0.05

  ;;J-V plot options
  logPotTmp       = ROUND_TO_NTH_DECIMAL_PLACE(ALOG10(potRange[1]))
  tickValues3     = (10L)^(LINDGEN(FIX(logPotTmp))+1)
  tickNames3      = !NULL
  FOR k=0,logPotTmp-1 DO tickNames3 = [tickNames3,STRING(FORMAT='("10!U",I0,"!N")',k+1)]
  ;; tickNames3      = STRING(FORMAT='('+STRCOMPRESS(N_ELEMENTS(logPotTmp-1),/REMOVE_ALL)+'())'
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
  nColors         = 256

  orig_p1Pos      = [0.1,0.1,0.95,0.8]
  orig_cbPos      = [0.1,0.96,0.95,0.98]

  IF ~KEYWORD_SET(plotDir) THEN BEGIN
     pDirSuff = '/cur_and_pot_analysis'
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
  ENDIF

  looking         = 3B
  ind             = 0
  WHILE (looking GT 0) DO BEGIN
     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
        looking--
        edind = ind
     ENDIF

     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
        looking--
        euind = ind
     ENDIF

     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
        looking--
        iuind = ind
     ENDIF
     ind++
  ENDWHILE

  IF ~(ARRAY_EQUAL(curPotList[0].time,curPotList[1].time) AND $
       ARRAY_EQUAL(curPotList[0].time,curPotList[2].time))    $
  THEN BEGIN
     IF (N_ELEMENTS(curPotList[0].time) NE N_ELEMENTS(curPotList[1].time)) OR $
        (N_ELEMENTS(curPotList[0].time) NE N_ELEMENTS(curPotList[2].time))    $
     THEN BEGIN
        PRINT,"Death!"
        STOP
     ENDIF

     IF ((WHERE(ABS(curPotList[0].time-curPotList[1].time) GT 1.))[0] NE -1) OR $
        ((WHERE(ABS(curPotList[0].time-curPotList[2].time) GT 1.))[0] NE -1)    $
     THEN BEGIN
        PRINT,"Whoa!"
        STOP
     ENDIF
  ENDIF

  ;;Time, the time
  time        = curPotList[edind].time

  ;;Current for plotting
  cur         = curPotList[edind].cur+curPotList[euind].cur+curPotList[iuind].cur

  ;;Errors
  curErr      = ABS(curPotList[edind].curErr) * errorBarFac

  posC_i      = WHERE(cur GT 0,nPos, $
                      COMPLEMENT=negC_i, $
                      NCOMPLEMENT=nNeg)

  ;;Et potential
  pot         = curPotList[edind].charE+curPotList[iuind].charE
  pot         = curPotList[edind].peakE+curPotList[iuind].peakE
  pot[posC_i] = curPotList[euind].peakE[posC_i]
  ;; pot[posC_i] = curPotList[euind].charE[posC_i]

  potErr      = ABS(curPotList[edind].peakErr+curPotList[iuind].peakErr)
  

  safe_i      = WHERE((curPotList[edind].peakE GE 0.) OR  $
                      (curPotList[euind].peakE GE 0.) OR  $
                      (curPotList[euind].peakE GE 0.),    $
                      nSafe)

  timeTitle       = 'Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]])

  IF nSafe LT 3 THEN STOP

  safe_i      = CGSETINTERSECTION(safe_i, $
                                  WHERE((curpotlist[0].n/curpotlist[0].n1 GT 3) OR $
                                        (curpotlist[1].n/curpotlist[1].n1 GT 3) OR $
                                        (curpotlist[2].n/curpotlist[2].n1 GT 3)))

  time_i      = WHERE(curPotList[edind].time GE t1 AND $
                      curPotList[edind].time LE t2,nTime)
  IF nTime LT 3 THEN STOP
  safe_i      = CGSETINTERSECTION(safe_i,time_i,COUNT=nSafe,NORESULT=-1)
  
  IF nSafe LT 3 THEN STOP

  ;; xRange      = [-10,15]
  jRange      = MINMAX(cur)
  ;; plot        = PLOT(cur[safe_i], $
  ;;                    pot[safe_i], $
  ;;                    XRANGE=jRange, $
  ;;                    YRANGE=potRange, $
  ;;                    /YLOG, $
  ;;                    LINESTYLE='', $
  ;;                    SYMBOL='*', $
  ;;                    XTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                    YTITLE='$\Phi$ (V)', $
  ;;                    XGRIDSTYLE=':', $
  ;;                    YGRIDSTYLE=':', $
  ;;                    XTICKLEN=1, $
  ;;                    YTICKLEN=1, $
  ;;                    XSUBTICKLEN=0.01, $
  ;;                    YSUBTICKLEN=0.01)

  ;; tMag         = (time[safe_i]-time[safe_i[0]])
  tDiff        = (time-time[0])
  tMag         = tDiff/tDiff[-1]

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window    = WINDOW(DIMENSIONS=winDim, $
                        BUFFER=savePlot)

     sPlot     = SCATTERPLOT(cur[safe_i], $
                             pot[safe_i], $
                             XRANGE=jRange, $
                             YRANGE=potRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=tMag[safe_i], $
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
     tInds       = (INDGEN(nTMarks)*N_ELEMENTS(safe_i))/(nTMarks-1)
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

  CTInds       = BYTSCL(tMag[safe_i])

  tRange            = [0,tDiff[safe_i[-1]]]
  errJRange         = jRange

  ;;Initialize things
  inds              = [0,1]
  tmpCurErr         = curErr[safe_i[inds]]

  plot_1            = ERRORPLOT((tDiff[safe_i[inds]]), $
                                cur[safe_i[inds]], $
                                tmpCurErr, $
                                XRANGE=tRange, $
                                ;; YRANGE=jRange, $
                                YRANGE=errJRange, $
                                XTITLE=timeTitle, $
                                YTITLE=curTitle, $
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
                                POSITION=p1pos, $
                                BUFFER=savePlot)

  ;;Now add all the other symbols
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     ;; tmpCurErr      = curErr[*,safe_i[inds]]
     tmpCurErr      = curErr[safe_i[inds]]

     plot_1         = ERRORPLOT((tDiff[safe_i[inds]]), $
                                cur[safe_i[inds]], $
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

  ;;The old, error bar–less way
  ;; plot_2            = SCATTERPLOT(tDiff[safe_i], $
  ;;                                 pot[safe_i], $
  ;;                                 ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
  ;;                                 YTITLE='$\Phi$ (V)', $
  ;;                                 RGB_TABLE=hammerCT, $
  ;;                                 MAGNITUDE=tMag[safe_i], $
  ;;                                 ;; LINESTYLE='', $
  ;;                                 SYMBOL='.', $
  ;;                                 SYM_SIZE=3.0, $
  ;;                                 /SYM_FILLED, $
  ;;                                 /CURRENT, $
  ;;                                 POSITION=p2pos)

  inds              = [0,1]
  tmpPotErr         = potErr[safe_i[inds]]

  errPotRange       = MINMAX(pot[safe_i])
  plot_2            = ERRORPLOT((tDiff[safe_i[inds]]), $
                                pot[safe_i[inds]], $
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
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     tmpPotErr      = potErr[safe_i[inds]]

     plot_2         = ERRORPLOT((tDiff[safe_i[inds]]), $
                                pot[safe_i[inds]], $
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


  ;;The old, error bar–less way
  ;; plot_3      = SCATTERPLOT(cur[safe_i], $
  ;;                           pot[safe_i], $
  ;;                           XRANGE=jRange, $
  ;;                           YRANGE=potRange, $
  ;;                           /YLOG, $
  ;;                           RGB_TABLE=hammerCT, $
  ;;                           MAGNITUDE=tMag[safe_i], $
  ;;                           SYMBOL=jvSym, $
  ;;                           SYM_SIZE=jvSymSize, $
  ;;                           SYM_THICK=jvSymThick, $
  ;;                           SYM_TRANSPARENCY=jvSymTransp, $
  ;;                           SYM_FILLED=jvSymFilled, $
  ;;                           YTICKVALUES=tickValues3, $
  ;;                           YTICKNAME=tickNames3, $
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

  ;; plot_1            = ERRORPLOT((tDiff[safe_i[inds]]), $
  ;;                               cur[safe_i[inds]], $
  ;;                               tmpErr, $
  ;;                               XRANGE=tRange, $
  ;;                               ;; YRANGE=jRange, $
  ;;                               YRANGE=errJRange, $
  ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[inds]]), $
  ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                               /CURRENT, $
  ;;                               POSITION=p1pos)

  inds              = [0,1]
  ;; tmpCurErr         = curErr[*,safe_i[inds]]
  ;; tmpPotErr         = potErr[*,safe_i[inds]]
  tmpCurErr         = curErr[safe_i[inds]]
  tmpPotErr         = potErr[safe_i[inds]]

  plot_3      = ERRORPLOT(cur[safe_i[inds]], $
                          pot[safe_i[inds]], $
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
                          YTICKVALUES=tickValues3, $
                          YTICKNAME=tickNames3, $
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
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     tmpCurErr      = curErr[safe_i[inds]]
     tmpPotErr      = potErr[safe_i[inds]]

     plot_3         = ERRORPLOT((cur[safe_i[inds]]), $
                                pot[safe_i[inds]], $
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


  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*N_ELEMENTS(safe_i))/(nTMarks-1)
  tickValues  = tMag[safe_i[tInds]]
  tickTimes   = time[safe_i[tInds]]
  tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
  tMagRange   = [tMag[safe_i[0]],tMag[safe_i[-1]]]
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

  STOP
  
END
