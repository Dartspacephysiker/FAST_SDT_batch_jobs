;;09/20/16
PRO JOURNAL__20160921__PLOT_PFLUX_WITH_SHADING_TO_INDICATE_MAGNITUDE__LEBESGUE

  COMPILE_OPT IDL2

  skip_to_map = 0
  no_mapJunk  = 1

  ;;Get the data back
    inDir  = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'
    inFile = '20160920--orb_6127--pFlux_package.sav'

    PRINT,'Restoring data package  ...'
    RESTORE,inDir+inFile

    stride = 1

    ;;Rainbow colors
    LOADCT,72
    TVLCT,r,g,b,/GET
    r = REVERSE(r)
    g = REVERSE(g)
    b = REVERSE(b)
    TVLCT,r,g,b


  magILAT  = magILAT[0:-1:stride]
  magMLT   = magMLT[0:-1:stride]
  magTJUL  = magTJUL[0:-1:stride]
  magTUTC  = magTUTC[0:-1:stride]
  PFluxB   = PFluxB[0:-1:stride]
  PFluxP   = PFluxP[0:-1:stride]


  dat            = pFluxB
  nDivisions     = 254
  ;; datMinMax      = [MIN(dat),MAX(dat)]
  ;; datRange       = datMinMax[1]-datMinMax[0]

  datMaxAmp      = ABS(MIN(dat)) > ABS(MAX(dat))
  datRange       = 2.*datMaxAmp

  datDelt        = (datRange)/FLOAT(nDivisions)

  ;; levels         = INDGEN(nDivisions+1)/FLOAT(nDivisions)*datRange+datMinMax[0]
  levels         = INDGEN(nDivisions+1)/FLOAT(nDivisions)*datRange-datMaxAmp
  posLevels      = levels[WHERE(levels GT 0,nPosLevels)]
  negLevels      = levels[WHERE(levels LT 0,nNegLevels)]

  posColorInds   = [(255-nPoslevels):255]

  nThisLevel     = MAKE_ARRAY(nDivisions,VALUE=0,/LONG)
  nThisPosLevel  = MAKE_ARRAY(nPosLevels,VALUE=0,/LONG)
  nThisNegLevel  = MAKE_ARRAY(nNegLevels,VALUE=0,/LONG)

  indThisLevel   = LIST()
  FOR k=0,nDivisions-1 DO BEGIN
     indThisLevel.Add,WHERE((dat GT levels[k]),nTmp)
     nThisLevel[k] = nTmp
  ENDFOR

  indThisPosLevel = LIST()
  FOR k=0,nPosLevels-1 DO BEGIN
     indThisPosLevel.Add,WHERE((dat GE posLevels[k]),nTmp)
     nThisPosLevel[k] = nTmp
  ENDFOR

  indThisNegLevel = LIST()
  FOR k=0,nNegLevels-1 DO BEGIN
     indThisNegLevel.Add,WHERE((dat LE negLevels[k]),nTmp)
     nThisNegLevel[k] = nTmp
  ENDFOR

  indZeroLevel = LIST()  
  indZeroLevel.Add,WHERE((dat GE MAX(negLevels)) AND (dat LE MIN(posLevels)),nZero)

  ;;Plot all these guys
  nPosTot      = TOTAL(nThisPosLevel)
  nNegTot      = TOTAL(nThisNegLevel)
  nTot         = nPosTot+nNegTot

  xPosTot      = MAKE_ARRAY(nPosTot,/FLOAT)
  yPosTot      = MAKE_ARRAY(nPosTot,/FLOAT)
  colorPosTot  = MAKE_ARRAY(nPosTot,/BYTE)

  xNegTot      = MAKE_ARRAY(nNegTot,/FLOAT)
  yNegTot      = MAKE_ARRAY(nNegTot,/FLOAT)
  colorNegTot  = MAKE_ARRAY(nNegTot,/BYTE)

  xTot         = MAKE_ARRAY(nTot,/FLOAT)
  yTot         = MAKE_ARRAY(nTot,/FLOAT)
  colorTot     = MAKE_ARRAY(nTot,/BYTE)

  curInd       = 0
  FOR k=0,nPosLevels-1 DO BEGIN
     IF nThisPosLevel[k] EQ 0 THEN CONTINUE


     x                     = magTUTC[indThisPosLevel[k]]-magTUTC[0]
     y                     = REPLICATE(posLevels[k],nThisPosLevel[k])
     colorInd              = 256-nPosLevels+k
     color                 = [r[colorInd],g[colorInd],b[colorInd]]

     tmpInds               = [curInd:(curInd+nThisPosLevel[k]-1)]
     xPosTot[tmpInds]      = x
     yPosTot[tmpInds]      = y
     colorPosTot[tmpInds]  = REPLICATE(colorInd,nThisPosLevel[k])     

     curInd               += nThisPosLevel[k]

  ENDFOR

  curInd       = 0
  FOR k=0,nNegLevels-1 DO BEGIN
     IF nThisNegLevel[k] EQ 0 THEN CONTINUE

     x                     = magTUTC[indThisNegLevel[k]]-magTUTC[0]
     y                     = REPLICATE(negLevels[k],nThisNegLevel[k])
     colorInd              = k
     color                 = [r[colorInd],g[colorInd],b[colorInd]]

     tmpInds               = [curInd:(curInd+nThisNegLevel[k]-1)]
     xNegTot[tmpInds]      = x
     yNegTot[tmpInds]      = y
     colorNegTot[tmpInds]  = REPLICATE(colorInd,nThisNegLevel[k])     

     curInd               += nThisNegLevel[k]

  ENDFOR

  xTot       = [TEMPORARY(xNegTot),TEMPORARY(xPosTot)]
  yTot       = [TEMPORARY(yNegTot),TEMPORARY(yPosTot)]
  colorTot   = [TEMPORARY(colorNegTot),TEMPORARY(colorPosTot)]

  scPlotArr  = SCATTERPLOT(xTot, $
                           yTot, $
                           ;; XRANGE=[MIN(x),MAX(x)], $
                           XRANGE=[MIN(magTUTC-magTUTC[0]),MAX(magTUTC-magTUTC[0])], $
                           ;; YRANGE=[MIN(posLevels),MAX(posLevels)], $
                           YRANGE=[MIN(levels),MAX(levels)], $
                           SYMBOL='Square', $
                           /SYM_FILLED, $
                           SYM_SIZE=0.3, $
                           RGB_TABLE=[TRANSPOSE(r),TRANSPOSE(g),TRANSPOSE(b)], $
                           MAGNITUDE=colorTot, $
                           ;; SYM_COLOR=color, $
                           OVERPLOT=havePlot, $
                           CURRENT=window)

  ;;Now the list of lists
  ;; masterPosList = LIST()
  ;; FOR k=0,nPosLevels-2 DO BEGIN

  ;;    tmpList = LIST()
  ;;    IF nThisPosLevel[k] EQ 0 THEN BEGIN
  ;;       ;; tmpList.Add,-1
  ;;       masterPosList.Add,tmpList
  ;;       CONTINUE
  ;;    ENDIF

  ;;    l1_i = indThisPosLevel[k]

  ;;    GET_STREAKS,l1_i, $
  ;;                START_I=start_ii, $
  ;;                STOP_I=stop_ii, $
  ;;                SINGLE_I=single_ii, $
  ;;                N_STREAKS=n_streaks, $
  ;;                /QUIET

  ;;    IF n_streaks GT 0 THEN BEGIN

  ;;       nPolygons = 0
  ;;       FOR kk=0,n_streaks-1 DO BEGIN
  ;;          polyFirst = l1_i[start_ii[kk]]
  ;;          polyLast  = l1_i[stop_ii[kk]]

  ;;          ;;Check level above
  ;;          IF nThisPosLevel[k+1] EQ 0 THEN CONTINUE ;This is a plateau

  ;;          l2_ii = WHERE( (indThisPosLevel[k+1] GE polyFirst) AND $
  ;;                         (indThisPosLevel[k+1] LE polyLast),nLevel2)

  ;;          IF nLevel2 EQ 0 THEN CONTINUE ;This particular polygon is a plateau

  ;;          l2_i     = (indThisPosLevel[k+1])[l2_ii]
  ;;          CASE nLevel2 OF
  ;;             1: BEGIN ;;Triangle thing
  ;;                ;;First column indexes 
  ;;                polygon = [[polyFirst,k],[polyLast,k], $
  ;;                           [l2_i,k+1], $
  ;;                           [polyFirst,k]]
  ;;             END
  ;;             ELSE: BEGIN ;;More than 1, so we've got a polygon

  ;;                testMin1 = MIN(ABS(polyFirst-l2_i),ii_1)
  ;;                testMin2 = MIN(ABS(polyLast-l2_i),ii_2)
                 
  ;;                IF ii_1 EQ ii_2 THEN STOP

  ;;                polygon = [[polyFirst,k],[polyLast,k], $
  ;;                           [l2_i[ii_2],k+1],[l2_i[ii_1],k+1], $
  ;;                           [polyFirst,k]]

  ;;             END
  ;;          ENDCASE
  ;;          tmpList.Add,polygon
  ;;          nPolygons++
  ;;       ENDFOR




  ;;    ENDIF ELSE BEGIN
  ;;       is_single_point = 1
  ;;    ENDELSE

  ;;    masterPosList.Add,tmpList
     

  ;; ENDFOR



  ;; STOP


  ;; IF ~KEYWORD_SET(skip_to_map) THEN BEGIN
  ;;    FA_FIELDS_BUFS,magTUTC,1024,DELTA_T=1.0e-5, $
  ;;                   BUF_STARTS=strt_i,BUF_ENDS=stop_i

  ;;    nBufs = N_ELEMENTS(strt_i)
  ;;    FOR k=0,nBufs-1 DO BEGIN
  ;;       dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])

  ;;       ;; y               = pfluxb[strt_i[k]:stop_i[k]:stride]
  ;;       ;; x               = magTUTC[strt_i[k]:stop_i[k]:stride]
  ;;       y               = pfluxb[strt_i[k]:stop_i[k]]
  ;;       x               = magTUTC[strt_i[k]:stop_i[k]]

  ;;       goodGuys        = WHERE(ABS(y) GT 0.001,nGood,COMPLEMENT=killZero,NCOMPLEMENT=nKill)

  ;;       IF nGood EQ 0 THEN CONTINUE

  ;;       ;;Get streak of zeros
  ;;       GET_STREAKS,killZero,START_I=ptStrt_i,STOP_I=ptStop_i,N_STREAKS=nPt, $
  ;;                   MIN_STREAK_TO_KEEP=5, $
  ;;                   /QUIET
  ;;       IF nPT GT 0 THEN BEGIN
  ;;          CASE killZero[ptStrt_i[-1]] OF
  ;;             0: BEGIN
  ;;                x            = x[killZero[ptStop_i[-1]]:-1]
  ;;                y            = y[killZero[ptStop_i[-1]]:-1]
  ;;             END
  ;;             ELSE: BEGIN
  ;;                x            = x[0:killZero[ptStrt_i[-1]]]
  ;;                y            = y[0:killZero[ptStrt_i[-1]]]
  ;;             END
  ;;          ENDCASE
  ;;       ENDIF

  ;;       xRange          = [MIN(x),MAX(x)]
  ;;       yRange          = [MIN(y),MAX(y)]
  ;;       xTickFormat     = 'LABEL_DATE'
  ;;       xTickUnits      = 'Time'

  ;;       window          = WINDOW(DIMENSIONS=[800,600])
  ;;       margin          = [0.12, 0.12, 0.12, 0.12]

  ;;       jPlot           = PLOT(x, $
  ;;                              y, $
  ;;                              ;; NAME='E-f', $
  ;;                              COLOR=BFieldCol, $
  ;;                              ;; SYMBOL='+', $
  ;;                              ;; LINESTYLE='', $
  ;;                              AXIS_STYLE=1, $
  ;;                              SYM_TRANSPARENCY=symTransp, $
  ;;                              ;; XTITLE='Time', $
  ;;                              ;; YTITLE='(mV/m)$^2$', $
  ;;                              YTITLE='Poynting Flux along B (mW/m$^2$)', $
  ;;                              XRANGE=xRange, $
  ;;                              YRANGE=yRange, $
  ;;                              ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
  ;;                              XTICKFORMAT=xTickFormat, $
  ;;                              XTICKUNITS=xTickUnits, $
  ;;                              MARGIN=margin, $
  ;;                              ;; /OVERPLOT, $
  ;;                              CURRENT=window)


  ;;       ;; polyPlot       = POLYGON([x,REVERSE(x)],[y,REPLICATE(0,N_ELEMENTS(y))], $
  ;;       ;;                          /DATA, $
  ;;       ;;                          /FILL_BACKGROUND, $
  ;;       ;;                          FILL_COLOR="light steel blue")
  ;;       ;; PATTERN_ORIENTATION=45, $
  ;;       ;; PATTERN_SPACING=4 $


  ;;       ;;Get color array for this sucker
  ;;       posTemp = BYTSCL(y,MIN=0,TOP=127)
  ;;       negTemp = BYTSCL(y,MAX=0,TOP=127)
  ;;       check   = WHERE(negTemp EQ 127,nCheck)
  ;;       ;; IF nCheck GT 0 THEN BEGIN
  ;;       ;;    negTemp[check] -= 
  ;;       negTemp -= 127S
  ;;       final   = BYTE(negTemp+posTemp+128)

  ;;       FOR kk=0,N_ELEMENTS(x)-2 DO BEGIN
           

  ;;          color   = [r[final[kk]],g[final[kk]],b[final[kk]]]
  ;;          tmpPolyX = [x[kk],x[kk+1],x[kk+1],x[kk]]
  ;;          tmpPolyY = [y[kk],y[kk+1],0,0]
  ;;          polyPlot       = POLYGON(tmpPolyX,tmpPolyY, $
  ;;                                   /DATA, $
  ;;                                   LINESTYLE=0, $
  ;;                                   /FILL_BACKGROUND, $
  ;;                                   COLOR=color, $
  ;;                                   FILL_COLOR=color)
  ;;          ;; )


  ;;       ENDFOR


  ;;       jPlot           = PLOT(x, $
  ;;                              y, $
  ;;                              ;; NAME='E-f', $
  ;;                              COLOR=BFieldCol, $
  ;;                              ;; SYMBOL='+', $
  ;;                              ;; LINESTYLE='', $
  ;;                              AXIS_STYLE=1, $
  ;;                              SYM_TRANSPARENCY=symTransp, $
  ;;                              ;; XTITLE='Time', $
  ;;                              ;; YTITLE='(mV/m)$^2$', $
  ;;                              YTITLE='Poynting Flux along B (mW/m$^2$)', $
  ;;                              XRANGE=xRange, $
  ;;                              YRANGE=yRange, $
  ;;                              ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
  ;;                              XTICKFORMAT=xTickFormat, $
  ;;                              XTICKUNITS=xTickUnits, $
  ;;                              MARGIN=margin, $
  ;;                              /OVERPLOT, $
  ;;                              CURRENT=window)

  ;;       STOP

  ;;    ENDFOR

  ;; ENDIF


  ;;   IF ~KEYWORD_SET(no_mapJunk) THEN BEGIN
  ;;      mapProj = 1
  ;;      SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
  ;;                                       HEMI='NORTH', $
  ;;                                       OUT_MAP=map, $
  ;;                                       INIT_MAP_PROJ_AND_RETURN=mapProj

  ;;      ;; SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
  ;;      ;;                                  HEMI='NORTH', $
  ;;      ;;                                  OUT_MAP=map, $
  ;;      ;;                                  OUTPLOTARR=outPlot

  ;;      coords = MAP_PROJ_FORWARD(magMLT*15.,magILAT,MAP_STRUCTURE=mapProj)

  ;;      ;;scale
  ;;      offset = MIN(coords,DIMENSION=2) 
  ;;      scales = MAX(coords,DIMENSION=2)-MIN(coords,DIMENSION=2)
  ;;      coordTry = coords

  ;;      tryCoord = 1

  ;;      coordTry[tryCoord,*] = coordTry[tryCoord,*] - offset[tryCoord]
  ;;      coordTry[tryCoord,*] /= scales[tryCoord]		      
  ;;      ;; coordTry[1,*] = coordTry[1,*] - offset[1]
  ;;      ;; coordTry[1,*] /= scales[1]		      

  ;;      pfboff   = pfluxb[0]
  ;;      pfbscale = MAX(pfluxb)-MIN(pFluxb)
  ;;      pfbsc    = (pfluxb-pfboff)/pfbscale*0.5

  ;;      ;;Now invert
  ;;      coordTry[tryCoord,*] += pfbsc
  ;;      coordTry[tryCoord,*] *= scales[tryCoord]
  ;;      coordTry[tryCoord,*] = coordTry[tryCoord,*] + offset[tryCoord]

  ;;      invCoords     = MAP_PROJ_INVERSE(coordTry,MAP_STRUCTURE=mapProj)

  ;;      SIMPLE_STEREOGRAPHIC_SCATTERPLOT,invCoords[0,*],invCoords[1,*], $
  ;;                                       HEMI='NORTH'

  ;;   ENDIF
  ;;   STOP

END

