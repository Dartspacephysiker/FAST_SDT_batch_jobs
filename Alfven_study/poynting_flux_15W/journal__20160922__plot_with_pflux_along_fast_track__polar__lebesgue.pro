;;09/21/16
PRO JOURNAL__20160922__PLOT_WITH_PFLUX_ALONG_FAST_TRACK__POLAR__LEBESGUE

  COMPILE_OPT IDL2

  skip_to_map = 0

  filter_zeros = 0

  orbStr = '6127'

  ;;Get the data back
  inDir  = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'
  inFile = '20160920--orb_6127--pFlux_package.sav'

  PRINT,'Restoring data package  ...'
  RESTORE,inDir+inFile

  stride    = 10
  minStreak = CEIL(50/FLOAT(stride)) > 3

  ;;Freq. restrictions
  maxPeriod      = 1/100.
  minPeriod      = 1/132.

  ;;Rainbow colors
  LOADCT,72
  TVLCT,r,g,b,/GET
  r = REVERSE(r)
  g = REVERSE(g)
  b = REVERSE(b)
  TVLCT,r,g,b


  pfHeightFactor = 0.8


  magILAT  = magILAT[0:-1:stride]
  magMLT   = magMLT[0:-1:stride]
  magTJUL  = magTJUL[0:-1:stride]
  magTUTC  = magTUTC[0:-1:stride]
  PFluxB   = PFluxB[0:-1:stride]
  PFluxP   = PFluxP[0:-1:stride]

  streakLen = 1024/stride

  window   = WINDOW(DIMENSIONS=[800,800])

  ;;Set up map w/ plot, get scales and offsets
  JOURNAL__20160921__TEST_MAP_COORD_CONVERSION, $
     PLOTARR=plotArr, $
     MAP_STRUCTURE=mapProj, $
     CARTESIAN_COORDS__CIRCLES=convArr, $
     /SCALE_CARTESIANS, $
     CARTESIAN_OFFSETS=offset, $
     CARTESIAN_SCALES=scales, $
     /SUPPRESS_LEGEND, $
     CURRENT=window

  coords  = MAP_PROJ_FORWARD(magMLT*15.,magILAT,MAP_STRUCTURE=mapProj)

  ;;scale coords
  offset = N_ELEMENTS(offset) GT 0 ? offset : MIN(coords,DIMENSION=2) 
  scales  = N_ELEMENTS(scales) GT 0 ? scales : MAX(coords,DIMENSION=2)-MIN(coords,DIMENSION=2)

  FOR k=0,1 DO BEGIN
     coords[k,*] -= offset[k]
     coords[k,*] /= scales[k]
  ENDFOR

  pfboff   = pfluxb[0]
  pfbscale = MAX(pfluxb)-MIN(pFluxb)
  ;; pfbsc    = (pfluxb-pfboff)/pfbscale*pfHeightFactor
  pfbsc    = (pfluxb)/pfbscale*pfHeightFactor


  tmpI              = INDGEN(N_ELEMENTS(magTUTC))

  nPFluxB           = N_ELEMENTS(pFluxB)


  ;; dat            = pFluxB
  dat            = pfbsc
  nDivisions     = 254
  ;; datMinMax      = [MIN(dat),MAX(dat)]
  ;; datRange       = datMinMax[1]-datMinMax[0]

  datMaxAmp      = ABS(MIN(pfbsc)) > ABS(MAX(pfbsc))
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


     ;; x                     = magTUTC[indThisPosLevel[k]]-magTUTC[0]
     ;; y                     = REPLICATE(posLevels[k],nThisPosLevel[k])

     x                     = coords[0,indThisPosLevel[k]]
     y                     = REPLICATE(posLevels[k],nThisPosLevel[k])+ $
                             coords[1,indThisPosLevel[k]]
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

     ;; x                     = magTUTC[indThisNegLevel[k]]-magTUTC[0]
     ;; y                     = REPLICATE(negLevels[k],nThisNegLevel[k])

     x                     = coords[0,indThisNegLevel[k]]
     y                     = REPLICATE(negLevels[k],nThisNegLevel[k])+ $
                             coords[1,indThisNegLevel[k]]
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

  that = PLOT(coords, $
              NAME='Orbit' + orbStr, $
              ;; XRANGE=[-1,1], $
              ;; YRANGE=[-1,1])
              XRANGE=[0,1], $
              YRANGE=[0,1], $
              /OVERPLOT)

  scPlot  = SCATTERPLOT(xTot, $
                        yTot, $
                        ;; XRANGE=[MIN(x),MAX(x)], $
                        ;; XRANGE=[MIN(magTUTC-magTUTC[0]),MAX(magTUTC-magTUTC[0])], $
                        ;; YRANGE=[MIN(posLevels),MAX(posLevels)], $
                        ;; YRANGE=[MIN(levels),MAX(levels)], $
                        SYMBOL='VLine', $
                        /SYM_FILLED, $
                        SYM_SIZE=1.0, $
                        RGB_TABLE=[TRANSPOSE(r),TRANSPOSE(g),TRANSPOSE(b)], $
                        MAGNITUDE=colorTot, $
                        ;; SYM_COLOR=color, $
                        /OVERPLOT, $
                        CURRENT=window)

  ;; legend = LEGEND(TARGET=[REVERSE(plotArr[*]),that], $
  ;;                 POSITION=[0.28,0.88])
  legend = LEGEND(TARGET=that, $
                  POSITION=[0.28,0.88])


  c = COLORBAR(ORIENTATION=0, $
               POSITION=[0.05,0.05,0.95,0.10], $
               RGB_TABLE=[TRANSPOSE(r),TRANSPOSE(g),TRANSPOSE(b)], $
               RANGE=(ABS(MIN(pFluxB)) > ABS(MAX(pFluxB))) * [-1.,1], $
               TITLE='Poynting Flux along B (mW/m$^2$)')
  
  STOP


END

