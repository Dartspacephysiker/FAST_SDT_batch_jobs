;;09/21/16
PRO JOURNAL__20160921__DO_PLOT_WITH_PFLUX_ALONG_FAST_TRACK__POLAR

  COMPILE_OPT IDL2

  skip_to_map = 0

  filter_zeros = 0

  ;;Get the data back
  inDir  = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'
  inFile = '20160920--orb_6127--pFlux_package.sav'

  PRINT,'Restoring data package  ...'
  RESTORE,inDir+inFile

  stride    = 100
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


  pfHeightFactor = 1.0


  magILAT  = magILAT[0:-1:stride]
  magMLT   = magMLT[0:-1:stride]
  magTJUL  = magTJUL[0:-1:stride]
  magTUTC  = magTUTC[0:-1:stride]
  PFluxB   = PFluxB[0:-1:stride]
  PFluxP   = PFluxP[0:-1:stride]

  streakLen = 1024/stride

  window   = WINDOW(DIMENSIONS=[800,600])

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

  tryCoord = 1
  coordTry = coords

  ;; coordTry[tryCoord,*] = coordTry[tryCoord,*] - offset[tryCoord]
  ;; coordTry[tryCoord,*] /= scales[tryCoord]		      
  ;; coordTry[1,*] = coordTry[1,*] - offset[1]
  ;; coordTry[1,*] /= scales[1]		      

  pfboff   = pfluxb[0]
  pfbscale = MAX(pfluxb)-MIN(pFluxb)
  pfbsc    = (pfluxb-pfboff)/pfbscale*pfHeightFactor

  ;;Now invert
  coordTry[tryCoord,*] += pfbsc
  ;; coordTry[tryCoord,*] *= scales[tryCoord]
  ;; coordTry[tryCoord,*] = coordTry[tryCoord,*] + offset[tryCoord]

  ;; invCoords     = MAP_PROJ_INVERSE(coordTry,MAP_STRUCTURE=mapProj)


  tmpI      = INDGEN(N_ELEMENTS(magTUTC))

  ;; times     = magTUTC[tmpI]-magTUTC[0]
  ;; angle     = ATAN(coords[1,tmpI],coords[0,tmpI])
  ;; mag       = SQRT((coords[0,tmpI])^2+(coords[1,tmpI])^2)

  ;; angleTmp  = ATAN(coordTry[1,tmpI],coordTry[0,tmpI])
  ;; magTmp    = SQRT((coordTry[0,tmpI])^2+(coordTry[1,tmpI])^2)


  nVMid     = 2
  nHMid     = 2
  vSpace    = 1./(nVMid+1)
  hSpace    = 1./(nHMid+1)
  vScaleVec = INDGEN(2+nVMid)/FLOAT(2+nVMid)
  hScaleVec = INDGEN(2+nHMid)/FLOAT(2+nHMid)

  
  nPFluxB   = N_ELEMENTS(pFluxB)

  diffVecs  = [coordTry[0,*]-coords[0,*],coordTry[1,*]-coords[1,*]]
  diffX     = coords[0,[1:nPfluxB-1]] - coords[0,[0:nPFluxB-2]]
  diffY     = coords[1,[1:nPfluxB-1]] - coords[1,[0:nPFluxB-2]]
  ;;For each pFlux measurement there is one polygon
  nPtsTot = (nVMid+nHMid)*2+4
  ;; polyVerts = MAKE_ARRAY(nPointsPerSide,nPointsPerSide,nPFluxB)
  polyVerts = MAKE_ARRAY(nPtsTot+1,2,nPFluxB-1) ;;One extra to seal the deal

  ;;Vertices
  PRINT,'Vertices'
  PRINT,0,nHMid+1,nHMid+1+nVMid+1,(nHMid+1)*2+nVMid+1,nPtsTot

  polyVerts[0,*,*] = coords[*,[0:nPFluxB-2]]
  ;; polyVerts[nHMid+1,*,*] = [coords[0,[1:nPFluxB-1]],coords[1,[1:nPFluxB-1]]]
  polyVerts[nHMid+1,*,*] = coords[*,[1:nPFluxB-1]]
  ;; polyVerts[nHMid+1+nVMid+1,*,*] = [coordTry[0,[1:nPFluxB-1]],coordTry[1,[1:nPFluxB-1]]]
  polyVerts[nHMid+1+nVMid+1,*,*] = coordTry[*,[1:nPFluxB-1]]
  ;; polyVerts[(nHMid+1)*2+nVMid+1,*,*] = [coordTry[0,[1:nPFluxB-1]],coordTry[1,[1:nPFluxB-1]]]
  polyVerts[(nHMid+1)*2+nVMid+1,*,*] = coordTry[*,[0:nPFluxB-2]]

  polyVerts[nPtsTot,*,*]             = coords[*,[0:nPFluxB-2]]

  ;;First and last horizontal row (and last
  PRINT,'Now horizontals'
  FOR k=1,nHMid DO BEGIN

     k2  = nPtsTot-nVMid-k-1

     PRINT,k
     PRINT,k2

     polyVerts[k,*,*] = [coords[0,[0:(nPFluxB-2)]]+hScaleVec[k]*diffX, $
                         coords[1,[0:(nPFluxB-2)]]+hScaleVec[k]*diffY]

     ;; polyVerts[k,*,*] = coords[*,[0:(nPFluxB-2)]]+hScaleVec[k]*coords[*,[1:(nPFluxB-1)]]

     polyVerts[k2,*,*] = [coordTry[0,[0:(nPFluxB-2)]]+hScaleVec[k]*diffX, $
                          coordTry[1,[0:(nPFluxB-2)]]+hScaleVec[k]*diffY]

     ;; polyVerts[nPtsTot-nVMid-2-k,*,*] = coordTry[*,[0:(nPFluxB-2)]]+hScaleVec[k]*coordTry[*,[1:(nPFluxB-1)]]

  ENDFOR
  ;; polyVerts[[1:(nHMid)],[1:(nHMid)],*] = [


  ;;Left and right vertical rows
  PRINT,'Now verticals'
  FOR k=nHMid+2,(nHMid+1+nVMid) DO BEGIN

     vSCnt = k-nHMid-1

     k2 = nPtsTot-vSCnt

     PRINT,k
     PRINT,k2

     ;;Handle the horizontals, which are fixed along polygon columns
     polyVerts[k,0,*]  = polyVerts[nHMid+1,0,*]
     polyVerts[k2,0,*] = polyVerts[0,0,*]

     ;;Now the verticals, which have to be scaled ever so carefully
     polyVerts[k,1,*]  = polyVerts[nHMid+1,1,*]+vScaleVec[vSCnt]*diffVecs[1,[0:nPFluxB-2]]

     polyVerts[k2,1,*] = polyVerts[nPtsTot,1,*]+vScaleVec[vSCnt]*diffVecs[1,[0:nPFluxB-2]]

     ;; polyVerts[nPtsTot/2+k,0,*] = polyVerts[0,0,*]

  ENDFOR


  ;; tmpDat = TRANSPOSE(polyverts[*,*,11])
  ;; this = PLOT(tmpDat, $
  ;;             LINESTYLE='', $
  ;;             SYMBOL='*', $
  ;;             XRANGE=[MIN(tmpDat[0,*]),max(tmpDat[0,*])], $
  ;;             YRANGE=[MIN(tmpDat[1,*]),max(tmpDat[1,*])])

  that = PLOT(coords, $
              NAME='FAST track', $
              ;; XRANGE=[-1,1], $
              ;; YRANGE=[-1,1])
              XRANGE=[0,1], $
              YRANGE=[0,1], $
              /OVERPLOT)

  ;; legend = LEGEND(TARGET=[REVERSE(plotArr[*]),that], $
  ;;                 POSITION=[0.28,0.88])
  legend = LEGEND(TARGET=that, $
                  POSITION=[0.28,0.88])


  FA_FIELDS_BUFS,magTUTC,streakLen,DELTA_T=1.0e-5, $
                 BUF_STARTS=strt_i,BUF_ENDS=stop_i

  sRates = 1./(magTUTC[(strt_i)+1]-magTUTC[strt_i])

  ;;Byte-scaled Poynting flux
  posTemp = BYTSCL(pFluxB,MIN=0,TOP=127)
  negTemp = BYTSCL(pFluxB,MAX=0,TOP=127)
  check   = WHERE(negTemp EQ 127,nCheck)
  negTemp -= 127S
  final   = BYTE(TEMPORARY(negTemp)+TEMPORARY(posTemp)+128)
  

  nBufs = N_ELEMENTS(strt_i)
  FOR k=0,nBufs-1 DO BEGIN
     ;; dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])

     tmpI            = [strt_i[k]:stop_i[k]]
     pFluxTmp        = pFluxB[tmpI]

     IF (sRates[k] GE 1./(FlOAT(stride)*minPeriod ) ) OR $
        (sRates[k] LE 1./(FLOAT(stride)*maxPeriod ) ) THEN CONTINUE


     goodGuys        = WHERE(ABS(pFluxTmp) GT 0.05,nGood, $
                             COMPLEMENT=killZero,NCOMPLEMENT=nKill)

     IF nGood EQ 0 THEN CONTINUE

     ;;Get streak of zeros
     IF (killZero[0] NE -1) AND KEYWORD_SET(filter_zeros) THEN BEGIN
        GET_STREAKS,killZero,START_I=ptStrt_i,STOP_I=ptStop_i,N_STREAKS=nPt, $
                    MIN_STREAK_TO_KEEP=minStreak, $
                    /QUIET
        IF nPT GT 0 THEN BEGIN
           CASE killZero[ptStrt_i[-1]] OF
              0: BEGIN
                 tmp_ii  = [killZero[ptStop_i[-1]]:-1]
              END
              ELSE: BEGIN
                 tmp_ii  = [0:killZero[ptStrt_i[-1]]]
              END
           ENDCASE
           tmpI          = tmpI[tmp_ii]
        ENDIF
     ENDIF

     y                   = pfluxb[tmpI]
     mltMod              = coords[0,tmpI]
     ilatMod             = coords[1,tmpI]
     mlt                 = magMLT[tmpI]*15.
     ilat                = magILAT[tmpI]


     ;;Get color array for this sucker
     ;; posTemp = BYTSCL(y,MIN=0,TOP=127)
     ;; negTemp = BYTSCL(y,MAX=0,TOP=127)
     ;; check   = WHERE(negTemp EQ 127,nCheck)
     ;; IF nCheck GT 0 THEN BEGIN
     ;;    negTemp[check] -= 
     ;; negTemp -= 127S
     colorInd            = final[tmpI]

     FOR kk=0,N_ELEMENTS(mltMod)-2 DO BEGIN
        

        color   = [r[colorInd[kk]],g[colorInd[kk]],b[colorInd[kk]]]

        tmpPolyX       = REFORM(polyVerts[*,0,tmpI[kk]])
        tmpPolyY       = REFORM(polyVerts[*,1,tmpI[kk]])

        ;; PRINT,tmpPolyX
        ;; PRINT,tmpPolyY

        ;; IF kk EQ 13 THEN BEGIN
        ;;    window2 = WINDOW()
        ;;    ;; FOR lll=0,N_ELEMENTS(tmpPolyX)-2,2 DO BEGIN
        ;;    ;;    PRINT,lll
        ;;    ;;    coulouir = (GENERATE_LIST_OF_RANDOM_COLORS(1))[0]
        ;;    ;; ;;    ;; plot = PLOT(tmpPolyX[lll:lll+1]-MIN(tmpPolyX), $
        ;;    ;; ;;    ;; plot = PLOT(INDGEN, $
        ;;    ;;    plot = PLOT(tmpPolyY[lll:lll+1], $
        ;;    ;;                SYMBOL='*', $
        ;;    ;;                ;; LINESTYLE='', $
        ;;    ;;                COLOR=coulouir, $
        ;;    ;;                SYM_COLOR=coulouir, $
        ;;    ;;                ;; XRANGE=[MIN(tmpPolyX),MAX(tmpPolyX)], $
        ;;    ;;                YRANGE=[MIN(tmpPolyY),MAX(tmpPolyY)], $
        ;;    ;;                CURRENT=window2)
        ;;    ;; ENDFOR
        ;;       plot = SCATTERPLOT(tmpPolyX,tmpPolyY, $
        ;;                   SYMBOL='*', $
        ;;                          CLIP=0, $
        ;;                   ;; LINESTYLE='', $
        ;;                   ;; COLOR=coulouir, $
        ;;                   SYM_COLOR=coulouir, $
        ;;                   ;; XRANGE=[MIN(tmpPolyX),MAX(tmpPolyX)], $
        ;;                   ;; YRANGE=[MIN(tmpPolyY),MAX(tmpPolyY)], $
        ;;                   CURRENT=window2)

        ;; ENDIF
        polyPlot       = POLYGON(tmpPolyX,tmpPolyY, $
                                 /DATA, $
                                 LINESTYLE=0, $
                                 /FILL_BACKGROUND, $
                                 COLOR=color, $
                                 FILL_COLOR=color)
        ;; )


     ENDFOR


     ;; symTransp       = 80
     ;; jPlot           = SCATTERPLOT(mltMod, $
     ;;                               ilatMod, $
     ;;                               ;; NAME='E-f', $
     ;;                               ;; COLOR=BFieldCol, $
     ;;                               SYMBOL='+', $
     ;;                               ;; LINESTYLE='', $
     ;;                               ;; AXIS_STYLE=1, $
     ;;                               SYM_TRANSPARENCY=symTransp, $
     ;;                               ;; XTITLE='Time', $
     ;;                               ;; YTITLE='(mV/m)$^2$', $
     ;;                               ;; YTITLE='Poynting Flux along B (mW/m$^2$)', $
     ;;                               ;; XRANGE=xRange, $
     ;;                               ;; YRANGE=yRange, $
     ;;                               ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
     ;;                               ;; XTICKFORMAT=xTickFormat, $
     ;;                               ;; XTICKUNITS=xTickUnits, $
     ;;                               ;; MARGIN=margin, $
     ;;                               /OVERPLOT, $
     ;;                               CURRENT=window)


     ;; jPlot.rgb_table = [[r[colorInd]],[g[colorInd]],[b[colorInd]]]
     ;; jPlot.magnitude = pFluxB[tmpI]


  ENDFOR

  STOP


END
