;;09/20/16
;;Not quite ...
PRO JOURNAL__20160920__PLOT_SHADED_PFLUX_ALONG_FAST_TRACK

  COMPILE_OPT IDL2

  skip_to_map = 0

  ;;Get the data back
  inDir  = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/'
  inFile = '20160920--orb_6127--pFlux_package.sav'

  PRINT,'Restoring data package  ...'
  RESTORE,inDir+inFile

  stride = 100

  ;;Rainbow colors
  LOADCT,70
  TVLCT,r,g,b,/GET
  r = REVERSE(r)
  g = REVERSE(g)
  b = REVERSE(b)
  TVLCT,r,g,b


  pfHeightFactor = 3.


  magILAT  = magILAT[0:-1:stride]
  magMLT   = magMLT[0:-1:stride]
  magTJUL  = magTJUL[0:-1:stride]
  magTUTC  = magTUTC[0:-1:stride]
  PFluxB   = PFluxB[0:-1:stride]
  PFluxP   = PFluxP[0:-1:stride]

  streakLen = 1024/stride

  ;;Scale the coordinates

  mapProj = 1
  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
                                   HEMI='NORTH', $
                                   OUT_MAP=map, $
                                   INIT_MAP_PROJ_AND_RETURN=mapProj

  coords = MAP_PROJ_FORWARD(magMLT*15.,magILAT,MAP_STRUCTURE=mapProj)

  ;;scale
  offset = MIN(coords,DIMENSION=2) 
  scales = MAX(coords,DIMENSION=2)-MIN(coords,DIMENSION=2)
  coordTry = coords

  tryCoord = 1

  coordTry[tryCoord,*] = coordTry[tryCoord,*] - offset[tryCoord]
  coordTry[tryCoord,*] /= scales[tryCoord]		      
  ;; coordTry[1,*] = coordTry[1,*] - offset[1]
  ;; coordTry[1,*] /= scales[1]		      

  pfboff   = pfluxb[0]
  pfbscale = MAX(pfluxb)-MIN(pFluxb)
  pfbsc    = (pfluxb-pfboff)/pfbscale*pfHeightFactor

  ;;Now invert
  coordTry[tryCoord,*] += pfbsc
  coordTry[tryCoord,*] *= scales[tryCoord]
  coordTry[tryCoord,*] = coordTry[tryCoord,*] + offset[tryCoord]

  invCoords     = MAP_PROJ_INVERSE(coordTry,MAP_STRUCTURE=mapProj)


  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
  ;; SIMPLE_STEREOGRAPHIC_SCATTERPLOT,invCoords[0,*],invCoords[1,*], $
                                   HEMI='NORTH', $
                                   THICK=0.75, $
                                   OUT_MAP=map, $
                                   OUTPLOTARR=outPlot, $
                                   /NO_SYMBOL




  IF ~KEYWORD_SET(skip_to_map) THEN BEGIN
     FA_FIELDS_BUFS,magTUTC,streakLen,DELTA_T=1.0e-5, $
                    BUF_STARTS=strt_i,BUF_ENDS=stop_i

     nBufs = N_ELEMENTS(strt_i)
     FOR k=0,nBufs-1 DO BEGIN
        ;; dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])

        tmpI            = [strt_i[k]:stop_i[k]]
        ilatTmp         = invCoords[1,tmpI]

        goodGuys        = WHERE(ABS(ilatTmp) GT 0.001,nGood, $
                                COMPLEMENT=killZero,NCOMPLEMENT=nKill)

        IF nGood EQ 0 THEN CONTINUE

        ;;Get streak of zeros
        IF killZero[0] NE -1 THEN BEGIN
           GET_STREAKS,killZero,START_I=ptStrt_i,STOP_I=ptStop_i,N_STREAKS=nPt, $
                       MIN_STREAK_TO_KEEP=50/stride, $
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
        mltMod              = invCoords[0,tmpI]
        ilatMod             = invCoords[1,tmpI]
        mlt                 = magMLT[tmpI]*15.
        ilat                = magILAT[tmpI]

        ;; xRange          = [MIN(mltMod),MAX(mltMod)]
        ;; yRange          = [MIN(ilatMod),MAX(ilatMod)]
        ;; xTickFormat     = 'LABEL_DATE'
        ;; xTickUnits      = 'Time'

        ;; window          = WINDOW(DIMENSIONS=[800,600])
        ;; margin          = [0.12, 0.12, 0.12, 0.12]

        ;; jPlot           = PLOT(mltMod, $
        ;;                        ilatMod, $
        ;;                        ;; NAME='E-f', $
        ;;                        COLOR=BFieldCol, $
        ;;                        ;; SYMBOL='+', $
        ;;                        ;; LINESTYLE='', $
        ;;                        AXIS_STYLE=1, $
        ;;                        SYM_TRANSPARENCY=symTransp, $
        ;;                        ;; XTITLE='Time', $
        ;;                        ;; YTITLE='(mV/m)$^2$', $
        ;;                        YTITLE='Poynting Flux along B (mW/m$^2$)', $
        ;;                        XRANGE=xRange, $
        ;;                        YRANGE=yRange, $
        ;;                        ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
        ;;                        XTICKFORMAT=xTickFormat, $
        ;;                        XTICKUNITS=xTickUnits, $
        ;;                        MARGIN=margin, $
        ;;                        ;; /OVERPLOT, $
        ;;                        CURRENT=window)


        ;; polyPlot       = POLYGON([mltMod,REVERSE(mltMod)],[ilatMod,REPLICATE(0,N_ELEMENTS(ilatMod))], $
        ;;                          /DATA, $
        ;;                          /FILL_BACKGROUND, $
        ;;                          FILL_COLOR="light steel blue")
        ;; PATTERN_ORIENTATION=45, $
        ;; PATTERN_SPACING=4 $


        ;;Get color array for this sucker
        posTemp = BYTSCL(y,MIN=0,TOP=127)
        negTemp = BYTSCL(y,MAX=0,TOP=127)
        check   = WHERE(negTemp EQ 127,nCheck)
        ;; IF nCheck GT 0 THEN BEGIN
        ;;    negTemp[check] -= 
        negTemp -= 127S
        final   = BYTE(negTemp+posTemp+128)

        FOR kk=0,N_ELEMENTS(mltMod)-2 DO BEGIN
           

           color   = [r[final[kk]],g[final[kk]],b[final[kk]]]
           ;; tmpPolyX = [mltMod[kk],mltMod[kk+1],mlt[kk+1],mlt[kk]]
           ;; tmpPolyY = [ilatMod[kk],ilatMod[kk+1],ilat[kk+1],ilat[kk]]
           tmpPolyX = [mltMod[kk],MEAN(mltMod[kk:(kk+1)]), $
                       mltMod[kk+1],MEAN([mltMod[kk+1],mlt[kk+1]]), $
                       mlt[kk+1],MEAN(mlt[(kk+1):kk:-1]), $
                       mlt[kk]]
           tmpPolyY = [ilatMod[kk],MEAN(ilatMod[kk:(kk+1)]), $
                       ilatMod[kk+1],MEAN([ilatMod[kk+1],ilat[kk+1]]), $
                       ilat[kk+1],MEAN(ilat[(kk+1):kk:-1]), $
                       ilat[kk]]
           ;; tmpPolyY = [ilatMod[kk],ilatMod[kk+1],ilat[kk+1],ilat[kk]]

           ;; datPlot        = SCATTERPLOT(tmpPolyX,tmpPolyY, $
           ;;                              /OVERPLOT, $
           ;;                              SYM_COLOR='Red')

           polyPlot       = POLYGON(tmpPolyX,tmpPolyY, $
                                    /DATA, $
                                    LINESTYLE=0, $
                                    /FILL_BACKGROUND, $
                                    COLOR=color, $
                                    FILL_COLOR=color)
           ;; )


        ENDFOR


        symTransp       = 80
        jPlot           = SCATTERPLOT(mltMod, $
                                      ilatMod, $
                                      ;; NAME='E-f', $
                                      ;; COLOR=BFieldCol, $
                                      SYMBOL='+', $
                                      ;; LINESTYLE='', $
                                      ;; AXIS_STYLE=1, $
                                      SYM_TRANSPARENCY=symTransp, $
                                      ;; XTITLE='Time', $
                                      ;; YTITLE='(mV/m)$^2$', $
                                      ;; YTITLE='Poynting Flux along B (mW/m$^2$)', $
                                      ;; XRANGE=xRange, $
                                      ;; YRANGE=yRange, $
                                      ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
                                      ;; XTICKFORMAT=xTickFormat, $
                                      ;; XTICKUNITS=xTickUnits, $
                                      ;; MARGIN=margin, $
                                      /OVERPLOT, $
                                      CURRENT=window)


        jPlot.rgb_table = [[r[final]],[g[final]],[b[final]]]
        jPlot.magnitude = pFluxB[tmpI]


     ENDFOR

  ENDIF

  STOP


  tmpI      = [strt_i[k]:stop_i[k]]
  tmpI      = INDGEN(N_ELEMENTS(magTUTC))

  times     = magTUTC[tmpI]-magTUTC[0]
  angle     = ATAN(coords[1,tmpI],coords[0,tmpI])
  mag       = SQRT((coords[0,tmpI])^2+(coords[1,tmpI])^2)

  angleTmp  = ATAN(coordTry[1,tmpI],coordTry[0,tmpI])
  magTmp    = SQRT((coordTry[0,tmpI])^2+(coordTry[1,tmpI])^2)

  anglePlot = PLOT(times,angle)
  magPlot   = PLOT(times,mag)

  polar     = POLARPLOT(mag,angle)

  polar2    = POLARPLOT(magTmp,angleTmp,LINESTYLE='',SYMBOL='*',/OVERPLOT)

  FOR jj=0,N_ELEMENTS(magTmp)-2 DO BEGIN
           color   = [r[final[jj]],g[final[jj]],b[final[jj]]]
           tmpPolyX = [x[jj],x[jj+1],x[jj+1],x[jj]]
           tmpPolyY = [y[jj],y[jj+1],ilat[jj+1],ilat[jj]]

        ;; polyPlot       = POLYGON([x,REVERSE(x)],[y,REPLICATE(0,N_ELEMENTS(y))], $
        ;;                          /DATA, $
        ;;                          /FILL_BACKGROUND, $
        ;;                          FILL_COLOR="light steel blue")
  ENDFOR

END

