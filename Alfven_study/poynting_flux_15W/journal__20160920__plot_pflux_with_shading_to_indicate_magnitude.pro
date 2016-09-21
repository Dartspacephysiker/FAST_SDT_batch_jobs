;;09/20/16
PRO JOURNAL__20160920__PLOT_PFLUX_WITH_SHADING_TO_INDICATE_MAGNITUDE

  COMPILE_OPT IDL2

  skip_to_map = 1

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


  magILAT  = magILAT[0:-1:stride]
  magMLT   = magMLT[0:-1:stride]
  magTJUL  = magTJUL[0:-1:stride]
  magTUTC  = magTUTC[0:-1:stride]
  PFluxB   = PFluxB[0:-1:stride]
  PFluxP   = PFluxP[0:-1:stride]



    IF ~KEYWORD_SET(skip_to_map) THEN BEGIN
       FA_FIELDS_BUFS,magTUTC,1024,DELTA_T=1.0e-5, $
                      BUF_STARTS=strt_i,BUF_ENDS=stop_i

       nBufs = N_ELEMENTS(strt_i)
       FOR k=0,nBufs-1 DO BEGIN
          dummy           = LABEL_DATE(DATE_FORMAT=['%I:%S%2'])

          ;; y               = pfluxb[strt_i[k]:stop_i[k]:stride]
          ;; x               = magTUTC[strt_i[k]:stop_i[k]:stride]
          y               = pfluxb[strt_i[k]:stop_i[k]]
          x               = magTUTC[strt_i[k]:stop_i[k]]

          goodGuys        = WHERE(ABS(y) GT 0.001,nGood,COMPLEMENT=killZero,NCOMPLEMENT=nKill)

          IF nGood EQ 0 THEN CONTINUE

          ;;Get streak of zeros
          GET_STREAKS,killZero,START_I=ptStrt_i,STOP_I=ptStop_i,N_STREAKS=nPt, $
                      MIN_STREAK_TO_KEEP=5, $
                      /QUIET
          IF nPT GT 0 THEN BEGIN
             CASE killZero[ptStrt_i[-1]] OF
                0: BEGIN
                   x            = x[killZero[ptStop_i[-1]]:-1]
                   y            = y[killZero[ptStop_i[-1]]:-1]
                END
                ELSE: BEGIN
                   x            = x[0:killZero[ptStrt_i[-1]]]
                   y            = y[0:killZero[ptStrt_i[-1]]]
                END
             ENDCASE
          ENDIF

          xRange          = [MIN(x),MAX(x)]
          yRange          = [MIN(y),MAX(y)]
          xTickFormat     = 'LABEL_DATE'
          xTickUnits      = 'Time'

          window          = WINDOW(DIMENSIONS=[800,600])
          margin          = [0.12, 0.12, 0.12, 0.12]

          jPlot           = PLOT(x, $
                                 y, $
                                 ;; NAME='E-f', $
                                 COLOR=BFieldCol, $
                                 ;; SYMBOL='+', $
                                 ;; LINESTYLE='', $
                                 AXIS_STYLE=1, $
                                 SYM_TRANSPARENCY=symTransp, $
                                 ;; XTITLE='Time', $
                                 ;; YTITLE='(mV/m)$^2$', $
                                 YTITLE='Poynting Flux along B (mW/m$^2$)', $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
                                 XTICKFORMAT=xTickFormat, $
                                 XTICKUNITS=xTickUnits, $
                                 MARGIN=margin, $
                                 ;; /OVERPLOT, $
                                 CURRENT=window)


          ;; polyPlot       = POLYGON([x,REVERSE(x)],[y,REPLICATE(0,N_ELEMENTS(y))], $
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

          FOR kk=0,N_ELEMENTS(x)-2 DO BEGIN
             

             color   = [r[final[kk]],g[final[kk]],b[final[kk]]]
             tmpPolyX = [x[kk],x[kk+1],x[kk+1],x[kk]]
             tmpPolyY = [y[kk],y[kk+1],0,0]
             polyPlot       = POLYGON(tmpPolyX,tmpPolyY, $
                                      /DATA, $
                                      LINESTYLE=0, $
                                      /FILL_BACKGROUND, $
                                      COLOR=color, $
                                      FILL_COLOR=color)
             ;; )


          ENDFOR


          jPlot           = PLOT(x, $
                                 y, $
                                 ;; NAME='E-f', $
                                 COLOR=BFieldCol, $
                                 ;; SYMBOL='+', $
                                 ;; LINESTYLE='', $
                                 AXIS_STYLE=1, $
                                 SYM_TRANSPARENCY=symTransp, $
                                 ;; XTITLE='Time', $
                                 ;; YTITLE='(mV/m)$^2$', $
                                 YTITLE='Poynting Flux along B (mW/m$^2$)', $
                                 XRANGE=xRange, $
                                 YRANGE=yRange, $
                                 ;; YRANGE=[MIN(sjTemp[winAlf_i]),MAX(sjTemp[winAlf_i])], $
                                 XTICKFORMAT=xTickFormat, $
                                 XTICKUNITS=xTickUnits, $
                                 MARGIN=margin, $
                                 /OVERPLOT, $
                                 CURRENT=window)

          STOP

       ENDFOR

    ENDIF


    mapProj = 1
    SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
                                     HEMI='NORTH', $
                                     OUT_MAP=map, $
                                     INIT_MAP_PROJ_AND_RETURN=mapProj

    ;; SIMPLE_STEREOGRAPHIC_SCATTERPLOT,magmlt*15.,magilat, $
    ;;                                  HEMI='NORTH', $
    ;;                                  OUT_MAP=map, $
    ;;                                  OUTPLOTARR=outPlot

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
    pfbsc    = (pfluxb-pfboff)/pfbscale*0.5

    ;;Now invert
    coordTry[tryCoord,*] += pfbsc
    coordTry[tryCoord,*] *= scales[tryCoord]
    coordTry[tryCoord,*] = coordTry[tryCoord,*] + offset[tryCoord]

    invCoords     = MAP_PROJ_INVERSE(coordTry,MAP_STRUCTURE=mapProj)

    SIMPLE_STEREOGRAPHIC_SCATTERPLOT,invCoords[0,*],invCoords[1,*], $
                                     HEMI='NORTH'


    STOP

END

