;;09/21/16
;;The conclusion is that when using a stereographic polar plot, I can pretty much swap out for polar coords
PRO JOURNAL__20160921__TEST_MAP_COORD_CONVERSION, $
   PLOTARR=plotArr, $
   MAP_STRUCTURE=mapProj, $
   CARTESIAN_COORDS__CIRCLES=convArr, $
   SCALE_CARTESIANS=scale_Cartesians, $
   CARTESIAN_OFFSETS=offset, $
   CARTESIAN_SCALES=scales, $
   SUPPRESS_LEGEND=suppress_legend, $
   CURRENT=window

  COMPILE_OPT IDL2

  mapProjType = 'Goodes Homolosine'

  mapProjType = 'Polar Stereographic'

  ;; plotLat   = [-30,0,40]
  CASE STRUPCASE(mapProjType) OF
     'GOODES HOMOLOSINE': BEGIN
        bratitude = 15*(INDGEN(11) - 5)
        xRange  = 3.0e7*[-1,1]
        yRange  = 1e7*[-1,1]

     END
     'POLAR STEREOGRAPHIC': BEGIN
        ;; bratitude = 5*INDGEN(8) + 50
        bratitude = [60,70,80,90]
     END
  ENDCASE
  plotLat   = bratitude
  plotNames = STRING(FORMAT='(I0)',plotLat)
  nPlotLat  = N_ELEMENTS(plotLat)
  color     = GENERATE_LIST_OF_RANDOM_COLORS(nPlotLat)
  color     = REPLICATE('black',nPlotLat)
  ;; linestyle = GENERATE_LIST_OF_RANDOM_LINESTYLES(nPlotLat)
  linestyle = REPLICATE('--',nPlotLat)

  mapProj   = MAP_PROJ_INIT(mapProjType)


  gridLon   = DINDGEN(361) - 180
  latitude  = bratitude

  totArr    = MAKE_ARRAY(2,361,11)
  convArr   = MAKE_ARRAY(2,361,11)

  FOR i=0,(N_ELEMENTS(latitude) - 1) DO BEGIN
     lat             = latitude[i]
     gridLat         = REPLICATE(lat, 361)

     totArr[*,*,i]   = [TRANSPOSE(gridLon),TRANSPOSE(gridLat)]
     convArr[*,*,i]  = MAP_PROJ_FORWARD(gridLon,gridLat,MAP_STRUCTURE=mapProj)


  ENDFOR

  IF KEYWORD_SET(scale_Cartesians) THEN BEGIN
     offset = REFORM([MIN(convArr[0,*,*]),MIN(convArr[1,*,*])])
     scales = REFORM([MAX(convArr[0,*,*]),MAX(convArr[1,*,*])])-offset

     FOR k=0,1 DO BEGIN
        convArr[k,*,*] -= offset[k]
        convArr[k,*,*] /= scales[k]
     ENDFOR
  ENDIF


  ;;This should be latitude 
  plotArr     = MAKE_ARRAY(nPlotLat,/OBJ)
  ;; xRange  = [MIN(convArr[0,*,*]),MAX(convArr[0,*,*])]
  ;; yRange  = [MIN(convArr[1,*,*]),MAX(convArr[1,*,*])]

  FOR k=0,nPlotLat-1 DO BEGIN
     junk = MIN(ABS(latitude-plotLat[k]),lat_i)

     plotArr[k] =PLOT(convArr[*,*,lat_i], $
                      ;; TITLE=mapProjType + ' (Cartesian conversion)', $
                      NAME=plotNames[k], $
                      ;; XRANGE=plotRanges[0,*], $
                      ;; YRANGE=plotRanges[1,*], $
                      AXIS_STYLE=0, $
                      XRANGE=xRange, $
                      YRANGE=yRange, $
                      COLOR=color[k], $
                      LINESTYLE=linestyle[k], $
                      OVERPLOT=k GT 0, $
                      CURRENT=window)
     
  ENDFOR

  IF ~KEYWORD_SET(suppress_legend) THEN BEGIN
     legend = LEGEND(TARGET=REVERSE(plotArr[*]), $
                     POSITION=[0.28,0.88])
  ENDIF


END
