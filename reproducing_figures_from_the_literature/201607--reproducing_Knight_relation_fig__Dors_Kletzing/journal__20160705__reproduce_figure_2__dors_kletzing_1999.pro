;;07/05/16
PRO JOURNAL__20160705__REPRODUCE_FIGURE_2__DORS_KLETZING_1999

  COMPILE_OPT IDL2

  make_abs              = 1

  T_m                   = 500.D ;eV
  dens_m                = 1.D   ; cm^-3

  R_B                   = [  3,  3,  3,  3,  3, $
                            10, 10, 10, 10, 10, $
                            30, 30, 30, 30, 30, $
                           100,100,100,100,100, $
                           1e6,1e6,1e6,1e6,1e6]

  kappa                 = [1.5,3,5,10,0, $
                           1.5,3,5,10,0, $
                           1.5,3,5,10,0, $
                           1.5,3,5,10,0, $
                           1.5,3,5,10,0]

  ;; kappa                 = [3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0]

  in_potBar             = 10.D^(DOUBLE(INDGEN(25)/4.-2))

  nPlots                = N_ELEMENTS(R_B)
  plotArr               = MAKE_ARRAY(nPlots,/OBJ)

  ;; lineStyle             = [':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-"]

  lineStyle             = ['__',':',"--","-.","-", $
                           '__',':',"--","-.","-", $
                           '__',':',"--","-.","-", $
                           '__',':',"--","-.","-", $
                           '__',':',"--","-.","-"]

  lineThick             = 1.0

  xRange                = [1e-2,1e4]
  yRange                = [5.5e-7,1e-3]
  xTitle                = 'e$\Delta\Phi$/K!Dth!N'
  yTitle                = 'Current Density (A m!U-2!N)'
  fontSize              = 18
  window                = WINDOW(DIMENSIONS=[1200,800])

  n_RB_texts            = 5
  textArr               = 'R!DB!N = ' + STRING(FORMAT='(I0)',R_B[UNIQ(R_B)])
  textObjArr            = MAKE_ARRAY(n_RB_texts,/OBJ)

  iText                 = 0
  FOR iPlot=0,nPlots-1 DO BEGIN
     kTemp              = kappa[iPlot]
     RTemp              = R_B[iPlot]
     PRINT,"kappa: ",kTemp
     PRINT,"R_B  : ",RTemp
     ;; plotName           = STRING(FORMAT='("Kappa = ",I0,", R_B = ",I0)',kTemp,RTemp)
     plotName           = STRING(FORMAT='("Kappa = ",I0)',kTemp)

     kappa_j            = KNIGHT_RELATION__DORS_KLETZING_11(kTemp,T_m,dens_m,pot,RTemp, $
                                                            IN_POTBAR=in_potBar, $
                                                            OUT_POTBAR=potBar)

     IF KEYWORD_SET(make_abs) THEN BEGIN
        kappa_j         = ABS(kappa_j)
     ENDIF
     plotArr[iPlot]     = PLOT(in_potBar,kappa_j, $
                               NAME=plotName, $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               XTITLE=xTitle, $
                               YTITLE=yTitle, $
                               LINESTYLE=lineStyle[iPlot], $
                               FONT_SIZE=fontSize, $
                               THICK=lineThick, $
                               OVERPLOT=iPlot GT 0, $
                               CURRENT=window)

     IF kTemp EQ 10 THEN BEGIN
        maxwell_j       = KNIGHT_RELATION__DORS_KLETZING_4(T_m,dens_m,pot,RTemp, $
                                                           IN_POTBAR=in_potBar, $
                                                           OUT_POTBAR=potBar)

        iPlot++
        IF KEYWORD_SET(make_abs) THEN BEGIN
           maxwell_j    = ABS(maxwell_j)
        ENDIF
        plotArr[iPlot]  = PLOT(in_potBar, $
                               NAME='Maxwellian', $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               LINESTYLE=lineStyle[iPlot], $
                               THICK=lineThick, $
                               /OVERPLOT, $
                               CURRENT=window)

        textObjArr[iText] = TEXT(1000,1.1*maxwell_j[-2], $
                                 textArr[iText], $
                                 /DATA)

        iText++
     ENDIF


  ENDFOR

  legPos            = [0.4,0.8]
  legFontSize       = 18
  legFont           = 'Courier'
  legend            = LEGEND(TARGET=plotArr[0:N_ELEMENTS(UNIQ(kappa,SORT(kappa)))-1], $
                             POSITION=legPos, $
                             FONT_SIZE=legFontSize, $
                             FONT_NAME=legFont, $
                             ;; ALIGNMENT=0.5, $
                             ;; VERTICAL_ALIGNMENT=0.5, $
                             /NORMAL)

  


  ;; FOR k=0,n_RB_texts-1 DO BEGIN
     
  ;;    textObjArr[k] = TEXT(
  ;; ENDFOR

  STOP

END
