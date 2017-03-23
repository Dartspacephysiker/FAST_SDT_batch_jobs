;;07/05/16
PRO JOURNAL__20160705__REPRODUCE_FIGURE_2__DORS_KLETZING_1999,SAVE_PNG=save_png, $
   SET_FOR_ORIGINAL_FIG=set_for_original_fig, $
   SET_FOR_KAPPA_PAPER=set_for_kappa_paper, $
   SET_FOR_ORB_1843=set_for_orb_1843

  COMPILE_OPT IDL2

  CASE 1 OF
     KEYWORD_SET(set_for_original_fig): BEGIN
        plotSN          = 'Dors_Kletzing_1999__Figure_2--original.png'
     END
     KEYWORD_SET(set_for_kappa_paper): BEGIN
        plotSN          = 'Dors_Kletzing_1999-esque.png'
     END
     KEYWORD_SET(set_for_orb_1843): BEGIN
        plotSN          = 'Dors_Kletzing_1999--orb_1843_vals.png'
     END
     ELSE: BEGIN
        plotSN          = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
                          '--Dors_Kletzing_1999__Figure_2.png'
     END
  ENDCASE

  make_abs              = 0

  T_m                   = 500.D ;eV
  dens_m                = 1.D   ; cm^-3

  R_B                   = [  3,  3,  3,  3,  3, $
                            10, 10, 10, 10, 10, $
                            30, 30, 30, 30, 30, $
                           100,100,100,100,100, $
                           1D3,1D3,1D3,1D3,1D3]

  ;;More extreme
  ;; k1                    = 1.6
  k1                    = 1.55
  k2                    = 2.0
  k3                    = 2.5
  ;; k2                    = 10
  ;; k3                    = 20
  k4                    = 5
  k5                    = 10

  ;;More extreme
  kappa                 = [k1,k2,k3,k4,k5, $
                           k1,k2,k3,k4,k5, $
                           k1,k2,k3,k4,k5, $
                           k1,k2,k3,k4,k5, $
                           k1,k2,k3,k4,k5]
  changeKappa           = 5 ;Switch to Maxwellian here

  lineStyle             = ['-','__',"--","-.",":", $
                           '-','__',"--","-.",":", $
                           '-','__',"--","-.",":", $
                           '-','__',"--","-.",":", $
                           '-','__',"--","-.",":"]

  color                 = ['orange','red','green','blue','black', $
                           'orange','red','green','blue','black', $
                           'orange','red','green','blue','black', $
                           'orange','red','green','blue','black', $
                           'orange','red','green','blue','black']

  ;; R_B                   = [  3,  3,  3,  3, $
  ;;                           10, 10, 10, 10, $
  ;;                           30, 30, 30, 30, $
  ;;                          100,100,100,100, $
  ;;                          1e6,1e6,1e6,1e6]

  ;; kappa                 = [3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0, $
  ;;                          3,5,10,0]

  ;; lineStyle             = [':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-", $
  ;;                          ':',"--","-.","-"]

  ;; color                 = ['red','green','blue','black', $
  ;;                          'red','green','blue','black', $
  ;;                          'red','green','blue','black', $
  ;;                          'red','green','blue','black', $
  ;;                          'red','green','blue','black']

  in_potBar             = 10.D^(DOUBLE(INDGEN(25)/4.-2))

  n_RB_texts            = 5

  lineThick             = 1.0

  xRange                = [1e-2,1e4]
  yRange                = [3e-7,1e-3]
  xTitle                = 'e$\Delta\Phi$/K!Dth!N'
  yTitle                = 'Current Density (A m!U-2!N)'
  fontSize              = 18
  window                = WINDOW(DIMENSIONS=[960,800])

  IF KEYWORD_SET(set_for_original_fig) THEN BEGIN
     in_potBar          = 10.D^(DOUBLE(INDGEN(25)/4.-2))

       R_B              = [  3,  3,  3,  3, $
                            10, 10, 10, 10, $
                            30, 30, 30, 30, $
                           100,100,100,100, $
                           1e6,1e6,1e6,1e6]
     
       lineStyle        = [':','__',"-.","-", $
                           ':','__',"-.","-", $
                           ':','__',"-.","-", $
                           ':','__',"-.","-", $
                           ':','__',"-.","-"]
       
       color            = ['black','red','green','blue', $
                           'black','red','green','blue', $
                           'black','red','green','blue', $
                           'black','red','green','blue', $
                           'black','red','green','blue']
     kappa              = [3.0,5,10,100.0, $
                           3.0,5,10,100.0, $
                           3.0,5,10,100.0, $
                           3.0,5,10,100.0, $
                           3.0,5,10,100.0]

     changeKappa        = 10

     ;; xRange             = [1e-2,1e3]
     ;; yRange             = [1e-4,1e2]

     yTickValues        = 10.^(INDGEN(4)-6)
     yTickName          = STRING(FORMAT='("1e",I0)',INDGEN(4)-6)

  ENDIF

  Tk_m                  = T_m
  Tm_m                  = T_m

  densk_m               = dens_m
  densm_m               = dens_m

  IF KEYWORD_SET(set_for_orb_1843) THEN BEGIN

     in_potBar          = 10.D^(DOUBLE(INDGEN(25)/4.-2))

     R_B                = [30, 30, 30, 30, $
                          598,598,598,598, $
                          1D5,1D5,1D5,1D5]
     
     lineStyle          = [':','__',"-.","-", $
                           ':','__',"-.","-", $
                           ':','__',"-.","-"]
     
     color              = ['purple','blue','brown','gold', $
                           'purple','blue','brown','gold', $
                           'purple','blue','brown','gold']

     k1                 = 1.59
     k2                 = 2.2
     k3                 = 5

     kappa              = [k1,k2,k3,100, $
                           k1,k2,k3,100, $
                           k1,k2,k3,100, $
                           k1,k2,k3,100]

     changeKappa        = k3

     potBar_bars        = [10,18]

     ;; xRange             = [1e-2,1e3]
     ;; yRange             = [1e-4,1e2]

     yTickValues        = 10.^(INDGEN(4)-6)
     yTickName          = STRING(FORMAT='("1e",I0)',INDGEN(4)-6)

  ENDIF

  IF KEYWORD_SET(set_for_kappa_paper) THEN BEGIN
     Tk_m    = 2830.D         ;eV
     densk_m = 0.03D          ; cm^-3

     Tm_m    = 40.D           ;eV
     densm_m = 0.026D         ; cm^-3

     yRange  = [3e-9,1e-5]

  R_B                   = [  3,  3,  3,  3, $
                            10, 10, 10, 10, $
                            30, 30, 30, 30, $
                           100,100,100,100, $
                           1e6,1e6,1e6,1e6]

  ;;More extreme
  k1                    = 1.78
  k3                    = 2.5
  k4                    = 5
  k5                    = 10

  ;;More extreme
  kappa                 = [k1,k3,k4,k5, $
                           k1,k3,k4,k5, $
                           k1,k3,k4,k5, $
                           k1,k3,k4,k5, $
                           k1,k3,k4,k5]

  changeKappa           = 1.78     ;Switch to Maxwellian here

  skipKappa             = [k3,k4]

  lineStyle             = ['-','__',"--","-.", $
                           '-','__',"--","-.", $
                           '-','__',"--","-.", $
                           '-','__',"--","-.", $
                           '-','__',"--","-."]

  color                 = ['orange','green','blue','black', $
                           'orange','green','blue','black', $
                           'orange','green','blue','black', $
                           'orange','green','blue','black', $
                           'orange','green','blue','black']

  ENDIF

  textArr               = 'R!DB!N = ' + STRING(FORMAT='(I0)',R_B[UNIQ(R_B)])
  textObjArr            = MAKE_ARRAY(n_RB_texts-N_ELEMENTS(skipKappa),/OBJ)
  xText                 = 140
  textMin               = MIN(ABS(in_potBar-xText),textInd)

  nPlots                = N_ELEMENTS(R_B)-N_ELEMENTS(skipKappa)
  plotArr               = MAKE_ARRAY(nPlots,/OBJ)

  iText                 = 0
  FOR iPlot=0,nPlots-1 DO BEGIN
     kTemp              = kappa[iPlot]
     RTemp              = R_B[iPlot]
     PRINT,"kappa: ",kTemp
     PRINT,"R_B  : ",RTemp
     ;; plotName           = STRING(FORMAT='("Kappa = ",I0,", R_B = ",I0)',kTemp,RTemp)
     plotName           = STRING(FORMAT='("Kappa = ",F0.2)',kTemp)

     kappa_j            = KNIGHT_RELATION__DORS_KLETZING_11(kTemp,Tk_m,densk_m,pot,RTemp, $
                                                            IN_POTBAR=in_potBar, $
                                                            OUT_POTBAR=potBar)

     IF N_ELEMENTS(skipKappa) GT 0 THEN BEGIN
        IF (WHERE(kTemp EQ skipKappa))[0] NE -1 THEN BEGIN
           iPlot++
           CONTINUE
        ENDIF
     ENDIF

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
                               XTICKFORMAT='exponentlabel', $
                               YTICKFORMAT='exponentlabel', $
                               ;; YTICKVALUES=yTickValues, $
                               ;; YTICKNAME=yTickName, $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               FONT_SIZE=fontSize, $
                               THICK=lineThick, $
                               OVERPLOT=iPlot GT 0, $
                               CURRENT=window)

     IF kTemp EQ changeKappa THEN BEGIN
        maxwell_j       = KNIGHT_RELATION__DORS_KLETZING_4(Tm_m,densm_m,pot,RTemp, $
                                                           IN_POTBAR=in_potBar, $
                                                           OUT_POTBAR=potBar)

        iPlot++
        IF KEYWORD_SET(make_abs) THEN BEGIN
           maxwell_j    = ABS(maxwell_j)
        ENDIF
        plotArr[iPlot]  = PLOT(in_potBar,maxwell_j, $
                               NAME='Maxwellian', $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               THICK=lineThick, $
                               /OVERPLOT, $
                               CURRENT=window)

        textObjArr[iText] = TEXT(1000,( (1.1*maxwell_j[textInd]) < ( yRange[1] ) ), $
                                 textArr[iText], $
                                 /DATA)

        iText++
     ENDIF


  ENDFOR

  IF KEYWORD_SET(potBar_bars) THEN BEGIN
     nBars = N_ELEMENTS(potBar_bars)

     barPlots = MAKE_ARRAY(nBars,/OBJ)

     barTxtArr = MAKE_ARRAY(nBars,/OBJ)
     FOR k=0,nBars-1 DO BEGIN
        tmpx = [potBar_bars[k],potBar_bars[k]]
        tmpy = [1D-9,1D-3]

        barPlots[k] = PLOT(tmpx,tmpy, $
                           LINESTYLE='--', $
                           /OVERPLOT)

        barTxtArr[k] = TEXT(potBar_bars[k], $
                            MIN(yRange)*0.9, $
                            xTitle + '='+STRCOMPRESS(potBar_bars[k],/REMOVE_ALL), $
                            CLIP=0)
     ENDFOR

     
  ENDIF

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

  
  IF KEYWORD_SET(save_png) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Dors_Kletzing_1999/'

     PRINT,'Saving ' + plotSN + ' ...'
     window.Save,plotDir+plotSN
     window.Close
     window = !NULL
  ENDIF

  ;; FOR k=0,n_RB_texts-1 DO BEGIN
     
  ;;    textObjArr[k] = TEXT(
  ;; ENDFOR

END
