;;07/05/16
PRO JOURNAL__20160705__REPRODUCE_FIGURE_2__DORS_KLETZING_1999,SAVE_PNG=save_png, $
   SET_FOR_ORIGINAL_FIG=set_for_original_fig

  COMPILE_OPT IDL2

  plotSN                = 'Dors_Kletzing_1999__Figure_2.png'

  make_abs              = 0

  T_m                   = 500.D ;eV
  dens_m                = 1.D   ; cm^-3

  R_B                   = [  3,  3,  3,  3,  3, $
                            10, 10, 10, 10, 10, $
                            30, 30, 30, 30, 30, $
                           100,100,100,100,100, $
                           1e6,1e6,1e6,1e6,1e6]

  ;;More extreme
  k1                    = 1.6
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
  window                = WINDOW(DIMENSIONS=[1200,800])

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

  ENDIF

  textArr               = 'R!DB!N = ' + STRING(FORMAT='(I0)',R_B[UNIQ(R_B)])
  textObjArr            = MAKE_ARRAY(n_RB_texts,/OBJ)
  xText                 = 140
  textMin               = MIN(ABS(in_potBar-xText),textInd)

  nPlots                = N_ELEMENTS(R_B)
  plotArr               = MAKE_ARRAY(nPlots,/OBJ)

  iText                 = 0
  FOR iPlot=0,nPlots-1 DO BEGIN
     kTemp              = kappa[iPlot]
     RTemp              = R_B[iPlot]
     PRINT,"kappa: ",kTemp
     PRINT,"R_B  : ",RTemp
     ;; plotName           = STRING(FORMAT='("Kappa = ",I0,", R_B = ",I0)',kTemp,RTemp)
     plotName           = STRING(FORMAT='("Kappa = ",F0.1)',kTemp)

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
                               COLOR=color[iPlot], $
                               FONT_SIZE=fontSize, $
                               THICK=lineThick, $
                               OVERPLOT=iPlot GT 0, $
                               CURRENT=window)

     IF kTemp EQ changeKappa THEN BEGIN
        maxwell_j       = KNIGHT_RELATION__DORS_KLETZING_4(T_m,dens_m,pot,RTemp, $
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
