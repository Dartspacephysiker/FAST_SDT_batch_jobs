;;2016/10/28
PRO JOURNAL__20161028__PLOT_DERIV_OF_DK_EQ_14,SAVE_PNG=save_png

  COMPILE_OPT idl2

  plotSN = 'Dors_Kletzing_1999__Figure_3_deriv.png'

  make_abs              = 1

  T_m                   = 500.D ;eV
  dens_m                = 1.D   ; cm^-3

  R_B                   = [  3,  3,  3,  3,  3, $
                            10, 10, 10, 10, 10, $
                            30, 30, 30, 30, 30, $
                           100,100,100,100,100, $
                           1e6,1e6,1e6,1e6,1e6]

  ;;More extreme
  kappa                 = [1.5001,2.0,2.5,5,10, $
                           1.5001,2.0,2.5,5,10, $
                           1.5001,2.0,2.5,5,10, $
                           1.5001,2.0,2.5,5,10, $
                           1.5001,2.0,2.5,5,10]
  ;;Less extreme; why even show k = 10?
  ;; kappa                 = [1.8,3,5,10,0, $
  ;;                          1.8,3,5,10,0, $
  ;;                          1.8,3,5,10,0, $
  ;;                          1.8,3,5,10,0, $
  ;;                          1.8,3,5,10,0]

  lineStyle             = [':','__',"--","--","-", $
                           ':','__',"--","--","-", $
                           ':','__',"--","--","-", $
                           ':','__',"--","--","-", $
                           ':','__',"--","--","-"]

  color                 = ['black','red','green','blue','violet', $
                           'black','red','green','blue','violet', $
                           'black','red','green','blue','violet', $
                           'black','red','green','blue','violet', $
                           'black','red','green','blue','violet']

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

  in_potBar             = 10.D^(DOUBLE(INDGEN(33)/4.-3))

  pot                   = TEMPORARY(in_potBar) * T_m

  nPlots                = N_ELEMENTS(R_B)
  plotArr               = MAKE_ARRAY(nPlots,/OBJ)

  lineThick             = 1.5

  xRange                = [1e-3,1e5]
  yRange                = [1e-6,1e3]
  xTitle                = 'e$\Delta\Phi$/K!Dth!N'
  yTitle                = 'd(EFlux Density)/dPot (m!U-2!Ns!U-1!N)'
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
     plotName           = STRING(FORMAT='("Kappa = ",F0.1)',kTemp)

     ;; IF kTemp EQ 1.51 THEN STOP 
     STOP
     kappa_dEF          = KAPPA_1__DORS_KLETZING_EQ_15__D_EFLUX_D_POT( $
                          kTemp,T_m,dens_m,pot,RTemp, $
                          IN_POTBAR=in_potBar, $
                          OUT_POTBAR=potBar)

     IF KEYWORD_SET(make_abs) THEN BEGIN
        kappa_dEF       = ABS(kappa_dEF)
     ENDIF

     ;; plotArr[iPlot]     = PLOT(in_potBar,kappa_dEF, $
     plotArr[iPlot]     = PLOT(potBar,kappa_dEF, $
                               NAME=plotName, $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               XTITLE=xTitle, $
                               YTITLE=yTitle, $
                               XSTYLE=1, $
                               YSTYLE=1, $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               FONT_SIZE=fontSize, $
                               THICK=lineThick, $
                               OVERPLOT=iPlot GT 0, $
                               CURRENT=window)

     IF kTemp EQ 5 THEN BEGIN
        maxwell_dEF = KAPPA_1__DORS_KLETZING_EQ_14__D_EFLUX_D_POT__MAXWELL( $
                      T_m,dens_m,pot,RTemp, $
                      IN_POTBAR=in_potBar, $
                      OUT_POTBAR=potBar)

        iPlot++
        IF KEYWORD_SET(make_abs) THEN BEGIN
           maxwell_dEF    = ABS(maxwell_dEF)
        ENDIF
        ;; plotArr[iPlot]  = PLOT(in_potBar,maxwell_dEF, $
        plotArr[iPlot]  = PLOT(potBar,maxwell_dEF, $
                               NAME='Maxwellian', $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               LINESTYLE=lineStyle[iPlot], $
                               XSTYLE=1, $
                               YSTYLE=1, $
                               COLOR=color[iPlot], $
                               THICK=lineThick, $
                               /OVERPLOT, $
                               CURRENT=window)

        textObjArr[iText] = TEXT(100,1.1*maxwell_dEF[-2], $
                                 textArr[iText], $
                                 /DATA)

        iText++
     ENDIF


  ENDFOR

  legPos            = [0.5,0.8]
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