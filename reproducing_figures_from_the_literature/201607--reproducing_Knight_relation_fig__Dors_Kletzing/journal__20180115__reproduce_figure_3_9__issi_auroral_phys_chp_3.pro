;;07/05/16
PRO JOURNAL__20180115__REPRODUCE_FIGURE_3_9__ISSI_AURORAL_PHYS_CHP_3, $
   MAXPOT=maxPot, $
   SET_FOR_ORIGINAL_FIG=set_for_original_fig, $
   SET_FOR_KAPPA_EQUALS=set_for_kappa_equals, $
   SAVE_PNG=save_png, $
   BUFFER=buffer

  COMPILE_OPT IDL2

  CASE 1 OF
     KEYWORD_SET(set_for_original_fig): BEGIN
        plotSN          = 'ISSI_AurPhys_Fig_3_9--original.png'
     END
     KEYWORD_SET(set_for_kappa_equals): BEGIN
        plotSN          = 'ISSI_AurPhys_Fig_3_9-kappa_eq_.png'
     END
     ELSE: BEGIN
        plotSN          = 'ISSI_AurPhys_Fig_3_9-kappa_eq_.png'
     END
  ENDCASE
  add_legend            = 0
  fontSize              = 20
  
  make_abs              = 0

  T_m                   = 1000.D ;eV
  dens_m                = 0.3D   ; cm^-3

  onlyMaxwellian  = KEYWORD_SET(set_for_original_fig)

  IF ~KEYWORD_SET(onlyMaxwellian) THEN BEGIN
     kappa        = KEYWORD_SET(set_for_kappa_equals) ? set_for_kappa_equals : 3
     newNavn      = (STRSPLIT(plotSN,'.',/EXTRACT))[0]
     newSuff      = STRJOIN(STRSPLIT(STRING(FORMAT='(F0.2)',kappa),'.',/EXTRACT),'_')+'.png'
     ;; plotSN.Replace(".png",newSuff) 
     plotSN       = newNavn + newSuff
  ENDIF

  IF KEYWORD_SET(maxPot) THEN BEGIN
     potStr             = STRING(FORMAT='(A0,G0.0)','-potMax_eq_',maxPot)
     ;; potStr             = STRSPLIT(potStr,'E+0',/EXTRACT)
     potStr             = potStr.Replace('E','D')
     potStr             = potStr.Replace('+','')
     potStr             = potStr.Replace('0','')
     
     plotSN             = STRSPLIT(plotSN,'.',/EXTRACT)
     plotSN             = STRJOIN([plotSN[0]+potStr,plotSN[1]],'.')
  ENDIF ELSE BEGIN
     maxPot             = 1D6
  ENDELSE
  pot                   = POWGEN(10,maxPot,1.15)

  R_B                   = [  1, $
                             3, $
                            10, $
                            30, $
                           100, $
                           300, $
                           1D3]

  potBar_bars           = [1,10,100,1000]

  lineStyle       = ['-','__',"--","-.",":","-",'__']

  color           = ['orange','red','green','blue','black','purple','pink']

  ;; in_potBar    = 10.D^(DOUBLE(INDGEN(25)/4.-2))

  n_RB_texts      = 7

  lineThick       = 2.0

  xRange          = [MIN(pot),MAX(pot)]
  yRange          = [1e-7,1e-3]
  ;; xTitle       = 'e$\Delta\Phi$/K!Dth!N'
  xTitle          = '$\Delta\Phi_\parallel$ [Volt]'
  yTitle          = 'Current Density [A m!U-2!N]'
  fontSize        = 18
  window          = WINDOW(DIMENSIONS=[1200,800],BUFFER=buffer)

  Tk_m            = T_m
  Tm_m            = T_m

  densk_m         = dens_m
  densm_m         = dens_m

  ;; IF KEYWORD_SET(set_for_kappa_paper) THEN BEGIN
  ;;    Tk_m    = 2830.D         ;eV
  ;;    densk_m = 0.03D          ; cm^-3

  ;;    Tm_m    = 40.D           ;eV
  ;;    densm_m = 0.026D         ; cm^-3

  ;;    yRange  = [3e-9,1e-5]

  ;; R_B                   = [  3,  3,  3,  3, $
  ;;                           10, 10, 10, 10, $
  ;;                           30, 30, 30, 30, $
  ;;                          100,100,100,100, $
  ;;                          1e6,1e6,1e6,1e6]

  ;; ;;More extreme
  ;; k1                    = 1.78
  ;; k3                    = 2.5
  ;; k4                    = 5
  ;; k5                    = 10

  ;; ;;More extreme
  ;; kappa                 = [k1,k3,k4,k5, $
  ;;                          k1,k3,k4,k5, $
  ;;                          k1,k3,k4,k5, $
  ;;                          k1,k3,k4,k5, $
  ;;                          k1,k3,k4,k5]

  ;; changeKappa           = 1.78     ;Switch to Maxwellian here

  ;; skipKappa             = [k3,k4]

  ;; lineStyle             = ['-','__',"--","-.", $
  ;;                          '-','__',"--","-.", $
  ;;                          '-','__',"--","-.", $
  ;;                          '-','__',"--","-.", $
  ;;                          '-','__',"--","-."]

  ;; color                 = ['orange','green','blue','black', $
  ;;                          'orange','green','blue','black', $
  ;;                          'orange','green','blue','black', $
  ;;                          'orange','green','blue','black', $
  ;;                          'orange','green','blue','black']

  ;; ENDIF

  textArr               = 'R!DB!N = ' + STRING(FORMAT='(I0)',R_B[UNIQ(R_B)])
  textObjArr            = MAKE_ARRAY(n_RB_texts-N_ELEMENTS(skipKappa),/OBJ)
  xText                 = 300000
  textMin               = MIN(ABS(pot-xText),textInd)

  nPlots                = N_ELEMENTS(R_B)-N_ELEMENTS(skipKappa)
  plotArr               = MAKE_ARRAY(nPlots,/OBJ)

  plotTitle             = 'Dors and Kletzing eq. ' + (KEYWORD_SET(onlyMaxwellian) ? '(4): Maxwellian J-V' : '(11): Kappa J-V ($\kappa =$' + STRING(FORMAT='(F0.2)',kappa) + ')')

  iText                 = 0
  FOR iPlot=0,nPlots-1 DO BEGIN

     RTemp           = R_B[iPlot]
     PRINT,"R_B  : ",RTemp

     IF ~KEYWORD_SET(onlyMaxwellian) THEN BEGIN

        kappa_j      = KNIGHT_RELATION__DORS_KLETZING_11(kappa,Tk_m,densk_m,pot,RTemp, $
                                                         ;; IN_POTBAR=in_potBar, $
                                                         OUT_POTBAR=potBar, $
                                                         /NO_MULT_BY_CHARGE)

        IF KEYWORD_SET(make_abs) THEN BEGIN
           kappa_j      = ABS(kappa_j)
        ENDIF
        plotArr[iPlot]  = PLOT(pot,kappa_j, $
                               NAME=textArr[iText], $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               TITLE=plotTitle, $
                               XTITLE=xTitle, $
                               YTITLE=yTitle, $
                               ;; XTICKFORMAT='exponentlabel', $
                               ;; YTICKFORMAT='exponentlabel', $
                               ;; YTICKVALUES=yTickValues, $
                               ;; YTICKNAME=yTickName, $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               FONT_SIZE=fontSize, $
                               THICK=lineThick, $
                               OVERPLOT=iPlot GT 0, $
                               CURRENT=window)

        textObjArr[iText] = TEXT(100000,( (0.65*kappa_j[textInd]) < ( yRange[1] ) ), $
                                 textArr[iText], $
                                 FONT_SIZE=fontSize, $
                                 /DATA)

        iText++
     ENDIF

     doIt               = KEYWORD_SET(onlyMaxwellian)
     IF ~doIt THEN IF KEYWORD_SET(kTemp) AND N_ELEMENTS(changeKappa) GT 0 THEN BEGIN
        doIt            = kTemp EQ changeKappa
     ENDIF
     
     IF doIt THEN BEGIN

        maxwell_j       = KNIGHT_RELATION__DORS_KLETZING_4(Tm_m,densm_m,pot,RTemp, $
                                                           ;; IN_POTBAR=in_potBar, $
                                                           OUT_POTBAR=potBar, $
                                                           /NO_MULT_BY_CHARGE)
        IF KEYWORD_SET(make_abs) THEN BEGIN
           maxwell_j    = ABS(maxwell_j)
        ENDIF
        plotArr[iPlot]  = PLOT(pot,maxwell_j, $
                               NAME=textArr[iText], $
                               TITLE=plotTitle, $
                               XTITLE=xTitle, $
                               YTITLE=yTitle, $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=1, $
                               YLOG=1, $
                               ;; XTICKFORMAT='exponentlabel', $
                               ;; YTICKFORMAT='exponentlabel', $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               THICK=lineThick, $
                               FONT_SIZE=fontSize, $
                               OVERPLOT=KEYWORD_SET(onlyMaxwellian) ? (iPlot GT 0) : 1, $
                               CURRENT=window)

        textObjArr[iText] = TEXT(100000,( (0.65*maxwell_j[textInd]) < ( yRange[1] ) ), $
                                 textArr[iText], $
                                 FONT_SIZE=fontSize, $
                                 /DATA)

        iText++
     ENDIF


  ENDFOR

  densText = TEXT(100, $
                  yRange[1]/3, $
                  STRING(FORMAT='(A0,F0.1,A0)','n$_e$ = ',dens_m,' cm!U-3!N'), $
                  FONT_SIZE=fontSize, $
                  /DATA)

  tempText = TEXT(100, $
                  yRange[1]/6, $
                  STRING(FORMAT='(A0,I0,A0)','T$_e$ = ',T_m,' K'), $
                  FONT_SIZE=fontSize, $                  
                  /DATA)

  IF KEYWORD_SET(potBar_bars) THEN BEGIN
     nBars = N_ELEMENTS(potBar_bars)

     barPlots = MAKE_ARRAY(nBars,/OBJ)

     barTxtArr = MAKE_ARRAY(nBars,/OBJ)
     FOR k=0,nBars-1 DO BEGIN
        thisInd = VALUE_CLOSEST2(potBar,potBar_bars[k],/CONSTRAINED)
        tmpx = [pot[thisInd],pot[thisInd]]
        tmpy = [1D-9,1D-3]

        barPlots[k] = PLOT(tmpx,tmpy, $
                           LINESTYLE='--', $
                           /OVERPLOT)

        barTxtArr[k] = TEXT(pot[thisInd], $
                            10.^(ALOG10(yRange[0]) + 0.8*( ALOG10(yRange[1]) - ALOG10(yRange[0]))), $
                            'e $\Delta \Phi$ / T ='+STRCOMPRESS(potBar_bars[k],/REMOVE_ALL), $
                            /DATA, $
                            CLIP=0)
     ENDFOR

     
  ENDIF

  IF KEYWORD_SET(add_legend) THEN BEGIN
     legPos            = [0.4,0.8]
     legFontSize       = 16
     legFont           = 'Courier'
     legend            = LEGEND(TARGET=REVERSE(plotArr), $
                                POSITION=legPos, $
                                FONT_SIZE=legFontSize, $
                                FONT_NAME=legFont, $
                                ;; ALIGNMENT=0.5, $
                                ;; VERTICAL_ALIGNMENT=0.5, $
                                /NORMAL)
  ENDIF

  
  IF KEYWORD_SET(save_png) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/ISSI_AurPhys_Fig_3_9/'

     PRINT,'Saving ' + plotSN + ' ...'
     window.Save,plotDir+plotSN
     window.Close
     window = !NULL
  ENDIF

  ;; FOR k=0,n_RB_texts-1 DO BEGIN
     
  ;;    textObjArr[k] = TEXT(
  ;; ENDFOR

END

