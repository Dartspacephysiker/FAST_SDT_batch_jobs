;;07/05/16
PRO JOURNAL__20180115__EXPLORE_J_V_RELATION__DENS_TEMP, $
   MAXPOT=maxPot, $
   MINPOT=minPot, $
   XLOG=xLog, $
   YLOG=yLog, $
   YRANGE=yRange, $
   TEMPERATURE=temperature, $
   DENSITY=density, $
   TREAT_DENS_AS_FAST_DENS_AND_MAP_W_BARBOSA=treat_dens_as_being_at_FAST, $
   SET_FOR_MAXWELLIAN=set_for_Maxwellian, $
   KAPPAVAL=kappaVal, $
   SAVE_PNG=save_png, $
   BUFFER=buffer

  COMPILE_OPT IDL2

  plotPref = 'Explore_J_V'
  IF KEYWORD_SET(treat_dens_as_being_at_FAST) THEN plotPref += '-FASTDens'
  CASE 1 OF
     KEYWORD_SET(set_for_Maxwellian): BEGIN
        plotSN          = plotPref + '-Maxwellian.png'
     END
     KEYWORD_SET(kappaVal): BEGIN
        plotSN          = plotPref + '-kappa_eq_.png'
     END
     ELSE: BEGIN
        plotSN          = plotPref + '-kappa_eq_.png'
     END
  ENDCASE
  add_legend            = 0
  fontSize              = 20
  
  make_abs              = 0

  onlyMaxwellian  = KEYWORD_SET(set_for_Maxwellian)

  IF ~KEYWORD_SET(onlyMaxwellian) THEN BEGIN
     kappa        = KEYWORD_SET(kappaVal) ? kappaVal : 3
     newNavn      = (STRSPLIT(plotSN,'.',/EXTRACT))[0]
     newSuff      = STRJOIN(STRSPLIT(STRING(FORMAT='(F0.2)',kappa),'.',/EXTRACT),'_')+'.png'
     ;; plotSN.Replace(".png",newSuff) 
     plotSN       = newNavn + newSuff
  ENDIF

  IF KEYWORD_SET(minPot) THEN BEGIN
     potStr             = STRING(FORMAT='(A0,G0.0)','-potMin_eq_',minPot)
     ;; potStr             = STRSPLIT(potStr,'E+0',/EXTRACT)
     IF minPot GE 1D4 THEN BEGIN
        potStr             = potStr.Replace('E','D')
        potStr             = potStr.Replace('+','')
        potStr             = potStr.Replace('0','')
     ENDIF
     
     plotSN             = STRSPLIT(plotSN,'.',/EXTRACT)
     plotSN             = STRJOIN([plotSN[0]+potStr,plotSN[1]],'.')
  ENDIF ELSE BEGIN
     minPot             = 10
  ENDELSE

  IF KEYWORD_SET(maxPot) THEN BEGIN
     potStr             = STRING(FORMAT='(A0,G0.0)','-potMax_eq_',maxPot)
     ;; potStr             = STRSPLIT(potStr,'E+0',/EXTRACT)

     IF maxPot GE 1D4 THEN BEGIN
        potStr          = potStr.Replace('E','D')
        potStr          = potStr.Replace('+','')
        potStr          = potStr.Replace('0','')
     ENDIF
     
     plotSN             = STRSPLIT(plotSN,'.',/EXTRACT)
     plotSN             = STRJOIN([plotSN[0]+potStr,plotSN[1]],'.')
  ENDIF ELSE BEGIN
     maxPot             = 1D6
  ENDELSE

  CASE 1 OF
     (ALOG10(maxPot) - ALOG10(minPot)) LT 2: BEGIN
        pot             = [minPot:maxPot:10.^(FLOOR(ALOG10(minPot))-2)]
        xLog            = N_ELEMENTS(xLog) GT 0 ? xLog : 0
     END
     ELSE: BEGIN
        pot             = POWGEN(minPot,maxPot,1.15)
        xLog            = N_ELEMENTS(xLog) GT 0 ? xLog : 1
     END
  ENDCASE
  
  T_m                   = KEYWORD_SET(temperature) ? temperature : 1000.D ;eV
  dens_m                = KEYWORD_SET(density    ) ? density     : 0.3D   ; cm^-3

  TString               = STRING(FORMAT='(I0,A0)',T_m,'eV')
  NString               = STRING(FORMAT='(F0.2,A0)',dens_m,'cm-3')
  NString               = NString.Replace('.','_')
  TandNStr              = '-' + TEMPORARY(TString) + '-' + TEMPORARY(NString)
  plotSN                = plotSN.Replace('.png',TEMPORARY(TandNStr)+'.png')

  R_B                   = [  1, $
                             3, $
                            10, $
                            30, $
                           100, $
                           300, $
                           1D3]

  potBar_bars           = [1,10,100,1000]

  lineStyle       = ['-',"-:","--","-.",'__',":",'-']

  color           = ['orange','red','green','blue','black','purple','pink']

  ;; in_potBar    = 10.D^(DOUBLE(INDGEN(25)/4.-2))

  n_RB_texts      = 7

  lineThick       = 2.0

  xRange          = [MIN(pot),MAX(pot)]

  yRange          = KEYWORD_SET(yRange) ? yRange : [1D-7,1D-3]*1D6
  yLog            = N_ELEMENTS(yLog) GT 0 ? yLog : 1

  ;; xTitle       = 'e$\Delta\Phi$/K!Dth!N'
  xTitle          = '$\Delta\Phi_\parallel$ [Volt]'
  yTitle          = 'Current Density [$\mu$A m!U-2!N]'
  fontSize        = 18
  window          = WINDOW(DIMENSIONS=[1200,800],BUFFER=buffer)

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

     tmpDens         = dens_m

     IF KEYWORD_SET(treat_dens_as_being_at_FAST) THEN BEGIN

        tmpDens     = DENSITY_FACTOR__BARBOSA_1977(pot,T_m,!NULL,dens_m,RTemp)

        IF N_ELEMENTS(WHERE(FINITE(tmpDens))) NE N_ELEMENTS(pot) THEN STOP

     ENDIF

     IF ~KEYWORD_SET(onlyMaxwellian) THEN BEGIN

        current         = KNIGHT_RELATION__DORS_KLETZING_11(kappa,T_m,tmpDens,pot,RTemp, $
                                                         ;; IN_POTBAR=in_potBar, $
                                                         OUT_POTBAR=potBar, $
                                                         /NO_MULT_BY_CHARGE)*1D6

        IF KEYWORD_SET(make_abs) THEN BEGIN
           current      = ABS(current)
        ENDIF
        plotArr[iPlot]  = PLOT(pot,current, $
                               NAME=textArr[iText], $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=xLog, $
                               YLOG=yLog, $
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

     ENDIF

     doIt               = KEYWORD_SET(onlyMaxwellian)
     IF ~doIt THEN IF KEYWORD_SET(kTemp) AND N_ELEMENTS(changeKappa) GT 0 THEN BEGIN
        doIt            = kTemp EQ changeKappa
     ENDIF
     
     IF doIt THEN BEGIN

        current         = KNIGHT_RELATION__DORS_KLETZING_4(T_m,tmpDens,pot,RTemp, $
                                                           ;; IN_POTBAR=in_potBar, $
                                                           OUT_POTBAR=potBar, $
                                                           /NO_MULT_BY_CHARGE)*1D6
        IF KEYWORD_SET(make_abs) THEN BEGIN
           maxwell_j    = ABS(maxwell_j)
        ENDIF
        plotArr[iPlot]  = PLOT(pot,current, $
                               NAME=textArr[iText], $
                               TITLE=plotTitle, $
                               XTITLE=xTitle, $
                               YTITLE=yTitle, $
                               XRANGE=xRange, $
                               YRANGE=yRange, $
                               XLOG=xLog, $
                               YLOG=yLog, $
                               ;; XTICKFORMAT='exponentlabel', $
                               ;; YTICKFORMAT='exponentlabel', $
                               LINESTYLE=lineStyle[iPlot], $
                               COLOR=color[iPlot], $
                               THICK=lineThick, $
                               FONT_SIZE=fontSize, $
                               OVERPLOT=KEYWORD_SET(onlyMaxwellian) ? (iPlot GT 0) : 1, $
                               CURRENT=window)

     ENDIF

     IF ~KEYWORD_SET(treat_dens_as_being_at_FAST) THEN BEGIN
        textObjArr[iText] = TEXT((xLog ? $
                                  10.^(ALOG10(xRange[0]) + 0.8*( ALOG10(xRange[1]) - ALOG10(xRange[0]))) : $
                                  xRange[0] + 0.8*( xRange[1] - xRange[0])), $
                                 ( (0.65*current[textInd]) < ( yRange[1] ) ), $
                                 textArr[iText], $
                                 FONT_SIZE=fontSize, $
                                 /DATA)

     ENDIF
     iText++

  ENDFOR

  dStr     = KEYWORD_SET(treat_dens_as_being_at_FAST) ? 'n$_F$ = ' : 'n$_e$ = '
  densText = TEXT(xLog ? $
                  10.^(ALOG10(xRange[0]) + 0.04*(ALOG10(xRange[1]) - ALOG10(xRange[0]))) : $
                  xRange[0] + 0.1*(xRange[1] - xRange[0]), $
                  yLog ? yRange[1]/3 : yRange[0] + 0.9*(yRange[1] - yRange[0]), $
                  STRING(FORMAT='(A0,F0.1,A0)',dStr,dens_m,' cm!U-3!N'), $
                  FONT_SIZE=fontSize, $
                  /DATA)

  tempText = TEXT(xLog ? $
                  10.^(ALOG10(xRange[0]) + 0.04*(ALOG10(xRange[1]) - ALOG10(xRange[0]))) : $
                       xRange[0] + 0.1*(xRange[1] - xRange[0]), $
                  yLog ? yRange[1]/6 : yRange[0] + 0.85*(yRange[1] - yRange[0]), $
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
        tmpy = [MIN(yRange),MAX(yRange)]

        barPlots[k] = PLOT(tmpx,tmpy, $
                           LINESTYLE='--', $
                           /OVERPLOT)

        barTxtArr[k] = TEXT(pot[thisInd], $
                            yLog ? 10.^(ALOG10(yRange[0]) + 0.8*( ALOG10(yRange[1]) - ALOG10(yRange[0]))) : $
                            yRange[0] + 0.8*( yRange[1] - yRange[0]), $
                            'e $\Delta \Phi$ / T ='+STRCOMPRESS(potBar_bars[k],/REMOVE_ALL), $
                            /DATA, $
                            CLIP=0)
     ENDFOR

     
  ENDIF

  IF KEYWORD_SET(add_legend) OR KEYWORD_SET(treat_dens_as_being_at_FAST) THEN BEGIN
     legPos            = [0.85,0.55]
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


