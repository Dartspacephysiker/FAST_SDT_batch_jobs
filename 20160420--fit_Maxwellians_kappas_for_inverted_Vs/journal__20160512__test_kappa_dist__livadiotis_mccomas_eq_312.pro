;2016/05/12
;Test dat out!!
;RESULT: ...
PRO JOURNAL__20160512__TEST_KAPPA_DIST__LIVADIOTIS_MCCOMAS_EQ_312

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
  ;;"Magnetosphere"
  T               = 1e3      ;eV
  ;; n               = 1e6      ;m^-3
  E_b             = 1e3      ;bulk energy, eV

  ;;"Ionosphere"
  ;; T            = 0.05     ;eV
  ;; n            = 1e11     ;m^-3
  
  eLogSpacing     = 0.05
  N_points        = 100
  start_logEnergy = 0
  energies        = 10^(INDGEN(N_points)*eLogSpacing+start_logEnergy)

  kappa           = 150      ; Close to Gaussian?
  bulkAngle       = [0]
  ;; bulkAngle       = [0,15,30,45,60,90,120]

  ;;Angle plots
  xLog            = 1
  yLog            = 1
  xTitle          = "Energy (eV)"
  yTitle          = " (inverse velocity cubed, for now)"
  aPlotArr        = MAKE_ARRAY(N_ELEMENTS(bulkAngle),/OBJ)
  colorList       = GENERATE_LIST_OF_RANDOM_COLORS(N_ELEMENTS(bulkAngle))
  lStyleList      = GENERATE_LIST_OF_RANDOM_LINESTYLES(N_ELEMENTS(bulkAngle))
  FOR i=0,N_ELEMENTS(bulkAngle)-1 DO BEGIN

     A            = [E_b, T, kappa]
     ;; A            = [E_b, T, kappa, n, bulkAngle[i]*180/!PI]
     KAPPA_DIST__LIVADIOTIS_MCCOMAS_EQ_312,energies,A,F,pders

     aPlotArr[i]  = PLOT(energies,F, $
                         NAME='$\theta_B$: ' + STRCOMPRESS(bulkAngle[i],/REMOVE_ALL) + '!Uo!N', $
                         XTITLE=xTitle, $
                         YTITLE=yTitle, $
                         OVERPLOT=i GT 0, $
                         LINESTYLE=lStyleList[i], $
                         COLOR=colorList[i], $
                         XLOG=xLog, $
                         YLOG=yLog)
  
  ENDFOR

  aLegend         = LEGEND(TARGET=aPlotArr[*])

  STOP
END