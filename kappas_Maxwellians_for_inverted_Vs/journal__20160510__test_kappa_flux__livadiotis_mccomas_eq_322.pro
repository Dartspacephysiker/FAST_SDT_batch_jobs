;2016/05/11
;Test dat out!!
;RESULT: Not helpful; I don't understand flux units! Let's try again with Dors & Kletzing's equation.
PRO JOURNAL__20160510__TEST_KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322

  diff_angles     = 0
  diff_kappas     = 1

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/20160420--fit_Maxwellians_kappas_for_inverted_Vs/Orbit_10000'
  saveDir         = '/SPENCEdata/software/sdt/batch_jobs/20160420--fit_Maxwellians_kappas_for_inverted_Vs/'
  savePlotPref    = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--test_kappa_nFlux__LM_eq_322'
  savePlot        = 1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
  ;;"Magnetosphere"
  T               = 1000L      ;eV
  n               = 1L         ;cm^-3
  E_b             = 1000L      ;bulk energy, eV

  ;;"Ionosphere"
  ;; T            = 0.05     ;eV
  ;; n            = 1e11     ;m^-3
  
  ;;Get the power back
  eSpecDec        = saveDir
  RESTORE,eSpecDec + 'Energy_spectra__orb_10000__18_08_36-18_09_00.sav'

  ;; eLogSpacing     = 0.05
  ;; N_points        = 100
  ;; start_logEnergy = 0
  energies        = REVERSE(REFORM(eSpec.v[0,*]))

  kappa           = [1.55,2.0,3,5,10]
  bulkAngle       = 0
  ;; bulkAngle       = [-15,0,17]
  ;; bulkAngle       = [0,15,30,45,60,90,120]

  CASE 1 OF
     KEYWORD_SET(diff_angles): BEGIN
        nPlots    = N_ELEMENTS(bulkAngle)
        plotNPref = '$\theta_B$: '
        plotItem  = STRCOMPRESS(bulkAngle,/REMOVE_ALL)
        plotNSuff = '!Uo!N'
        titleSuff = ', $\kappa$ = ' + STRCOMPRESS(kappa[0],/REMOVE_ALL) + ')'
        saveDatN  = '--bulkAngles'

        kappa     = REPLICATE(kappa[0],nPlots)
     END
     KEYWORD_SET(diff_kappas): BEGIN
        nPlots    = N_ELEMENTS(kappa)
        plotNPref = '$\kappa$: '
        plotItem  = STRCOMPRESS(kappa,/REMOVE_ALL)
        plotNSuff = ''
        titleSuff = ', $\theta_B$ = ' + STRCOMPRESS(bulkAngle[0],/REMOVE_ALL) + '!Uo!N)'
        saveDatN  = '--kappas'

        bulkAngle = REPLICATE(bulkAngle[0],nPlots)
     END
  ENDCASE

  ;;Angle plots
  xLog            = 1
  yLog            = 1
  xTitle          = "Energy (eV)"
  yTitle          = "Flux (#/cm!U2!N-s)"
  title           = STRING(FORMAT='("Kappa flux distribution (T = ",F0.2," eV, n = ",F0.2," cm!U-3!N",A0)',$
                           T,n,titleSuff)
  aPlotArr        = MAKE_ARRAY(nPlots,/OBJ)
  colorList       = GENERATE_LIST_OF_RANDOM_COLORS(nPlots)
  lStyleList      = GENERATE_LIST_OF_RANDOM_LINESTYLES(nPlots)
  window          = WINDOW(DIM=[800,600])
  FOR i=0,nPlots-1 DO BEGIN

     A            = [E_b, T, kappa[i], n, bulkAngle[i]*!PI/180.]
     KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322,energies,A,F,pders,/CMSQ_S_UNITS

     aPlotArr[i]  = PLOT(energies,F, $
                         NAME=plotNPref + plotItem[i] + plotNSuff, $
                         TITLE=title, $
                         XTITLE=xTitle, $
                         YTITLE=yTitle, $
                         OVERPLOT=i GT 0, $
                         LINESTYLE=lStyleList[i], $
                         THICK=2.0, $
                         COLOR=colorList[i], $
                         XLOG=xLog, $
                         YLOG=yLog, $
                         CURRENT=window)
  
  ENDFOR

  aLegend         = LEGEND(TARGET=aPlotArr[*],POSITION=[0.75,0.50],/NORMAL)

  IF KEYWORD_SET(savePlot) THEN BEGIN
     saveName = savePlotPref + saveDatN + '.png'
     PRINT,'Saving plot to ' + plotDir+saveName + '...'
     window.save,plotDir+saveName
  ENDIF

  STOP
END