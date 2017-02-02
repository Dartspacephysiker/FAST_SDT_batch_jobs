;;08/04/16
;Chris' email:
;Here’s some extra tasks:
; 
;1. Plot the measured value of J against the model values.
;
;2. Now try doing it with the characteristic electron energies
;replaced with the potentials you get from the fits inside the source
;cone. There should then be two curves for phi – one for the
;Maxwellian fit + the ion char En. and the other for the Kappa fits +
;the ion char En. I suggest this because the energy flux/flux to get
;the phi is only good if phi >> Te.
;

;;Here are the calls I made to do some damage:
;SET_TXTOUTPUT_DIR,txtOutputDir,/FOR_SDT,/ADD_TODAY 
;OPENW,lun,txtOutputDir+'20160804--linear_fits--kappa_and_gauss--orbit_1849.txt',/GET_LUN
;JOURNAL__20160804__MCFADDEN_ET_AL_1998__CHRIS_WORK_1_AND_2__USE_2D_FITS__EACH_ANGLE_DENSED__ALL_THREE_POTENTIALS,/PRINT_KAPPA_GAUSS_CORR,/ADD_LINEAR_FITS,/SAVE_PLOT,RBCORRLUN=lun 
;CLOSE,lun 
;FREE_LUN,lun
;
PRO JOURNAL__20160804__MCFADDEN_ET_AL_1998__CHRIS_WORK_1_AND_2__USE_2D_FITS__EACH_ANGLE_DENSED__ALL_THREE_POTENTIALS, $
   ADD_LINEAR_FITS=add_linear_fits, $
   MAGRATIO1=magRatio1, $
   MAGRATIO2=magRatio2, $
   MAGRATIO2=magRatio3, $
   USE_JE_CURRENT=use_je_current, $
   USE_JMAG_CURRENT=use_jMag_current, $
   HIGHDENS_THRESH=highDens_thresh, $
   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
   SDT_CALC__NO_MODEL=SDT_calc__no_model, $
   XLOG=xLog, $
   YLOG=yLog, $
   SAVE_PLOT=save_plot, $
   PRINT_KAPPA_GAUSS_CORR=print_kappa_gauss_corr, $
   RBCORRLUN=RBCorrLun

  COMPILE_OPT IDL2

  orbit               = 1849

  ;;What we were using in the journal from the other day to generate these fit files
  eAngleCharE        = [330,30]
  energy_electrons   = [100.,36000.]

  ;;Defaults
  defR_B              = 3.0
  IF N_ELEMENTS(magRatio1) EQ 0 THEN BEGIN
     magRatio1        = defR_B
  ENDIF

  IF N_ELEMENTS(magRatio2) EQ 0 THEN BEGIN
     magRatio2        = defR_B
  ENDIF

  IF N_ELEMENTS(magRatio3) EQ 0 THEN BEGIN
     magRatio3        = defR_B
  ENDIF

  KAPPA_FITFILE_STRING,outSuff, $
                       R_B=magRatio1, $
                       ;; USE_DATA_DENS=use_data_dens, $
                       ;; SDT_CALC__NO_MODEL=SDT_calc__no_model, $
                       LKAPPA_THRESH=lKappa_thresh, $
                       HKAPPA_THRESH=hKappa_thresh, $
                       HIGHDENS_THRESH=highDens_thresh

  ;;inFiles
  saveDir            = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile            = '20160804--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'

  fitFile            = '20160804--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150.sav'

  inSaveFile         = '20160804--McFadden_et_al_1998_Fig_1--four_currents--2dfits' + outSuff + '.sav'

  ;;outFiles
  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPlotPref        = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--model_vs_obs--two_panels--2dfits'+outSuff

  ;;Comes with fit2dgauss_inf_list, fit2dkappa_inf_list, gaussfits, je, jee, kappafits, synthpackage
  RESTORE,saveDir+fitFile

  ;;prevent clobbering by next RESTORE
  je_fitFile         = je
  jee_fitFile        = jee

  ;;Comes with chare, chare_kappa_interp, chari, chari_kappa_interp, chartot, chartot_kappa_interp, 
  ;;                  esa_name, jesa, je_kappa_interp, ji_kappa_interp, jmag, jmag_kappa_interp, jtot_kappa_interp, kappastr, 
  ;;                  kappa_current, maxwell_current, R_B
  RESTORE,saveDir+inSaveFile

  IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN BEGIN
     PRINT,"These lists are out of order! You're about to enter a world of confusion and pain, and I beg you reconsider."
     STOP
  ENDIF

  kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DKappa_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                                   /DESTROY_INFO_LIST, $
                                                   OUT_GOOD_I=includeK_i, $
                                                   OUT_GOOD_T=includeK_t, $
                                                   OUT_BAD_I=excludeK_i, $
                                                   OUT_BAD_T=excludeK_t)

  gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DGauss_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=100.1, $
                                                   /DESTROY_INFO_LIST, $
                                                   OUT_GOOD_I=includeG_i, $
                                                   OUT_GOOD_T=includeG_t, $
                                                   OUT_BAD_I=excludeG_i, $
                                                   OUT_BAD_T=excludeG_t)

  PARSE_KAPPA_FIT_STRUCTS,kappa2D.params1D, $
                          A=a, $
                          TIME=kappaTime, $
                          STRUCT_A=AStruct, $
                          NAMES_A=ANames, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus

  PARSE_KAPPA_FIT_STRUCTS,gauss2D.params1D, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=gaussTime, $
                          NAMES_A=AGaussNames, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now pick our currents and potentials, get things ready to plot, etc.
  setup = {kappaS:Astruct, $
           gaussS:AStructGauss, $
           charE:charE_kappa_interp, $
           charI:charI_kappa_interp, $
           charTot:charTot_kappa_interp, $
           Jtot:Jtot_kappa_interp, $
           JMag:jMag_kappa_interp, $
           Je:Je_kappa_interp, $
           Ji:Ji_kappa_interp}


  ;;Number one, use the kappa bulk energy as the potential
  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current1,obsName1,obsSuff1, $
                              kappaPot1,gaussPot1, $
                              potName1,potTitleStr1, $
                              USE_JE_CURRENT=use_je_current, $
                              USE_JMAG_CURRENT=use_jMag_current

  ;;Number two, using bulk energies as electron potential
  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current2,obsName2,obsSuff2, $
                              kappaPot2,gaussPot2, $
                              potName2,potTitleStr2, $
                              USE_JE_CURRENT=use_je_current, $
                              USE_JMAG_CURRENT=use_jMag_current, $
                              /BOTH_USE_KAPPA_BULKENERGY

  ;;Number three, use the data-derived potential
  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current3,obsName3,obsSuff3, $
                              kappaPot3,gaussPot3, $
                              potName3,potTitleStr3, $
                              USE_JE_CURRENT=use_je_current, $
                              USE_JMAG_CURRENT=use_jMag_current, $
                              /BOTH_USE_MAXWELL_BULKENERGY


  ;; kappaPotBPD      = CREATEBOXPLOTDATA(AStruct.bulk_energy)
  ;; gaussPotBPD      = CREATEBOXPLOTDATA(AStructGauss.bulk_energy)

  ;; BP               = BOXPLOT([kappaPotBPD,gaussPotBPD],XTITLE="Potential (V)")
  
  CASE 1 OF
     KEYWORD_SET(SDT_calc__no_model): BEGIN
        PRINT,"If you want to do this, visit the other journal from today that is specifically for using the SDT-calcked currents."
        STOP
     END
     ELSE: BEGIN
        GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                         kappaPot1,gaussPot1,magRatio1, $
                                         kappa_current1,gauss_current1,obs_current1, $
                                         DENSITY_KAPPA2D=kappa2D.dens, $
                                         DENSITY_GAUSS2D=gauss2D.dens
     END
  ENDCASE
  
  ;; CASE 1 OF
  ;;    KEYWORD_SET(SDT_calc__no_model): BEGIN
        ;; GET_2DFIT_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
        ;;                                        kappa_current2,gauss_current2, $
        ;;                                        ENERGY_ELECTRONS=energy_electrons, $
        ;;                                        ANGLE=eAngleCharE
     ;; END
     ;; ELSE: BEGIN
  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot2,gaussPot2,magRatio2, $
                                   kappa_current2,gauss_current2,obs_current2, $
                                   DENSITY_KAPPA2D=kappa2D.dens, $
                                   DENSITY_GAUSS2D=gauss2D.dens
  ;;    END
  ;; ENDCASE

  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot3,gaussPot3,magRatio3, $
                                   kappa_current3,gauss_current3,obs_current3, $
                                   DENSITY_KAPPA2D=kappa2D.dens, $
                                   DENSITY_GAUSS2D=gauss2D.dens


  ;; IF KEYWORD_SET(make_currents_positive) THEN BEGIN
  ;;Make currents positive here, please
  KAPPA_FLIP_CURRENT,kappa_current1,gauss_current1,obs_current1
  KAPPA_FLIP_CURRENT,kappa_current2,gauss_current2,obs_current2
  KAPPA_FLIP_CURRENT,kappa_current3,gauss_current3,obs_current3
  ;; ENDIF

  ;;Now junk the junk
  ;; kappa_current1[WHERE(excludeK_i)] = 0.0D
  ;; kappa_current2[WHERE(excludeK_i)] = 0.0D
  ;; kappa_current3[WHERE(excludeK_i)] = 0.0D

  ;; gauss_current1[WHERE(excludeG_i)] = 0.0D
  ;; gauss_current2[WHERE(excludeG_i)] = 0.0D
  ;; gauss_current3[WHERE(excludeG_i)] = 0.0D

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now for plotting

  plotExt            = '.png'
  ;; plotName           = STRING(FORMAT='(A0,' + $
  ;;                             'A0,A0,A0,A0,' + $
  ;;                             'A0,A0,A0,A0,' + $
  ;;                             'A0,A0,A0)', $
  ;;                             outPlotPref, $
  ;;                             '--CURRENT1_',obsSuff1,'--POT1_',potName1, $
  ;;                             '--CURRENT2_',obsSuff2,'--POT2_',potName2, $
  ;;                             '--CURRENT3_',obsSuff3,'--POT3_',potName3, $
  ;;                             plotExt)

  plotName           = STRING(FORMAT='(A0,' + $
                              'A0,A0,A0,A0,' + $
                              'A0,A0,' + $
                              'A0,A0,' + $
                              'A0)', $
                              outPlotPref, $
                              '--CURRENT_',obsSuff1,'--POT1_',potName1, $
                              '--POT2_',potName2, $
                              '--POT3_',potName3, $
                              plotExt)

  plotTitle1 = 'Model J!D||!N vs. ' + obsName1 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')
  plotTitle2 = 'Model J!D||!N vs. ' + obsName2 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')
  plotTitle2 = 'Model J!D||!N vs. ' + obsName3 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')

  xTitle    = obsName1 + ' Current (microA/m!U2!N)'
  yTitle    = 'Model J!D||!N (microA/m!U2!N)'

  WINDOW_CUSTOM_SETUP,NPLOTROWS=1, $
                      NPLOTCOLUMNS=3, $
                      WINDOW_TITLE=(KEYWORD_SET(orbit) ? $
                                    'FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ' ': $
                                    '') + $
                                      '(R!DB!N = ' + $
                      STRCOMPRESS(FIX(magRatio1),/REMOVE_ALL) + ')', $
                      SPACE_VERT_BETWEEN_ROWS=0.04, $
                      SPACE_FOR_ROW_NAMES=0.00, $
                      SPACE__WINTITLE=0.05, $
                      SPACE__XTITLE=0.08, $
                      SPACE__YTITLE=0.06, $
                      XTITLE=xTitle, $
                      YTITLE=yTitle, $
                      ;; ROW_NAMES=[plotTitle1,plotTitle2], $
                      COLUMN_NAMES=[potTitleStr1,potTitleStr2,potTitleStr3], $
                      /MAKE_NEW, $
                      CURRENT_WINDOW=window, $
                      ;; WINDOW_DIMENSIONS=[800,900]
                      WINDOW_DIMENSIONS=[1800,800]

  ;;If using the laptop, set WINDOW_DIMENSIONS=[1550,800] and SPACE_

  xLog     = 0
  yLog     = 0

  xRange   = [0,2.5]

  yRange   = [0,3.0]

  plotArr1 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current1,gauss_current1,obs_current1, $
                                                     EXCLUDE_KAPPA_I=excludeK_i, $
                                                     EXCLUDE_GAUSS_I=excludeG_i, $
                                                     ADD_LINEAR_FITS=add_linear_fits, $
                                                     ;; MAGRATIO=magRatio1, $
                                                     OBSNAME=obsName1, $
                                                     POSITION=WINDOW_CUSTOM_NEXT_POS(), $
                                                     /SUPPRESS_TITLE, $
                                                     /SUPPRESS_AXIS_TITLES, $
                                                     LEGEND__FONT_SIZE=12, $
                                                     XLOG=xLog, $
                                                     YLOG=yLog, $
                                                     XRANGE=xRange, $
                                                     YRANGE=yRange, $
                                                     WINDOW=window, $
                                                     BUFFER=buffer)
  
  plotArr2 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current2,gauss_current2,obs_current2, $
                                                     EXCLUDE_KAPPA_I=excludeK_i, $
                                                     EXCLUDE_GAUSS_I=excludeG_i, $
                                                     ADD_LINEAR_FITS=add_linear_fits, $
                                                     ;; MAGRATIO=magRatio2, $
                                                     OBSNAME=obsName2, $
                                                     POSITION=WINDOW_CUSTOM_NEXT_POS(/NEXT_ROW), $
                                                     /SUPPRESS_TITLE, $
                                                     /SUPPRESS_AXIS_TITLES, $
                                                     /SUPPRESS_YTICKMARKS, $
                                                     SUPPRESS_LEGEND=~KEYWORD_SET(add_linear_fits), $
                                                     /SUPPRESS_SCATTER_LEGEND, $
                                                     LEGEND__FONT_SIZE=12, $
                                                     XLOG=xLog, $
                                                     YLOG=yLog, $
                                                     XRANGE=xRange, $
                                                     YRANGE=yRange, $
                                                     WINDOW=window, $
                                                     BUFFER=buffer)
  
  plotArr3 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current3,gauss_current3,obs_current3, $
                                                     EXCLUDE_KAPPA_I=excludeK_i, $
                                                     EXCLUDE_GAUSS_I=excludeG_i, $
                                                     ADD_LINEAR_FITS=add_linear_fits, $
                                                     ;; MAGRATIO=magRatio3, $
                                                     OBSNAME=obsName3, $
                                                     POSITION=WINDOW_CUSTOM_NEXT_POS(/NEXT_ROW), $
                                                     /SUPPRESS_TITLE, $
                                                     /SUPPRESS_AXIS_TITLES, $
                                                     /SUPPRESS_YTICKMARKS, $
                                                     SUPPRESS_LEGEND=~KEYWORD_SET(add_linear_fits), $
                                                     /SUPPRESS_SCATTER_LEGEND, $
                                                     LEGEND__FONT_SIZE=12, $
                                                     XLOG=xLog, $
                                                     YLOG=yLog, $
                                                     XRANGE=xRange, $
                                                     YRANGE=yRange, $
                                                     WINDOW=window, $
                                                     BUFFER=buffer)
  


  IF KEYWORD_SET(save_plot) THEN BEGIN
     PRINT,"Saving plot to " + plotName + ' ...'
     window.Save,plotDir+plotName

     window.Close
     window = !NULL
  ENDIF

  IF KEYWORD_SET(print_kappa_gauss_corr) THEN BEGIN
     IF N_ELEMENTS(RBCorrLun) EQ 0 THEN RBCorrLun = -1 ;stdout

     PRINTF,RBCorrLun,"******************************"
     PRINTF,RBCorrLun,potTitleStr1
     PRINTF,RBCorrLun,"******************************"
     PRINT_KAPPA_GAUSS_CORRS_FOR_VARIOUS_MAGRATIOS,AStruct,AStructGauss, $
        obs_current1, $
        kappaPot1,gaussPot1, $
        EXCLUDE_KAPPA_I=excludeK_i, $
        EXCLUDE_GAUSS_I=excludeG_i, $
        DENSITY_KAPPA2D=kappa2D.dens, $
        DENSITY_GAUSS2D=gauss2D.dens, $
        RBCORRLUN=RBCorrLun

     PRINTF,RBCorrLun,''
     PRINTF,RBCorrLun,"******************************"
     PRINTF,RBCorrLun,potTitleStr2
     PRINTF,RBCorrLun,"******************************"
     PRINT_KAPPA_GAUSS_CORRS_FOR_VARIOUS_MAGRATIOS,AStruct,AStructGauss, $
        obs_current2, $
        kappaPot2,gaussPot2, $
        EXCLUDE_KAPPA_I=excludeK_i, $
        EXCLUDE_GAUSS_I=excludeG_i, $
        DENSITY_KAPPA2D=kappa2D.dens, $
        DENSITY_GAUSS2D=gauss2D.dens, $
        RBCORRLUN=RBCorrLun

     PRINTF,RBCorrLun,''
     PRINTF,RBCorrLun,"******************************"
     PRINTF,RBCorrLun,potTitleStr3
     PRINTF,RBCorrLun,"******************************"
     PRINT_KAPPA_GAUSS_CORRS_FOR_VARIOUS_MAGRATIOS,AStruct,AStructGauss, $
        obs_current3, $
        kappaPot3,gaussPot3, $
        EXCLUDE_KAPPA_I=excludeK_i, $
        EXCLUDE_GAUSS_I=excludeG_i, $
        DENSITY_KAPPA2D=kappa2D.dens, $
        DENSITY_GAUSS2D=gauss2D.dens, $
        RBCORRLUN=RBCorrLun

  ENDIF

END

