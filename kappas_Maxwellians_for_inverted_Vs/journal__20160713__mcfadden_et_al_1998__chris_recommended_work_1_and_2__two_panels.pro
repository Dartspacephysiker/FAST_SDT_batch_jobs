;;07/13/16
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
;3. Then generate a scatter plot with the potential on the x-axis and
;the current on the y-axis. Do this for three cases - one using the 
;total potential as you have already derived, and the
;other two for the potential as given by the Kappa fits and Maxwelllan
;fits. Let’s see if there are any distinctive trends here.
;
;4. The source density is an important parameter here. We need to
;think about it a bit more and how the fitted results are best
;compared with the observations.
;
PRO JOURNAL__20160713__MCFADDEN_ET_AL_1998__CHRIS_RECOMMENDED_WORK_1_AND_2__TWO_PANELS, $
   MAGRATIO1=magRatio1, $
   MAGRATIO2=magRatio2, $
   ;; USE_JE_CURRENT=use_je_current, $
   ;; USE_JMAG_CURRENT=use_jMag_current, $
   ;; USE_CHARE_POT=use_charE_pot, $
   ;; USE_BULKENERGY_POT=use_bulkEnergy_pot, $
   ;; NO_CHARI_FOR_POT=no_charI_for_pot, $
   MAX_KAPPA=max_kappa, $
   EXCLUDE_BAD_FITS=exclude_bad_fits, $
   XLOG=xLog, $
   YLOG=yLog, $
   SAVE_PLOT=save_plot

  COMPILE_OPT IDL2

  orbit              = 1849

  ;;inFiles
  saveDir            = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile            = '20160713--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'
  inSaveFile         = '20160713--McFadden_et_al_1998_Fig_1--four_currents.sav'

  ;;outFiles
  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPlotPref        = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--model_vs_obs--two_panels'

  RESTORE,saveDir+fitFile
  RESTORE,saveDir+inSaveFile

  ;;Defaults
  IF N_ELEMENTS(magRatio1) EQ 0 THEN BEGIN
     magRatio1        = R_B     ;get from restored saveFile
  ENDIF

  IF N_ELEMENTS(magRatio2) EQ 0 THEN BEGIN
     magRatio2        = R_B     ;get from restored saveFile
  ENDIF

  PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                          A=a, $
                          TIME=kappaTime, $
                          STRUCT_A=AStruct, $
                          NAMES_A=ANames, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus

  PARSE_KAPPA_FIT_STRUCTS,gaussFits, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=gaussTime, $
                          NAMES_A=AGaussNames, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus
  
  setup = {kappaS:Astruct, $
           gaussS:AStructGauss, $
           charE:charE_kappa_interp, $
           charI:charI_kappa_interp, $
           charTot:charTot_kappa_interp, $
           Jtot:Jtot_kappa_interp, $
           JMag:jMag_kappa_interp, $
           Je:Je_kappa_interp, $
           Ji:Ji_kappa_interp}

  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current1,obsName1,obsSuff1, $
                              kappaPot1,gaussPot1, $
                              potName1,potTitleStr1 ;, $
                              ;; USE_JE_CURRENT=use_je_current, $
                              ;; USE_JMAG_CURRENT=use_jMag_current, $
                              ;; /USE_CHARE_POT ;, $
                                ;; BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
                                ;; BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy
                                                          ;; NO_CHARI_FOR_POT=no_charI_for_pot
  
  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot1,gaussPot1,magRatio1, $
                                   kappa_current1,gauss_current1,obs_current1, $
                                   /MAKE_CURRENTS_POSITIVE
  SCREEN_KAPPA_MAXWELL_FITS_BEFORE_PLOTTING,kappa_current1,gauss_current1,obs_current1,kappaPot1,gaussPot1, $
                                            AStruct,AGaussStruct, $
                                            fitStatus,gaussFitStatus, $
                                            MAX_KAPPA=max_kappa, $
                                            EXCLUDE_BAD_FITS=exclude_bad_fits
  
  ;;Number two, using bulk energies as electron potential
  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current2,obsName2,obsSuff2, $
                              kappaPot2,gaussPot2, $
                              potName2,potTitleStr2, $
                              ;; USE_JE_CURRENT=use_je_current, $
                              ;; USE_JMAG_CURRENT=use_jMag_current, $
                              /USE_BULKENERGY_POT ;, $
                                ;; BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
                                ;; BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy, $
  ;; NO_CHARI_FOR_POT=no_charI_for_pot
  
  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot2,gaussPot2,magRatio2, $
                                   kappa_current2,gauss_current2,obs_current2, $
                                   /MAKE_CURRENTS_POSITIVE

  SCREEN_KAPPA_MAXWELL_FITS_BEFORE_PLOTTING,kappa_current2,gauss_current2,obs_current2,kappaPot2,gaussPot2, $
                                            AStruct,AGaussStruct, $
                                            fitStatus,gaussFitStatus, $
                                            MAX_KAPPA=max_kappa, $
                                            EXCLUDE_BAD_FITS=exclude_bad_fits
  ;;plotSaveName
  plotExt            = '.png'
  plotName           = STRING(FORMAT='(A0,A0,A0,A0,A0,A0,A0,A0,A0,A0)', $
                              outPlotPref, $
                              '--CURRENT1_', $
                              obsSuff1, $
                              '--POT1_', $
                              potName1, $
                              '--CURRENT2_', $
                              obsSuff2, $
                              '--POT2_', $
                              potName2, $
                              plotExt)

  plotTitle1 = 'Model Currents vs. ' + obsName1 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')
  plotTitle2 = 'Model Currents vs. ' + obsName2 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')

  ;; xTitle    = 'Observed Current (' + CGGREEK('mu') + 'A/m!U2!N)'
  ;; yTitle    = 'Model Current (' + CGGREEK('mu') + 'A/m!U2!N)'
  ;; xTitle    = 'Observed Current (microA/m!U2!N)'
  xTitle    = obsName1 + ' Current (microA/m!U2!N)'
  yTitle    = 'Model Current (microA/m!U2!N)'

  WINDOW_CUSTOM_SETUP,NPLOTROWS=2, $
                      NPLOTCOLUMNS=1, $
                      WINDOW_TITLE=(KEYWORD_SET(orbit) ? 'FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) : ''), $
                      SPACE_VERT_BETWEEN_ROWS=0.04, $
                      SPACE_FOR_ROW_NAMES=0.06, $
                      SPACE__WINTITLE=0.05, $
                      SPACE__XTITLE=0.08, $
                      SPACE__YTITLE=0.06, $
                      XTITLE=xTitle, $
                      YTITLE=yTitle, $
                      ;; ROW_NAMES=[plotTitle1,plotTitle2], $
                      ROW_NAMES=[potTitleStr1,potTitleStr2], $
                      /MAKE_NEW, $
                      CURRENT_WINDOW=window, $
                      WINDOW_DIMENSIONS=[800,900]


  xLog     = 0
  yLog     = 0

  xRange   = [0,2.5]

  yRange   = [0,2.5]
  ;; yRange   = [0,7.5]

  plotArr1 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current1,gauss_current1,obs_current1, $
                                                     MAGRATIO=magRatio1, $
                                                     ;; ORBIT=orbit, $
                                                     ;; POTTITLESTR=potTitleStr1, $
                                                     ;; USE_POTSTR_AS_TITLE=use_potStr_as_title, $
                                                     ;; /USE_POTSTR_AS_TITLE, $
                                                     OBSNAME=obsName1, $
                                                     ;; PLOTNAME=plotName, $
                                                     POSITION=WINDOW_CUSTOM_NEXT_POS(), $
                                                     ;; POSITION=position, $
                                                     ;; SUPPRESS_AXIS_TITLES=suppress_axis_titles, $
                                                     ;; SUPPRESS_LEGEND=suppress_legend, $
                                                     ;; SUPPRESS_TITLE=suppress_title, $
                                                     /SUPPRESS_TITLE, $
                                                     /SUPPRESS_AXIS_TITLES, $
                                                     /SUPPRESS_XTICKMARKS, $
                                                     ;; /SUPPRESS_LEGEND, $
                                                     LEGEND__FONT_SIZE=12, $
                                                     XLOG=xLog, $
                                                     YLOG=yLog, $
                                                     XRANGE=xRange, $
                                                     YRANGE=yRange, $
                                                     WINDOW=window, $
                                                     BUFFER=buffer)
  
  plotArr2 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current2,gauss_current2,obs_current2, $
                                                     MAGRATIO=magRatio2, $
                                                     ;; ORBIT=orbit, $
                                                     ;; POTTITLESTR=potTitleStr2, $
                                                     ;; USE_POTSTR_AS_TITLE=use_potStr_as_title, $
                                                     ;; /USE_POTSTR_AS_TITLE, $
                                                     OBSNAME=obsName2, $
                                                     ;; PLOTNAME=plotName, $
                                                     POSITION=WINDOW_CUSTOM_NEXT_POS(/NEXT_ROW), $
                                                     ;; POSITION=position, $
                                                     ;; SUPPRESS_AXIS_TITLES=suppress_axis_titles, $
                                                     ;; SUPPRESS_LEGEND=suppress_legend, $
                                                     ;; SUPPRESS_TITLE=suppress_title, $
                                                     /SUPPRESS_TITLE, $
                                                     /SUPPRESS_AXIS_TITLES, $
                                                     /SUPPRESS_LEGEND, $
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

END

