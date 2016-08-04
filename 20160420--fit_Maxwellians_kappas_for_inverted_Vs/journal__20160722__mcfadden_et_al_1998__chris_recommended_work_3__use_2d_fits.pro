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
PRO JOURNAL__20160722__MCFADDEN_ET_AL_1998__CHRIS_RECOMMENDED_WORK_3__USE_2D_FITS, $
   MAGRATIO=magRatio, $
   USE_JE_CURRENT=use_je_current, $
   USE_JMAG_CURRENT=use_jMag_current, $
   NO_CHARI_FOR_POT=no_charI_for_pot, $
   ALL_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
   ALL_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy, $
   XLOG=xLog, $
   YLOG=yLog, $
   XRANGE=xRange, $
   YRANGE=yRange, $
   EXCLUDE_BAD_FITS=exclude_bad_fits, $
   SAVE_PLOT=save_plot

  COMPILE_OPT IDL2

  orbNum             = 1849

  ;;inFiles
  saveDir            = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile            = '20160713--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'
  fitFile            = '20160721--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'
  fitFile            = '20160722--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--30to30.sav'
  fitFile_noFA       = '20160722--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times_nostart_from_fieldaligned--90to90.sav'
  
  inSaveFile         = '20160713--McFadden_et_al_1998_Fig_1--four_currents.sav'

  ;;outFiles
  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPlotPref        = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--current_vs_pot--2dfit'

  RESTORE,saveDir+fitFile
  RESTORE,saveDir+inSaveFile

  ;;prevent clobbering by next RESTORE
  je_fitFile         = je
  jee_fitFile        = jee

  ;;Defaults
  IF N_ELEMENTS(magRatio) EQ 0 THEN BEGIN
     magRatio        = R_B      ;get from restored saveFile
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
                              obs_current,obsName,obsSuff, $
                              kappaPot,gaussPot, $
                              potName,potTitleStr, $
                              USE_JE_CURRENT=use_je_current, $
                              USE_JMAG_CURRENT=use_jMag_current, $
                              USE_BULKENERGY_POT=use_bulkEnergy_pot, $
                              BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
                              BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy, $
                              NO_CHARI_FOR_POT=no_charI_for_pot
                         
  AStruct.N *=1.5

  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot,gaussPot,magRatio, $
                                   kappa_current,gauss_current,obs_current, $
                                   /MAKE_CURRENTS_POSITIVE
                              
  ;;plotSaveName
  plotExt            = '.png'
  plotName           = STRING(FORMAT='(A0,A0,A0,A0,A0,A0)', $
                              outPlotPref, $
                              '--CURRENT_', $
                              obsSuff, $
                              '--POT_', $
                              potName, $
                              plotExt)

  window             = WINDOW(DIMENSIONS=[1000,750])

  xRange = [2e3,2.5e4]
  xLog   = 1
  yLog   = 0

  SCREEN_KAPPA_MAXWELL_FITS_BEFORE_PLOTTING,kappa_current,gauss_current,obs_current,kappaPot, $
                                            AStruct,AGaussStruct, $
                                            fitStatus,gaussFitStatus, $
                                            MAX_KAPPA=max_kappa, $
                                            EXCLUDE_BAD_FITS=exclude_bad_fits


  plotArr = PLOT_KAPPA_MAXWELL_OBSERVED_CURRENT_VS_POTENTIAL(kappa_current,gauss_current,obs_current,kappaPot, $
                                                             MAGRATIO=magRatio, $
                                                             ORBIT=orbit, $
                                                             POTTITLESTR=potTitleStr, $
                                                             OBSNAME=obsName, $
                                                             ;; POSITION=position, $
                                                             SUPPRESS_TITLE=suppress_title, $
                                                             SUPPRESS_AXIS_TITLES=suppress_axis_titles, $
                                                             SUPPRESS_LEGEND=suppress_legend, $
                                                             SUPPRESS_XTICKMARKS=suppress_xTickMarks, $
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
