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
PRO JOURNAL__20160804__MCFADDEN_ET_AL_1998__CHRIS_RECOMMENDED_WORK_3__USE_2D_FITS, $
   MAGRATIO=magRatio, $
   USE_JE_CURRENT=use_je_current, $
   USE_JMAG_CURRENT=use_jMag_current, $
   NO_CHARI_FOR_POT=no_charI_for_pot, $
   HIGHDENS_THRESH=highDens_thresh, $
   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
   SDT_CALC__NO_MODEL=SDT_calc__no_model, $
   ALL_USE_KAPPA_BULKENERGY=both_use_kappa_bulkenergy, $
   ALL_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkenergy, $
   XLOG=xLog, $
   YLOG=yLog, $
   XRANGE=xRange, $
   YRANGE=yRange, $
   SAVE_PLOT=save_plot

  COMPILE_OPT IDL2

  orbNum             = 1849

  KAPPA_FITFILE_STRING,outSuff, $
                       R_B=magRatio, $
                       ;; USE_DATA_DENS=use_data_dens, $
                       ;; SDT_CALC__NO_MODEL=SDT_calc__no_model, $
                       LKAPPA_THRESH=lKappa_thresh, $
                       HKAPPA_THRESH=hKappa_thresh, $
                       HIGHDENS_THRESH=highDens_thresh

  ;;inFiles
  saveDir            = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile            = '20160804--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150.sav'
  outSuff            = '--150_to_150'

  inSaveFile         = '20160804--McFadden_et_al_1998_Fig_1--four_currents--2dfits' + outSuff + '.sav'

  ;;outFiles
  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPlotPref        = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--current_vs_pot--2dfit'

  RESTORE,saveDir+fitFile

  ;;prevent clobbering by next RESTORE
  je_fitFile         = je
  jee_fitFile        = jee

  RESTORE,saveDir+inSaveFile

  ;;Defaults
  IF N_ELEMENTS(magRatio) EQ 0 THEN BEGIN
     magRatio        = R_B      ;get from restored saveFile
  ENDIF

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
                                                   OUT_GOOD_T=includeK_t)

  gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DGauss_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=100.1, $
                                                   /DESTROY_INFO_LIST, $
                                                   OUT_GOOD_I=includeG_i, $
                                                   OUT_GOOD_T=includeG_t)

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
                         
  GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                   kappaPot,gaussPot,magRatio, $
                                   kappa_current,gauss_current,obs_current, $
                                   DENSITY_KAPPA2D=kappa2D.dens, $
                                   DENSITY_GAUSS2D=gauss2D.dens, $
                                   /QUIET

                              
  KAPPA_FLIP_CURRENT,kappa_current,gauss_current,obs_current


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
