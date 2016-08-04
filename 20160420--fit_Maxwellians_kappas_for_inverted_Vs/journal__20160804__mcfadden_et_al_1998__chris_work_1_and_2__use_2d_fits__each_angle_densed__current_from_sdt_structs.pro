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
; In this pro, we're simply plotting the current obtained from the SDT routine J_2D_FS
PRO JOURNAL__20160804__MCFADDEN_ET_AL_1998__CHRIS_WORK_1_AND_2__USE_2D_FITS__EACH_ANGLE_DENSED__CURRENT_FROM_SDT_STRUCTS, $
   USE_JE_CURRENT=use_je_current, $
   USE_JMAG_CURRENT=use_jMag_current, $
   HIGHDENS_THRESH=highDens_thresh, $
   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
   XLOG=xLog, $
   YLOG=yLog, $
   SAVE_PLOT=save_plot

  COMPILE_OPT IDL2

  orbit               = 1849

  ;;What we were using in the journal from the other day to generate these fit files
  eAngleCharE        = [330,30]
  energy_electrons   = [100.,36000.]

  ;;Defaults
  ;; defR_B              = 3.0
  ;; IF N_ELEMENTS(magRatio1) EQ 0 THEN BEGIN
  ;;    magRatio1        = defR_B
  ;; ENDIF

  KAPPA_FITFILE_STRING,outSuff, $
                       ;; R_B=magRatio1, $
                       USE_DATA_DENS=use_data_dens, $
                       /SDT_CALC__NO_MODEL, $
                       LKAPPA_THRESH=lKappa_thresh, $
                       HKAPPA_THRESH=hKappa_thresh, $
                       HIGHDENS_THRESH=highDens_thresh

  ;;inFiles
  saveDir            = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile            = '20160804--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'

  fitFile            = '20160804--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150.sav'
  outSuff            = '--150_to_150'

  inSaveFile         = '20160804--McFadden_et_al_1998_Fig_1--four_currents--2dfits' + outSuff + '.sav'

  ;;outFiles
  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPlotPref        = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--model_vs_obs--2DFit_SDT_calc'+outSuff

  ;;Comes with fit2dgauss_inf_list, fit2dkappa_inf_list, gaussfits, je, jee, kappafits, synthpackage
  RESTORE,saveDir+fitFile

  ;;prevent clobbering by next RESTORE
  je_fitFile         = je
  jee_fitFile        = jee

  ;;Comes with chare, chare_kappa_interp, chari, chari_kappa_interp, chartot, chartot_kappa_interp, 
  ;;                  esa_name, jesa, je_kappa_interp, ji_kappa_interp, jmag, jmag_kappa_interp, jtot_kappa_interp, kappastr, 
  ;;                  kappa_current, maxwell_current, R_B
  RESTORE,saveDir+inSaveFile

  kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DKappa_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                                   /DESTROY_INFO_LIST)

  gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DGauss_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=100.1, $
                                                   /DESTROY_INFO_LIST)

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
                              USE_JMAG_CURRENT=use_jMag_current, $
                              /BOTH_USE_KAPPA_BULKENERGY

  ;; kappaPotBPD      = CREATEBOXPLOTDATA(AStruct.bulk_energy)
  ;; gaussPotBPD      = CREATEBOXPLOTDATA(AStructGauss.bulk_energy)

  ;; BP               = BOXPLOT([kappaPotBPD,gaussPotBPD],XTITLE="Potential (V)")
  
  GET_2DFIT_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
                                         kappa_current1,gauss_current1, $
                                         ENERGY_ELECTRONS=energy_electrons, $
                                         ANGLE=eAngleCharE

  KAPPA_FLIP_CURRENT,kappa_current1,gauss_current1,obs_current1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now for plotting

  plotExt            = '.png'
  plotName           = STRING(FORMAT='(A0,A0,A0,A0)', $
                              outPlotPref, $
                              '--CURRENT1_', $
                              obsSuff1, $
                              plotExt)

  plotTitle1 = 'Model Currents vs. ' + obsName1 + ' Current' + $
               (KEYWORD_SET(orbit) ? '!C(FAST orbit ' +STRCOMPRESS(orbit,/REMOVE_ALL) + ')' : '')

  xTitle    = obsName1 + ' Current (microA/m!U2!N)'
  yTitle    = 'Model Current (microA/m!U2!N)'

  WINDOW_CUSTOM_SETUP,NPLOTROWS=1, $
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
                      ROW_NAMES=potTitleStr1, $
                      /MAKE_NEW, $
                      CURRENT_WINDOW=window, $
                      ;; WINDOW_DIMENSIONS=[800,900]
                      WINDOW_DIMENSIONS=[1000,800]


  xLog     = 0
  yLog     = 0

  xRange   = [0,2.5]

  yRange   = [0,2.5]

  plotArr1 = PLOT_KAPPA_MAXWELL_AND_OBSERVED_CURRENT(kappa_current1,gauss_current1,obs_current1, $
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
                                                     /FOR_INTEGRATED_2DFIT_CURRENTS, $
                                                     WINDOW=window, $
                                                     BUFFER=buffer)
  
  IF KEYWORD_SET(save_plot) THEN BEGIN
     PRINT,"Saving plot to " + plotName + ' ...'
     window.Save,plotDir+plotName

     window.Close
     window = !NULL
  ENDIF

END

