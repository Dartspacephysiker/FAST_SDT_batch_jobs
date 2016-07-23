;;07/21/16 Time to figure it out
PRO JOURNAL__20160722__ORBIT_1849__GET_BUNCHA_2DFITS_THEN_PREDICT_CURRENT__DENS_FROM_LOSSCONE_FIT

  COMPILE_OPT IDL2

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1849__McFadden_et_al_inverted_V'

  outDir                       = '~/software/sdt/batch_jobs/saves_output_etc/'
  ;; fitFile                      = '20160721--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'
  ;; fitFile                      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--40to40.sav'
  fitFile                      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--30to30.sav'
  ;; fitFile                      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'

  eeb_or_ees                   = 'eeb'

  spectra_avg_interval         = 4
  ;; bounds                    = [160:210:50]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:226:2]/spectra_avg_interval

  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees                = 'ees'
  ;; bounds                    = [46:54:spectra_avg_interval]

  do_all_times                 = 1
  add_full_fits                = 1
  fit_each_angle               = 1
  start_from_fieldaligned      = 1
  vary_bulk_energy             = 1

  fit_each__skip_bad_fits      = 1
  fit_each__show_and_prompt    = 1
  fit_fail__user_prompt        = 0
  dont_take_stock_of_bulkangle = 1

  synthPackage                 = 1
  average_over_angleRange      = 0

  energy_electrons             = [3e1,3.6e4]
  min_peak_energy              = 900

  output_density_estimates     = 0
  output_dens__energies        = 0

  output_dens__angles          = 0
  dens_est_eRange              = [30,3.5e4]
  only_dens_estimates          = 0

  t1Str                        = '97-02-08/10:11:22'
  ;; t1Str                        = '97-02-08/10:11:36'
  ;; t2Str                        = '97-02-08/10:11:52'
  t2Str                        = '97-02-08/10:11:52'

  t1                           = STR_TO_TIME(t1Str)
  t2                           = STR_TO_TIME(t2Str)

  estimate_A_from_data         = 1
  dont_print_estimates         = 0

  n_below_peak                 = 2
  n_above_peak                 = 5
  dont_fit_below_thresh_value  = 1
  bulk_offset                  = 0

  add_gaussian_estimate        = 1
  use_SDT_Gaussian_fit         = 0
  add_oneCount_curve           = 1

  no_plots                     = 1
  save_fitPlots                = 1
  saveData                     = 1
  plot_full_fit                = 1
  add_fitParams_text           = 1
  add_angle_label              = 1

  ;;Angle stuff
  only_fieldaligned            = 0
  electron_angleRange          = [-28,28]
  ;; electron_angleRange          = [-40,40]
  ;; fit2D_density_angleRange     = [-180-electron_angleRange[0],180-electron_angleRange[1]]
  fit2D_density_angleRange     = [-30,30]
  ;; fit2D_density_angleRange     = [-40,40]
 ;; electron_angleRange          = [-180,180]

  max_iter                     = 1000
  fit2d_max_iter               = 1000

  fit_tol                      = 1e-3
  fit2d_tol                    = 1e-3

  kappa_est                    = 2.7

  T_est_fac                    = 1.3
  N_est_fac                    = 7.0
  bulkE_est_fac                = 1.0

  TGauss_est_fac               = 0.3
  NGauss_est_fac               = 1.0
  bulkEGauss_est_fac           = 1.0

  estFacs                      = {T:T_est_fac, $
                                  N:N_est_fac, $
                                  B_E:bulkE_est_fac, $
                                  TGauss:TGauss_est_fac, $
                                  NGauss:NGauss_est_fac, $
                                  B_EGauss:bulkEGauss_est_fac}

  KAPPA_EFLUX_FIT2D, $
     T1=t1, $
     T2=t2, $
     ENERGY_ELECTRONS=energy_electrons, $
     LOAD_DAT_FROM_FILE=loadFile, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     FIT_EACH_ANGLE=fit_each_angle, $
     FIT_EACH__AVERAGE_OVER_ANGLERANGE=fit_each__average_over_angleRange, $
     FIT_EACH__SYNTH_SDT_STRUCT=synthPackage, $
     FIT_EACH__SKIP_BAD_FITS=fit_each__skip_bad_fits, $
     FIT_EACH__START_FROM_FIELDALIGNED=start_from_fieldaligned, $
     FIT_EACH__VARY_BULK_ENERGY=vary_bulk_energy, $
     FIT_EACH__SHOW_AND_PROMPT=fit_each__show_and_prompt, $
     FIT_FAIL__USER_PROMPT=fit_fail__user_prompt, $
     SDT_TIME_INDS=bounds, $
     DO_ALL_TIMES=do_all_times, $
     MIN_PEAK_ENERGY=min_peak_energy, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa_est, $
     SDT_DAT=dat, $
     BULK_OFFSET=bulk_offset, $
     DONT_TAKE_STOCK_OF_BULKANGLE=dont_take_stock_of_bulkangle, $
     TREAT_FIELDALIGNED_AS_BULK=treat_fieldaligned_as_bulk, $
     ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
     ESTIMATE_FACTORS=estFacs, $
     DONT_PRINT_ESTIMATES=dont_print_estimates, $
     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
     DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_ABOVE_PEAK=n_above_peak, $
     CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
     FIT_TOLERANCE=fit_tol, $
     FIT2D_TOLERANCE=fit2d_tol, $
     MAX_ITERATIONS=max_iter, $
     FIT2D_MAX_ITERATIONS=fit2d_max_iter, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
     USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
     ADD_ONECOUNT_CURVE=add_oneCount_curve, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     ADD_ANGLE_LABEL=add_angle_label, $
     ELECTRON_ANGLERANGE=electron_angleRange, $
     FIT2D_DENSITY_ANGLERANGE=fit2D_density_angleRange, $
     NO_PLOTS=no_plots, $
     SAVE_FITPLOTS=save_fitplots, $
     PLOT_FULL_FIT=plot_full_fit, $
     PLOTDIR=plotDir, $
     OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
     OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
     OUTPUT_DENS__ENERGIES=output_dens__energies, $
     OUTPUT_DENS__ANGLES=output_dens__angles, $
     OUT_DENS_STRUCT=out_dens, $
     OUT_PEAK_DENS_STRUCT=out_peak_dens, $
     ONLY_DENS_ESTIMATES=only_dens_estimates, $
     OUT_FITTED_PARAMS=out_kappaParams, $
     OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
     OUT_KAPPA_FIT_STRUCTS=kappaFits, $
     OUT_GAUSS_FIT_STRUCTS=gaussFits, $
     OUT_FIT2DKAPPA_INF_LIST=fit2dKappa_inf_list, $
     OUT_FIT2DGAUSS_INF_LIST=fit2dGauss_inf_list, $
     ADD_FULL_FITS=add_full_fits, $
     OUT_ERANGE_PEAK=out_eRange_peak, $
     OUT_PARAMSTR=out_paramStr, $
     TXTOUTPUTDIR=txtOutputDir

  CASE eeb_or_ees OF
     'eeb': BEGIN
        GET_2DT_TS,'j_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons,ANGLE=electron_angleRange
        GET_2DT_TS,'je_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Jee',ENERGY=energy_electrons,ANGLE=electron_angleRange
     END
     'ees': BEGIN
        GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons,ANGLE=electron_angleRange
        GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2,NAME='Jee',ENERGY=energy_electrons,ANGLE=electron_angleRange
     END
  ENDCASE

  GET_DATA,'Je',DATA=je
  GET_DATA,'Jee',DATA=jee

  PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                          A=a, $
                          STRUCT_A=Astruct, $
                          TIME=time, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus  

  PARSE_KAPPA_FIT_STRUCTS,gaussFits, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=time, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus  

  PRINT_KAPPA_LOOP_FIT_SUMMARY,fitStatus,gaussfitStatus


  IF KEYWORD_SET(saveData) THEN BEGIN
     saveStr = 'SAVE,'
     IF N_ELEMENTS(je) GT 0 THEN BEGIN
        saveStr += 'je,'
     ENDIF
     IF N_ELEMENTS(jee) GT 0 THEN BEGIN
        saveStr += 'jee,'
     ENDIF

     IF N_ELEMENTS(electron_angleRange) GT 0 THEN BEGIN
        saveStr += 'electron_angleRange,'
     ENDIF

     IF N_ELEMENTS(energy_electrons) GT 0 THEN BEGIN
        saveStr += 'energy_electrons,'
     ENDIF

     IF N_ELEMENTS(kappaFits) GT 0 THEN BEGIN
        saveStr += 'kappaFits,'
     ENDIF

     IF N_ELEMENTS(gaussFits) GT 0 THEN BEGIN
        saveStr += 'gaussFits,'
     ENDIF

     ;; IF N_ELEMENTS(synthStr_SDT_kappa) GT 0 THEN BEGIN
     ;;    saveStr += 'synthStr_SDT_kappa,'
     ;; ENDIF
     ;; IF N_ELEMENTS(synthStr_SDT_gauss) GT 0 THEN BEGIN
     ;;    saveStr += 'synthStr_SDT_gauss,'
     ;; ENDIF

     IF N_ELEMENTS(synthPackage) GT 0 THEN BEGIN
        saveStr += 'synthPackage,'
     ENDIF

     IF N_ELEMENTS(strings) GT 0 THEN BEGIN
        saveStr += 'strings,'
     ENDIF

     IF N_ELEMENTS(fit2dKappa_inf_list) GT 0 THEN BEGIN
        saveStr += 'fit2dKappa_inf_list,'
     ENDIF

     IF N_ELEMENTS(fit2dGauss_inf_list) GT 0 THEN BEGIN
        saveStr += 'fit2dGauss_inf_list,'
     ENDIF

     PRINT,'Saving ' + fitFile + ' ...'

     saveStr += 'FILENAME=outDir+fitFile'
     good     = EXECUTE(saveStr)
  ENDIF

  PRINT,"DONE!"

  STOP

END

