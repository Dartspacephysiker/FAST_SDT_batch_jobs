PRO JOURNAL__20160712__ORB_1773__KAPPA_FIT_INVERTED_V_FROM_ELPHIC_ET_AL_1998__W_SYNTHETIC_SDT_STRUCTS

  COMPILE_OPT IDL2

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1773__McFadden_et_al_inverted_V'

  try_synthetic_SDT_struct     = 1

  outDir                       = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile                      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--Elphic_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'
  ;; Use burst bounds, optionally average
  ;;126  1997-02-07/20:49:41.061
  ;;226  1997-02-07/20:49:48.973
  ;; eeb_or_ees                   = 'eeb'

  ;; spectra_avg_interval         = 0
  ;; bounds                    = [160:210:50]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:226:2]/spectra_avg_interval

  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees                = 'ees'
  ;; bounds                    = [16:28]

  do_all_times                 = 1
  add_full_fits                = 1
  fit_each_angle               = 0

  energy_electrons             = [3e1,3.6e4]
  min_peak_energy              = 100

  output_density_estimates     = 0
  output_dens__energies        = 0

  output_dens__angles          = 0
  dens_est_eRange              = [30,3.5e4]
  only_dens_estimates          = 0

  ;; t1Str                        = '97-02-01/09:26:15'
  ;; t2Str                        = '97-02-01/09:27:10'
  
  t1Str                        = '97-02-01/09:26:00'
  t2Str                        = '97-02-01/09:27:20'

  t1                           = STR_TO_TIME(t1Str)
  t2                           = STR_TO_TIME(t2Str)

  estimate_A_from_data         = 1
  n_below_peak                 = 2
  n_after_peak                 = 7
  dont_fit_below_thresh_value  = 1
  bulk_offset                  = 0

  add_gaussian_estimate        = 1
  use_SDT_Gaussian_fit         = 0
  add_oneCount_curve           = 1

  no_plots                     = 1
  save_fitPlots                = 0
  plot_full_fit                = 1
  add_fitParams_text           = 1

  ;;Angle stuff
  only_fieldaligned            = 0
  electron_angleRange          = [-135,135]
  ;; electron_angleRange          = [-14,14]

  max_iter                     = 1000
  fit_tol                      = 1e-2

  kappa_est                    = 1.9

  T_est_fac                    = 0.3
  N_est_fac                    = 7.0
  bulkE_est_fac                = 1.0

  TGauss_est_fac               = 0.05
  NGauss_est_fac               = 1.0
  bulkEGauss_est_fac           = 1.0

  estFacs                      = {T:T_est_fac, $
                                  N:N_est_fac, $
                                  B_E:bulkE_est_fac, $
                                  TGauss:TGauss_est_fac, $
                                  NGauss:NGauss_est_fac, $
                                  B_EGauss:bulkEGauss_est_fac}

  KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0__EFLUX_UNITS, $ ;X,A,F,pders, $
     T1=t1, $
     T2=t2, $
     ENERGY_ELECTRONS=energy_electrons, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_avg_interval, $
     FIT_EACH_ANGLE=fit_each_angle, $
     SDT_TIME_INDS=bounds, $
     DO_ALL_TIMES=do_all_times, $
     MIN_PEAK_ENERGY=min_peak_energy, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa_est, $
     SDT_DAT=dat, $
     BULK_OFFSET=bulk_offset, $
     ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
     ESTIMATE_FACTORS=estFacs, $
     DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
     USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
     ADD_ONECOUNT_CURVE=add_oneCount_curve, $
     FIT_TOLERANCE=fit_tol, $
     MAX_ITERATIONS=max_iter, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     ONLY_FIT_FIELDALIGNED_ANGLE=only_fieldaligned, $
     ELECTRON_ANGLERANGE=electron_angleRange, $
     NO_PLOTS=no_plots, $
     SAVE_FITPLOTS=save_fitPlots, $
     PLOT_FULL_FIT=plot_full_fit, $
     PLOTDIR=plotDir, $
     OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
     OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
     OUTPUT_DENS__ENERGIES=output_dens__energies, $
     OUTPUT_DENS__ANGLES=output_dens__angles, $
     OUT_DENS_STRUCT=out_dens, $
     ONLY_DENS_ESTIMATES=only_dens_estimates, $
     OUT_FITTED_PARAMS=out_fitted_params, $
     OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
     OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
     OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
     TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
     OUT_SYNTHETIC_SDT_KAPPA_STRUCTS=synthStr_SDT_kappa, $
     OUT_SYNTHETIC_SDT_GAUSS_STRUCTS=synthStr_SDT_gauss, $
     ADD_FULL_FITS=add_full_fits


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

  PRINT,'Saving ' + fitFile + ' ...'
  IF N_ELEMENTS(synthStr_SDT_kappa) GT 0 THEN BEGIN
     SAVE,out_kappa_fit_structs,out_gauss_fit_structs, $
          je,jee, $
          synthStr_SDT_kappa,synthStr_SDT_gauss, $
          FILENAME=outDir+fitFile
  ENDIF ELSE BEGIN
     SAVE,out_kappa_fit_structs,out_gauss_fit_structs, $
          je,jee, $
          FILENAME=outDir+fitFile
  ENDELSE

  fitStatus                    = !NULL
  gaussFitStatus               = !NULL
  FOR i=0,N_ELEMENTS(out_kappa_fit_structs)-1 DO BEGIN
     fitStatus                 = [fitStatus,out_kappa_fit_structs[i].fitStatus]
     gaussFitStatus            = [gaussFitStatus,out_gauss_fit_structs[i].fitStatus]
  ENDFOR
  badFits_i                    = WHERE(fitStatus GT 0,nBadFits)  
  badGaussFits_i               = WHERE(gaussFitStatus GT 0,nBadGaussFits)  

  PRINT,""
  PRINT,"****************************************"
  PRINT,'NTotalFits    : ',N_ELEMENTS(fitStatus)
  PRINT,''
  PRINT,"NbadFits      : ",nBadFits
  PRINT,"NbadGaussFits : ",nBadGaussFits
  PRINT,"NBothBad      : ",N_ELEMENTS(CGSETINTERSECTION(badFits_i,badGaussFits_i))

  PARSE_KAPPA_FIT_STRUCTS,out_kappa_fit_structs, $
                          A=a, $
                          STRUCT_A=Astruct, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus  

  PARSE_KAPPA_FIT_STRUCTS,out_gauss_fit_structs, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus  

  STOP
  

END
