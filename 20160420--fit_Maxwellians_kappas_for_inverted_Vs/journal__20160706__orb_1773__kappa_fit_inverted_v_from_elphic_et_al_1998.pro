;;07/06/16
PRO JOURNAL__20160706__ORB_1773__KAPPA_FIT_INVERTED_V_FROM_ELPHIC_ET_AL_1998

  COMPILE_OPT IDL2

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1773__Elphic_et_al_1998_inverted_V'

  ;; Use burst bounds, optionally average
  ;;126  1997-02-07/20:49:41.061
  ;;226  1997-02-07/20:49:48.973
  ;; eeb_or_ees                = 'ees'

  ;; spectra_avg_interval      = 0
  ;; bounds                    = [160:210:50]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                 = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                 = [126:226:2]/spectra_avg_interval

  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  eeb_or_ees             = 'ees'
  bounds                 = [0:40:8]

  min_peak_energy        = 300

  output_density_estimates  = 0
  output_dens__energies     = 0

  output_dens__angles       = 0
  dens_est_eRange           = [30,3.5e4]
  only_dens_estimates       = 0
  ;; out_dens_filePref         = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--density_estimates'

  t1Str                     = '97-02-01/09:26:15'
  t2Str                     = '97-02-01/09:27:10'
  
  t1                        = STR_TO_TIME(t1Str)
  t2                        = STR_TO_TIME(t2Str)

  estimate_A_from_data      = 1
  n_below_peak              = 4
  n_after_peak              = 9

  bulk_offset               = 0

  add_gaussian_estimate     = 1
  add_oneCount_curve        = 1

  add_fitParams_text        = 1
  save_fitPlots             = 1

  ;;Angle stuff
  only_fieldaligned         = 0
  electron_angleRange       = [225,135]

  max_iter                  = 1000
  fit_tol                   = 1e-3

  kappa_est                 = 2.8

  T_est_fac                 = 0.5
  N_est_fac                 = 3.
  bulkE_est_fac             = 1.0

  TGauss_est_fac            = 0.05
  NGauss_est_fac            = 1.2
  bulkEGauss_est_fac        = 0.8

  estFacs                   = {T:T_est_fac, $
                               N:N_est_fac, $
                               B_E:bulkE_est_fac, $
                               TGauss:TGauss_est_fac, $
                               NGauss:NGauss_est_fac, $
                               B_EGauss:bulkEGauss_est_fac}

  KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0__EFLUX_UNITS, $ ;X,A,F,pders, $
     T1=t1, $
     T2=t2, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_avg_interval, $
     SDT_TIME_INDS=bounds, $
     MIN_PEAK_ENERGY=min_peak_energy, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa_est, $
     SDT_DAT=dat, $
     BULK_OFFSET=bulk_offset, $
     ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
     ESTIMATE_FACTORS=estFacs, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
     ADD_ONECOUNT_CURVE=add_oneCount_curve, $
     FIT_TOLERANCE=fit_tol, $
     MAX_ITERATIONS=max_iter, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     ONLY_FIT_FIELDALIGNED_ANGLE=only_fieldaligned, $
     ELECTRON_ANGLERANGE=electron_angleRange, $
     SAVE_FITPLOTS=save_fitPlots, $
     PLOTDIR=plotDir, $
     OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
     OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
     OUTPUT_DENS__ENERGIES=output_dens__energies, $
     OUTPUT_DENS__ANGLES=output_dens__angles, $
     OUT_DENS_STRUCT=out_dens, $
     ONLY_DENS_ESTIMATES=only_dens_estimates
  
END
