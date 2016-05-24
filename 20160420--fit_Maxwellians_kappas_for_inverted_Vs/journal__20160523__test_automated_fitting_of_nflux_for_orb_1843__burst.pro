;2016/05/15 Here we are 
PRO JOURNAL__20160523__TEST_AUTOMATED_FITTING_OF_NFLUX_FOR_ORB_1843__BURST

  ;; Use burst bounds, optionally average
  ;;126  1997-02-07/20:49:41.061
  ;;226  1997-02-07/20:49:48.973
  eeb_or_ees             = 'eeb'
  ;; bounds                 = [126:226:2]
  spectra_avg_interval   = 4
  bounds                 = [126:226]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                 = [126:226:2]/spectra_avg_interval

  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees             = 'ees'
  ;; bounds                 = [16:28]

  t1Str                  = '97-02-07/20:49:31'
  t2Str                  = '97-02-07/20:50:19'
  
  t1                     = STR_TO_TIME(t1Str)
  t2                     = STR_TO_TIME(t2Str)

  estimate_A_from_data   = 1
  n_below_peak           = 5
  n_after_peak           = 10

  bulk_offset            = 0

  add_gaussian_estimate  = 1
  add_fitParams_text     = 1
  save_fitPlots          = 1

  max_iter               = 100
  fit_tol                = 1e-1

  kappa_est              = 3.0

  KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0, $ ;X,A,F,pders, $
     T1=t1, $
     T2=t2, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_avg_interval, $
     SDT_TIME_INDS=bounds, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa, $
     SDT_DAT=dat, $
     BULK_OFFSET=bulk_offset, $
     ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
     FIT_TOLERANCE=fit_tol, $
     MAX_ITERATIONS=max_iter, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     SAVE_FITPLOTS=save_fitPlots
  

END
