;2016/05/15 Here we are 
PRO JOURNAL__20160515__TEST_AUTOMATED_FITTING_OF_NFLUX_FOR_ORB_1773

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1773'

  t1Str = '97-2-1/09:26:20'
  t2Str = '97-2-1/09:26:50'
  
  t1    = STR_TO_TIME(t1Str)
  t2    = STR_TO_TIME(t2Str)

  bounds = [3:22]
  ;; bounds = 5

  estimate_A_from_data = 1
  n_below_peak         = 5
  n_after_peak         = 10

  bulk_offset          = 0

  add_gaussian_estimate = 1
  add_fitParams_text   = 1
  save_fitPlots        = 1

  max_iter             = 100
  fit_tol              = 1e-1

  kappa_est            = 3.0

  KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0, $ ;X,A,F,pders, $
     T1=t1, $
     T2=t2, $
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
     SAVE_FITPLOTS=save_fitPlots, $
     PLOTDIR=plotDir
  

END
