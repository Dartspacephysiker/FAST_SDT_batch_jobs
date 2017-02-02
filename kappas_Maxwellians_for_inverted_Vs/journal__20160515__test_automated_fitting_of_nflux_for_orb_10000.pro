;2016/05/15 Here we are 
PRO JOURNAL__20160515__TEST_AUTOMATED_FITTING_OF_NFLUX_FOR_ORB_10000

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_10000'

  t1Str = '99-3-2/18:08:36'
  t2Str = '99-3-2/18:09:00'
  
  t1    = STR_TO_TIME(t1Str)
  t2    = STR_TO_TIME(t2Str)

  bounds = [4:16]

  estimate_A_from_data = 1
  n_below_peak         = 2
  n_after_peak         = 12
  check_for_higher_flux_peaks = 0

  add_gaussian         = 1
  add_fitParams_text   = 1
  save_fitPlots        = 1

  max_iter             = 200
  fit_tol              = 1e-2

  KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0, $ ;X,A,F,pders, $
     T1=t1, $
     T2=t2, $
     SDT_TIME_INDS=bounds, $
     DENSITY_EST=n_est, $
     TEMPERATURE_EST=T, $
     KAPPA_EST=kappa, $
     SDT_DAT=dat, $
     ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks, $
     ADD_GAUSSIAN_ESTIMATE=add_gaussian, $
     FIT_TOLERANCE=fit_tol, $
     MAX_ITERATIONS=max_iter, $
     ADD_FITPARAMS_TEXT=add_fitParams_text, $
     SAVE_FITPLOTS=save_fitPlots, $
     PLOTDIR=plotDir
  

END