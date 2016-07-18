;2016/06/30 OK, we think we know what's up with the densities. Now we want to fit to various ranges of pitch angles para validar el modelo.
PRO JOURNAL__20160630__ORB_1843__FIT_INCREASING_PITCH_ANGLE_SPREAD

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1843'

  ;; Use burst bounds, optionally average
  ;;126  1997-02-07/20:49:41.061
  ;;226  1997-02-07/20:49:48.973
  eeb_or_ees                = 'eeb'
  ;; bounds                 = [126:226:2]
  spectra_avg_interval      = 4
  ;; bounds                 = [0:4]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]+1
  bounds                    = 0
  ;; bounds                 = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                 = [126:226:2]/spectra_avg_interval

  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees             = 'ees'
  ;; bounds                 = [16:28]

  ;;Angle stuff
  append_fullRange          = 1
  var_delta                 = 10
  nAngleRanges              = 17
  offset                    = 0
  electron_angleRanges      = [[(INDGEN(nAngleRanges)+1+offset)*(-var_delta)], $
                               [(INDGEN(nAngleRanges)+1+offset)*( var_delta)]]

  ;;Append the full meal
  IF KEYWORD_SET(append_fullRange) THEN BEGIN
     electron_angleRanges   = [[electron_angleRanges],TRANSPOSE([0,360])]
     nAngleRanges++
  ENDIF

  output_density_estimates  = 1
  output_dens__energies     = 0

  output_dens__angles       = 1
  dens_est_eRange           = [30,3.5e4]
  ;; dens_est_eRange        = [1.3e3,1.7e4]
  only_dens_estimates       = 0
  ;; out_dens_filePref      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--density_estimates'

  t1Str                     = '97-02-07/20:49:47.1'
  t2Str                     = '97-02-07/20:49:49'
  
  t1                        = STR_TO_TIME(t1Str)
  t2                        = STR_TO_TIME(t2Str)

  estimate_A_from_data      = 1
  n_below_peak              = 4
  n_after_peak              = 10

  bulk_offset               = 0

  add_gaussian_estimate     = 1
  add_oneCount_curve        = 1

  add_fitParams_text        = 1
  save_fitPlots             = 1

  max_iter                  = 1000
  fit_tol                   = 1e-3

  kappa_est                 = 2.8

  T_est_fac                 = 0.4
  N_est_fac                 = 2.
  bulkE_est_fac             = 1.0

  TGauss_est_fac            = 0.05
  NGauss_est_fac            = 3.0
  bulkEGauss_est_fac        = 0.8

  estFacs                   = {T:T_est_fac, $
                               N:N_est_fac, $
                               B_E:bulkE_est_fac, $
                               TGauss:TGauss_est_fac, $
                               NGauss:NGauss_est_fac, $
                               B_EGauss:bulkEGauss_est_fac}

  FOR iRange=0,nAngleRanges-1 DO BEGIN
     electron_angleRange    = REFORM(electron_angleRanges[iRange,*])

     time_for_dens_output   = (iRange EQ nAngleRanges-1)
     IF time_FOR_dens_output THEN PRINT,'Time for densities, seäor'

     KAPPA_EFLUX_FIT, $ ;X,A,F,pders, $
        T1=t1, $
        T2=t2, $
        EEB_OR_EES=eeb_or_ees, $
        SPECTRA_AVERAGE_INTERVAL=spectra_avg_interval, $
        SDT_TIME_INDS=bounds, $
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
        OUTPUT_DENSITY_ESTIMATES=KEYWORD_SET(time_for_dens_output), $
        OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
        OUTPUT_DENS__ENERGIES=output_dens__energies, $
        OUTPUT_DENS__ANGLES=output_dens__angles, $
        OUT_DENS_STRUCT=out_dens, $
        OUT_PEAK_DENS_STRUCT=out_peak_dens, $
        ;; OUT_DENS_FILEPREF=out_dens_filePref, $
        ONLY_DENS_ESTIMATES=only_dens_estimates, $
        OUT_FITTED_PARAMS=out_fitted_params, $
        OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
        OUT_ERANGE_PEAK=out_eRange_peak, $
        OUT_PARAMSTR=out_paramStr, $
        TXTOUTPUTDIR=txtOutputDir

  ENDFOR

  ;; fit_dens                  = {loopType:out_N_str, $
  ;;                              vars:out_N_loop, $
  ;;                              N:out_N_ratios, $
  ;;                              N_range:[MIN(out_N_ratios),MAX(out_N_ratios)], $
  ;;                              N_delta:(out_n_ests[1:-1]-out_n_ests[0:-2]), $
  ;;                              var_delta:var_delta, $
  ;;                              var_dim:dim, $
  ;;                              is_multiplicative:KEYWORD_SET(output_dens__energies), $
  ;;                              fName_suff:out_N_fN_str}

  IF ~ARRAY_EQUAL(out_peak_dens.vars[0:nAngleRanges-1,*],electron_angleRanges) THEN BEGIN
     PRINT,"Angle ranges don't match!"
     STOP
  ENDIF

  fit_N                     = out_fitted_params[3,*]
  fit_N_Gauss               = out_fitted_Gauss_params[3,*]
  fit_dens                  = {loopType:'Angles (deg)', $
                               energies:out_eRange_peak, $
                               angles:TRANSPOSE(electron_angleRanges), $
                               N:REFORM(fit_N), $
                               N_Gauss:REFORM(fit_N_Gauss), $
                               N_SDT:out_peak_dens.N, $
                               N_range:[MIN(fit_N),MAX(fit_N)], $
                               N_delta:fit_N[1:-1]-fit_N[0:-2], $
                               var_delta:var_delta, $
                               var_dim:2, $
                               is_multiplicative:0, $
                               fName_suff:'fit_densities'}

  ;; fit_dens.loopType         = 'Angles (deg)'
  ;; fit_dens.fName_suff       = 'fit_densities'
  ;; fit_dens.N                = REFORM(out_fitted_params[3,*])
  ;; fit_dens.N_range          = [MIN(out_N_ratios),MAX(out_N_ratios)]
  ;; fit_dens.N_delta          = fit_dens.N[1:-1]-fit_dens.N[0:-2]

  fit_densFN                = STRING(FORMAT='(A0,"--fit_density_ests--",A0,".txt")', $
                                     GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                     out_paramStr)

  PRINT_DENS_ESTIMATE_STRUCT_V2,fit_dens, $
                             TO_FILE=fit_densFN, $
                             OUTDIR=txtOutputDir

END
