;;2016/10/08 So now we're selecting four orbits with which to do fits.

PRO JOURNAL__20161008__ORBIT_1849__OUTRIGHT_2DFIT

  COMPILE_OPT IDL2

  orbit                = 1773 ;for outplot
  show_post_plots      = 1
  save_kappa_plot      = 1

  restore_fitFile      = 0
  SET_PLOT_DIR,plotDir, $
               /FOR_SDT, $
               /ADD_TODAY, $
               ADD_SUFF='/kappa_fits/Orbit_1849__McFadden_et_al_inverted_V--outright_2d'

  outDir               = '~/software/sdt/batch_jobs/saves_output_etc/'

  ;; fitFile           = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150--mpfitfun1d--saveme.sav'
  fitFile              = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150--mpfitfun1d--notindividualfit.sav'

  save_diff_eFlux_to_file = 'orb_1849--diff_eflux--eeb--output_from_get_losscone_and_eflux_data.sav'
  ;; diff_eFlux_file   = 'orb_1849--diff_eflux--eeb--output_from_get_losscone_and_eflux_data.sav'

  eeb_or_ees           = 'eeb'

  electron_angleRange  = [-35,35]

  spectra_average_interval  = 6
  ;; bounds                    = [160:210:50]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; bounds                    = [126:138]/spectra_avg_interval & bounds  = bounds[uniq(bounds)]
  ;; ;; bounds                    = [126:226:2]/spectra_avg_interval

  ;; bounds                       = [87,88,89,90,91,95,99]
  ;; bounds  = INDGEN(5)
  ;; Use survey bounds
  ;; 16  1997-02-07/20:49:41.338
  ;; 28  1997-02-07/20:49:48.934
  ;; eeb_or_ees                = 'ees'
  ;; bounds                    = [46:54:spectra_avg_interval]

  do_all_times                 = 1
  add_full_fits                = 1
  fit2D__only_fit_peak_eRange  = 0
  fit2D__only_fit_aboveMin     = 1
  fit2D__keep_wholeFit         = 1  
  fit2D__bulk_e_anisotropy     = 1
  fit2D__exclude_lca_from_fit   = 1
  fit2D__disable_bFunc          = 1
  ;; fit2D__bulk_e_anis_factor    = 0.3

  use_mpFit1D                  = 1

  fit1D__skip_bad_fits         = 1
  fit1D__show_and_prompt       = 0
  fit2D__show_each_candidate   = 0
  fit2D__add_boundaries         = 1
  fit_fail__user_prompt        = 0

  synthPackage                 = 1
  average_over_angleRange      = 1

  energy_electrons             = [3e1,3.3e4]
  ;; electron_lca                 = 40
  electron_lca                 = [150,-150]
  min_peak_energy              = 800

  t1Str                        = '97-02-08/10:11:22'
  ;; t1Str                        = '97-02-08/10:11:36'
  ;; t2Str                        = '97-02-08/10:11:52'
  t2Str                        = '97-02-08/10:11:52'

  t1                           = STR_TO_TIME(t1Str)
  t2                           = STR_TO_TIME(t2Str)

  estimate_A_from_data         = 1
  dont_print_estimates         = 1
  dont_print_fitinfo           = 1
  print_2DFitInfo              = 1

  n_below_peak                 = 3
  n_above_peak                 = 7
  n_below_peak2D               = 3
  n_above_peak2D               = 7
  dont_fit_below_thresh_value  = 0
  bulk_offset                  = 0

  add_gaussian_estimate        = 1
  add_oneCount_curve           = 0

  no_plots                     = 1
  save_fitPlots                = 1
  saveData                     = 1
  plot_full_fit                = 1
  add_fitParams_text           = 1
  add_angle_label              = 1

  max_iter                     = 4000
  fit2D_max_iter               = 4000

  fit_tol                      = 1e-3
  fit2D_tol                    = 1e-4

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
;;... And strings!!!!
  plotNamePref                  = ''
  CASE 1 OF
     KEYWORD_SET(fit2D__only_fit_peak_eRange): BEGIN
        plotNamePref += '--only_fit_peak_eRange'
     END
     KEYWORD_SET(fit2D__only_fit_aboveMin): BEGIN
        plotNamePref += STRING(FORMAT='("--fit_above_",I0,"_eV")',min_peak_energy)
     END
     ELSE: BEGIN
     END
  ENDCASE
  
  IF KEYWORD_SET(fit2D__disable_bFunc) THEN BEGIN
     plotNamePref    += '--No_bFunc'
  ENDIF

  IF KEYWORD_SET(fit2D__exclude_lca_from_fit) THEN BEGIN
     plotNamePref    += '--exc_LCA'
  ENDIF

  fitFile                       = fitFile + plotNamePref + '.sav'

  IF KEYWORD_SET(restore_fitFile) THEN BEGIN
     PRINT,'Restoring ' + fitFile + ' ...'
     RESTORE,outDir+fitFile
  ENDIF ELSE BEGIN
     KAPPA_EFLUX_FIT2D, $
        T1=t1, $
        T2=t2, $
        SDT_TIME_INDS=bounds, $
        DO_ALL_TIMES=do_all_times, $
        ENERGY_ELECTRONS=energy_electrons, $
        LOAD_DAT_FROM_FILE=diff_eFlux_file, $
        LOAD_DIR=outDir, $
        SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
        EEB_OR_EES=eeb_or_ees, $
        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
        MIN_PEAK_ENERGY=min_peak_energy, $
        N_ENERGIES_BELOW_PEAK=n_below_peak, $
        N_ENERGIES_ABOVE_PEAK=n_above_peak, $
        N_BELOW_PEAK2D=n_below_peak2D, $
        N_ABOVE_PEAK2D=n_above_peak2D, $
        CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_higher_peaks_set_peakEn, $
        TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
        DONT_FIT_BELOW_THRESH_VALUE=dont_fit_below_thresh_value, $
        DENSITY_EST=n_est, $
        TEMPERATURE_EST=T, $
        KAPPA_EST=kappa_est, $
        SDT_DAT=dat, $
        ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
        ESTIMATE_FACTORS=estFacs, $
        DONT_PRINT_ESTIMATES=dont_print_estimates, $
        DONT_PRINT_FITINFO=dont_print_fitInfo, $
        FIT1D__MAX_ITERATIONS=max_iter, $
        FIT1D__TOLERANCE=fit_tol, $
        FIT1D__AVERAGE_OVER_ANGLERANGE=average_over_angleRange, $
        FIT1D__SKIP_BAD_FITS=fit1D__skip_bad_fits, $
        FIT1D__SHOW_AND_PROMPT=fit1D__show_and_prompt, $
        FIT1D__USER_PROMPT_ON_FAIL=fit1D_fail__user_prompt, $
        FIT2D__ONLY_FIT_ERANGE_AROUND_PEAK=fit2D__only_fit_peak_eRange, $
        FIT2D__ONLY_FIT_ERANGE_ABOVE_MIN=fit2D__only_fit_aboveMin, $
        FIT2D__SHOW_AND_PROMPT__EACH_CANDIDATE=fit2D__show_each_candidate, $
        FIT2D__PRINT_FITINFO=print_2DFitInfo, $
        FIT2D__TOLERANCE=fit2D_tol, $
        FIT2D__MAX_ITERATIONS=fit2D_max_iter, $
        FIT2D__LOSSCONE_ANGLE=fit2D__lossCone_angle, $
        FIT2D__USE_BULK_E_ANISOTROPY=fit2D__bulk_e_anisotropy, $
        FIT2D__BULK_E_ANISO_FACTOR=fit2D__bulk_e_anis_factor, $
        ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
        USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
        USE_MPFIT1D=use_mpFit1D, $
        ADD_ONECOUNT_CURVE=add_oneCount_curve, $
        ADD_FITPARAMS_TEXT=add_fitParams_text, $
        ADD_ANGLE_LABEL=add_angle_label, $
        ELECTRON_ANGLERANGE=electron_angleRange, $
        ELECTRON_LOSSCONE_ANGLE=electron_lca, $
        NO_PLOTS=no_plots, $
        SAVE_FITPLOTS=save_fitplots, $
        PLOT_FULL_FIT=plot_full_fit, $
        PLOTDIR=plotDir, $
        OUT_FITTED_PARAMS=out_kappaParams, $
        OUT_FITTED_GAUSS_PARAMS=out_gaussParams, $
        OUT_KAPPA_FIT_STRUCTS=kappaFits, $
        OUT_GAUSS_FIT_STRUCTS=gaussFits, $
        OUT_FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
        OUT_FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
        OUT_SYNTH_SDT_STRUCTS=synthPackage, $
        ADD_FULL_FITS=add_full_fits, $
        OUT_ERANGE_PEAK=out_eRange_peak, $
        OUT_PARAMSTR=out_paramStr, $
        OUT_STRINGS=strings, $
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
                          FITSTATUS=fitStatus, $
                          USE_MPFIT1D=use_mpFit1D

  PARSE_KAPPA_FIT_STRUCTS,gaussFits, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=time, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus, $
                          USE_MPFIT1D=use_mpFit1D


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

     IF N_ELEMENTS(fit2DKappa_inf_list) GT 0 THEN BEGIN
        saveStr += 'fit2DKappa_inf_list,'
     ENDIF

     IF N_ELEMENTS(fit2DGauss_inf_list) GT 0 THEN BEGIN
        saveStr += 'fit2DGauss_inf_list,'
     ENDIF

     PRINT,'Saving ' + fitFile + ' ...'

     saveStr += 'FILENAME=outDir+fitFile'
     good     = EXECUTE(saveStr)
  ENDIF

  PRINT,"DONE!"

  ENDELSE

  fit2DK = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                         FIT_TYPE='Kappa') 

  fit2DG = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                         FIT_TYPE='Maxwellian') 

  IF KEYWORD_SET(show_post_plots) THEN BEGIN

     POST_KAPPA2D_FIT_PLOTS,fit2DK,fit2DG,orbit,plotNamePref,plotDir,save_kappa_plot

  ENDIF

  ;; pap   = PLOT(time-time[0],this.fitParams[2,*],YLOG=1,SYMBOL='*',LINESTYLE='')
  PRINT,FORMAT='("(N w/ k ≤ 2.5)/nTot : ",I0,"/",I0)', $
        N_ELEMENTS(WHERE(fit2DK.fitParams[2,*] LE 2.5)), $
        N_ELEMENTS(fit2DKappa_inf_list)

  STOP

END

