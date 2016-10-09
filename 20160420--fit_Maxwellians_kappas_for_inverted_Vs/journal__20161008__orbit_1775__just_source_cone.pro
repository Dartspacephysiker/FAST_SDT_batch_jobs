;;2016/09/08 Right … so I guess I haven't been fitting just within the source cone. How now?

PRO JOURNAL__20161008__ORBIT_1775__JUST_SOURCE_CONE

  COMPILE_OPT IDL2

  fit2D__show_each_candidate = 0

  orbit                = 1775 ;for outplot
  show_post_plots      = 1
  save_kappa_plot      = 0

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 0
  restore_fitFile      = 0

  ;;survey window
  ;; eeb_or_ees           = 'ees'
  ;; t1Str                = '97-02-01/13:53:10'
  ;; t2Str                = '97-02-01/13:56:15'

  ;;burst window 1
  eeb_or_ees           = 'eeb'
  t1Str                = '97-02-01/16:15:07'
  t2Str                = '97-02-01/16:15:25'

  ;;burst window 2
  eeb_or_ees           = 'eeb'
  t1Str                = '97-02-01/16:15:48'
  t2Str                = '97-02-01/16:16:05'

  ;; spectra_average_interval = 8

  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 500

  electron_angleRange  = [-30,30]
  energy_electrons     = [3e1,3.3e4]
  electron_lca         = [150,-150]
  min_peak_energy      = 300

  make_Strangeway_summary    = 1
  save_Strangeway_ps         = 0

  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ELECTRON_LOSSCONEANGLE=electron_lca, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        CHI2_THRESHOLD=chi2_thresh, $
                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                        RESTORE_FITFILE=restore_fitFile, $
                        T1STR=t1Str, $
                        T2STR=t2Str, $
                        SHOW_POST_PLOTS=show_post_plots, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        SAVE_KAPPA_PLOTS=save_kappa_plot, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        FIT2D_KAPPA_INF_LIST=fit2D_kappa_inf_list, $
                        FIT2D_GAUSS_INF_LIST=fit2D_gauss_inf_list, $
                        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file
                        
  STOP

  IF KEYWORD_SET(make_Strangeway_summary) THEN BEGIN
     SINGLE_SUMMARY,STR_TO_TIME(t1),STR_TO_TIME(t2), $
                    tplot_vars=tplot_vars, $
                    TLIMIT_NORTH=tlimit_north, $
                    tlimit_south=tlimit_south,tlimit_all=tlimit_all, $
                    /SCREEN_PLOT, $
                    USE_FAC_V=use_fac_v, $
                    USE_FAC=use_fac, $
                    NO_BLANK_PANELS=no_blank_panels, $
                    SAVE_PS=save_Strangeway_ps, $
                    SAVE_PNG=save_png
  ENDIF
END

