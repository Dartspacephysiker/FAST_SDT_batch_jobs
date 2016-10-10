;;10/10/16
PRO JOURNAL__20161010__JANHUNEN_2001_ORBITS__OUTRIGHT_2DFIT

  COMPILE_OPT IDL2

  ;;get orbTimes here
  @journal__20161010__janhunen_2001_orbits__info.pro

  show_post_plots      = 1
  save_kappa_plot      = 1

  fit2D__save_all_candidate_plots = 0
  fit2D__show_each_candidate = 0

  show_Strangeway_summary    = 1
  save_Strangeway_ps         = 1
  add_kappa_panel            = 1
  add_chare_panel            = 1
  log_kappaPlot              = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  restore_fitFile      = 0

  ;;Which Janhunen event?
  ;; 0: Inverted V, but not Maxwellian                                      
  ;; 1: Inverted V, but not Maxwellian                                      
  ;; 2: Inverted V, but not Maxwellian; no exact times given!               
  ;; 3: "Quasi-Maxwellian"                                                  
  ;; 4: "Quasi-Maxwellian"                                                  
  ;; 5: "Diffuse auroral event containing no inverted-V-type precipitation" 
  ;; 6: No inverted V, No exact times given!                                
  ;; 7: No inverted V,No exact times given!                                 
  ;; 8: No inverted V,No exact times given!                                 
  evtNum               = 3

  ;;survey window
  eeb_or_ees           = 'eeb'
  burstItvl            = 0

  ;;String setup
  orbit                = orbs      [evtNum]
  t1Str                = orbTimes[0,evtNum]
  t2Str                = orbTimes[1,evtNum]
  bonusPref            = bonusPrefs[evtNum]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     t1Str             = (orbBurstTimes[evtNum])[0,burstItvl]
     t2Str             = (orbBurstTimes[evtNum])[1,burstItvl]
     bonusPref        += '--burstItvl_' + STRCOMPRESS(burstItvl,/REMOVE_ALL)
  ENDIF

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 500
  lowDens_thresh       = 0.00005

  electron_angleRange  = [-30,30]
  energy_electrons     = [3e1,3.3e4]
  electron_lca         = [150,-150]
  min_peak_energy      = 400

  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ELECTRON_LOSSCONEANGLE=electron_lca, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        CHI2_THRESHOLD=chi2_thresh, $
                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                        HIGHDENSITY_THRESHOLD=highDens_thresh, $
                        LOWDENSITY_THRESHOLD=lowDens_thresh, $
                        RESTORE_FITFILE=restore_fitFile, $
                        T1STR=t1Str, $
                        T2STR=t2Str, $
                        SHOW_POST_PLOTS=show_post_plots, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                        SAVE_KAPPA_PLOTS=save_kappa_plot, $
                        SAVEKAPPA_BONUSPREF=bonusPref, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                        OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                        FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                        FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file

  IF KEYWORD_SET(show_Strangeway_summary) THEN BEGIN
     SINGLE_SUMMARY,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
                    tplot_vars=tplot_vars, $
                    EEB_OR_EES=eeb_OR_ees, $
                    TLIMIT_NORTH=tlimit_north, $
                    tlimit_south=tlimit_south,tlimit_all=tlimit_all, $
                    /SCREEN_PLOT, $
                    ADD_KAPPA_PANEL=add_kappa_panel, $
                    ADD_CHARE_PANEL=add_chare_panel, $
                    LOG_KAPPAPLOT=log_kappaPlot, $
                    USE_FAC_V=use_fac_v, $
                    USE_FAC_NOT_V=use_fac, $
                    NO_BLANK_PANELS=no_blank_panels, $
                    SAVE_PS=save_Strangeway_ps, $
                    SAVE_PNG=save_png, $
                    FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                    FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                    KAPPAFITS=kappaFits, $
                    GAUSSFITS=gaussFits, $
                    CHI2_THRESHOLD=chi2_thresh, $
                    CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                    HIGHDENSITY_THRESHOLD=highDens_thresh, $
                    LOWDENSITY_THRESHOLD=lowDens_thresh

  ENDIF


END
