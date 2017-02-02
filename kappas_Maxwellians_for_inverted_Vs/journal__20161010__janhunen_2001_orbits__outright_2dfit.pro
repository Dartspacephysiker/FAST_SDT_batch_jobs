;;10/10/16
PRO JOURNAL__20161010__JANHUNEN_2001_ORBITS__OUTRIGHT_2DFIT

  COMPILE_OPT IDL2

  ;;get orbTimes here
  @journal__20161010__info__janhunen_2001_orbits.pro

  show_post_plots      = 0
  save_kappa_plot      = 0
  close_kp_after_save  = 0

  ;;Kill this one: 14:34:52.89
  ;; debug__skip_to_this_time    = STR_TO_TIME("98-01-22/14:34:52.00")
  ;; debug__break_on_this_time   = STR_TO_TIME("98-01-22/14:34:52.89")

  ;; debug__skip_to_this_time    = STR_TO_TIME("97-01-26/20:14:10.00")

  fit1D__save_plotSlices          = 0
  fit2D__save_all_candidate_plots = 0
  fit2D__show_each_candidate      = 0

  show_Strangeway_summary  = 1
  sway__save_ps            = 1
  sway__add_kappa_panel    = 0
  sway__add_chare_panel    = 1
  sway__add_Newell_panel   = 0
  sway__log_kappaPlot      = 0

  show_kappa_summary       = 1
  kSum__save_ps            = 1
  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 1

  kStats__save_stuff       = 0

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 0
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
  ;; 9: Dombeck et al. [2013] orbit 11002
  ;;10: Dombeck et al. [2013] orbit 11076
  ;;11: Dombeck et al. [2013] orbit 11097
  ;;12: Dombeck et al. [2013] orbit 11109
  ;;13: Dombeck et al. [2013] orbit 11024
  ;;14: Dombeck et al. [2013] orbit 11056
  ;;15: Dombeck et al. [2013] orbit 11067
  ;;16: Bonus, orbit 1771 (poking on either side of Elphic et al. [1998] orbit 
  ;;17: Bonus, orbit 1770 (poking on either side of Elphic et al. [1998] orbit 
  ;;18: Bonus, orbit 6717 from Chaston et al. [2006], the dens cavity pape
  ;;19: Bonus, orbit 5805–world's longest continual observation of monoenergetic aurora
  ;;20: Bonus, orbit 5825 (big current, strict mono. Whence come the obs. currents? )
  ;;21: Bonus, orbit 1713 (Semi-big current, strict mono.
  ;;22: Bonus, orbit 5616 (Semi-big current, strict mono. Cleaner? Maybe not. I hope.
  ;;23: Bonus, orbit 12136--Kelvin-Helmholtz???
  evtNum               = 23

  ;;survey window
  eeb_or_ees           = 'ees'
  burstItvl            = 0

  ;;String setup
  orbit                = orbs      [evtNum]
  t1Str                = orbTimes[0,evtNum]
  t2Str                = orbTimes[1,evtNum]
  bonusPref            = bonusPrefs[evtNum]
  south                = southArr[evtNum]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     t1Str             = (orbBurstTimes[evtNum])[0,burstItvl]
     t2Str             = (orbBurstTimes[evtNum])[1,burstItvl]
     bonusPref        += '--burstItvl_' + STRCOMPRESS(burstItvl,/REMOVE_ALL)
     kStats__include_these_startstops = (kStats_startStops__eeb[evtNum])[burstItvl]
  ENDIF ELSE BEGIN
     kStats__include_these_startstops = kStats_startStops__ees[evtNum]
  ENDELSE

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 25
  lowDens_thresh       = 0.001
  diffEflux_thresh     = 5e7
  nPkAbove_dEF_thresh  = 5

  electron_angleRange  = [-30,30]
  energy_electrons     = [3e1,3.0e4]
  electron_lca         = [150,-150]
  min_peak_energy      = 300

  ;;Southern Hemi
  IF KEYWORD_SET(south) THEN BEGIN
     electron_angleRange  = [150,-150]
     energy_electrons     = [3e1,3.0e4]
     electron_lca         = [-30,30]
     min_peak_energy      = 300
     fit2D__density_angleRange     = [150,-150]
  ENDIF

  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ELECTRON_LOSSCONEANGLE=electron_lca, $
                        ENERGY_ELECTRONS=energy_electrons, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        SOUTH=south, $
                        CHI2_THRESHOLD=chi2_thresh, $
                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                        HIGHDENSITY_THRESHOLD=highDens_thresh, $
                        LOWDENSITY_THRESHOLD=lowDens_thresh, $
                        DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                        N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                        RESTORE_FITFILE=restore_fitFile, $
                        T1STR=t1Str, $
                        T2STR=t2Str, $
                        SHOW_POST_PLOTS=show_post_plots, $
                        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                        SAVE_KAPPA_PLOTS=save_kappa_plot, $
                        SAVEKAPPA_BONUSPREF=bonusPref, $
                        CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                        PLOTDIR=plotDir, $
                        SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                        SWAY__SAVE_PS=sway__save_ps, $
                        SWAY__SAVE_PNG=sway__save_png, $
                        SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                        SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                        SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                        SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                        SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                        KSUM__SAVE_PS=kSum__save_ps, $
                        KSUM__SAVE_PNG=kSum__save_png, $
                        KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                        KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                        OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                        FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                        FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                        KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                        KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops,$
                        DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time

END
