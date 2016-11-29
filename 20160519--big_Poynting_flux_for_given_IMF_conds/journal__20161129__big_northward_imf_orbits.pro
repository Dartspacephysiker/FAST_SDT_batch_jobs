;;2016/11/29
PRO JOURNAL__20161129__BIG_NORTHWARD_IMF_ORBITS

  COMPILE_OPT IDL2

  ;;get orbTimes here
  @journal__20161129__info__big_northward_imf_orbits.pro

  show_post_plots      = 0
  save_kappa_plot      = 0
  close_kp_after_save  = 0

  ;;Kill this one: 14:34:52.89
  ;; debug__skip_to_this_time    = STR_TO_TIME("98-01-22/14:34:52.00")
  ;; debug__break_on_this_time   = STR_TO_TIME("98-01-22/14:34:52.89")

  ;; debug__skip_to_this_time    = STR_TO_TIME("97-01-26/20:14:10.00")

  fit1D__save_plotSlices          = 1
  fit2D__save_all_candidate_plots = 1
  fit2D__show_each_candidate      = 1
  fit2D__show_only_data           = 1
  fit2D__PA_zRange                = [1e7,1e10]

  show_Strangeway_summary  = 1
  sway__save_ps            = 1
  sway__add_kappa_panel    = 0
  sway__add_chare_panel    = 1
  sway__add_Newell_panel   = 1
  sway__Newell_interp      = 1
  sway__log_kappaPlot      = 0

  show_kappa_summary       = 0
  kSum__save_ps            = 1
  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 1

  kStats__save_stuff       = 0

  save_diff_eFlux_file = 0
  load_diff_eFlux_file = 1
  restore_fitFile      = 0

  ;;Which KH event? (Subtract 1, son)
  ;;01--5583--NOT UPDATED
  ;;02--6016--NOT UPDATED
  ;;03--6104--NOT UPDATED
  ;;04--6692--NOT UPDATED
  ;;05--6693--NOT UPDATED
  ;;06--8644--NOT UPDATED
  ;;07--8762--NOT UPDATED
  ;;08--9831--NOT UPDATED
  ;;09--9832--NOT UPDATED
  ;;10--9918             
  evtNum               = 8
  evtNum--

  ;;survey window
  eeb_or_ees           = 'eeb'
  eesItvl              = 1
  burstItvl            = 6

  ;;String setup
  orbit                = orbs      [evtNum]
  CASE SIZE(orbTimes,/TYPE) OF
     11: BEGIN
        CASE N_ELEMENTS(SIZE(orbTimes[evtNum],/DIM)) OF
           1: BEGIN
              t1Str    = orbTimes[evtNum,0]
              t2Str    = orbTimes[evtNum,1]
           END
           2: BEGIN
              multiple_ees = 1
              IF N_ELEMENTS(eesItvl) EQ 0 THEN eesItvl = 0
              t1Str    = orbTimes[evtNum,0,eesItvl]
              t2Str    = orbTimes[evtNum,1,eesItvl]
              END
        ENDCASE
     END
     7: BEGIN
        t1Str          = orbTimes[0,evtNum]
        t2Str          = orbTimes[1,evtNum]
     END
  ENDCASE
  bonusPref            = bonusPrefs[evtNum]
  south                = southArr[evtNum]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN BEGIN
     t1Str             = (orbBurstTimes[evtNum])[0,burstItvl]
     t2Str             = (orbBurstTimes[evtNum])[1,burstItvl]
     bonusPref        += '--burstItvl_' + STRCOMPRESS(burstItvl,/REMOVE_ALL)
     ;; kStats__include_these_startstops = (kStats_startStops__eeb[evtNum])[burstItvl]
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(multiple_ees) THEN BEGIN
        bonusPref     += '--eesItvl_' + STRCOMPRESS(eesItvl,/REMOVE_ALL)
     ENDIF
     ;; kStats__include_these_startstops = kStats_startStops__ees[evtNum]
  ENDELSE

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 1000
  lowDens_thresh       = 0.000001
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

  SET_PLOT_DIR,plotDir, $
               /FOR_SDT, $
               ADD_SUFF='/big_Poynting_flux_for_given_IMF_conds/' + $
               GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '/'

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
                        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                        FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
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
                        SWAY__NEWELL_INTERP=sway__Newell_interp, $
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
