;;10/10/16
PRO JOURNAL__20161010__JANHUNEN_2001_ORBITS__OUTRIGHT_2DFIT

  COMPILE_OPT IDL2

  routName = 'JOURNAL__20161010__JANHUNEN_2001_ORBITS__OUTRIGHT_2DFIT'

  ;;get orbTimes here
  @journal__20161010__info__janhunen_2001_orbits.pro

  only_1D_fits                      = 0
  fit1D__sourceCone_energy_spectrum = 1
  fit1D__nFlux                      = 1
  fit1D__weighting                  = 2 ;1 = lin 2 = square
  fit1D__clampTemperature           = 0
  fit1D__clampDensity               = 0

  add_oneCount_curve                = 1

  daPlots_cAP                       = 0
  fit1D__save_plotSlices            = 1
  fit2D__save_all_plots             = 0
  fit2D__show_each_candidate        = 0
  fit2D__show_only_data             = 0
  fit2D__weighting                  = 2 ;1 = lin 2 = square
  fit2D__clampTemperature           = 0
  fit2D__clampDensity               = 0
  fit2D__estimate_sourceCone_from_dist = 0B
  fit2D__density_angleRange         = 'ALL__EXCL_ATM'
  ;; fit2D__density_angleRange         = [-175,175]

  ;;PostScript options
  timeBars                 = 1

  eps                      = 1

  show_Strangeway_summary  = 1
  sway__save_ps            = 1
  sway__add_kappa_panel    = 0
  sway__add_chare_panel    = 1
  sway__add_Newell_panel   = 1
  sway__log_kappaPlot      = 0

  show_kappa_summary  = 1
  ;; kSum__eAngle        = [-180,180]
  kSum__save_ps       = 1
  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 1
  kSum__add_meas_T_and_N = 1
  kSum__GRL           = 1
  kSum__oPlot_pot     = 1

  kStats__save_stuff   = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 0
  restore_fitFile      = 0

  ;; show_post_plots      = 0
  ;; save_kappa_plot      = 0
  ;; close_kp_after_save  = 0

  ;;Kill this one: 14:34:52.89
  ;; debug__skip_to_this_time    = STR_TO_TIME("98-01-22/14:34:52.00")
  ;; debug__break_on_this_time   = STR_TO_TIME("98-01-22/14:34:52.89")

  ;; debug__skip_to_this_time    = STR_TO_TIME("97-01-26/20:14:10.00")

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
  evtNum               = 1

  ;;survey window
  eeb_or_ees           = eeb_or_ees__recommande[evtNum]
  burstItvl            = 0
  ;; ;;survey window
  ;; eeb_or_ees           = 'ees'
  ;; burstItvl            = 0

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

  ;; electron_angleRange  = [-30,30]
  electron_angleRange  = 'lc'
  ;; electron_angleRange  = [330,30]
  energy_electrons     = N_ELEMENTS(energy_electrons__recommande[evtNum]) GT 0 ? $
                         energy_electrons__recommande[evtNum]          : $
                         [3e1,3.1e4]
  ;; energy_electrons     = [3e1,3.0e4]
  ;; electron_lca         = [150,-150]
  ;; min_peak_energy      = 300

  min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : $
                         (N_ELEMENTS(min_peak_energy_recommande[evtNum]) GT 0 ? $
                          min_peak_energy_recommande[evtNum] : 500)
  max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL


  ;;Southern Hemi
  IF KEYWORD_SET(south) THEN BEGIN
     electron_angleRange  = [150,-150]
     energy_electrons     = [3e1,3.0e4]
     electron_lca         = [-30,30]
     min_peak_energy      = 300
     fit2D__density_angleRange     = [150,-150]
  ENDIF

  ;;survey window
  eeb_or_ees           = eeb_or_ees__recommande[evtNum]
  burstItvl            = 0

  ;;String setup
  orbit                = orbs      [evtNum]
  t1Str                = orbTimes[0,evtNum]
  t2Str                = orbTimes[1,evtNum]
  bonusPref            = bonusPrefs[evtNum]

  IF (STRUPCASE(eeb_or_ees) EQ 'EEB')  OR (STRUPCASE(eeb_or_ees) EQ 'IEB') THEN BEGIN
     t1Str             = (orbBurstTimes[evtNum])[0,burstItvl]
     t2Str             = (orbBurstTimes[evtNum])[1,burstItvl]
     bonusPref        += '--burstItvl_' + STRCOMPRESS(burstItvl,/REMOVE_ALL)
     kStats__include_these_startstops = (kStats_startStops__eeb[evtNum])[0,*,burstItvl]
  ENDIF ELSE BEGIN
     kStats__include_these_startstops = kStats_startStops__ees[evtNum]
  ENDELSE

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 25
  lowDens_thresh       = 0.05
  diffEflux_thresh     = 3e7
  nPkAbove_dEF_thresh  = 5

  IF orbit EQ 3123 THEN BEGIN

     chi2_over_dof_thresh = 50

  ENDIF
  
  ;;Current and potential analysis
  curAndPot_analysis        = 1

  cAP_struct = { $
               remake_masterFile : 1B, $
               map_to_100km : 1, $
               use_all_currents : 0, $
               use_ed_current : 1, $
               use_iu_current : 0, $
               use_eu_current : 0, $
               use_mag_current : 0, $
               use_charE_for_downPot : 1, $
               T_plusMinusFac_for_pot : 0L, $
               use_peakE_for_downPot : 0B, $
               add_iu_pot : N_ELEMENTS(cAP__add_iu_pot) GT 0 ? (N_ELEMENTS(cAP__add_iu_pot[evtNum]) GT 0 ? cAP__add_iu_pot[evtNum] : 1) :1, $
               tRanges : cAP_tRanges_list[evtNum], $
               ;; moment_energyArr : [[100,3.0e4],[100,3.0e4],[100,2.4e4]]
               moment_energyArr : [[energy_electrons],[energy_electrons],[100,2.4e4]], $
               plot_j_v_potBar : 0B, $
               plot_jv_a_la_Elphic : 0B, $
               plot_T_and_N : 0B, $
               plot_j_v_and_theory : 0B, $
               plot_j_v__fixed_t_and_n : 0B, $
               plot_j_v_map__r_b_and_kappa__fixed_t_and_n : daPlots_cAP, $
               plot_en_specs : 0B, $
               en_specs__movie : 0B, $
               jv_theor__R_B_init : 300, $
               jv_theor__kappa_init : 10, $
               jv_theor__kappaLims  : [1.530,11], $
               ;; jv_theor__TempLims       : [0,0], $
               ;; jv_theor__DensLims      : [0,0], $
               ;; jv_theor__magRatioLims  : [2,100], $

               ;;JV theory options

               ;; jv_theor__fit_je         : 1, $
               jv_theor__fit_both : 0, $
               use_msph_sourcecone_for_dens : [1,0,0], $
               use_msph_sourcecone_for_temp : [0,0,0], $
               all_pitchAngles : 0, $
               allPitch_except_atm_lc : 0, $
               ;; jv_theor__initial_source_R_E : 5.0D, $
               jv_theor__initial_source__Polar : 1, $
               ;; jv_theor__initial_source__equator : 0, $
               ;; jv_theor__iterative_game : 0, $
               ;; jv_theor__itergame_NFac   : 3.0, $
               jv_theor__itergame_tie_R_B_and_dens : 1}

  IF KEYWORD_SET(timeBars) AND KEYWORD_SET(cAP_struct) THEN IF (WHERE(TAG_NAMES(cAP_struct) EQ 'TRANGES'))[0] NE -1 THEN BEGIN
     timeBars                  = cAP_struct.tRanges
  ENDIF

  spectra_average_interval = spectra_average_interval_list[evtNum]

  show_post_plots      = 0
  save_postKappa_plots = 0
  close_kp_after_save  = 0

  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ;; ELECTRON_LOSSCONEANGLE=electron_lca, $
                        MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                        ENERGY_ELECTRONS=energy_electrons, $
                        UPGOING=upgoing, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        MAX_PEAK_ENERGY=max_peak_energy, $
                        PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
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
                        ONLY_1D_FITS=only_1D_fits, $
                        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                        FIT1D__NFLUX=fit1D__nFlux, $
                        FIT1D__WEIGHTING=fit1D__weighting, $
                        FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
                        FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
                        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                        FIT2D__WEIGHTING=fit2D__weighting, $
                        FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
                        FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
                        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                        FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
                        ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                        SAVE_POSTKAPPA_PLOTS=save_postKappa_plots, $
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
                        KSUM__EANGLE=kSum__eAngle, $
                        KSUM__SAVE_PS=kSum__save_ps, $
                        KSUM__SAVE_PNG=kSum__save_png, $
                        KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                        KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                        KSUM__ADD_MEASURED_T_AND_N=kSum__add_meas_T_and_N, $
                        KSUM__GRL=kSum__GRL, $
                        KSUM__OPLOT_POT=kSum__oPlot_pot, $
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
                        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time, $
                        ORIGINATING_ROUTINE=routName, $
                        CURANDPOT_ANALYSIS=curAndPot_analysis, $
                        CAP_STRUCT=cAP_struct, $
                        TIMEBARS=timeBars, $
                        EPS=eps
  

END
