;;2016/10/11
PRO JOURNAL__20161011__THE_CLASSICS__OUTRIGHT_2DFIT

  COMPILE_OPT IDL2

  routName = 'JOURNAL__20161011__THE_CLASSICS__OUTRIGHT_2DFIT'

  ;;get orbTimes here
  @journal__20161011__info__the_classics.pro

  only_1D_fits                      = 0
  fit1D__sourceCone_energy_spectrum = 1
  fit1D__nFlux                      = 1
  fit1D__weighting                  = 2 ;1 = lin 2 = square
  fit1D__clampTemperature           = 0
  fit1D__clampDensity               = 0

  add_oneCount_curve                = 1

  fit1D__save_plotSlices            = 0
  fit2D__save_all_plots             = 0
  fit2D__show_each_candidate        = 0
  fit2D__show_only_data             = 0
  fit2D__weighting                  = 2 ;1 = lin 2 = square
  fit2D__clampTemperature           = 0
  fit2D__clampDensity               = 0
  fit2D__estimate_sourceCone_from_dist = 0B
  fit2D__density_angleRange         = [-175,175]

  ;;PostScript options
  timeBars                 = 1

  eps                      = 1

  show_Strangeway_summary  = 0
  sway__save_ps            = 1
  sway__add_kappa_panel    = 0
  sway__add_chare_panel    = 1
  sway__add_Newell_panel   = 1
  sway__log_kappaPlot      = 0

  show_kappa_summary  = 1
  kSum__save_ps       = 1
  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 1
  kSum__add_meas_T_and_N = 1
  kSum__GRL           = 1
  kSum__oPlot_pot     = 1

  kStats__save_stuff   = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  restore_fitFile      = 0

  ;;Which totally classic event?
  ;; '0 :  Ergun_et_al_1998'
  ;; '1 :  McFadden_et_al_1998'
  ;; '2 :  Elphic_et_al_1998'
  ;; '3 :  Carlson_et_al_2001'
  evtNum               = 2

  ;;2017/03/22
  ;; evtNum               = 3

  ;;If doing upgoing electrons
  peak_energy__start_at_highE       = 0
  upgoing                           = 0

  electron_angleRange  = 'lc'
  ;; electron_angleRange  = [330,30]
  energy_electrons     = N_ELEMENTS(energy_electrons__recommande[evtNum]) GT 0 ? $
                         energy_electrons__recommande[evtNum]          : $
                         [3e1,3.1e4]
  ;; electron_lca         = [150,-150]
  ;; electron_lca         = 'lc'
  min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : $
                         (N_ELEMENTS(min_peak_energy_recommande[evtNum]) GT 0 ? $
                          min_peak_energy_recommande[evtNum] : 500)
  max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL

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
  diffEflux_thresh     = 5e7
  nPkAbove_dEF_thresh  = 5

  IF orbit EQ 1773 THEN BEGIN

     fit2D__density_angleRange = [-150,150]

  ENDIF

  IF orbit EQ 1789 THEN BEGIN

     kSum__add_chi2_line  = 10
     
     min_peak_energy   = 800
     ;; energy_electrons  = [0,3.0e4]

     fit2D__density_angleRange = [-145,145]

     energy_electrons          = [7e2,3.15e4]

     IF N_ELEMENTS(burstItvl) GT 0 AND (STRUPCASE(eeb_or_ees) EQ 'EEB') THEN BEGIN
        IF burstItvl EQ 1 THEN BEGIN ;Carlson et al. [1998] state that this interval is cold electron–free

           manual_angle_correction = -15

        ENDIF
     ENDIF
     
     t1Str = '97-02-02/21:01:55'
     t2Str = '97-02-02/21:02:20'

     chi2_over_dof_thresh = 50
     lowDens_thresh       = 0.002
     diffEflux_thresh     = 1e7
     nPkAbove_dEF_thresh  = 5

  ENDIF

  IF orbit EQ 1843 THEN BEGIN

     kSum__add_chi2_line  = 15
     
     fit2D__density_angleRange         = [-150,150]

     chi2_over_dof_thresh = 50

  ENDIF

  IF orbit EQ 1849 THEN BEGIN

     kSum__add_chi2_line  = 10
     
     fit2D__density_angleRange = [-45,45]

     chi2_over_dof_thresh = 20
     lowDens_thresh       = 0.002
     diffEflux_thresh     = 1e7
     nPkAbove_dEF_thresh  = 5

  ENDIF

  ;;Current and potential analysis
  curAndPot_analysis        = 1
  cAP_remake_masterFile     = 1
  cAP_map_to_100km          = 1
  cAP_use_all_currents      = 0
  cAP_use_ed_current        = 1
  cAP_use_iu_current        = 0
  cAP_use_eu_current        = 0
  cAP_use_mag_current       = 0
  cAP_use_charE_for_downPot = 1
  ;; cAP_use_peakE_for_downPot = 0
  cAP_add_iu_pot            = 1

  cAP_tRanges               = cAP_tRanges_list[evtNum]

  ;; cAP_moment_energyArr      = [[100,3.0e4],[100,3.0e4],[100,2.4e4]]
  cAP_moment_energyArr      = [[energy_electrons],[energy_electrons],[100,2.4e4]]
  
  cAP_plot_j_v_potBar          = 0B
  cAP_plot_jv_a_la_Elphic      = 0B
  cAP_plot_T_and_N             = 0B
  cAP_plot_j_v_and_theory      = 0B
  cAP_plot_j_v__fixed_t_and_n  = 1B
  cAP_plot_j_v_map__r_b_and_kappa__fixed_t_and_n = 1B
  cAP_plot_en_specs            = 0B
  cAP_en_specs__movie          = 0B
  cAP_jv_theor__R_B_init       = 300
  cAP_jv_theor__kappa_init     = 10
  cAP_jv_theor__kappaLims      = [1.540,11]
  ;; cAP_jv_theor__TempLims       = [0,0]
  ;; cAP_jv_theor__DensLims       = [0,0]
  ;; cAP_jv_theor__magRatioLims   = [2,100]
  ;;JV theory options
  ;; cAP_jv_theor__fit_je         = 1
  cAP_jv_theor__fit_both        = 0
  cAP_use_msph_sourcecone_for_dens = [1,0,0]
  cAP_use_msph_sourcecone_for_temp = [0,0,0]
  cAP_all_pitchAngles              = 0
  cAP_allPitch_except_atm_lc       = 0

  ;; cAP_jv_theor__initial_source_R_E = 5.0D
  cAP_jv_theor__initial_source__Polar = 1
  ;; cAP_jv_theor__initial_source__equator = 0
  cAP_jv_theor__iterative_game  = 0
  ;; cAP_jv_theor__itergame_NFac   = 3.0
  cAP_jv_theor__itergame_tie_R_B_and_dens = 1

  IF KEYWORD_SET(timeBars) THEN BEGIN
     timeBars                  = cAP_tRanges
  ENDIF

  spectra_average_interval = spectra_average_interval_list[evtNum]

  show_post_plots      = 0
  save_postKappa_plots = 0
  close_kp_after_save  = 0

  ;; debug__skip_to_this_time     = 
  ;; debug__skip_to_this_time  = STR_TO_TIME('97-02-01/09:26:31')
  ;; debug__break_on_this_time = STR_TO_TIME('97-02-01/09:26:31')

  ;;Orbit 1773
  ;; debug__skip_to_this_time = STR_TO_TIME('1997-02-01/09:26:14.2')
  ;; debug__skip_to_this_time = STR_TO_TIME('1997-02-01/09:26:23.0')
  ;; debug__skip_to_this_time = STR_TO_TIME('1997-02-01/09:27:01.2')
  
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
                        CURANDPOT_TRANGES=cAP_tRanges, $
                        CURANDPOT_MOMENT_ENERGYARR=cAP_moment_energyArr, $
                        CURANDPOT_REMAKE_MASTERFILE=cAP_remake_masterFile, $
                        CURANDPOT_MAP_TO_100KM=cAP_map_to_100km, $
                        CURANDPOT_USE_ALL_CURRENTS=cAP_use_all_currents, $
                        CURANDPOT_USE_DOWNGOING_ELECTRON_CURRENT=cAP_use_ed_current, $
                        CURANDPOT_USE_UPGOING_ION_CURRENT=cAP_use_iu_current, $
                        CURANDPOT_USE_UPGOING_ELECTRON_CURRENT=cAP_use_eu_current, $
                        CURANDPOT_USE_MAGNETOMETER_CURRENT=cAP_use_mag_current, $
                        CURANDPOT_USE_CHAR_EN_FOR_DOWNPOT=cAP_use_charE_for_downPot, $
                        CURANDPOT_USE_PEAK_EN_FOR_DOWNPOT=cAP_use_peakE_for_downPot, $
                        CURANDPOT_ADD_UPGOING_ION_POT=cAP_add_iu_pot, $
                        CURANDPOT_PLOT_J_V_POTBAR=cAP_plot_j_v_potBar, $
                        CURANDPOT_PLOT_JV_A_LA_ELPHIC=cAP_plot_jv_a_la_Elphic, $
                        CURANDPOT_PLOT_T_AND_N=cAP_plot_T_and_N, $
                        CURANDPOT_PLOT_J_V_AND_THEORY=cAP_plot_j_v_and_theory, $
                        CURANDPOT_PLOT_J_V__FIXED_T_AND_N=cAP_plot_j_v__fixed_t_and_n, $
                        CURANDPOT_PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N=cAP_plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
                        CURANDPOT_PLOT_EN_SPECS=cAP_plot_en_specs, $
                        CURANDPOT_EN_SPECS__MOVIE=cAP_en_specs__movie, $
                        CURANDPOT_JV_THEOR__R_B_INIT=cAP_jv_theor__R_B_init, $
                        CURANDPOT_JV_THEOR__KAPPA_INIT=cAP_jv_theor__kappa_init, $
                        CURANDPOT_JV_THEOR__KAPPALIMS=cAP_jv_theor__kappaLims, $   
                        CURANDPOT_JV_THEOR__TEMPLIMS=cAP_jv_theor__TempLims, $    
                        CURANDPOT_JV_THEOR__DENSLIMS=cAP_jv_theor__DensLims, $    
                        CURANDPOT_JV_THEOR__MAGRATIOLIMS=cAP_jv_theor__magRatioLims, $
                        CURANDPOT_JV_THEOR__FIT_JE=cAP_jv_theor__fit_je, $
                        CURANDPOT_JV_THEOR__FIT_BOTH=cAP_jv_theor__fit_both, $
                        CURANDPOT_USE_MSPH_SOURCECONE_FOR_DENS=cAP_use_msph_sourcecone_for_dens, $
                        CURANDPOT_USE_MSPH_SOURCECONE_FOR_TEMP=cAP_use_msph_sourcecone_for_temp, $
                        CURANDPOT_ALL_PITCHANGLES=cAP_all_pitchAngles, $
                        CURANDPOT_ALLPITCH_EXCEPT_ATM_LC=cAP_allPitch_except_atm_lc, $
                        CURANDPOT_JV_THEOR__INITIAL_SOURCE_R_E=cAP_jv_theor__initial_source_R_E, $
                        CURANDPOT_JV_THEOR__INITIAL_SOURCE__POLARSAT=cAP_jv_theor__initial_source__Polar, $
                        CURANDPOT_JV_THEOR__INITIAL_SOURCE__EQUATOR=cAP_jv_theor__initial_source__equator, $
                        CURANDPOT_JV_THEOR__ITERATIVE_DENSITY_AND_R_B_GAME=cAP_jv_theor__iterative_game, $
                        CURANDPOT_JV_THEOR__ITERATIVE_GAME__DENSITY_INCREASE=cAP_jv_theor__itergame_NFac, $
                        CURANDPOT_JV_THEOR__ITERATIVE_GAME__TIE_RB_AND_DENS=cAP_jv_theor__itergame_tie_R_B_and_dens, $
                        CURANDPOT__MAP__MULTI_MAGRATIO_ARRAY=cAP_map__multi_magRatio_array, $
                        CURANDPOT__MAP__MULTI_KAPPA_ARRAY=cAP_map__multi_kappa_array, $
                        CURANDPOT__MAP__2D=cAP_map__2D, $
                        TIMEBARS=timeBars, $
                        EPS=eps
  
END

