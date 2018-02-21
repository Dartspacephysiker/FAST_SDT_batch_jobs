;;2018/02/20
;; 'x' is the number of orbits that we wish we could have said that we had examined in the GRL that we submitted. The final number ends
;; up being some mystery of the universe
PRO JOURNAL__20180220__REHUNT_FOR_X

  COMPILE_OPT IDL2

  routName = 'JOURNAL__20180220__REHUNT_FOR_X'

  ;;get orbTimes here
  @journal__20171109__info__hunt_for_x.pro

  bonusBonusPref                    = '-linearEnergyShift-GRLRESPONSE2'

  only_1D_fits                      = 0

  fit1D__sourceCone_energy_spectrum = 1

  fit1D__nFlux                      = 1
  fit2D__nFlux                      = 0

  fit__linear_energy_shift          = 1
  fit__JE_over_E                    = 0
  fit__LES__take_stock_of_RB        = 0

  fit1D__weighting                  = 2 ;1 = lin 2 = square
  fit1D__clampTemperature           = 0
  fit1D__clampDensity               = 0

  add_oneCount_curve                = 1

  daPlots_cAP                       = 1
  fit1D__save_plotSlices            = 1
  fit2D__save_all_plots             = 0
  fit2D__show_each_candidate        = 0
  fit2D__show_only_data             = 0
  fit2D__weighting                  = 2 ;1 = lin 2 = square
  fit2D__clampTemperature           = 0
  fit2D__clampDensity               = 0
  fit2D__estimate_sourceCone_from_dist = 0B
  fit2D__extend_fitStruct_eRange    = 0

  ;; fit2D__density_angleRange         = [-175,175]
  fit2D__density_angleRange = 'ALL__EXCL_ATM'
  fit2D__temperature_angleRange = 'LC'
  fit2D__faConductance_angleRange = 'LC'

  fit2D__temperature_type  = 'AVG' ;or 'AVG'
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
  kSum__add_parm_errors_from_file = 0
  kSum__add_parm_errors__nRolls = 10000
  kSum__add_parm_errors__use_most_prob = 1

  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 1
  kSum__add_meas_T_and_N = 1
  kSum__GRL           = 1
  kSum__oPlot_pot     = 1

  kStats__save_stuff   = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  restore_fitFile      = 1

  jv_theor__also_eFlux = 0
  jv_theor__only_eFlux = 0

  ;;Which totally classic event?
  ;; '0 :  orbit 1767' Dud? Maybe picked minE too high? (500eV)
  ;; '1 :  orbit 1764'   kappa >sim 2, so not amazing
  ;; '2 :  orbit 1757'   Bonus!
  ;; '3 :  orbit 1756'   Bonus!
  ;; '4 :  orbit 1753'   Bonus!
  ;; '5 :  orbit 3000SH' Looks like there's a bunch of EM-field nonsense happening during the period I thought was baller
  ;; '6 :  orbit 3001' Lower altitudes, like 1000 km
  ;; '7 :  orbit 8002' Bonus! (Back to high altitudes on nightside!)
  ;; '8 :  orbit 8003' Bonus!
  ;; '9 :  orbit 8004' Bonus!
  ;; '10:  orbit 7998' Bonus!
  ;; '11:  orbit 7995' Bonus!
  ;; '12:  orbit 7994' Not bonus …
  ;; '13:  orbit 7991' Semi-bonus; marginal at best on arc edges, but meets criteria
  ;; '14:  orbit 7990' Bonus!
  evtNum               = 2

  ;;If doing upgoing electrons
  peak_energy__start_at_highE       = 0
  upgoing                           = 0

  electron_angleRange  = 'lc'
  energy_electrons     = N_ELEMENTS(energy_electrons__recommande[evtNum]) GT 0 ? $
                         energy_electrons__recommande[evtNum]          : $
                         [3e1,3.1e4]
  min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : $
                         (N_ELEMENTS(min_peak_energy_recommande[evtNum]) GT 0 ? $
                          min_peak_energy_recommande[evtNum] : 500)
  max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL

  ;; msph_sourcecone_halfWidth = 90

  ;;survey window
  eeb_or_ees           = eeb_or_ees__recommande[evtNum]
  burstItvl            = 0

  ;;String setup
  orbit                = orbs      [evtNum]
  t1Str                = orbTimes[0,evtNum]
  t2Str                = orbTimes[1,evtNum]
  bonusPref            = bonusPrefs[evtNum] + (N_ELEMENTS(bonusBonusPref) GT 0 ? bonusBonusPref : '')

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
  diffEflux_thresh     = 3e6
  nPkAbove_dEF_thresh  = 2

  ;; IF orbit EQ 1789 THEN BEGIN

  ;;    kSum__add_chi2_line  = 10
     
  ;;    min_peak_energy   = 800
  ;;    ;; energy_electrons  = [0,3.0e4]

  ;;    fit2D__density_angleRange = [-145,145]

  ;;    energy_electrons          = [7e2,3.15e4]

  ;;    IF N_ELEMENTS(burstItvl) GT 0 AND (STRUPCASE(eeb_or_ees) EQ 'EEB') THEN BEGIN
  ;;       IF burstItvl EQ 1 THEN BEGIN ;Carlson et al. [1998] state that this interval is cold electron–free

  ;;          manual_angle_correction = -15

  ;;       ENDIF
  ;;    ENDIF
     
  ;;    t1Str = '97-02-02/21:01:55'
  ;;    t2Str = '97-02-02/21:02:20'

  ;;    chi2_over_dof_thresh = 50
  ;;    lowDens_thresh       = 0.002
  ;;    diffEflux_thresh     = 1e7
  ;;    nPkAbove_dEF_thresh  = 5

  ;; ENDIF

  ;; IF orbit EQ 1843 THEN BEGIN

  ;;    kSum__add_chi2_line  = 15
     
  ;;    ;; fit2D__density_angleRange         = [-150,150]
  ;;    fit2D__density_angleRange = 'ALL__EXCL_ATM'

  ;;    chi2_over_dof_thresh = 50

  ;; ENDIF

  ;; IF orbit EQ 1849 THEN BEGIN

  ;;    kSum__add_chi2_line  = 10
     
  ;;    fit2D__density_angleRange = [-45,45]

  ;;    chi2_over_dof_thresh = 20
  ;;    lowDens_thresh       = 0.002
  ;;    diffEflux_thresh     = 1e7
  ;;    nPkAbove_dEF_thresh  = 5

  ;; ENDIF

  ;;Current and potential analysis
  curAndPot_analysis        = 1
  cAP_struct = { $
               remake_masterFile : 0B, $
               map_to_100km : 1, $
               use_all_currents : 0B, $
               use_ed_current : 1B, $
               use_iu_current : 0B, $
               use_eu_current : 0B, $
               use_mag_current : 0B, $
               use_char_en_for_downPot : 0B, $
               T_plusMinusFac_for_pot  : 0L, $
               use_peak_en_for_downPot : 1B, $
               add_iu_pot : N_ELEMENTS(cAP__add_iu_pot) GT 0 ? (N_ELEMENTS(cAP__add_iu_pot[evtNum]) GT 0 ? cAP__add_iu_pot[evtNum] : 1) : 1, $
               tRanges : cAP_tRanges_list[evtNum], $
               ;; moment_energyArr : [[100,3.0e4],[100,3.0e4],[100,2.4e4]]
               moment_energyArr : [[energy_electrons],[energy_electrons],[100,2.4e4]], $
               plot_j_v_potBar : 0B, $
               plot_jv_a_la_Elphic : 1B, $
               plot_T_and_N : 0B, $
               plot_j_v_and_theory : 0B, $
               plot_j_v__fixed_t_and_n : 0B, $
               plot_j_v_map__r_b_and_kappa__fixed_t_and_n : daPlots_cAP, $
               plot_en_specs : 0B, $
               en_specs__movie : 0B, $
               jv_theor__R_B_init : 30, $
               jv_theor__kappa_init : 10, $
               jv_theor__kappaLims  : [1.520,11], $
               jv_theor__also_eFlux : KEYWORD_SET(jv_theor__also_eFlux), $
               jv_theor__only_eFlux : KEYWORD_SET(jv_theor__only_eFlux), $
               ;; jv_theor__TempLims       : [0,0], $
               ;; jv_theor__DensLims      : [0,0], $
               ;; jv_theor__magRatioLims  : [2,100], $

               ;;JV theory options

               ;; jv_theor__fit_je         : 1, $
               jv_theor__fit_both : 0, $
               use_msph_sourcecone_for_dens : [1,0,0], $
               use_msph_sourcecone_for_temp : [0,0,0], $
               temperature_type             : fit2D__temperature_type, $
               aRange__temp_e_down         : fit2D__temperature_angleRange, $
               ;; eRange__temp_list         :, $
               use_energies_above_peak_for_temp : [1,0,0], $
               ;; msph_sourcecone_halfWidth : 150, $
               ;; msph_sourcecone_halfWidth : msph_sourcecone_halfWidth, $
               all_pitchAngles : 0, $
               allPitch_except_atm_lc : 0, $
               ;; jv_theor__initial_source_R_E : 5.0D, $
               jv_theor__initial_source__Polar : 1, $
               ;; jv_theor__initial_source__equator : 0, $
               ;; jv_theor__iterative_game : 0, $
               ;; jv_theor__itergame_NFac   : 3.0, $
               jv_theor__itergame_tie_R_B_and_dens : 1, $
               in_bonusPref                        : bonusPref, $
               plots_in_buffer                     : 1}

  IF KEYWORD_SET(aRange__dens_e_down) THEN BEGIN
     STR_ELEMENT,cAP_struct,'aRange__dens_e_down',aRange__dens_e_down,/ADD_REPLACE
  ENDIF

  IF orbit EQ 7990 THEN BEGIN
     cAP_struct.use_msph_sourcecone_for_temp[0] = 1B
     chi2_over_dof_thresh = 40
  ENDIF

  ;;make sure temperature is limited
  IF KEYWORD_SET(kFit__unlimited_temp[evtNum]) THEN BEGIN
     kFit__limited[1,1] = 0
     kFit__limits[1,1]  = 2D5
  ENDIF

  IF KEYWORD_SET(timeBars) AND KEYWORD_SET(cAP_struct) THEN IF (WHERE(TAG_NAMES(cAP_struct) EQ 'TRANGES'))[0] NE -1 THEN BEGIN
     timeBars                  = cAP_struct.tRanges
  ENDIF

  spectra_average_interval = spectra_average_interval_list[evtNum]
  spectra_average_interval = 4
  show_post_plots      = 0
  save_postKappa_plots = 0
  close_kp_after_save  = 0

  n_below_peak1D       = KEYWORD_SET(fit__JE_over_E) ? -1 : -1
  n_above_peak1D       = 30
  n_below_peak2D       = KEYWORD_SET(fit__JE_over_E) ? -1 : -1
  n_above_peak2D       = 30
  phi__use_energy_before_peak = 1 ;the Kaeppler et al. [2014] thing (see bottom of second par, page 10,170)
  
  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ;; ELECTRON_LOSSCONEANGLE=electron_lca, $
                        MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                        ENERGY_ELECTRONS=energy_electrons, $
                        UPGOING=upgoing, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        MAX_PEAK_ENERGY=max_peak_energy, $
                        PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
                        PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
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
                        FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
                        FIT__JE_OVER_E=fit__JE_over_E, $
                        FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
                        FIT1D__N_BELOW_PEAK=n_below_peak1D, $
                        FIT1D__N_ABOVE_PEAK=n_above_peak1D, $
                        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                        FIT1D__NFLUX=fit1D__nFlux, $
                        FIT1D__WEIGHTING=fit1D__weighting, $
                        FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
                        FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
                        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                        FIT2D__N_BELOW_PEAK=n_below_peak2D, $
                        FIT2D__N_ABOVE_PEAK=n_above_peak2D, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                        FIT2D__WEIGHTING=fit2D__weighting, $
                        FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
                        FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
                        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                        FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
                        FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
                        FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
                        FIT2D__TEMPERATURE_TYPE=fit2D__temperature_type, $
                        FIT2D__EXTEND_FITSTRUCT_ERANGE=fit2D__extend_fitStruct_eRange, $
                        FIT2D__NFLUX=fit2D__nFlux, $
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
                        KSUM__ADD_PARM_ERRORS_FROM_FILE=kSum__add_parm_errors_from_file, $
                        KSUM__ADD_PARM_ERRORS__NROLLS=kSum__add_parm_errors__nRolls, $
                        KSUM__ADD_PARM_ERRORS__USE_MOST_PROB=kSum__add_parm_errors__use_most_prob, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                        OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
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


