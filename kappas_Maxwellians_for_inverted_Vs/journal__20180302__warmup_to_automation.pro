;2018/03/02
PRO JOURNAL__20180302__WARMUP_TO_AUTOMATION

  COMPILE_OPT IDL2,STRICTARRSUBS

  routName = 'JOURNAL__20180216__THE_CLASSICS__RESPOND_AGAIN_TO_REFEREE__LINEAR_ENERGY_SHIFT'

  ;;get orbTimes here
  @journal__20161010__info__janhunen_2001_orbits.pro


  bonusBonusPref                    = '-linearEnergyShift-GRLRESPONSE3'

  dens__Liemohn_Khaz   = 0
  lkARange             = [-90,90]
  ;; lkARange             = "ALL__EXCL_ATM"

  ;; debug__skip_to_this_time          = '1997-02-01/09:27:01.57'
  ;; debug__break_on_this_time         = '1997-02-01/09:27:01.57'

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
  fit2D__extend_fitStruct_eRange    = 0 ;to 50 keV, je crois?

  fit2D__temperature_type  = 'AVG' ;or 'AVG'
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
  ;; kSum__eAngle        = [-180,180]
  kSum__save_ps       = 1
  kSum__add_parm_errors_from_file = 0
  kSum__add_parm_errors__nRolls = 10000
  kSum__add_parm_errors__use_most_prob = 1
  kSum__chi2Bounds    = [0,5]

  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 2 ;give value at which you'd like line
  kSum__add_meas_T_and_N = 1
  kSum__GRL           = 1
  kSum__oPlot_pot     = 1

  kStats__save_stuff   = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  restore_fitFile      = 1

  jv_theor__also_eFlux = 0
  jv_theor__only_eFlux = 0

  ;; show_post_plots      = 0
  ;; save_kappa_plot      = 0
  ;; close_kp_after_save  = 0

  ;;Kill this one: 14:34:52.89
  ;; debug__skip_to_this_time    = STR_TO_TIME("98-01-22/14:34:52.00")
  ;; debug__break_on_this_time   = STR_TO_TIME("98-01-22/14:34:52.89")

  ;; debug__skip_to_this_time    = STR_TO_TIME("97-01-26/20:14:10.00")

  ;;Which Janhunen event?
  ;; 0: Orbit 3091  – Inverted V, but not Maxwellian
  ;; 1: Orbit 3123  – Inverted V, but not Maxwellian
  ;; 2: Orbit 3147  – Inverted V, but not Maxwellian; no exact times given!
  ;; 3: Orbit 3219  – "Quasi-Maxwellian"
  ;; 4: Orbit 3370  – "Quasi-Maxwellian"
  ;; 5: Orbit 3498  – "Diffuse auroral event containing no inverted-V-type precipitation"
  ;; 6: Orbit 0000  – No inverted V, No exact times given!
  ;; 7: Orbit 7628  – No inverted V,No exact times given!
  ;; 8: Orbit 0000  – No inverted V,No exact times given!
  ;; 9: Orbit 11002 – Dombeck et al. [2013] orbit 11002
  ;;10: Orbit 11076 – Dombeck et al. [2013] orbit 11076
  ;;11: Orbit 11097 – Dombeck et al. [2013] orbit 11097
  ;;12: Orbit 11109 – Dombeck et al. [2013] orbit 11109
  ;;13: Orbit 11024 – Dombeck et al. [2013] orbit 11024
  ;;14: Orbit 11056 – Dombeck et al. [2013] orbit 11056
  ;;15: Orbit 11067 – Dombeck et al. [2013] orbit 11067
  ;;16: Orbit 1771  – Bonus, orbit 1771 (poking on either side of Elphic et al. [1998] orbit 
  ;;17: Orbit 1770  – Bonus, orbit 1770 (poking on either side of Elphic et al. [1998] orbit 
  ;;18: Orbit 6717  – Bonus, orbit 6717 from Chaston et al. [2006], the dens cavity pape
  ;;19: Orbit 5805  – Bonus, orbit 5805–world's longest continual observation of monoenergetic aurora
  ;;20: Orbit 5825  – Bonus, orbit 5825 (big current, strict mono. Whence come the obs. currents? )
  ;;21: Orbit 1713  – Bonus, orbit 1713 (Semi-big current, strict mono.
  ;;22: Orbit 5616  – Bonus, orbit 5616 (Semi-big current, strict mono. Cleaner? Maybe not. I hope.
  ;;23: Orbit 12136 – Bonus, orbit 12136--Kelvin-Helmholtz???
  evtNum               = 19

  ;; Orb 5805 mono obs are so long that we need to divide
  IF evtNum EQ 19 THEN BEGIN
     subEvtNum = 0 ;0
     CASE subEvtNum OF
        0: BEGIN
           orbTimes[*,evtNum] = ['1998-02-09/' + ['01:38:46','01:42:40']]
           ;; cAP_tRanges_list[evtNum] = orbTimes[*,evtNum]
           cAP_tRanges_list[evtNum] = ['1998-02-09/' + ['01:40:18','01:41:42']]
           cAP__add_iu_pot[evtNum]  = 1
           cAP__iu_pot_tids[evtNum] = [['1998-02-09/' + ['01:40:01','01:40:25']], $
                                       ['1998-02-09/' + ['01:41:15','01:41:22']]]

           ;; One seemingly linear J-V relation, possibly with kappa < 3.5
           ;; 01:41:15.952-01:42:25.657

           ;; Nuvver during 01:40:23.991–01:41:08.348 (also good eFlux-V relationship expressed!)

           ;; min_peak_energyArr       = 
           max_peak_energyArr       = [3.1E4,3.1E4,1.0e3]
        END
        1: BEGIN
           orbTimes[*,evtNum] = ['1998-02-09/' + ['01:42:40','01:46:40']]
           cAP_tRanges_list[evtNum] = orbTimes[*,evtNum]
        END
     ENDCASE

     bonusBonusPref = STRING(FORMAT='("-subEvt",I0)',subEvtNum) + bonusBonusPref

     kStats_startStops__ees[evtNum] = orbTimes[*,evtNum]
  ENDIF
  spectra_average_interval_list[evtNum] = 4


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

  ;; msph_sourcecone_halfWidth = 90

  ;;survey window
  eeb_or_ees           = eeb_or_ees__recommande[evtNum]
  burstItvl            = 0

  READ,

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
  chi2_over_dof_thresh = 50
  lowDens_thresh       = 0.05
  diffEflux_thresh     = 3D6
  nPkAbove_dEF_thresh  = 3

  ;; fit2D__density_angleRange = 'ALL__EXCL_ATM'
  fit2D__density_angleRange = KEYWORD_SET(dens__Liemohn_Khaz) ?  $
                              lkARange : 'ALL__EXCL_ATM'
  fit2D__temperature_angleRange = 'LC'
  fit2D__faConductance_angleRange = 'LC'

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
               iu_pot_tids : N_ELEMENTS(cAP__iu_pot_tids) GT 0 ? (N_ELEMENTS(cAP__iu_pot_tids[evtNum]) GT 0 ? cAP__iu_pot_tids[evtNum] : 0) : 0, $
               tRanges : cAP_tRanges_list[evtNum], $
               ;; moment_energyArr : [[100,3.0e4],[100,3.0e4],[100,2.4e4]]
               moment_energyArr : [[energy_electrons],[energy_electrons],[100,2.4e4]], $
               plot_j_v_potBar : 0B, $
               plot_jv_a_la_Elphic : daPlots_cAP, $
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
               jv_theor__Liemohn_and_Khazanov_dens : KEYWORD_SET(dens__Liemohn_Khaz), $

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

  IF KEYWORD_SET(min_peak_energyArr) THEN BEGIN
     STR_ELEMENT,cAP_struct,'min_peak_energyArr',min_peak_energyArr,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(max_peak_energyArr) THEN BEGIN
     STR_ELEMENT,cAP_struct,'max_peak_energyArr',max_peak_energyArr,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(aRange__dens_e_down) THEN BEGIN
     STR_ELEMENT,cAP_struct,'aRange__dens_e_down',aRange__dens_e_down,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(timeBars) AND KEYWORD_SET(cAP_struct) THEN IF (WHERE(TAG_NAMES(cAP_struct) EQ 'TRANGES'))[0] NE -1 THEN BEGIN
     timeBars                  = cAP_struct.tRanges
  ENDIF

  spectra_average_interval = spectra_average_interval_list[evtNum]

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
                        KSUM__CHI2BOUNDS=kSum__chi2Bounds, $
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
