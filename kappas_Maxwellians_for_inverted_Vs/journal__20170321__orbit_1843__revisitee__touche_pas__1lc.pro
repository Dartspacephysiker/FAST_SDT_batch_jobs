PRO JOURNAL__20170321__ORBIT_1843__REVISITEE__TOUCHE_PAS__1LC, $
   PLOT_JV_A_LA_ELPHIC=plot_jv_a_la_Elphic, $
   PLOT_J_V_POTBAR=plot_j_v_potBar, $    
   PLOT_T_AND_N=plot_T_and_N, $       
   PLOT_J_V_AND_THEORY=plot_j_v_and_theory, $
   PLOT_J_V__FIXED_T_AND_N=plot_j_v__fixed_t_and_n, $
   JV_THEOR__ITERATIVE_DENSITY_AND_R_B_GAME=jv_theor__iterative_game, $
   PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N=plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
   PLOT_MAGCURRENT_VS_CURRENT=plot_magCurrent_vs_current, $
   PLOT_EN_SPECS=plot_en_specs, $
   SAVEPLOT=savePlot, $
   WHICH_TRANGE=which_tRange

  COMPILE_OPT IDL2

  routName                = 'JOURNAL__20170321__ORBIT_1843__REVISITEE__NE_TOUCHE_PAS__1LC'

  ;;get orbTimes here
  orbit                   = 1843
  orbPref                 = 'Orbit_' + STRCOMPRESS(orbit,/REMOVE_ALL)
  bonusPref               = orbPref + '-efter_Elphic_ea_1998_Fig2-quiteMoney-'

  ;;They'll just walk up and bring you the keys! MO MONEY MO MONEY MO MONEY
  plot_times              = '1997-02-07/' + $
                            [['20:49:30','20:50:10']]
  orbTimes                = plot_times
  orbBurstTimes           = plot_times

  downTimesStr            = plot_times
  upTimesStr              = downTimesStr

  Elphic1998_defaults     = 1

  error_estimates         = 1
  remake_masterFile       = 0
  map_to_100km            = 1

  add_oneCount_stats      = 1

  ;; useInds__relChange      = 1
  ;; fracChange_TDown        = 0.5
  ;; fracChange_NDown        = 0.25
  ;; fracError_TDown         = 0.20
  ;; fracError_NDown         = 0.05
  ;; max_TDown               = 700
  ;; ;; min_TDown               = 180
  ;; max_NDown               = 0.1
  ;; min_NDown               = 0.07
  ;; fracError_TDown         = 0.20
  ;; fracError_NDown         = 0.10
  use_fracError_JDown     = 1
  fracError_JDown         = 1.0
  useInds__twoLumps       = 1
  tRanges                 = '1997-02-07/' + $
                            [['20:49:46.5', $
                              '20:49:52'], $
                             ['20:49:55', $
                              '20:50:10']]

  tRanges                 = '1997-02-07/' + $
                            [['20:49:45', $
                              '20:50:10'], $
                             ['20:49:55', $
                              '20:50:10']]

  hvilken_tRange          = KEYWORD_SET(which_tRange) ? which_tRange : 0
  tRanges                 = tRanges[*,hvilken_tRange]

  plot_t1                 = STR_TO_TIME(plot_times[0])
  plot_t2                 = STR_TO_TIME(plot_times[1])
  add_iu_pot              = 1
  use_ed_current          = 1
  use_iu_current          = 0
  use_all_currents        = 0

  interactive_overplot    = 0
  
  savePlot                = N_ELEMENTS(savePlot) GT 0 ? savePlot : 0B
  savePSuff = STRSPLIT(tRanges,'/',/EXTRACT)
  savePSuff = STRSPLIT([savePSuff[0,1],savePSuff[1,1]],':',/EXTRACT)
  savePSuff = STRJOIN(savePSuff[0,1:2],'_') + '-' + STRJOIN(savePSuff[1,1:2],'_')
  savePSuff               = STRING(FORMAT='("tR",I0,"__")',hvilken_tRange) + savePSuff

  ;;Which plots?
  plot_jv_a_la_Elphic     = KEYWORD_SET(plot_jv_a_la_Elphic    ) ? plot_jv_a_la_Elphic     : 0B
  plot_j_v_potBar         = KEYWORD_SET(plot_j_v_potBar        ) ? plot_j_v_potBar         : 0B
  plot_T_and_N            = KEYWORD_SET(plot_T_and_N           ) ? plot_T_and_N            : 0B
  plot_j_v_and_theory     = KEYWORD_SET(plot_j_v_and_theory    ) ? plot_j_v_and_theory     : 0B
  plot_j_v__fixed_t_and_n = KEYWORD_SET(plot_j_v__fixed_t_and_n) ? plot_j_v__fixed_t_and_n : 0B
  plot_j_v_map__r_b_and_kappa__fixed_t_and_n = KEYWORD_SET(plot_j_v_map__r_b_and_kappa__fixed_t_and_n) ? plot_j_v_map__r_b_and_kappa__fixed_t_and_n : 0B

  ;; fExt                    = '.png'
  fExt                    = '.eps'
  a_la_Elphic_spName      = bonusPref + fExt
  jvpotBar_spName         = bonusPref + 'j_vs_potBar__' + savePSuff + fExt
  TN_spName               = bonusPref + 'T_and_N__' + savePSuff + fExt
  JV_theor_spName         = bonusPref + 'j_v_data_n_theory__' + savePSuff + fExt
  j_v__fixTandN__spName   = bonusPref + 'j_v_fixTandN__' + savePSuff + fExt

  ;;Options for j_v_potBar plot
  jvpotBar__j_on_yAxis    = 1

  ;; nKappa                    = N_ELEMENTS(map__multi_kappa_array)
  ;; nRB                       = N_ELEMENTS(map__multi_magRatio_array)
  ;; map2D__multi_kappa_array  = map__multi_kappa_array # MAKE_ARRAY(nRB,/FLOAT,VALUE=1.0)
  map__2D                 = 1

  ;;Options for TandN plot
  TN_yLog_nDown           = 0B

  ;;Options for j_v_and_theory plot
  plot_j_ratios           = 1B
  plot_ion_elec_ratios    = 1B
  JV_theor__fit_time_series = 1B
  jv_theor__minPot        = 1500
  jv_theor__maxPot        = 4000
  jv_theor__minCur        = 1D-6
  jv_theor__maxCur        = !NULL
  jv_theor__kappaLims     = [1.501,11]
  ;; jv_theor__TempLims      = [,]
  ;; jv_theor__DensLims      = [,]
  jv_theor__magRatioLims  = [40,1D3]

  ;; jv_theor__R_B_init      = 1E4
  ;; jv_theor__kappa_init    = 10

  jv_theor__R_B_init      = 20
  jv_theor__kappa_init    = 3.0

  jv_theor__itergame_tie_R_B_and_dens = 1

  units                   = 'eFlux'
  ;; units                = 'flux'
  ;; units                = 'dfStd'

  outDir                  = '~/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  masterFile              = bonusPref + 'Fig2_ingredients.sav'

  saveCurPotFile          = bonusPref + 'Fig2__meal.sav'
  save_diff_eFlux_file    = 1
  load_diff_eFlux_file    = 1
  restore_fitFile         = 1

  ;;survey window
  eeb_or_eesArr            = ['ees','ies']
  spectra_average_interval = 2

  ;; eeb_or_eesArr           = ['eeb','ieb']
  ;; spectra_average_interval = 12


  order                   = [0,1,2]
  label                   = ['downgoing_e','upgoing_e','upgoing_i']

  ;;OPTIONS! OPTIONS! OPTIONS!
  ;; aRange__moments_e_down  = [315.,45.]
  aRange__moments_e_down  = 'lc'
  ;; aRange__moments_i_up    = [0.,360.]

  aRange__moments_i_up    = 'lc'
  aRange__peakEn_i_up     = 'lc'
  aRange__charE_i_up      = 'lc'

  ;; blankers                = !NULL
  blankers                = 'lc'

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  aRange__moments_list    = LIST(aRange__moments_e_down,blankers,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(blankers,blankers,aRange__peakEn_i_up)
  aRange__charE_list      = LIST(blankers,blankers,aRange__charE_i_up)

  use_msph_sourcecone_for_dens = [1,0,0]
  use_msph_sourcecone_for_temp = [1,0,0]
  
  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

  use_sc_pot_for_lowerbound = 1
  pot__from_file          = 0
  pot__save_file          = 0
  pot__all                = 0
  pot__from_fa_potential  = 1
  moment_energyArr        = [[300,3.15D4],[400,3.15D4],[100,2.45D4]]

  min_peak_energyArr      = [500,100,100]
  max_peak_energyArr      = [3.15D4,3.15D4,2.45e4]

  CURRENT_AND_POTENTIAL_SUITE, $
     ORBIT=orbit, $
     ORBTIMES=orbTimes, $
     ORBBURSTTIMES=orbBurstTimes, $
     PLOT_T1=plot_t1, $
     PLOT_T2=plot_t2, $
     BONUSPREF=bonusPref, $
     DOWNTIMESSTR=downTimesStr, $
     UPTIMESSTR=upTimesStr, $
     TIMESLIST=timesList, $
     UNITS=units, $
     OUTDIR=outDir, $
     MASTERFILE=masterFile, $
     REMAKE_MASTERFILE=remake_masterFile, $
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
     EEB_OR_EESARR=eeb_or_eesArr, $
     ORDER=order, $
     LABEL=label, $
     ADD_ONECOUNT_STATS=add_oneCount_stats, $
     USE_MSPH_SOURCECONE_FOR_DENS=use_msph_sourcecone_for_dens, $
     USE_MSPH_SOURCECONE_FOR_TEMP=use_msph_sourcecone_for_temp, $
     ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
     ARANGE__MOMENTS_E_UP=aRange__moments_e_up, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     ARANGE__PEAKEN_E_DOWN=aRange__peakEn_e_down, $
     ARANGE__PEAKEN_E_UP=aRange__peakEn_e_up, $
     ARANGE__PEAKEN_I_UP=aRange__peakEn_i_up, $
     ARANGE__CHARE_E_DOWN=aRange__charE_e_down, $
     ARANGE__CHARE_E_UP=aRange__charE_e_up, $
     ARANGE__CHARE_I_UP=aRange__charE_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     MOMENT_ENERGYARR=moment_energyArr, $
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
     POT__FROM_FA_POTENTIAL=pot__from_fa_potential, $
     POT__CHASTON_STYLE=pot__Chaston_style, $
     POT__FROM_FILE=pot__from_file, $
     POT__SAVE_FILE=pot__save_file, $
     ARANGE__MOMENTS_LIST=aRange__moments_list, $
     ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
     ARANGE__CHARE_LIST=aRange__charE_list, $
     ELPHIC1998_DEFAULTS=Elphic1998_defaults, $
     MIN_PEAK_ENERGYARR=min_peak_energyArr, $
     MAX_PEAK_ENERGYARR=max_peak_energyArr, $
     PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
     UPGOINGARR=upgoingArr, $
     ERROR_ESTIMATES=error_estimates, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     MAP_TO_100KM=map_to_100km, $
     SAVECURPOTFILE=saveCurPotFile, $
     USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
     USE_UPGOING_ION_CURRENT=use_iu_current, $
     USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
     USE_MAGNETOMETER_CURRENT=use_mag_current, $
     USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
     USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
     ADD_UPGOING_ION_POT=add_iu_pot, $
     ERROR_BAR_FACTOR=errorBarFac, $
     USEI__RELCHANGE=useInds__relChange, $
     USEI__TWOLUMPS=useInds__twoLumps, $
     FRACCHANGE_TDOWN=fracChange_TDown, $
     FRACCHANGE_NDOWN=fracChange_NDown, $
     FRACERROR_NDOWN=fracError_NDown, $
     FRACERROR_JDOWN=fracError_JDown, $
     FRACERROR_TDOWN=fracError_TDown, $
     USE_FRACERROR_NDOWN=use_fracError_NDown, $
     USE_FRACERROR_JDOWN=use_fracError_JDown, $
     USE_FRACERROR_TDOWN=use_fracError_TDown, $
     MAX_TDOWN=max_TDown, $
     MIN_TDOWN=min_TDown, $
     MAX_NDOWN=max_NDown, $
     MIN_NDOWN=min_NDown, $
     TRANGES=tRanges, $
     MINPOT=minPot, $
     MAXPOT=maxPot, $
     MINCUR=minCur, $
     MAXCUR=maxCur, $
     JV_THEOR__MINPOT=jv_theor__minPot, $
     JV_THEOR__MAXPOT=jv_theor__maxPot, $
     JV_THEOR__MINCUR=jv_theor__minCur, $
     JV_THEOR__MAXCUR=jv_theor__maxCur, $
     JV_THEOR__PLOT_J_RATIOS=plot_j_ratios, $
     JV_THEOR__PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
     JV_THEOR__FIT_TIME_SERIES=jv_theor__fit_time_series, $
     JV_THEOR__R_B_INIT=jv_theor__R_B_init, $
     JV_THEOR__KAPPA_INIT=jv_theor__kappa_init, $
     JV_THEOR__KAPPALIMS=jv_theor__kappaLims, $   
     JV_THEOR__TEMPLIMS=jv_theor__TempLims, $    
     JV_THEOR__DENSLIMS=jv_theor__DensLims, $    
     JV_THEOR__MAGRATIOLIMS=jv_theor__magRatioLims, $
     JV_THEOR__FIT_JE=jv_theor__fit_je, $
     JV_THEOR__FIT_BOTH=jv_theor__fit_both, $
     ;; JV_THEOR__USE_MSPH_SOURCE=jv_theor__use_msph_source, $
     JV_THEOR__INITIAL_SOURCE_R_E=jv_theor__initial_source_R_E, $
     JV_THEOR__INITIAL_SOURCE__POLARSAT=jv_theor__initial_source__Polar, $
     JV_THEOR__INITIAL_SOURCE__EQUATOR=jv_theor__initial_source__equator, $
     JV_THEOR__ITERATIVE_DENSITY_AND_R_B_GAME=jv_theor__iterative_game, $
     JV_THEOR__ITERATIVE_GAME__TIE_RB_AND_DENS=jv_theor__itergame_tie_R_B_and_dens, $
     JV_THEOR__ITERATIVE_GAME__DENSITY_INCREASE=jv_theor__itergame_NFac, $
     ;; JV_THEOR__ADD_DENTON_ET_AL_2006_MODEL_COEFFS=add_Denton2006, $
     JVPOTBAR__J_ON_YAXIS=jvPotBar__j_on_yAxis, $
     JVPOTBAR__INTERACTIVE_OVERPLOT=interactive_overplot, $
     MAP__MULTI_MAGRATIO_ARRAY=map__multi_magRatio_array, $
     MAP__MULTI_KAPPA_ARRAY=map__multi_kappa_array, $
     MAP__2D=map__2D, $
     TN_YLOG_NDOWN=TN_yLog_nDown, $
     PLOT_J_V_POTBAR=plot_j_v_potBar, $
     PLOT_JV_A_LA_ELPHIC=plot_jv_a_la_Elphic, $
     PLOT_T_AND_N=plot_T_and_N, $
     PLOT_J_V_AND_THEORY=plot_j_v_and_theory, $
     PLOT_J_V__FIXED_T_AND_N=plot_j_v__fixed_t_and_n, $
     PLOT_J_V_MAP__R_B_AND_KAPPA__FIXED_T_AND_N=plot_j_v_map__r_b_and_kappa__fixed_t_and_n, $
     PLOT_MAGCURRENT_VS_CURRENT=plot_magCurrent_vs_current, $
     PLOT_EN_SPECS=plot_en_specs, $
     EN_SPECS__MOVIE=en_specs__movie, $
     A_LA_ELPHIC_SPNAME=a_la_Elphic_spName, $
     JVPOTBAR_SPNAME=jvpotBar_spName, $
     TN_SPNAME=TN_spName, $
     JV_THEOR_SPNAME=JV_theor_spName, $
     J_V__FIXTANDN__SPNAME=j_v__fixTandN__spName, $
     EN_SPEC__SPNAME=en_spec__spName, $
     ORIGINAL_PLOTIDEE=orig_plotIdee, $
     ORIGINATING_ROUTINE=routName, $
     SAVEPLOT=savePlot, $
     PLOTDIR=plotDir, $
     OUT_CURPOTLIST=curPotList, $
     OUT_JVPLOTDATA=jvPlotData, $
     OUT_AVGS_FOR_FITTING=avgs_JVfit, $
     OUT_SC_POT=out_sc_pot, $
     _REF_EXTRA=e

END
