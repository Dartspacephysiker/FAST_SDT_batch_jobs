PRO JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR

  COMPILE_OPT IDL2

  routName                = 'JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR'

  Elphic1998_defaults     = 1

  error_estimates         = 1
  remake_masterFile       = 1
  map_to_100km            = 1

  add_oneCount_stats      = 1

  plot_times              = ['1997-02-01/09:26:10.0', $
                             '1997-02-01/09:27:13.4']

  plot_t1                 = STR_TO_TIME(plot_times[0])
  plot_t2                 = STR_TO_TIME(plot_times[1])
  add_iu_pot              = 1
  use_all_currents        = 1

  interactive_overplot    = 0
  
  savePlot                = 0
  savePSuff               = '__fixedPotQQ'

  plot_jv_a_la_Elphic     = 0B
  ;; a_la_Elphic_spName      = 'errorbarsalso_downgoing_e.png'
  a_la_Elphic_spName      = 'errorbarsalso_downgoing_e' + savePSuff + '.png'

  plot_j_v_potBar         = 0B
  jvpotBar_spName         = 'j_vs_potBar__downgoing_e' + savePSuff + '.png'
  jvpotBar__j_on_yAxis    = 1

  plot_T_and_N            = 0B
  TN_spName               = 'T_and_N__downgoing_e' + savePSuff + '.png'
  
  ;;get orbTimes here
  orbit                   = 1773

  plot_j_v_and_theory     = 1B
  plot_j_ratios           = 0B
  plot_ion_elec_ratios    = 0B
  JV_theor_spName         = 'j_v_data_n_theory__downgoing_e' + savePSuff + '.png'
  jv_theor__minPot        = 1500
  jv_theor__maxPot        = 4000
  jv_theor__minCur        = 1D-6
  jv_theor__maxCur        = !NULL

  orbTimes                = plot_times
  orbBurstTimes           = plot_times
  bonusPref               = '--Elphic_et_al_1998--Fig2'


  ;;They'll just walk up and bring you the keys! MO MONEY MO MONEY MO MONEY
  downTimesStr            = plot_times

  upTimesStr              = downTimesStr

  ;; kStats_startStops__ees  = LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
  ;;                                                 ['09:27:05','09:27:15']])
  ;; kStats_startStops__eeb  = LIST('1997-02-01/' + [['09:26:12','09:26:23'], $
  ;;                                                 ['09:26:53','09:27:07.5']])

  units                   = 'eFlux'
  ;; units                = 'flux'
  ;; units                = 'dfStd'

  outDir                  = '~/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  datFile                 = 'Elphic_et_al__Fig2_ingredients__checkJError_downgoing_e.sav'

  saveCurPotFile          = 'Elphic_et_al__Fig2__meal__checkJError_downgoing_e.sav'
  save_diff_eFlux_file    = 1
  load_diff_eFlux_file    = 0
  ;; restore_fitFile         = 0

  ;;Which classic event?
  ;; '0 :  Elphic_et_al_1998'

  ;;survey window
  ;; eeb_or_eesArr           = ['ees','ies']
  eeb_or_eesArr           = ['eeb','ieb']
  spectra_average_interval = 4

  order                   = [0,1,2]
  label                   = ['downgoing_e','upgoing_e','upgoing_i']

  ;;OPTIONS! OPTIONS! OPTIONS!
  ;; aRange__moments_e_down  = [315.,45.]
  aRange__moments_e_down  = 'lc'
  ;; aRange__moments_i_up    = [0.,360.]

  aRange__moments_i_up    = '2lc'
  aRange__peakEn_i_up     = '2lc'
  aRange__charE_i_up      = '2lc'

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  aRange__moments_list    = LIST(aRange__moments_e_down,!NULL,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(!NULL,!NULL,aRange__peakEn_i_up)
  aRange__charE_list      = LIST(!NULL,!NULL,aRange__charE_i_up)

  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

  use_sc_pot_for_lowerbound = 1
  pot__save_file          = 0
  pot__from_fa_potential  = 1
  energyArr               = [[100,3.0e4],[0,3.0e4],[0,2.4e4]]

  ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
  ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
  min_peak_energyArr      = [300,100,100]
  max_peak_energyArr      = [3e4,3e4,2.4e4]

  CURRENT_AND_POTENTIAL_ANALYSIS, $
     ORBIT=orbit, $
     ORBTIMES=orbTimes, $
     ORBBURSTTIMES=orbBurstTimes, $
     BONUSPREF=bonusPref, $
     DOWNTIMESSTR=downTimesStr, $
     UPTIMESSTR=upTimesStr, $
     TIMESLIST=timesList, $
     UNITS=units, $
     OUTDIR=outDir, $
     DATFILE=datFile, $
     REMAKE_MASTERFILE=remake_masterFile, $
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
     EEB_OR_EESARR=eeb_or_eesArr, $
     ORDER=order, $
     LABEL=label, $
     ADD_ONECOUNT_STATS=add_oneCount_stats, $
     ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
     ARANGE__MOMENTS_E_UP=aRange__moments_e_up, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     ARANGE__PEAKEN_E_DOWN=aRange__peakEn_e_down, $
     ARANGE__PEAKEN_E_UP=aRange__peakEn_e_up, $
     ARANGE__PEAKEN_I_UP=aRange__peakEn_i_up, $
     ARANGE__CHARE_E_DOWN=aRange__charE_e_down, $
     ARANGE__CHARE_E_UP=aRange__charE_e_up, $
     ARANGE__CHARE_I_UP=aRange__charE_i_up, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     ENERGYARR=energyArr, $
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
     ;; DENS_ERRORS=dens_errors, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     MAP_TO_100KM=map_to_100km, $
     SAVECURPOTFILE=saveCurPotFile, $
     OUT_CURPOTLIST=curPotList

  CURRENT_AND_POTENTIAL_PLOTDATA_PREP,curPotList,jvPlotData, $
                                      T1=plot_t1, $
                                      T2=plot_t2, $
                                      USE_ALL_CURRENTS=use_all_currents, $
                                      USE_DOWNGOING_ELECTRON_CURRENT=use_ed_current, $
                                      USE_UPGOING_ION_CURRENT=use_iu_current, $
                                      USE_UPGOING_ELECTRON_CURRENT=use_eu_current, $
                                      USE_CHAR_EN_FOR_DOWNPOT=use_charE_for_downPot, $
                                      USE_PEAK_EN_FOR_DOWNPOT=use_peakE_for_downPot, $
                                      ADD_UPGOING_ION_POT=add_iu_pot, $
                                      ERROR_BAR_FACTOR=errorBarFac

  IF KEYWORD_SET(plot_jv_a_la_Elphic) THEN BEGIN
     PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,jvPlotData, $
        ORIGINAL_PLOTIDEE=orig_plotIdee, $
        SAVEPLOT=savePlot, $
        SPNAME=a_la_Elphic_spName, $
        ORIGINATING_ROUTINE=routName, $
        PLOTDIR=plotDir
  ENDIF

  IF KEYWORD_SET(plot_j_v_potBar) THEN BEGIN
     PLOT_J_VS_POTBAR,jvPlotData, $
                      J_ON_YAXIS=jvPotBar__j_on_yAxis, $
                      SAVEPLOT=savePlot, $
                      SPNAME=jvpotBar_spName, $
                      INTERACTIVE_OVERPLOT=interactive_overplot, $
                      ORIGINATING_ROUTINE=routName, $
                      PLOTDIR=plotDir
  ENDIF

  IF KEYWORD_SET(plot_T_and_N) THEN BEGIN
     PLOT_TEMPERATURE_AND_DENSITY_TSERIES, $
        jvPlotData, $
        ORIGINAL_PLOTIDEE=orig_plotIdee, $
        SAVEPLOT=savePlot, $
        SPNAME=TN_spName, $
        ORIGINATING_ROUTINE=routName, $
        PLOTDIR=plotDir, $
        OUT_WINDOW=window1, $
        OVERPLOTALL=overplotAll, $
        OVERPLOT_WINDOW=overplot_window
  ENDIF

  relChange_TDown = (JVPlotData.TDown [1:-1]-JVPlotData.TDown [0:-2])/JVPlotData.TDown [0:-2]
  relChange_NDown = (JVPlotData.NDown [1:-1]-JVPlotData.NDown [0:-2])/JVPlotData.NDown [0:-2]

  ;; smochange_TDown = WHERE(ABS(relChange_TDown) LE 0.1*JVPlotData.TDownErr/JVPlotData.TDown)
  ;; smochange_NDown = WHERE(ABS(relChange_NDown) LE 0.1*JVPlotData.NDownErr/JVPlotData.NDown)
  fracChange_TDown = 0.25
  fracChange_NDown = 0.25
  fracError_TDown  = 0.20
  fracError_NDown  = 0.10

  smochange_TDown = WHERE(ABS(relChange_TDown) LE fracChange_TDown)
  smochange_NDown = WHERE(ABS(relChange_NDown) LE fracChange_NDown)

  otrasCondiciones = WHERE((jvplotdata.cur LE 0) AND $
                           (ABS(JVPlotData.TDownErr/JVPlotData.TDown) LE fracError_TDown) AND $
                           (ABS(JVPlotData.NDownErr/JVPlotData.NDown) LE fracError_NDown))

  useInds         = CGSETINTERSECTION(smochange_NDown,smochange_TDown,COUNT=nSmo)

  useInds         = CGSETINTERSECTION(useInds,otrasCondiciones,COUNT=nSmo)
  IF KEYWORD_SET(plot_j_v_and_theory) THEN BEGIN

     PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                         CURPOTLIST=curPotList, $
                                         MINPOT=jv_theor__minPot, $
                                         MAXPOT=jv_theor__maxPot, $
                                         MINCUR=jv_theor__minCur, $
                                         MAXCUR=jv_theor__maxCur, $
                                         USEINDS=useInds, $
                                         PLOT_J_RATIOS=plot_j_ratios, $
                                         PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                         ORIGINATING_ROUTINE=routName, $
                                         PLOTDIR=plotDir, $
                                         SAVEPLOT=savePlot, $
                                         SPNAME=JV_theor_spName, $
                                         OUT_AVGS_FOR_FITTING=avgs_JVfit


  ENDIF

  ESTIMATE_JV_CURVE_FROM_AVERAGE_PARAMS,jvPlotData,avgs_JVfit  

  useInds      = useInds[SORT(jvplotdata.time[useInds])]

  PRINT,FORMAT='(I0,T5,A0,T35,A0,T45,A0,T55,A0,T65,A0)', $
        'i','Time','Temp','N','Pot','Current'
  FOR k=0,nSmo-1 DO BEGIN
     PRINT,FORMAT='(I0,T5,A0,T35,F-8.2,T45,F-8.2,T55,F-8.2,T65,F-8.2)', $
           k, $
           TIME_TO_STR(JVPlotData.time[useInds[k]]), $
           JVPlotData.TDown[useInds[k]], $
           JVPlotData.NDown[useInds[k]], $
           JVPlotData.pot[useInds[k]], $
           JVPlotData.cur[useInds[k]]
  ENDFOR
  PRINT,FORMAT='(A0,T35,F-8.2,T45,F-8.2,T55,F-8.2,T65,F-8.2)', $
        "Avg", $
        MEAN(JVPlotData.TDown[useInds]), $
        MEAN(JVPlotData.NDown[useInds]), $
        MEAN(JVPlotData.pot[useInds]), $
        MEAN(JVPlotData.cur[useInds])


  STOP

END
