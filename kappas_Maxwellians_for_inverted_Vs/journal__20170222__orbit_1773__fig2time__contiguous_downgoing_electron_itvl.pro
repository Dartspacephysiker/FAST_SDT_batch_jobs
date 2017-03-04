;;2017/02/22
PRO JOURNAL__20170222__ORBIT_1773__FIG2TIME__CONTIGUOUS_DOWNGOING_ELECTRON_ITVL

  COMPILE_OPT IDL2

  routName                = 'JOURNAL__20170222__ORBIT_1773__FIG2TIME__CONTIGUOUS_DOWNGOING_ELECTRON_ITVL'
  
  Elphic1998_defaults     = 1

  error_estimates         = 1
  remake_masterFile       = 1
  map_to_100km            = 1

  add_oneCount_stats      = 1
  
  savePlot                = 1
  sPName                  = 'errorbarsalso.png'
  plot_times              = ['1997-02-01/09:25:41.0', $
                             '1997-02-01/09:27:13.4']
  plot_t1                 = STR_TO_TIME(plot_times[0])
  plot_t2                 = STR_TO_TIME(plot_times[1])

  ;; sPName                  = 'noErrorBars.png'
  ;; errorBarFac             = 0.00000001

  ;;get orbTimes here
  orbit                   = 1773
  orbTimes                = ['97-02-01/09:25:40','97-02-01/09:27:13']
  ;; orbTimes                = ['97-02-01/09:25:40','97-02-01/09:29:30']
  orbBurstTimes           = ['97-02-01/09:25:40','97-02-01/09:29:30']
  bonusPref               = '--Elphic_et_al_1998--Fig2'

  ;; downTimesStr            = '1997-02-01/' + $
  ;;                           [['09:26:12','09:26:23'], $ ;;These are the money times that seem to give good kappa fits
  ;;                            ['09:26:53','09:27:07.5']]

  ;; downTimesStr            = '1997-02-01/' + $
  ;;                           [['09:26:12','09:27:07.5']]

  ;; upTimesStr              = '1997-02-01/' + $
  ;;                           [['09:25:41.0','09:25:49.4'], $
  ;;                            ['09:25:56.75','09:26:02.9'], $
  ;;                            ['09:26:04.03','09:26:09.51'], $
  ;;                            ['09:27:07.1','09:27:13.4']]

  ;;They'll just walk up and bring you the keys! MO MONEY MO MONEY MO MONEY
  downTimesStr            = '1997-02-01/' + $
                            [['09:25:40','09:29:30']]

  upTimesStr              = downTimesStr

  ;; timesList               = LIST(downTimes,upTimes)

  ;; kStats_startStops__ees  = LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
  ;;                                                 ['09:27:05','09:27:15']])
  ;; kStats_startStops__eeb  = LIST('1997-02-01/' + [['09:26:12','09:26:23'], $
  ;;                                                 ['09:26:53','09:27:07.5']])

  units                   = 'eFlux'
  ;; units                = 'flux'
  ;; units                = 'dfStd'
  
  outDir                  = '~/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  datFile                 = 'Elphic_et_al__Fig2_ingredients.sav'

  saveCurPotFile          = 'Elphic_et_al__Fig2__meal.sav'
  save_diff_eFlux_file    = 1
  load_diff_eFlux_file    = 1
  ;; restore_fitFile         = 0

  ;;Which classic event?
  ;; '0 :  Elphic_et_al_1998'

  ;;survey window
  eeb_or_eesArr           = ['ees','ies']
  ;; eeb_or_eesArr        = ['eeb','ieb']

  order                   = [0,1,2]
  label                   = ['downgoing_e','upgoing_e','upgoing_i']

  ;;OPTIONS! OPTIONS! OPTIONS!
  aRange__moments_e_down  = [0.,360.]
  aRange__moments_i_up    = [0.,360.]

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  aRange__moments_list    = LIST(aRange__moments_e_down,!NULL,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(!NULL,!NULL,!NULL)
  aRange__charE_list      = LIST(!NULL,!NULL,!NULL)

  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

  ;; energyArr               = [[4e1,3.0e4],[4e1,3.0e4],[4,2.4e4]]
  use_sc_pot_for_lowerbound = 1
  energyArr               = [[0,3.0e4],[0,3.0e4],[0,2.4e4]]

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
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     ENERGYARR=energyArr, $
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
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
     MAP_TO_100KM=map_to_100km, $
     SAVECURPOTFILE=saveCurPotFile, $
     OUT_CURPOTLIST=curPotList

  PLOT_THREEPANEL_ANALOG_TO_FIG2_ELPHIC_ETAL_1998,curPotList, $
     T1=plot_t1, $
     T2=plot_t2, $
     ORIGINAL_PLOTIDEE=orig_plotIdee, $
     SAVEPLOT=savePlot, $
     SPNAME=spName, $
     ORIGINATING_ROUTINE=routName, $
     PLOTDIR=plotDir, $
     ERROR_BAR_FACTOR=errorBarFac

END

