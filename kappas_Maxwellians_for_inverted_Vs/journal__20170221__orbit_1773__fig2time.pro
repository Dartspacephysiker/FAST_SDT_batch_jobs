;;2017/02/21
PRO JOURNAL__20170221__ORBIT_1773__FIG2TIME

  COMPILE_OPT IDL2

  error_estimates         = 1
  dens_errors             = 1
  remake_masterFile       = 1
  
  ;;get orbTimes here
  orbit                   = 1773
  orbTimes                = ['97-02-01/09:25:50','97-02-01/09:27:30']
  orbBurstTimes           = ['97-02-01/09:26:05','97-02-01/09:27:27']
  bonusPref               = '--Elphic_et_al_1998--Fig2'

  downTimesStr            = '1997-02-01/' + $
                            [['09:26:12','09:26:23'], $ ;;These are the money times that seem to give good kappa fits
                             ['09:26:53','09:27:07.5']]

  ;; downTimesStr            = '1997-02-01/' + $
  ;;                           [['09:26:12','09:27:07.5']]

  upTimesStr              = '1997-02-01/' + $
                            [['09:25:41.0','09:25:49.4'], $
                             ['09:25:56.75','09:26:02.9'], $
                             ['09:26:04.03','09:26:09.51'], $
                             ['09:27:07.1','09:27:13.4']]

  downTimes               = REFORM(STR_TO_TIME(downTimesStr),SIZE(downTimesStr,/DIMENSIONS))
  upTimes                 = REFORM(STR_TO_TIME(upTimesStr  ),SIZE(upTimesStr  ,/DIMENSIONS))

  ;; downTimes               = STR_TO_TIME(downTimesStr)
  ;; upTimes                 = STR_TO_TIME(upTimesStr  )
  ;; downTimes               = REFORM(downTimes,SIZE(downTimesStr,/DIMENSIONS))
  ;; upTimes                 = REFORM(upTimes  ,SIZE(upTimesStr  ,/DIMENSIONS))

  timesList               = LIST(downTimes,upTimes)

  ;; kStats_startStops__ees  = LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
  ;;                                                 ['09:27:05','09:27:15']])
  ;; kStats_startStops__eeb  = LIST('1997-02-01/' + [['09:26:12','09:26:23'], $
  ;;                                                 ['09:26:53','09:27:07.5']])

  units                   = 'eFlux'
  ;; units                = 'flux'
  ;; units                = 'dfStd'
  
  outDir                  = '~/software/sdt/batch_jobs/saves_output_etc/'
  datFile                 = 'Elphic_et_al__Fig2_ingredients.sav'

  save_diff_eFlux_file    = 1
  load_diff_eFlux_file    = 1
  ;; restore_fitFile         = 0

  ;;Which classic event?
  ;; '0 :  Elphic_et_al_1998'

  ;;survey window
  eeb_or_eesArr           = ['ees','ies']
  ;; eeb_or_eesArr        = ['eeb','ieb']

  order                   = [0,2,1]
  label                   = ['downgoing_e','upgoing_e','upgoing_i']

  aRange__moments_e_down  = [0,360]
  aRange__moments_i_up    = [0,360]

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  energyArr               = [[3e1,3.0e4],[3e1,3.0e4],[1e2,2.4e4]]
  aRange__moments_list    = LIST(aRange__moments_e_down,!NULL,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(!NULL,!NULL,[-150,150])
  aRange__charE_list      = LIST(!NULL,!NULL,!NULL)

  ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
  ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
  min_peak_energyArr      = [300,100,100]
  max_peak_energyArr      = [3e4,3e4,2.4e4]

  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

  CURRENT_AND_POTENTIAL_ANALYSIS, $
     ERROR_ESTIMATES=error_estimates, $
     DENS_ERRORS=dens_errors, $
     REMAKE_MASTERFILE=remake_masterFile, $
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
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
     EEB_OR_EESARR=eeb_or_eesArr, $
     ORDER=order, $
     LABEL=label, $
     ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     ENERGYARR=energyArr, $
     ARANGE__MOMENTS_LIST=aRange__moments_list, $
     ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
     ARANGE__CHARE_LIST=aRange__charE_list, $
     MIN_PEAK_ENERGYARR=min_peak_energyArr, $
     MAX_PEAK_ENERGYARR=max_peak_energyArr, $
     PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
     UPGOINGARR=upgoingArr

END


