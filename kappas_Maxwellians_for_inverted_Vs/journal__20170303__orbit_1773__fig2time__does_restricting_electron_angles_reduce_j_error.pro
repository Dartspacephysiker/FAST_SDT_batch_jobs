;2017/03/03
;Here are the winners from 2017/03/04's headbangerfest:
;  aRange__moments_e_down  = [330.,30.]
;  energyArr               = [[300,3.0e4],[0,3.0e4],[0,2.4e4]]

PRO JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR

  COMPILE_OPT IDL2

  routName                = 'JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR'

  Elphic1998_defaults     = 1

  error_estimates         = 1
  remake_masterFile       = 0
  map_to_100km            = 1

  add_oneCount_stats      = 1

  ;; plot_times              = ['1997-02-01/09:25:41.0', $
  ;;                            '1997-02-01/09:27:13.4']
  plot_times              = ['1997-02-01/09:26:10.0', $
                             '1997-02-01/09:27:13.4']

  plot_t1                 = STR_TO_TIME(plot_times[0])
  plot_t2                 = STR_TO_TIME(plot_times[1])
  add_iu_pot              = 1
  use_all_currents        = 1

  interactive_overplot    = 0
  
  savePlot                = 1
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


  orbTimes                = plot_times
  orbBurstTimes           = plot_times
  bonusPref               = '--Elphic_et_al_1998--Fig2'


  ;;They'll just walk up and bring you the keys! MO MONEY MO MONEY MO MONEY
  ;; downTimesStr            = '1997-02-01/' + $
  ;;                           [['09:25:40','09:29:30']]
  downTimesStr            = plot_times

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
  datFile                 = 'Elphic_et_al__Fig2_ingredients__checkJError_downgoing_e.sav'

  saveCurPotFile          = 'Elphic_et_al__Fig2__meal__checkJError_downgoing_e.sav'
  save_diff_eFlux_file    = 0
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
  ;; aRange__moments_e_down  = [315.,45.]
  aRange__moments_e_down  = 'lc'
  aRange__moments_i_up    = [0.,360.]

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  aRange__moments_list    = LIST(aRange__moments_e_down,!NULL,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(!NULL,!NULL,!NULL)
  aRange__charE_list      = LIST(!NULL,!NULL,!NULL)

  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

  use_sc_pot_for_lowerbound = 1
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
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     ENERGYARR=energyArr, $
     USE_SC_POT_FOR_LOWERBOUND=use_sc_pot_for_lowerbound, $
     POT__FROM_FA_POTENTIAL=pot__from_fa_potential, $
     POT__CHASTON_STYLE=pot__Chaston_style, $
     POT__FROM_FILE=pot__from_file, $
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

  STOP

  negcur_i      = WHERE(jvplotdata.cur LE 0)
  negcur_i      = negcur_i[SORT(jvplotdata.pot[negcur_i])]

  ;;The points that have a clear affinity for kappa = 2
  thesepointslovekappa_ii = WHERE((jvplotdata.pot[negcur_i] LE 4000) AND (jvplotdata.cur[negcur_i]*(-1D-6) GE 1D-6),nLovers)
  PRINT,"THESE POINTS LOVE KAPPA=2.0"
  loveKappa_i = negcur_i[thesepointslovekappa_ii]
  GET_STREAKS,loveKappa_i[SORT(loveKappa_i)],START_I=loveKappa_iStrt_ii,STOP_I=loveKappa_iStop_ii,OUT_STREAKLENS=streakLens
  times = TIME_TO_STR(jvplotdata.time[loveKappa_i[SORT(jvplotdata.time[loveKappa_i])]],/MS)
  FOR k=0,nLovers-1 DO BEGIN
     PRINT,TIME_TO_STR(jvplotdata.time[loveKappa_i[k]])
  ENDFOR

  useInds  = negcur_i
  useInds  = loveKappa_i

  ;; SAVE,KnightRelat30,KnightRelat300,KnightRelat3000,jvplotdata,FILENAME=
  ;; RESTORE,'
  R_Bs__M           = [30,300,3000]
  R_Bs__K           = [30,300,3000]
  kappas            = [2.0,2.0,2.0,1.6]
  kappa             = 2.0
  kappa1            = 1.6
  kap3001name       = STRING(FORMAT='("R!DB!N = ",I0," ($\kappa$=",F0.2,",T*=",I0,")")',R_B1,kappa1,TmultFac)
  TmultFac          = 1
  R_B1              = 3000

  nR_Bs__M          = N_ELEMENTS(R_Bs__M)
  nR_Bs__K          = N_ELEMENTS(R_Bs__K)
  nDer              = N_ELEMENTS(useInds)

  maxwellJVs        = MAKE_ARRAY(nR_Bs__M,nDer,/DOUBLE)
  kappaJVs          = MAKE_ARRAY(nR_Bs__K,nDer,/DOUBLE)

  MaxwellTransp     = 30
  MaxwellSym        = '*'
  MaxwellColors     = ['Red','Brown','Dark Green']
  MaxwellLinestyle  = ['']
  MaxwellNames      = 'R!DB!N = ' + STRING('(I0)',R_Bs__M[k])

  kappaTransp       = 30
  kappaSym          = ['x','tu']
  kappaColors       = ['Purple','Brown']
  kappaLinestyle    = ['']
  kappaNames        = 'R!DB!N = ' + STRING('(I0)',R_Bs__K) + STRING(' ("$\kappa$=",F0.2")")',kappas)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     maxwellJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
                                                        jvplotdata.ndown[useInds], $
                                                        jvplotdata.pot[useInds], $
                                                        R_Bs__M[k], $
                                                        /NO_MULT_BY_CHARGE)

  ENDFOR

  ;; KnightRelat300 = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
  ;;                                                   jvplotdata.ndown[useInds], $
  ;;                                                   jvplotdata.pot[useInds], $
  ;;                                                   300, $
  ;;                                                   /NO_MULT_BY_CHARGE)
  ;; KnightRelat3000 = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
  ;;                                                    jvplotdata.ndown[useInds], $
  ;;                                                    jvplotdata.pot[useInds], $
  ;;                                                    3000, $
  ;;                                                    /NO_MULT_BY_CHARGE)

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
                                                       jvplotdata.ndown[useInds], $
                                                       jvplotdata.pot[useInds], $
                                                       R_Bs__K[k], $
                                                       /NO_MULT_BY_CHARGE)

  ENDFOR
  ;; kRelat300 = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
  ;;                                                   jvplotdata.ndown[useInds], $
  ;;                                                   jvplotdata.pot[useInds], $
  ;;                                                   300, $
  ;;                                                   /NO_MULT_BY_CHARGE)
  ;; kRelat3000 = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
  ;;                                                jvplotdata.ndown[useInds], $
  ;;                                                jvplotdata.pot[useInds], $
  ;;                                                3000, $
  ;;                                                /NO_MULT_BY_CHARGE)
  ;; ;; kRelat3001 = KNIGHT_RELATION__DORS_KLETZING_11(kappa1,jvplotdata.tdown[useInds]*20., $
  ;; kRelat3001  = KNIGHT_RELATION__DORS_KLETZING_11(kappa1,jvplotdata.tdown[useInds]*TmultFac, $
  ;;                                                 jvplotdata.ndown[useInds], $
  ;;                                                 jvplotdata.pot[useInds], $
  ;;                                                 3000, $
  ;;                                                 /NO_MULT_BY_CHARGE)

  MaxwellPlots = MAKE_ARRAY(nR_Bs__M,/OBJ)
  kappaPlots   = MAKE_ARRAY(nR_Bs__K,/OBJ)
  wind         = WINDOW(DIMENSIONS=[1000,800])
  yLog         = 0
  dataLStyle   = ''
  dataSym      = 'o'
  dataName     = 'Data'
  xTitle       = 'Potential (V)'
  yTitle       = 'Current density ($\mu$A/m!U2!N), mapped to 100km'

  dataplot     = PLOT(jvplotdata.pot[useInds], $
                  jvplotdata.cur[useInds]*(-1D-6), $
                  LINESTYLE=dataLStyle, $
                  SYMBOL=dataSym, $
                  XTITLE=xTitle, $
                  YTITLE=yTitle, $
                  NAME=dataName, $
                  YLOG=yLog, $
                  /CURRENT)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     MaxwellPlots[k] = PLOT(jvplotdata.pot[useInds], $
                            MaxwellJVs[k,*], $
                            TRANSPARENCY=MaxwellTransp, $
                            LINESTYLE=MaxwellLinestyle[k], $
                            SYMBOL=MaxwellSym, $
                            COLOR=MaxwellColors[k], $
                            /OVERPLOT, $
                            NAME=MaxwellNames[k])
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaPlots[k] = PLOT(jvplotdata.pot[useInds], $
                        kappaJVs[*,k], $
                        TRANSPARENCY=kappaTransp, $
                        LINESTYLE='', $
                        SYMBOL=kappaSym[k], $
                        COLOR=kappaColors[k], $
                        /OVERPLOT, $
                        NAME=kappaNames[k])
  ENDFOR
  leg = LEGEND(TARGET=[dataplot, $
                       MaxwellPlots, $
                       kappaPlots])

  STOP

END
