;;2017/02/22
PRO JOURNAL__20170222__ORBIT_1773__FIG2TIME__CONTIGUOUS_DOWNGOING_ELECTRON_ITVL

  COMPILE_OPT IDL2

  Elphic1998_defaults     = 1

  error_estimates         = 1
  dens_errors             = 1
  remake_masterFile       = 0
  map_to_100km            = 1

  ;;get orbTimes here
  orbit                   = 1773
  orbTimes                = ['97-02-01/09:25:40','97-02-01/09:26:30']
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
  
  outDir                  = '~/software/sdt/batch_jobs/saves_output_etc/'
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

  aRange__moments_e_down  = [0.,360.]
  aRange__moments_i_up    = [0.,360.]

  label__which_eeb        = [0,0,1]
  label__which_times      = [0,1,0]
  energyArr               = [[4e1,3.0e4],[4e1,3.0e4],[4,2.4e4]]
  aRange__moments_list    = LIST(aRange__moments_e_down,!NULL,aRange__moments_i_up)
  aRange__peakEn_list     = LIST(!NULL,!NULL,!NULL)
  aRange__charE_list      = LIST(!NULL,!NULL,!NULL)

  ;; min_peak_energy      = KEYWORD_SET(upgoing) ? 100 : 500
  ;; max_peak_energy      = KEYWORD_SET(upgoing) ? 3e4 : !NULL
  min_peak_energyArr      = [300,100,100]
  max_peak_energyArr      = [3e4,3e4,2.4e4]

  ;;If doing upgoing electrons
  peak_energy__start_at_highEArr  = [0,1,1]
  upgoingArr                      = [0,1,1]

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
     ARANGE__MOMENTS_E_DOWN=aRange__moments_e_down, $
     ARANGE__MOMENTS_I_UP=aRange__moments_i_up, $
     WHICH_EEB__LABEL=label__which_eeb, $
     WHICH_TIMES__LABEL=label__which_times, $
     ENERGYARR=energyArr, $
     ARANGE__MOMENTS_LIST=aRange__moments_list, $
     ARANGE__PEAKEN_LIST=aRange__peakEn_list, $
     ARANGE__CHARE_LIST=aRange__charE_list, $
     ELPHIC1998_DEFAULTS=Elphic1998_defaults, $
     MIN_PEAK_ENERGYARR=min_peak_energyArr, $
     MAX_PEAK_ENERGYARR=max_peak_energyArr, $
     PEAK_ENERGY__START_AT_HIGHEARR=peak_energy__start_at_highEArr, $
     UPGOINGARR=upgoingArr, $
     ERROR_ESTIMATES=error_estimates, $
     DENS_ERRORS=dens_errors, $
     MAP_TO_100KM=map_to_100km, $
     SAVECURPOTFILE=saveCurPotFile, $
     OUT_CURPOTLIST=curPotList

  looking = 3B
  ind     = 0
  WHILE (looking GT 0) DO BEGIN
     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
        looking--
        edind = ind
     ENDIF

     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
        looking--
        euind = ind
     ENDIF

     IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
        looking--
        iuind = ind
     ENDIF
     ind++
  ENDWHILE

  IF ~(ARRAY_EQUAL(curPotList[0].time,curPotList[1].time) AND $
       ARRAY_EQUAL(curPotList[0].time,curPotList[2].time))    $
  THEN BEGIN
     IF (N_ELEMENTS(curPotList[0].time) NE N_ELEMENTS(curPotList[1].time)) OR $
        (N_ELEMENTS(curPotList[0].time) NE N_ELEMENTS(curPotList[2].time))    $
     THEN BEGIN
        PRINT,"Death!"
        STOP
     ENDIF

     IF ((WHERE(ABS(curPotList[0].time-curPotList[1].time) GT 1.))[0] NE -1) OR $
        ((WHERE(ABS(curPotList[0].time-curPotList[2].time) GT 1.))[0] NE -1)    $
     THEN BEGIN
        PRINT,"Whoa!"
        STOP
     ENDIF
  ENDIF

  ;; cur         = curPotList[edind].cur
  ;; cur         = curPotList[edind].cur+curPotList[iuind].cur
  ;; cur         = curPotList[edind].cur+curPotList[euind].cur
  time        = curPotList[edind].time
  cur         = curPotList[edind].cur+curPotList[euind].cur+curPotList[iuind].cur
  posC_i      = WHERE(cur GT 0,nPos, $
                      COMPLEMENT=negC_i, $
                      NCOMPLEMENT=nNeg)

  pot         = curPotList[edind].charE+curPotList[iuind].charE
  pot         = curPotList[edind].peakE+curPotList[iuind].peakE
  pot[posC_i] = curPotList[euind].peakE[posC_i]
  ;; pot[posC_i] = curPotList[euind].charE[posC_i]
  

  safe_i      = WHERE((curPotList[edind].peakE GE 0.) OR  $
                      (curPotList[euind].peakE GE 0.) OR  $
                      (curPotList[euind].peakE GE 0.),    $
                      nSafe)

  IF nSafe LT 3 THEN STOP

  safe_i      = CGSETINTERSECTION(safe_i, $
                                  WHERE((curpotlist[0].n/curpotlist[0].n1 GT 3) OR $
                                        (curpotlist[1].n/curpotlist[1].n1 GT 3) OR $
                                        (curpotlist[2].n/curpotlist[2].n1 GT 3)))
  t1 = STR_TO_TIME('1997-02-01/09:25:41.0')
  t2 = STR_TO_TIME('1997-02-01/09:27:13.4')
  time_i      = WHERE(curPotList[edind].time GE t1 AND $
                      curPotList[edind].time LE t2,nTime)
  IF nTime LT 3 THEN STOP
  safe_i      = CGSETINTERSECTION(safe_i,time_i,COUNT=nSafe,NORESULT=-1)
  
  IF nSafe LT 3 THEN STOP

  ;; xRange      = [-10,15]
  xRange      = MINMAX(cur)
  yRange      = [1,3e4]
  ;; plot        = PLOT(cur[safe_i], $
  ;;                    pot[safe_i], $
  ;;                    XRANGE=xRange, $
  ;;                    YRANGE=yRange, $
  ;;                    /YLOG, $
  ;;                    LINESTYLE='', $
  ;;                    SYMBOL='*', $
  ;;                    XTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                    YTITLE='$\Phi$ (V)', $
  ;;                    XGRIDSTYLE=':', $
  ;;                    YGRIDSTYLE=':', $
  ;;                    XTICKLEN=1, $
  ;;                    YTICKLEN=1, $
  ;;                    XSUBTICKLEN=0.01, $
  ;;                    YSUBTICKLEN=0.01)

  ;; tMag         = (time[safe_i]-time[safe_i[0]])
  tMag         = (time-time[0])
  tMag        /= tMag[-1]
  window       = WINDOW(DIMENSIONS=[900,600])
  rgbTable     = 4
  sPlot        = SCATTERPLOT(cur[safe_i], $
                             pot[safe_i], $
                             XRANGE=xRange, $
                             YRANGE=yRange, $
                             /YLOG, $
                             RGB_TABLE=rgbTable, $
                             MAGNITUDE=tMag[safe_i], $
                             SYMBOL='*', $
                             SYM_SIZE=2.0, $
                             SYM_THICK=2.0, $
                             SYM_TRANSPARENCY=70, $
                             XTITLE='j!D||!N($\mu$A m!U-2!N)', $
                             YTITLE='$\Phi$ (V)', $
                             XGRIDSTYLE=':', $
                             YGRIDSTYLE=':', $
                             XTICKLEN=1, $
                             YTICKLEN=1, $
                             XSUBTICKLEN=0.01, $
                             YSUBTICKLEN=0.01, $
                             /CURRENT, $
                             POSITION=[0.1,0.1,0.95,0.8])

  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*N_ELEMENTS(safe_i))/(nTMarks-1)
  tickValues  = tMag[safe_i[tInds]]
  tickTimes   = time[safe_i[tInds]]
  tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
  tMagRange   = [tMag[safe_i[0]],tMag[safe_i[-1]]]
  cb          = COLORBAR(RGB_TABLE=rgbTable, $
                         TICKNAME=tickName, $
                         TICKVALUES=tickValues, $
                         POSITION=[0.1,0.96,0.95,0.98], $
                         RANGE=tMagRange, $
                         ;; TEXT_ORIENTATION=180, $
                         /NORMAL)

  STOP

END
