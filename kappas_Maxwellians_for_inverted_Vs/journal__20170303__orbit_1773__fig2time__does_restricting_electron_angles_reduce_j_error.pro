;2017/03/03
PRO JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR

  COMPILE_OPT IDL2

  routName                = 'JOURNAL__20170303__ORBIT_1773__FIG2TIME__DOES_RESTRICTING_ELECTRON_ANGLES_REDUCE_J_ERROR'
  
  Elphic1998_defaults     = 1

  error_estimates         = 1
  remake_masterFile       = 0
  map_to_100km            = 1

  add_oneCount_stats      = 1
  
  savePlot                = 1
  spName                  = 'errorbarsalso_downgoing_e.png'
  errorBarFac             = 1.

  ;; sPName                  = 'noErrorBars.png'
  ;; errorBarFac             = 0.00000001

  ;;get orbTimes here
  orbit                   = 1773
  ;; orbTimes                = ['97-02-01/09:25:40','97-02-01/09:26:30']
  ;; orbTimes                = ['97-02-01/09:25:40','97-02-01/09:29:30']
  orbTimes                = ['97-02-01/09:25:40','97-02-01/09:27:13']
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
  aRange__moments_e_down  = [330.,30.]
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

  ;;Time, the time
  time        = curPotList[edind].time

  ;;Current for plotting
  ;; cur         = curPotList[edind].cur
  ;; cur         = curPotList[edind].cur+curPotList[iuind].cur
  ;; cur         = curPotList[edind].cur+curPotList[euind].cur
  cur         = curPotList[edind].cur+curPotList[euind].cur+curPotList[iuind].cur

  ;;Errors
  curErr      = ABS(curPotList[edind].curErr) * errorBarFac
  ;; curErr      = TRANSPOSE([[cur-curErr],[cur+curErr]])

  posC_i      = WHERE(cur GT 0,nPos, $
                      COMPLEMENT=negC_i, $
                      NCOMPLEMENT=nNeg)

  ;;Et potential
  pot         = curPotList[edind].charE+curPotList[iuind].charE
  pot         = curPotList[edind].peakE+curPotList[iuind].peakE
  pot[posC_i] = curPotList[euind].peakE[posC_i]
  ;; pot[posC_i] = curPotList[euind].charE[posC_i]

  potErr      = ABS(curPotList[edind].peakErr+curPotList[iuind].peakErr)
  

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
  jRange      = MINMAX(cur)
  yRange      = [1,3e4]
  ;; plot        = PLOT(cur[safe_i], $
  ;;                    pot[safe_i], $
  ;;                    XRANGE=jRange, $
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
  tDiff        = (time-time[0])
  tMag         = tDiff/tDiff[-1]
  rgbTable     = 4

  IF KEYWORD_SET(orig_plotIdee) THEN BEGIN
     window    = WINDOW(DIMENSIONS=[900,600], $
                        BUFFER=savePlot)

     sPlot     = SCATTERPLOT(cur[safe_i], $
                             pot[safe_i], $
                             XRANGE=jRange, $
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
                             POSITION=[0.1,0.1,0.95,0.8], $
                             BUFFER=savePlot)

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

  ENDIF

  window1      = WINDOW(DIMENSIONS=[900,600], $
                        BUFFER=savePlot)
  p1pos        = [0.10,0.08,0.46,0.50]
  p2pos        = [0.10,0.53,0.46,0.94]
  p3pos        = [0.54,0.08,0.95,0.94]
  ;; cbpos        = [0.1,0.96,0.96,0.99]
  ;; cbpos        = [0.52,0.97,0.95,0.99]
  cbpos        = [0.10,0.97,0.95,0.99]
  nColors      = 256
  ;; nColors      = N_ELEMENTS(safe_i)
  ;; stretch      = INDGEN(nColors-1)
  ;; stretch      = -100
  ;; stretch      = MAKE_ARRAY(nColors-1,VALUE=-80)
  ;; stretch      = [REPLICATE(10,127),REPLICATE(-10,128)]
  transpose    = 1
  hammerCT     = COLORTABLE(4,STRETCH=stretch,NCOLORS=nColors,TRANSPOSE=transpose)

  CTInds       = BYTSCL(tMag[safe_i])
  ;; hammerCT     = COLORTABLE(4
  ;; plot_1       = SCATTERPLOT((time[safe_i]-time[safe_i[0]]), $
  ;;                            cur[safe_i], $
  ;;                            XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
  ;;                            YTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                            RGB_TABLE=hammerCT, $
  ;;                            MAGNITUDE=tMag[safe_i], $
  ;;                            ;; LINESTYLE='', $
  ;;                            SYMBOL='.', $
  ;;                            SYM_SIZE=3.0, $
  ;;                            /SYM_FILLED, $
  ;;                            /CURRENT, $
  ;;                            POSITION=p1pos)
  ;; plot_1       = ERRORPLOT((time[safe_i]-time[safe_i[0]]), $
  ;;                            cur[safe_i], $
  ;;                            curErr[safe_i], $
  ;;                            XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
  ;;                            YTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                            RGB_TABLE=hammerCT, $
  ;;                            VERT_COLORS=BYTSCL(tMag[safe_i]), $
  ;;                            ;; LINESTYLE='', $
  ;;                            SYMBOL='*', $
  ;;                            SYM_SIZE=3.0, $
  ;;                            /SYM_FILLED, $
  ;;                            /CURRENT, $
  ;;                            POSITION=p1pos)

  tRange            = [0,tDiff[safe_i[-1]]]
  errSym            = '.'
  errSym_size       = 3.0
  errSym_fill       = 0
  errSym_capSize    = 0.05
  ;; errJRange         = MINMAX(curErr)
  errJRange         = jRange

  ;;Initialize things
  inds              = [0,1]
  ;; tmpCurErr         = curErr[*,safe_i[inds]]
  tmpCurErr         = curErr[safe_i[inds]]

  p1Title           = 'Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]])
  plot_1            = ERRORPLOT((tDiff[safe_i[inds]]), $
                                cur[safe_i[inds]], $
                                tmpCurErr, $
                                XRANGE=tRange, $
                                ;; YRANGE=jRange, $
                                YRANGE=errJRange, $
                                XTITLE=p1Title, $
                                YTITLE='j!D||!N($\mu$A m!U-2!N)', $
                                RGB_TABLE=hammerCT, $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                VERT_COLORS=CTInds[inds], $
                                LINESTYLE='', $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                POSITION=p1pos, $
                                BUFFER=savePlot)

  ;;Now add all the other symbols
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     ;; tmpCurErr      = curErr[*,safe_i[inds]]
     tmpCurErr      = curErr[safe_i[inds]]

     plot_1         = ERRORPLOT((tDiff[safe_i[inds]]), $
                                cur[safe_i[inds]], $
                                tmpCurErr, $
                                ;; RGB_TABLE=hammerCT, $
                                VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                LINESTYLE='', $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                ;; POSITION=p1pos, $
                                /OVERPLOT)
  ENDFOR

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Second plot

  ;;The old, error bar–less way
  ;; plot_2            = SCATTERPLOT(tDiff[safe_i], $
  ;;                                 pot[safe_i], $
  ;;                                 ;; XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[0]]), $
  ;;                                 YTITLE='$\Phi$ (V)', $
  ;;                                 RGB_TABLE=hammerCT, $
  ;;                                 MAGNITUDE=tMag[safe_i], $
  ;;                                 ;; LINESTYLE='', $
  ;;                                 SYMBOL='.', $
  ;;                                 SYM_SIZE=3.0, $
  ;;                                 /SYM_FILLED, $
  ;;                                 /CURRENT, $
  ;;                                 POSITION=p2pos)

  inds              = [0,1]
  ;; tmpPotErr         = curErr[*,safe_i[inds]]
  tmpPotErr         = potErr[safe_i[inds]]

  ;; errPotRange       = MINMAX(potErr[safe_i])
  errPotRange       = MINMAX(pot[safe_i])
  plot_2            = ERRORPLOT((tDiff[safe_i[inds]]), $
                                pot[safe_i[inds]], $
                                tmpPotErr, $
                                XRANGE=tRange, $
                                /YLOG, $
                                YRANGE=yRange, $
                                YTITLE='$\Phi$ (V)', $
                                RGB_TABLE=hammerCT, $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                                VERT_COLORS=CTInds[inds], $
                                LINESTYLE='', $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                POSITION=p2pos, $
                                XSHOWTEXT=0B)
  ;; plot_2.xshowtext  = 0B

  ;;Now add all the other symbols
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     ;; tmpPotErr      = potErr[*,safe_i[inds]]
     tmpPotErr      = potErr[safe_i[inds]]

     plot_2         = ERRORPLOT((tDiff[safe_i[inds]]), $
                                pot[safe_i[inds]], $
                                tmpPotErr, $
                                /YLOG, $
                                VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                LINESTYLE='', $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                /OVERPLOT)
  ENDFOR

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Third plot

  this3       = ROUND_TO_NTH_DECIMAL_PLACE(ALOG10(yRange[1]))
  tickValues3 = (10L)^(LINDGEN(FIX(this3))+1)
  tickNames3  = !NULL
  ;; jvSym       = '*'
  ;; jvSymSize   = 2.0
  ;; jvSymThick  = 2.0
  jvSym       = '*'
  jvSymSize   = 2.0
  jvSymThick  = 2.0
  jvSymTransp = 70
  jvSymFilled = 1
  FOR k=0,this3-1 DO tickNames3 = [tickNames3,STRING(FORMAT='("10!U",I0,"!N")',k+1)]

  ;;The old, error bar–less way
  ;; plot_3      = SCATTERPLOT(cur[safe_i], $
  ;;                           pot[safe_i], $
  ;;                           XRANGE=jRange, $
  ;;                           YRANGE=yRange, $
  ;;                           /YLOG, $
  ;;                           RGB_TABLE=hammerCT, $
  ;;                           MAGNITUDE=tMag[safe_i], $
  ;;                           SYMBOL=jvSym, $
  ;;                           SYM_SIZE=jvSymSize, $
  ;;                           SYM_THICK=jvSymThick, $
  ;;                           SYM_TRANSPARENCY=jvSymTransp, $
  ;;                           SYM_FILLED=jvSymFilled, $
  ;;                           YTICKVALUES=tickValues3, $
  ;;                           YTICKNAME=tickNames3, $
  ;;                           XTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                           YTITLE='$\Phi$ (V)', $
  ;;                           XGRIDSTYLE=':', $
  ;;                           YGRIDSTYLE=':', $
  ;;                           XTICKLEN=1, $
  ;;                           YTICKLEN=1, $
  ;;                           XSUBTICKLEN=0.01, $
  ;;                           YSUBTICKLEN=0.01, $
  ;;                           /CURRENT, $
  ;;                           POSITION=p3pos)

  ;; plot_1            = ERRORPLOT((tDiff[safe_i[inds]]), $
  ;;                               cur[safe_i[inds]], $
  ;;                               tmpErr, $
  ;;                               XRANGE=tRange, $
  ;;                               ;; YRANGE=jRange, $
  ;;                               YRANGE=errJRange, $
  ;;                               XTITLE='Seconds since ' + TIME_TO_STR(curPotList[0].time[safe_i[inds]]), $
  ;;                               YTITLE='j!D||!N($\mu$A m!U-2!N)', $
  ;;                               /CURRENT, $
  ;;                               POSITION=p1pos)

  inds              = [0,1]
  ;; tmpCurErr         = curErr[*,safe_i[inds]]
  ;; tmpPotErr         = potErr[*,safe_i[inds]]
  tmpCurErr         = curErr[safe_i[inds]]
  tmpPotErr         = potErr[safe_i[inds]]

  plot_3      = ERRORPLOT(cur[safe_i[inds]], $
                          pot[safe_i[inds]], $
                          tmpCurErr, $
                          tmpPotErr, $
                          XRANGE=jRange, $
                          YRANGE=yRange, $
                          /YLOG, $
                          ;; RGB_TABLE=hammerCT, $
                          LINESTYLE='', $
                          ERRORBAR_COLOR=hammerCT[*,CTInds[0]], $
                          ERRORBAR_CAPSIZE=errSym_capSize, $
                          SYMBOL=errSym, $
                          SYM_SIZE=errSym_size, $
                          SYM_FILLED=errSym_fill, $
                          VERT_COLORS=CTInds[inds], $
                          ;; SYMBOL=jvSym, $
                          ;; SYM_SIZE=jvSymSize, $
                          ;; SYM_THICK=jvSymThick, $
                          ;; SYM_TRANSPARENCY=jvSymTransp, $
                          ;; SYM_FILLED=jvSymFilled, $
                          YTICKVALUES=tickValues3, $
                          YTICKNAME=tickNames3, $
                          XTITLE='j!D||!N($\mu$A m!U-2!N)', $
                          YTITLE='$\Phi$ (V)', $
                          XGRIDSTYLE=':', $
                          YGRIDSTYLE=':', $
                          XTICKLEN=1, $
                          YTICKLEN=1, $
                          XSUBTICKLEN=0.01, $
                          YSUBTICKLEN=0.01, $
                          /CURRENT, $
                          POSITION=p3pos)

  ;;Now add all the other symbols
  FOR k=2,N_ELEMENTS(safe_i)-1,2 DO BEGIN

     inds           = [k,k+1]
     tmpCurErr      = curErr[safe_i[inds]]
     tmpPotErr      = potErr[safe_i[inds]]

     plot_3         = ERRORPLOT((cur[safe_i[inds]]), $
                                pot[safe_i[inds]], $
                                tmpCurErr, $
                                tmpPotErr, $
                                ;; RGB_TABLE=hammerCT, $
                                LINESTYLE='', $
                                VERT_COLORS=hammerCT[*,CTInds[inds]], $
                                ERRORBAR_COLOR=hammerCT[*,CTInds[k]], $
                                ERRORBAR_CAPSIZE=errSym_capSize, $
                                SYMBOL=errSym, $
                                SYM_SIZE=errSym_size, $
                                SYM_FILLED=errSym_fill, $
                                /CURRENT, $
                                /OVERPLOT)
  ENDFOR


  ;;And a colorbar thing
  nTMarks     = 5
  tInds       = (INDGEN(nTMarks)*N_ELEMENTS(safe_i))/(nTMarks-1)
  tickValues  = tMag[safe_i[tInds]]
  tickTimes   = time[safe_i[tInds]]
  tickName    = STRMID(TIME_TO_STR(tickTimes),11,15)
  tMagRange   = [tMag[safe_i[0]],tMag[safe_i[-1]]]
  cb          = COLORBAR(RGB_TABLE=hammerCT, $
                         TICKNAME=tickName, $
                         TICKVALUES=tickValues, $
                         POSITION=cbpos, $
                         RANGE=tMagRange, $
                         ;; TEXT_ORIENTATION=180, $
                         /NORMAL)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-believeIt.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

  STOP
  
  ;;Why are errors so large during the hottest action?
  ;; this = PLOT(tDiff[safe_i], $
  ;;             (ABS(curerr/cur))[safe_i], $
  ;;             LINESTYLE='', $
  ;;             SYMBOL='*', $
  ;;             YRANGE=jrange)
END

