;;2017/03/03
PRO JOURNAL__20170303__ORBIT_1773__COMPARE_DOWNGOING_CURRENT_WITH_CURRENT_OVER_ALL_ANGLE_RANGES

  COMPILE_OPT IDL2

  routName = 'JOURNAL__20170303__ORBIT_1773__COMPARE_DOWNGOING_CURRENT_WITH_CURRENT_OVER_ALL_ANGLE_RANGES'
  
  outDir  = '~/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'
  f1      = 'Elphic_et_al__Fig2__meal.sav'
  f2      = 'Elphic_et_al__Fig2__meal__checkJError_downgoing_e-aR_mom_eD_330-30-w_1Count.sav'

  RESTORE,outDir+f1
  cAll = curPotList[0] ;;over all angle ranges

  RESTORE,outDir+f2
  cS = curPotList[0] ;;over source-cone angles

  savePlot = 1
  sPName   = 'orb_1773-compare_down_and_up_electron_current.png'
  
  IF N_ELEMENTS(cS.time) GT N_ELEMENTS(cAll.time) THEN BEGIN

     aI    = LINDGEN(N_ELEMENTS(cAll.time)          )
     sI    = VALUE_CLOSEST2(cS.time,cAll.time)

  ENDIF ELSE BEGIN

     aI    = VALUE_CLOSEST2(cAll.time,cS.time)
     sI    = LINDGEN(N_ELEMENTS(cS.time)       )

  ENDELSE

  plotArr     = MAKE_ARRAY(3,/OBJ)
  yRange      = MINMAX([cS.cur[sI],cAll.cur[aI],cS.cur[sI]-cAll.cur[aI]])

  lineStyle   = [':','--','__']
  color       = ['black','red','blue']

  window      = WINDOW(DIMENSIONS=[900,600], $
                        BUFFER=savePlot)

  plotArr[0]  = PLOT(cS.time-cS.time[0],cAll.cur[aI]-cS.cur[sI], $
                     YRANGE=yRange, $
                     NAME='J!DA!N-J!DS!N', $
                     XTITLE='Seconds since ' + TIME_TO_STR(cS.time[sI[0]],/MS), $
                     YTITLE='Current density ($\micro$A/m!U2!N)', $
                     LINESTYLE=lineStyle[0], $
                     COLOR=color[0], $
                     /CURRENT)
  plotArr[1]  = PLOT(cS.time-cS.time[0],cS.cur[sI], $
                     NAME='J!DSource!N', $
                     YRANGE=yRange, $
                     LINESTYLE=lineStyle[1], $
                     COLOR=color[1], $
                     /OVERPLOT)
  plotArr[2]  = PLOT(cS.time-cS.time[0],cAll.cur[aI], $
                     NAME='J!DAll!N', $
                     YRANGE=yRange, $
                     LINESTYLE=lineStyle[2], $
                     COLOR=color[2], $
                     /OVERPLOT)

  leg         = LEGEND(TARGET=plotArr,POSITION=[25,10.5],/DATA)

  IF KEYWORD_SET(savePlot) THEN BEGIN
     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-believeIt.png'
     ENDIF
     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF
     PRINT,"Saving to " + sPName + ' ...'

     window.Save,plotDir+sPName
  ENDIF

END
