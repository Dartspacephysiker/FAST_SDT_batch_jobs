;2018/01/10
PRO JOURNAL__20180110__TEMPERATURE_AS_A_FUNC_OF_ANGLE

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  diff_eFlux_file = 'orb_1773-diff_eflux-ees-avg_itvl2-09_26_10__000-09_27_15__000.sav'

  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  plotPref = 'orb_1773-diff_eflux-ees-avg_itvl2'

  RESTORE,dir+diff_eFlux_file

  eRange = [600,30100]

  tidStr = '1997-02-01/09:27:01.57'

  ;; Plot opts
  TRange = [0,1000]

  ;; Plot strings
  tid    = S2T(tidStr)
  fTidStr = STRJOIN(STRSPLIT( $
            (plotPref + '-' + (STRSPLIT(tidStr,'/',/EXTRACT))[1]), $
            ':',/EXTRACT), $
                    '_')
  fTidStr = STRJOIN(STRSPLIT(fTidStr,'.',/EXTRACT),'__')

  eRStr   = (STRING(FORMAT='("-",F3.1,"-",I2,"keV")',eRange/1000.)).Replace('.','_')

  densStr = fTidStr + eRStr + '-dens.png'
  jStr    = fTidStr + eRStr + '-j.png'
  curStr  = fTidStr + eRStr + '-current.png'
  jeStr   = fTidStr + eRStr + '-je.png'
  tempStr = fTidStr + eRStr + '-temp.png'

  this   = VALUE_CLOSEST2(diff_eFlux.time,tid,/CONSTRAINED)
  PRINT,T2S(diff_eFlux.time[this],/MS)

  dat   = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,this)

  ;; aRange = [350,10]
  ;; step   = 10
  aRange   = [355,5]
  step     = 5
  N        = aRange[0]/step/2
  TArr     = MAKE_ARRAY(4,N+1,/FLOAT)
  NArr     = MAKE_ARRAY(N+1,/FLOAT)
  JArr     = MAKE_ARRAY(N+1,/FLOAT)
  JEArr    = MAKE_ARRAY(N+1,/FLOAT)
  jjeCovarArr = MAKE_ARRAY(N+1,/FLOAT)
  ;; TErrArr  = MAKE_ARRAY(4,N+1,/FLOAT)
  ;; NErrArr  = MAKE_ARRAY(N+1,/FLOAT)
  ;; JErrArr  = MAKE_ARRAY(N+1,/FLOAT)
  ;; JEErrArr = MAKE_ARRAY(N+1,/FLOAT)
  aArr     = MAKE_ARRAY(2,N+1,/FLOAT)
  errThingArr = MAKE_BLANK_GERSHMAN_ERROR_STRUCT( $
                N+1,13, $
                PRESSURE_COVAR_CALC=1, $
                HEATFLUX_COVAR_CALC=1)

  FOR k=0,N DO BEGIN
     aRTemp = [aRange[0]-k*step,aRange[1]+k*step]
     IF k EQ N THEN aRTemp = [0,360]

     TArr[*,k] = T_2D_FS(dat,ENERGY=eRange, $
                        ANGLE=aRTemp)

     NArr[k] = N_2D_FS(dat,ENERGY=eRange, $
                       ANGLE=aRTemp)
     JArr[k] = J_2D_FS(dat,ENERGY=eRange, $
                       ANGLE=aRTemp)
     JEArr[k] = JE_2D_FS(dat,ENERGY=eRange, $
                       ANGLE=aRTemp)
     jjeCovarArr[k] = JE_2D_FS(dat,ENERGY=eRange, $
                       ANGLE=aRTemp,/JJE)

     aArr[*,k] = aRTemp

     PRINT,FORMAT='("[",I0,",",I0,"]",T15,4(F7.2,:,TR3),F6.3)',aRTemp,TArr[*,k],NArr[k]

     errThing    = MOMENTERRORS_2D(dat, $
                                   ;; ENERGY=en, $
                                   ENERGY=eRange, $
                                   ANGLE=aRTemp, $
                                   BINS=bins, $
                                   PRESSURE_COVAR_CALC=1, $
                                   HEATFLUX_COVAR_CALC=1)

     errThingArr.N[k]      = errThing.N   
     errThingArr.Ux[k]     = errThing.Ux  
     errThingArr.Uy[k]     = errThing.Uy  
     errThingArr.Uz[k]     = errThing.Uz  
     errThingArr.Pxx[k]    = errThing.Pxx 
     errThingArr.Pyy[k]    = errThing.Pyy 
     errThingArr.Pzz[k]    = errThing.Pzz 
     errThingArr.Pxy[k]    = errThing.Pxy 
     errThingArr.Pxz[k]    = errThing.Pxz 
     errThingArr.Pyz[k]    = errThing.Pyz 
     errThingArr.Hx[k]     = errThing.Hx  
     errThingArr.Hy[k]     = errThing.Hy  
     errThingArr.Hz[k]     = errThing.Hz  
     errThingArr.R[k,*,*]  = errThing.R   

  ENDFOR

     ERROR_CALC_2D,errThingArr, $
                   N_=NArr, $
                   JF=jArr, $
                   JEF=jeArr, $
                   T_=TArr, $
                   NERR=NErrArr, $
                   JERR=jerrArr, $
                   JEERR=jeErrArr, $
                   CHAREERR=charEErrArr, $
                   TERR=TerrArr, $
                   JJE_COVAR=jjeCovarArr, $ ;; , $
                   JPERPF=jPerp, $
                   JPERPEF=jePerp, $
                   JJEPERP_COVAR=jjePerp_coVar, $
                   PAR_ERRORS=parErrs, $
                   PERP_ERRORS=perpErrs


  halfAngle = [aRange[1]:((aRange[0]+step)/2):step]

  densWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  densPlot = ERRORPLOT(halfAngle,NArr,NErrArr, $
                  XTITLE='Half-angle (deg)', $
                  YTITLE='Density (cm$^-3$)', $
                  TITLE=tidStr, $
                  /BUFFER, $
                  /CURRENT)

  PRINT,"Saving densPlot: " + densStr
  densWin.Save,plotDir+densStr
  densWin.Close
  densWin = !NULL

  ;; Now jPar
  jWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  jPlot = ERRORPLOT(halfAngle,jArr,jErrArr, $
                       XTITLE='Half-angle (deg)', $
                       YTITLE='Number flux (#/cm$^2$)', $
                       TITLE=tidStr, $
                       /BUFFER, $
                       /CURRENT)

  PRINT,"Saving jPlot: " + jStr
  jWin.Save,plotDir+jStr
  jWin.Close
  jWin = !NULL

  ;; Now curPar
  curWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  curPlot = ERRORPLOT(halfAngle,jArr*(-1.6D-9),jErrArr*(-1.6D-9), $
                      XTITLE='Half-angle (deg)', $
                      YTITLE='Current density ($\mu$A/m$^2$)', $
                      TITLE=tidStr, $
                      /BUFFER, $
                      /CURRENT)

  PRINT,"Saving jPlot: " + curStr
  curWin.Save,plotDir+curStr
  curWin.Close
  curWin = !NULL
  
  ;; Now jePar
  jeWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  jePlot = ERRORPLOT(halfAngle,jeArr,jeErrArr, $
                       XTITLE='Half-angle (deg)', $
                       YTITLE='Energy flux (mW/m$^2$)', $
                       TITLE=tidStr, $
                       /BUFFER, $
                       /CURRENT)

  PRINT,"Saving jePlot: " + jeStr
  jeWin.Save,plotDir+jeStr
  jeWin.Close
  jeWin = !NULL

  ;; Now temp
  TempWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  TempPlot1 = PLOT(halfAngle,TArr[0,*], $
                  XTITLE='Half-angle (deg)', $
                  YTITLE='Temperature (eV)', $
                  TITLE=tidStr, $
                   YRANGE=TRange, $
                   NAME='T$_{perp}$', $
                  /BUFFER, $
                  /CURRENT)
  TempPlot2 = PLOT(halfAngle,TArr[2,*], $
                   COLOR='Red', $
                   NAME='T$_{par}$', $
                   YRANGE=TRange, $
                  /BUFFER, $
                   /CURRENT, $
                   /OVERPLOT)
  TempPlot3 = ERRORPLOT(halfAngle,TArr[3,*],TErrArr, $
                        COLOR='Blue', $
                        NAME='T$_{avg}$', $
                        YRANGE=TRange, $
                        /BUFFER, $
                        /CURRENT, $
                        /OVERPLOT)

  leg = LEGEND(TARGET=[TempPlot1,TempPlot3,TempPlot2],POSITION=[180,300],/DATA)

  PRINT,"Saving TempPlot: " + TempStr
  TempWin.Save,plotDir+TempStr
  TempWin.Close
  TempWin = !NULL

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Old plots, no error bars
  ;;
  ;; densWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  ;; densPlot = PLOT(halfAngle,NArr, $
  ;;                 XTITLE='Half-angle (deg)', $
  ;;                 YTITLE='Density (cm$^-3$)', $
  ;;                 TITLE=tidStr, $
  ;;                 /BUFFER, $
  ;;                 /CURRENT)

  ;; PRINT,"Saving densPlot: " + densStr
  ;; densWin.Save,plotDir+densStr
  ;; densWin.Close
  ;; densWin = !NULL

  ;; ;; Now temp
  ;; TempWin  = WINDOW(DIMENSIONS=[800,800],/BUFFER)
  ;; TempPlot1 = PLOT(halfAngle,TArr[0,*], $
  ;;                 XTITLE='Half-angle (deg)', $
  ;;                 YTITLE='Temperature (eV)', $
  ;;                 TITLE=tidStr, $
  ;;                  NAME='T$_{perp}$', $
  ;;                 /BUFFER, $
  ;;                 /CURRENT)
  ;; TempPlot2 = PLOT(halfAngle,TArr[2,*], $
  ;;                  COLOR='Red', $
  ;;                  NAME='T$_{par}$', $
  ;;                 /BUFFER, $
  ;;                  /CURRENT, $
  ;;                  /OVERPLOT)
  ;; TempPlot3 = PLOT(halfAngle,TArr[3,*], $
  ;;                  COLOR='Blue', $
  ;;                  NAME='T$_{avg}$', $
  ;;                 TITLE=tidStr, $
  ;;                 /BUFFER, $
  ;;                  /CURRENT, $
  ;;                  /OVERPLOT)

  ;; leg = LEGEND(TARGET=[TempPlot1,TempPlot3,TempPlot2],POSITION=[180,300],/DATA)

  ;; PRINT,"Saving TempPlot: " + TempStr
  ;; TempWin.Save,plotDir+TempStr
  ;; TempWin.Close
  ;; TempWin = !NULL

  STOP
END
