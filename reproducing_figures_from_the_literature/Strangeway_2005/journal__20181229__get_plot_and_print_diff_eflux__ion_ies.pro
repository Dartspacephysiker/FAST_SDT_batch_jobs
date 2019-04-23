;2018/12/29
FUNCTION DOTP,v1,v2
  RETURN,(TRANSPOSE(v1) # v2)[0]
END
FUNCTION VECNORM,vec
  RETURN,(SQRT(TRANSPOSE(vec) # vec))[0]
END
FUNCTION VNORMALIZE,vec
  ;; RETURN,[vec[0],vec[1],vec[2]]/SQRT(vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2])
  RETURN,([vec[0],vec[1],vec[2]]/VECNORM(vec))
END
FUNCTION CROSSP_NORMED,v1,v2
  tmp = CROSSP(v1,v2)
  RETURN,VNORMALIZE(tmp)
END
PRO PRINT_DIST_FUNCTION,wanttime, $
                        diff_eflux, $
                        ORBSTR=orbStr, $
                        MAKECSV=makeCSV ;; , $
  ;; OUTLUN=outLun

  IF N_ELEMENTS(orbSTR) EQ 0 THEN BEGIN
     orbPref = ''
  ENDIF ELSE BEGIN
     orbPref = 'orb_'+orbStr+'_'
  ENDELSE

  ;; useLun = N_ELEMENTS(outLun) GT 0 ? outLun : -1

  junk = MIN(ABS(S2T(wanttime)-diff_eFlux.time),minInd)
  PRINT,T2S(diff_eFlux[minInd].time,/MS)

  curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE( $
               diff_eFlux, $
               minInd, $
               HAS_SC_POT=has_sc_pot, $
               UNITS=units)
  

  IF KEYWORD_SET(makeCSV) THEN BEGIN
     outDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/'
     outWantTStr = ((STRMID(wanttime,11,STRLEN(wantTime))).Replace(':','_')).Replace('.','__')
     outCSV = orbPref+'ies__'+outWantTStr+'.csv'

     PRINT,"Opening " + outCSV + ' ...'

     OPENW,outLun,outDir+outCSV,/GET_LUN
  ENDIF ELSE BEGIN
     outLun = -1
  ENDELSE


  ;; PRINT,FORMAT='(A4,TR4,A10,TR4,A6,TR4,A10,TR4,A10,TR4,A10,TR4,A6,TR4,A6)',"Ind","Energy","Theta","Data","DData","DEnergy","DTheta","Geom"

  ;; FOR iEn=0,curDataStr.nEnergy-1 DO BEGIN
  ;;    FOR iTh=0,curDataStr.nBins-1 DO BEGIN
  ;;       PRINT,FORMAT='(I4,TR4,G10.4,TR4,F6.2,TR4,G10.4,TR4,G10.4,TR4,G10.4,TR4,F6.2,TR4,F6.2)',iEn*curDataStr.nBins+iTh,curDataStr.energy[iEn,iTh],curDataStr.theta[iEn,iTh],curDataStr.data[iEn,iTh],curDataStr.ddata[iEn,iTh],curDataStr.denergy[iEn,iTh],curDataStr.dtheta[iEn,iTh],curDataStr.geom[iEn,iTh]
  ;;    ENDFOR
  ;; ENDFOR


  PRINTF,outLun,FORMAT='(A4,",",A10,",",A6,",",A10,",",A10,",",A10,",",A6,",",A6)',"Ind","Energy","Theta","Data","DData","DEnergy","DTheta","Geom"

   FOR iEn=0,curDataStr.nEnergy-1 DO BEGIN
      FOR iTh=0,curDataStr.nBins-1 DO BEGIN
         PRINTF,outLun,FORMAT='(I4,",",G10.4,",",F6.2,",",G10.4,",",G10.4,",",G10.4,",",F6.2,",",F6.2)',iEn*curDataStr.nBins+iTh,curDataStr.energy[iEn,iTh],curDataStr.theta[iEn,iTh],curDataStr.data[iEn,iTh],curDataStr.ddata[iEn,iTh],curDataStr.denergy[iEn,iTh],curDataStr.dtheta[iEn,iTh],curDataStr.geom[iEn,iTh] 
      ENDFOR
   ENDFOR

   IF KEYWORD_SET(makeCSV) THEN BEGIN
     PRINT,"Closing " + outCSV + ' ...'
      CLOSE,outLun
      FREE_LUN,outLun
   ENDIF

END

;; 20190422 Added printing-to-csv capability
PRO JOURNAL__20181229__GET_PLOT_AND_PRINT_DIFF_EFLUX__ION_IES

  COMPILE_OPT IDL2,STRICTARRSUBS

  ieb_or_ies = 'ies'

  ;; FØRSTE forsøk: plottet som viser energi mot diffEFlux for en enkel vinkel.
  ;; NB, index num er basert på bane 14000
  do20181229Stuff = 0

  ;; ANDRE forsøk: Alle observerte ion-fordelinger plottes og lages som .ps-filer
  do20181231Plots = 0
  combine_plots_in_PDF = 0

  plot_all_times = 0B

  fit2D__PA_zRange = 10.D^([4.,8.])

  ;; Good for orbits 14000, 9499
  energy_ions = [4,40]          

  ;; Good for orbit 8276
  energy_ions = [4,120]          

  printCSV = 0B
  printCSVTime = '1998-09-25/00:05:03.654'

  IF NOT plot_all_times THEN BEGIN

     ;; t1Str_for_diffEflux_plots = '2000-03-04/02:49:00' ;gjelder bane 14000
     ;; t2Str_for_diffEflux_plots = '2000-03-04/03:25:00'
   
     ;; t1Str_for_diffEflux_plots = '1997-04-15/05:25:00' ;gjelder bane 14000
     ;; t2Str_for_diffEflux_plots = '1997-04-15/05:45:00'
   
     ;; t1Str_for_diffEflux_plots = '1999-01-15/16:40:00' ;gjelder bane 9499 (20190325)
     ;; t2Str_for_diffEflux_plots = '1999-01-15/16:55:00'

     t1Str_for_diffEflux_plots = '1998-09-25/00:00:00' ;gjelder bane 8276 (20190325)
     t2Str_for_diffEflux_plots = '1998-09-25/00:19:00'

  ENDIF

  ;; TREDJE forsøk: Se hva vi får ut med moment-beregninger
  ;; At least for orbit 14000, these plots show that we don't need to go higher than ~40 eV in energy

  ;; 20190325 I am not immediately sure what the point of these plots was.
  ;; Next time I figure it out I should describe it ...
  do20181231PlotToConvinceAboutEnergy = 1
  do20181231Moments = 1
  makeEmOxygenMoments = 0

  ;; t = 0.
  ;; this = GET_FA_IES(

  calc_geom_factors         = 1
  deFlux__array_of_structs  = 1
  save_diff_eFlux_to_file   = 1
  load_diff_eFlux_from_file = N_ELEMENTS(remake_diff_eFlux) GT 0 ? ~remake_diff_eFlux : 1
  calib = 1

  tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)

  GET_FA_ORBIT,times,/TIME_ARRAY

  ;; enforce_diff_eFlux_sRate = 2.50
  enforce_diff_eFlux_sRates = 5.0

  winDims = [1200,800]

  ;; specAvgSuff = ''
  ;; CASE 1 OF
  ;;    KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
  ;;       avgitvlstr = (STRING(FORMAT='("-sRate",F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')
  ;;    END
  ;;    KEYWORD_SET(spectra_average_interval): BEGIN
  ;;       avgitvlstr = STRING(FORMAT='("-avgItvl",I0)',spectra_average_interval)
  ;;    END
  ;; ENDCASE

  ;; avgitvlstr = ''
  ;; threshEFluxStr = ''
  ;; upDownRatioStr = ''
  ;; leewardStr = ''
  ;; minNQualEStr = ''
  ;; ;; savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios"$
  ;; ;;            +avgItvlStr+threshEFluxStr+upDownRatioStr+minNQualEStr + leewardStr
  ;; saveSuff = ".sav"
  
  loadDir = '/thelonious_data1/FAST/'

  nHere = N_ELEMENTS(times)
  GET_DATA,"ORBIT",DATA=orbit
  orbit = orbit.y[nHere/2]
  DIFF_EFLUX_FNAME, $
     T1=times[0], $
     T2=times[-1], $
     ORBIT=orbit, $
     EEB_OR_EES=ieb_or_ies, $
     BONUSPREF=bonusPref ,$
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
     MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
     OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     LOADDIR=loadDir

  GET_DIFF_EFLUX,T1=times[0],T2=times[-1], $
                 EEB_OR_EES=ieb_or_ies, $
                 SC_POT=sc_pot, $
                 ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                 CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                 CALC_GEOM_FACTORS=calc_geom_factors, $
                 ARRAY_OF_STRUCTS_INSTEAD=deFlux__array_of_structs, $
                 SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                 OVERWRITE_EXISTING=overwrite_existing, $
                 DIFF_EFLUX_FILE=diff_eFlux_file, $
                 LOAD_DAT_FROM_FILE=load_diff_eFlux_from_file, $
                 LOAD_DIR=loadDir, $
                 OUT_DIFF_EFLUX=diff_eflux

  tDiffs     = diff_eFlux.end_time - diff_eFlux.time

  IF KEYWORD_SET(printCSV) THEN BEGIN
     orbStr = STRING(diff_eflux[0].orbit)

     PRINT_DIST_FUNCTION,printCSVTime, $
                         diff_eflux, $
                         ORBSTR=orbStr, $
                         /MAKECSV
  ENDIF

  IF KEYWORD_SET(do20181229Stuff) THEN BEGIN

     print,t2s(diff_eflux[605].time)
     ;; have ram ions at 2000-03-04/02:55:00
     ;; this is 2000-03-04/02:55:04

     dat = diff_eflux[605]

     STOP

     CONTOUR2D,dat, $
               ;; ANGLE=angle, $
               /POLAR, $
               /FILL, $
               ;; /OVERPLOT, $
               /MSEC, $
               LIMITS=limits, $
               /LABEL, $
               THICK=thick

     plot = PLOT(dat.energy[0:dat.nenergy-1,19],dat.data[0:dat.nenergy-1,19],/XLOG,/YLOG)

  ENDIF

  IF KEYWORD_SET(do20181231Plots) THEN BEGIN

     just_save_all = 1

     IF KEYWORD_SET(plot_all_times) THEN BEGIN
        t1 = diff_eFlux[0].time
        t2 = diff_eFlux[-1].time
     ENDIF ELSE BEGIN
        t1 = S2T(t1Str_for_diffEflux_plots)
        t2 = S2T(t2Str_for_diffEflux_plots)
     ENDELSE
     
     inds = VALUE_CLOSEST2(diff_eFlux.time,[t1,t2],/CONSTRAINED)

     startInd = inds[0]
     stopInd = inds[1]

     retrace = 1

     PLOT_DIFF_EFLUX__2D_DISTS,diff_eFlux, $
                               EEB_OR_EES=ieb_or_ies, $
                               ORBIT=orbit, $
                               SPEC_AVG_INTVL=spec_avg_intvl, $
                               STARTIND=startInd, $
                               STOPIND=stopInd, $
                               RETRACE=retrace,   $
                               IND__STRIDE=stride, $
                               JUST_SAVE_ALL=just_save_all, $
                               FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                               COMBINE_PLOTS_IN_PDF=combine_plots_in_PDF


     STOP

  ENDIF
                            
  IF KEYWORD_SET(do20181231PlotToConvinceAboutEnergy) THEN BEGIN

     dat = diff_eflux[800]

     energyChan = 8 ;eV
     maxList = MAKE_ARRAY(dat.nBins,/STRING,VALUE='')

     enInd = VALUE_CLOSEST2(dat.energy[0:dat.nenergy-1,0],energyChan,/CONSTRAINED)
     checkData = dat.data[enInd,0:dat.nBins-1]
     checkSort = SORT(checkData)
     maxDEFInd = VALUE_CLOSEST2(checkData[checkSort],MAX(checkData),/CONSTRAINED)
     maxDEFInd = checkSort[maxDEFInd]

     maxList[maxDEFInd] = '*'
     maxAngle = dat.theta[enInd,maxDEFInd]

     FOR kob=0,dat.nbins-1 DO PRINT,FORMAT='(F6.2,TR5,G10.3,TR5,A1)',dat.theta[46,kob],dat.data[46,kob],maxList[kob] ;dat.energy[0:dat.nenergy-1,0:dat.nbins-1]

     ;; At least for orbit 14000, these plots show that we don't need to go higher than ~40 eV in energy
     plot = PLOT(dat.energy[0:dat.nenergy-1,maxDEFInd], $
                 dat.data[0:dat.nenergy-1,maxDEFInd],/XLOG,/YLOG)
     plot1 = PLOT(dat.energy[0:dat.nenergy-1,maxDEFInd+1], $
                  dat.data[0:dat.nenergy-1,maxDEFInd+1],/XLOG,/YLOG,/OVERPLOT,COLOR='red')
     plot2 = PLOT(dat.energy[0:dat.nenergy-1,maxDEFInd-1], $
                  dat.data[0:dat.nenergy-1,maxDEFInd-1],/XLOG,/YLOG,/OVERPLOT,COLOR='blue')
     
     STOP

  ENDIF

  IF KEYWORD_SET(do20181231Moments) THEN BEGIN

     IF KEYWORD_SET(makeEmOxygenMoments) THEN BEGIN
        diff_eFlux[*].mass = diff_eFlux[0].mass * 16.
     ENDIF

     up_aRangeN = [90,270]
     down_aRangeN = [270,90]
     up_aRangeS = [270,90]
     down_aRangeS = [90,270]

     ;;eliminate ram ions
     IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        i_angleS     =[180.0,360.0]
        up_aRangeS   =[270.0,360.0]
        down_aRangeS =[180.0,270.0]
        
        i_angleN     =[0.0,180.0]
        up_aRangeN   =[90.0,180.0]
        down_aRangeN =[0.0 ,90.0]

     ENDIF

     IF KEYWORD_SET(only_cone_ions) THEN BEGIN
        i_angleS     =[180.0,360.0]
        up_aRangeS   =[300.0,360.0]
        down_aRangeS =[180.0,240.0]
        
        i_angleN     =[0.0,180.0]
        up_aRangeN   =[120.0,180.0]
        down_aRangeN =[0.0 ,60.0]

     ENDIF

     ;; SSDDD
     ;; Moments???
     ion_min_if_nan_scpots = 4.
     energy = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX( $
              diff_eFlux, $
              ENERGY=energy_ions, $
              SC_POT=tmpSc_pot, $
              EEB_OR_EES=ieb_or_ies, $
              MIN_IF_NAN_SCPOTS=ion_min_if_nan_scpots)

     IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
        aRange__moments = MAKE_ARRAY(2,N_ELEMENTS(diff_eFlux),VALUE=0.)
     ENDIF

     ;; these = WHERE(energy[1,*] LT energy[0,*],COMPLEMENT=notThese)
     ;; diff_eflux[these].valid = 0

     MOMENT_SUITE_2D,diff_eFlux, $
                     ENERGY=energy, $
                     ARANGE__MOMENTS=aRange__moments, $
                     SC_POT=tmpSc_pot, $
                     EEB_OR_EES=ieb_or_ies, $
                     /ERROR_ESTIMATES, $
                     ;; MAP_TO_100KM=map_to_100km, $ 
                     ORBIT=orbit, $
                     /NEW_MOMENT_ROUTINE, $
                     QUIET=quiet, $
                     OUTTIME=time, $
                     OUT_N=n, $
                     OUT_J_=j, $
                     OUT_JE=je, $
                     OUT_T=T, $
                     OUT_CHARE=charE, $
                     OUT_CURRENT=cur, $
                     OUT_JJE_COVAR=jje_coVar, $
                     OUT_ERRORS=errors, $
                     OUT_ERR_N=nErr, $
                     OUT_ERR_J_=jErr, $
                     OUT_ERR_JE=jeErr, $
                     OUT_ERR_T=TErr, $
                     OUT_ERR_CURRENT=curErr, $
                     OUT_ERR_CHARE=charEErr, $
                     INOUT_MAPRATIO=mapRatio, $
                     OUT_STRUCT=ionMomStruct, $
                     BATCH_MODE=batch_mode, $
                     MCFADDEN_STYLE_DIFF_EFLUX=deFlux__array_of_structs


     goodVals = WHERE(FINITE(ionmomstruct.n))

     goodVals = WHERE(FINITE(ionmomstruct.n) AND $
                      FINITE(ionmomstruct.j) AND $
                      FINITE(ionmomstruct.perp.j) AND $
                      (ionMomStruct.errors.Uz < 5))

     cmTokm = 1E-5
     velPerp = ionMomStruct.perp.j/ionMomStruct.n * cmTokm
     velPar = ionMomStruct.j/ionMomStruct.n * cmTokm

     doLinearPlots = 0
     doPlots = 0
     IF doPlots THEN BEGIN

        nPlot = PLOT(diff_eflux[goodVals].alt, $
                     ionMomStruct.n[goodVals], $
                     XTITLE='Altitude (km)', $
                     YTITLE='Dens (cm!U-3!N)', $
                     /YLOG, $
                     LINESTYLE='', $
                     SYMBOL='*', $
                     TITLE='IESA dens from measurements over 4-40 eV')

        IF doLinearPlots THEN BEGIN

           vPerpPlot = PLOT(diff_eflux[goodVals].alt, $
                            velPerp[goodVals], $
                            XTITLE='Altitude (km)', $
                            YTITLE='v_Perp (km/s)', $
                            ;; /YLOG, $
                            LINESTYLE='', $
                            SYMBOL='*', $
                            TITLE='From IESA measurements over 4-40 eV')

           vParPlot = PLOT(diff_eflux[goodVals].alt, $
                           velPar[goodVals], $
                           XTITLE='Altitude (km)', $
                           YTITLE='v_Par (km/s)!C(Negative is anti-earthward)', $
                           ;; /YLOG, $
                           LINESTYLE='', $
                           SYMBOL='*', $
                           TITLE='From IESA measurements over 4-40 eV')

        ENDIF ELSE BEGIN

           negVelPerpII = WHERE(velPerp[goodVals] LT 0,COMPLEMENT=posVelPerpII)
           negVelParII = WHERE(velPar[goodVals] LT 0,COMPLEMENT=posVelParII)

           IF negVelPerpII[0] NE -1 THEN BEGIN

              vPerpPlotNeg = PLOT(diff_eflux[goodVals[negVelPerpII]].alt, $
                                  velPerp[goodVals[negVelPerpII]] * (-1.), $
                                  NAME='Neg', $
                                  XTITLE='Altitude (km)', $
                                  YTITLE='v_Perp (km/s)', $
                                  /YLOG, $
                                  LINESTYLE='', $
                                  SYMBOL='*', $
                                  COLOR='RED', $
                                  TITLE='From IESA measurements over 4-40 eV!CNegative in red')
           ENDIF

           IF posVelPerpII[0] NE -1 THEN BEGIN

              vPerpPlotPos = PLOT(diff_eflux[goodVals[posVelPerpII]].alt, $
                                  velPerp[goodVals[posVelPerpII]], $
                                  NAME='Pos', $
                                  /YLOG, $
                                  LINESTYLE='', $
                                  SYMBOL='*', $
                                  COLOR='Black', $
                                  /OVERPLOT)
           ENDIF

           IF negVelParII[0] NE -1 THEN BEGIN

              vParPlotNeg = PLOT(diff_eflux[goodVals[negVelParII]].alt, $
                                 velPar[goodVals[negVelParII]] * (-1.), $
                                 NAME='Neg', $
                                 XTITLE='Altitude (km)', $
                                 YTITLE='v_Par (km/s)', $
                                 /YLOG, $
                                 LINESTYLE='', $
                                 SYMBOL='*', $
                                 COLOR='RED', $
                                 TITLE='From IESA measurements over 4-40 eV!CNegative in red is anti-earthward')
           ENDIF

           IF posVelParII[0] NE -1 THEN BEGIN

              vParPlotPos = PLOT(diff_eflux[goodVals[posVelParII]].alt, $
                                 velPar[goodVals[posVelParII]], $
                                 NAME='Pos', $
                                 /YLOG, $
                                 LINESTYLE='', $
                                 SYMBOL='*', $
                                 COLOR='Black', $
                                 /OVERPLOT)
           ENDIF

        ENDELSE
     ENDIF

     nDists = N_ELEMENTS(diff_eFlux)

     speedArr = MAKE_ARRAY(nDists,/FLOAT,VALUE=0.)
     vFASTAlongBArr = MAKE_ARRAY(nDists,/FLOAT,VALUE=0.)
     vFASTPerpArr = MAKE_ARRAY(nDists,/FLOAT,VALUE=0.)
     isZeroArr = MAKE_ARRAY(nDists,/FLOAT,VALUE=0.)

     FOR k=0,nDists-1 DO BEGIN

        bNorm = VNORMALIZE(diff_eFlux[k].b_model)
        velNorm = VNORMALIZE(diff_eFlux[k].fa_vel)
        vAlongB = DOTP(diff_eFlux[k].fa_vel,bNorm)

        vAlongBVec = vAlongB*bNorm

        vPerpVec = diff_eFlux[k].fa_vel-vAlongBVec

        ;; Update arrays
        vFASTPerpArr[k] = VECNORM(vPerpVec)
        vFASTAlongBArr[k] = vAlongB
        speedArr[k] = VECNORM(diff_eFlux[k].fa_vel)
        ;; This quantity has to be zero
        isZeroArr[k] = DOTP(vPerpVec,bNorm)

     ENDFOR

     ;; Make earthward positive for FAST velocities too so that alt stemmer
     this = WHERE(diff_eFlux.ilat LT 0)
     IF this[0] NE -1 THEN BEGIN
        vFASTAlongBArr[this] *= -1.
     ENDIF

     vFAST = {par: vFASTAlongBArr, $
              perp: vFASTPerpArr}
     vFAST = {par: vFASTAlongBArr, $
              perp: vFASTPerpArr, $
              speed:SQRT(vFAST.par*vFAST.par+vFAST.perp*vFAST.perp)}

     ;; Should be zero
     IF (WHERE(ABS(vFAST.speed-speedArr) GT 1E-3))[0] NE -1 THEN BEGIN
        PRINT,"Look out, Harry!"
        STOP
     ENDIF

     win = WINDOW(DIMENSIONS=winDims)

     vParPlot = ERRORPLOT(diff_eflux[goodVals].alt, $
                          velPar[goodVals],ABS(velPar[goodVals])*ionMomStruct.errors.Uz[goodVals], $
                          NAME='IESA', $
                          XTITLE='Altitude (km)', $
                          YTITLE='v_Par (km/s)!C(Negative is anti-earthward)', $
                          ;; /YLOG, $
                          LINESTYLE='', $
                          SYMBOL='x', $
                          FONT_SIZE=18, $
                          YRANGE=[-30,20], $
                          TITLE='From IESA measurements over 4-'+ $
                          STRING(FORMAT='(I3)',energy_ions[1]) + $
                          'eV', $
                          CURRENT=win)
     ;; vParPlot = PLOT(diff_eflux[goodVals].alt, $
     ;;                 velPar[goodVals], $
     ;;                 XTITLE='Altitude (km)', $
     ;;                 YTITLE='v_Par (km/s)!C(Negative is anti-earthward)', $
     ;;                 ;; /YLOG, $
     ;;                 LINESTYLE='', $
     ;;                 SYMBOL='*', $
     ;;                 TITLE='From IESA measurements over 4-40 eV', $
     ;;                 CURRENT=win)

     vFASTParPlot = PLOT(diff_eFlux[goodVals].alt, $
                         vFAST.par[goodVals], $
                         NAME='FAST', $
                         LINESTYLE='', $
                         SYMBOL='*', $
                         COLOR='BLUE', $
                         /OVERPLOT)

     vparDiffPlot = PLOT(diff_eFlux[goodVals].alt, $
                         vFAST.par[goodVals]+velPar[goodVals], $
                         NAME='Diff', $
                         LINESTYLE='', $
                         SYMBOL='+', $
                         COLOR='ORANGE', $
                         /OVERPLOT)

     leg = LEGEND(TARGET=[vParPlot,vFASTParPlot,vparDiffPlot])

     ;; Modded particle velocity
     win2 = WINDOW(DIMENSIONS=winDims)

     vPerpPlot = ERRORPLOT(diff_eflux[goodVals].alt, $
                          velPerp[goodVals],ABS(velPerp[goodVals])*ionMomStruct.errors.Ux[goodVals], $
                          NAME='IESA', $
                          XTITLE='Altitude (km)', $
                          YTITLE='v_Perp (km/s)', $
                          ;; /YLOG, $
                          LINESTYLE='', $
                          SYMBOL='x', $
                          YRANGE=[-30,20], $
                          FONT_SIZE=18, $
                           TITLE='From IESA measurements over 4-'+ $
                           STRING(FORMAT='(I3)',energy_ions[1]) + $
                           'eV', $
                          CURRENT=win2)
     ;; vPerpPlot = PLOT(diff_eflux[goodVals].alt, $
     ;;                 velPerp[goodVals], $
     ;;                 XTITLE='Altitude (km)', $
     ;;                 YTITLE='v_Perp (km/s)!C(Negative is anti-earthward)', $
     ;;                 ;; /YLOG, $
     ;;                 LINESTYLE='', $
     ;;                 SYMBOL='*', $
     ;;                 TITLE='From IESA measurements over 4-40 eV', $
     ;;                 CURRENT=win)

     vFASTPerpPlot = PLOT(diff_eFlux[goodVals].alt, $
                         vFAST.perp[goodVals], $
                         NAME='FAST', $
                         LINESTYLE='', $
                         SYMBOL='*', $
                         COLOR='BLUE', $
                         /OVERPLOT)

     vperpDiffPlot = PLOT(diff_eFlux[goodVals].alt, $
                         velPerp[goodVals]+vFAST.perp[goodVals], $
                         NAME='Diff', $
                         LINESTYLE='', $
                         SYMBOL='+', $
                         COLOR='ORANGE', $
                         /OVERPLOT)

     leg = LEGEND(TARGET=[vPerpPlot,vFASTPerpPlot,vperpDiffPlot])

     STOP

  ENDIF



  bNorm = VNORMALIZE(diff_eFlux[0].b_model)
  velNorm = VNORMALIZE(diff_eFlux[0].fa_vel)
  speed = VECNORM(diff_eFlux[0].fa_vel)
  vAlongB = DOTP(diff_eFlux[0].fa_vel,bNorm)

  vAlongBVec = vAlongB*bNorm

  vPerp = diff_eFlux[0].fa_vel-vAlongBVec
  ;; This quantity has to be zero
  isZero = DOTP(diff_eFlux[0].fa_vel-vAlongBVec,bNorm)
  
  GET_FAST_GEI_COORDS,diff_eFlux.time,OUT_STRUCT=GEICoords

END
