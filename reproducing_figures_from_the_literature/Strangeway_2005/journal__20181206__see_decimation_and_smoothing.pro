;2018/12/06
PRO JOURNAL__20181206__SEE_DECIMATION_AND_SMOOTHING, $
   SAVESEPARATE=saveSep

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; To get inFile, I set a breakpoint in Strangway_2005__v3.pro on line 984 (where magB, magp, magv are defined) and did
  ;; save,magB,magp,magv,FILENAME='journal__20181206-see_decimation_and_smoothing-orb8276.sav'

  inDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  inFile = 'journal__20181206-see_decimation_and_smoothing-orb8276.sav'

  IF KEYWORD_SET(saveSep) OR KEYWORD_SET(savePlot) THEN BEGIN
     outPlotPref = 'orb8276'
     pltExt = '.ps'
     SET_PLOT_DIR,outPlotDir,ADD_SUFF='/20181210-SWayDecimateSmooth/',/FOR_SDT

  ENDIF

  RESTORE,inDir + inFile

  t1 = magb.x[0]
  t2 = magb.x[-1]

  tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))


  data = magp

  data.x = data.x[SORT(data.x)]
  data.y = data.y[SORT(data.x)]

  finite_y         = FINITE(data.y)

  FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i


  firstTid = data.x[strt_i[0]]+5
  firstVal = data.y[strt_i[0]]

  ;; dBp = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
  ;;       magp, $
  ;;       INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
  ;;       ONESEC_TS=tS_1s)

  sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
  sPeriods = data.x[strt_i+1]-data.x[strt_i]
  nBufs    = N_ELEMENTS(strt_i)

  tmpDat = data
  finalSampRates = !NULL

  sRates = FIX(sRates)

  versions = LIST()
  sampRates = LIST()

  FOR k=0,1 DO BEGIN

     ;; curSampPeriod  = sPeriods[k]
     curSampRate    = sRates[k]
     tmpI           = [strt_i[k]:stop_i[k]]
     tmp            = {x:tmpDat.x[tmpI], $
                       y:tmpDat.y[tmpI]}

     ;; WHILE curSampPeriod GT 1 DO BEGIN
     earlyBreak     = 0
     WHILE curSampRate GT 1 DO BEGIN
        
        versions.Add,{x:tmp.x-firstTid,y:tmp.y-firstVal}
        sampRates.Add,curSampRate

        nCurrent       = N_ELEMENTS(tmp.x)

        IF KEYWORD_SET(verbose) AND ~KEYWORD_SET(quiet) THEN PRINT,k,', ',curSampRate,', ',nCurrent


        CASE 1 OF
           KEYWORD_SET(interp_4Hz_to_1s): BEGIN

              CASE 1 OF
                 (curSampRate LT 4): BEGIN

                    ;; PRINT,"What should you actually do here? What you ARE doing is using the current sample rate as the stride"
                    ;; STOP
                    ;; tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                    ;;          y:tmp.y[0:nCurrent-1:curSampRate]}

                    ;; curSampRate /= curSampRate

                 END
                 ( curSampRate EQ 4 ): BEGIN

                    ;;Don't jump over every fourth data point here
                    ;; tmp   = {x:tmp.x[0:nCurrent-1], $
                    ;;          y:tmp.y[0:nCurrent-1]}

                    earlyBreak = 1

                 END
                 ( curSampRate EQ 8 ): BEGIN

                    ;;3-point smooth
                    tmp.y = SMOOTH(tmp.y,3,/NAN,MISSING=!VALUES.F_NaN)

                    ;;Jump over every other data point here
                    tmp   = {x:tmp.x[0:nCurrent-1:2], $
                             y:tmp.y[0:nCurrent-1:2]}

                    curSampRate /= 2

                    ;; IF KEYWORD_SET(interp_4Hz_to_1s) THEN earlyBreak = 1

                 END
                 ( (curSampRate MOD 4) EQ 0): BEGIN

                    ;;7-point smooth
                    tmp.y = SMOOTH(tmp.y,7,/NAN,MISSING=!VALUES.F_NaN)

                    tmp   = {x:tmp.x[0:nCurrent-1:4], $
                             y:tmp.y[0:nCurrent-1:4]}

                    curSampRate /= 4

                 END
                 ELSE: BEGIN
                 END
              ENDCASE

           END
           ELSE: BEGIN
              
              CASE 1 OF
                 (curSampRate LE 1): BEGIN
                    earlyBreak = 1
                 END
                 (curSampRate LE 4): BEGIN

                    ;; PRINT,"What should you actually do here? What you ARE doing is using the current sample rate as the stride"
                    ;; STOP

                    ;;(2N-1)-point smooth
                    tmp.y = SMOOTH(tmp.y,FIX(curSampRate*2-1) < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)

                    tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                             y:tmp.y[0:nCurrent-1:curSampRate]}

                    curSampRate /= curSampRate

                 END
                 ( (curSampRate MOD 4) EQ 0): BEGIN

                    ;;7-point smooth
                    tmp.y = SMOOTH(tmp.y,7 < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)

                    tmp   = {x:tmp.x[0:nCurrent-1:4], $
                             y:tmp.y[0:nCurrent-1:4]}

                    curSampRate /= 4

                 END
                 ELSE: BEGIN
                    PRINT,'What???? Samprate is ' + STRCOMPRESS(curSampRate,/REMOVE_ALL)
                    ;; earlyBreak = 1
                    STOP
                 END
              ENDCASE

           END
        ENDCASE

        IF KEYWORD_SET(interp_4Hz_to_1s) AND ( curSampRate LE 4 ) THEN BREAK

        IF KEYWORD_SET(earlyBreak) THEN BREAK

     ENDWHILE
     finalSampRates = [finalSampRates,curSampRate]

     IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

        IF N_ELEMENTS(final) EQ 0 THEN BEGIN

           final = TEMPORARY(tmp)

        ENDIF ELSE BEGIN

           final = {x:[final.x,tmp.x], $
                    y:[final.y,tmp.y]}

        ENDELSE

     ENDIF ELSE BEGIN

        nCurrent = N_ELEMENTS(tmp.x)
        ;;OK, it's been smoothed and decimated to one second. 
        ;;Now let's get Alfvénic and DC components
        DCdat = SMOOTH(tmp.y,7 < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)
        
        ACdat = tmp.y-DCdat

        IF N_ELEMENTS(final) EQ 0 THEN BEGIN

           final = {x:tmp.x, $
                    DC:TEMPORARY(DCDat), $
                    AC:TEMPORARY(ACDat)}

        ENDIF ELSE BEGIN

           final = {x:[final.x,tmp.x], $
                    DC:[final.DC,TEMPORARY(DCDat)], $
                    AC:[final.AC,TEMPORARY(ACDat)]}

        ENDELSE

     ENDELSE

     BREAK

  ENDFOR

  versions.Add,{x:final.x-firstTid,DC:final.DC-firstVal,AC:final.AC}
  sampRates.Add,curSampRate

  nVersions = N_ELEMENTS(versions)+1
  plotArr = MAKE_ARRAY(nVersions,/OBJ)

  pltCount = 0

  transps = REVERSE(ROUND(FINDGEN(nVersions)/(nVersions-1)*50))
  ;; transps[*] = 0

  colorArr = ['Black','Purple','Red','Orange','Blue','Blue']

  win = WINDOW(DIMENSIONS=[1200,800])

  fSize = 30
  fSizeLeg = 20
  thick = 3
  FOR k=0,nVersions-3 DO BEGIN

     ;; junk = MIN(ABS(versions[k].x

     print,versions[k].x[0],versions[k].x[1]-versions[k].x[0]
  ;;    versions[k].x = versions[k].x-firstTid

     ;; tmpName = STRING(FORMAT='("NSmooths : ",I0," - sRate : ",I0," Hz")', $
     ;;                  k,sampRates[k])
     tmpName = STRING(FORMAT='(I0," Hz (Sm = ",I0,")")', $
                      sampRates[k],k)

     PRINT,tmpName
     plotArr[k] = PLOT(versions[k].x,versions[k].y, $
                       XRANGE=[0,15], $
                       YRANGE=[-6,0.5], $
                       XTITLE="Time since "+T2S(firstTid), $
                       YTITLE="$dB$ (nT)", $
                       FONT_SIZE=fSize, $
                       NAME=tmpName, $
                       TRANSPARENCY=transps[k], $
                       COLOR=colorArr[k], $
                       THICK=THICK, $
                       OVERPLOT=pltCount NE 0, $
                       CURRENT=win)

     IF KEYWORD_SET(saveSep) THEN BEGIN
        legend = LEGEND(TARGET=REVERSE(plotArr[0:pltCount]), $
                        POSITION=[0.7,0.75], $
                        /NORMAL, $
                        FONT_SIZE=fSizeLeg)

        suff = STRING(FORMAT='("-",I02,A0)',pltCount,pltExt)
        outPlotN = outPlotPref + suff
        PRINT,"Saving to " + outPlotN
        win.Save,outPlotDir+outPlotN

        legend.Delete
     ENDIF

     pltCount++


     ;; STOP

  ENDFOR

  plotArr[k] = PLOT(versions[k].x,versions[k].DC, $
                    ;; XTITLE="Time since "+T2S(firstTid), $
                    ;; YTITLE="$dB$ (nT)", $
                    NAME="DC", $
                    TRANSPARENCY=0, $
                    COLOR=colorArr[k], $
                    THICK=THICK, $
                    OVERPLOT=pltCount NE 0, $
                    CURRENT=win)

  plotArr[k] = PLOT(versions[k].x,versions[k].AC, $
                    ;; XTITLE="Time since "+T2S(firstTid), $
                    ;; YTITLE="$dB$ (nT)", $
                    NAME="AC", $
                    TRANSPARENCY=0, $
                    LINESTYLE='--', $
                    COLOR=colorArr[k+1], $
                    THICK=THICK, $
                    OVERPLOT=pltCount NE 0, $
                    CURRENT=win)

  legend = LEGEND(TARGET=REVERSE(plotArr), $
                  POSITION=[0.7,0.75], $
                  /NORMAL, $
                  FONT_SIZE=fSizeLeg)

  IF KEYWORD_SET(saveSep) THEN BEGIN
     suff = STRING(FORMAT='("-",I02,A0)',pltCount,pltExt)
     outPlotN = outPlotPref + suff
     PRINT,"Saving to " + outPlotN
     win.Save,outPlotDir+outPlotN
  ENDIF


  STOP

  IF KEYWORD_SET(savePlot) THEN BEGIN

  ENDIF

END
