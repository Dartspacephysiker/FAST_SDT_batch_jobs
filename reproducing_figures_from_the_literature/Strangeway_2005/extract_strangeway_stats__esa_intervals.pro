;;09/24/16
FUNCTION EXTRACT_STRANGEWAY_STATS__ESA_INTERVALS, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2

  defStat = 2 ;average

  CASE 1 OF
     KEYWORD_SET(averages): BEGIN
        stat   = 2
     END
     KEYWORD_SET(integrals): BEGIN
        stat   = 3
     END
     ELSE: BEGIN
        stat   = defStat
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(day): BEGIN
        side   = 1
     END
     KEYWORD_SET(night): BEGIN
        side   = 2
     END
     ELSE: BEGIN
        side   = 0
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(north): BEGIN
        hemi   = 1
     END
     KEYWORD_SET(south): BEGIN
        hemi   = 2
     END
     ELSE: BEGIN
        hemi   = 0 ;both
        PRINT,"Both hemispheres ..."
     END
  ENDCASE


  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_2005/'
  hashFile     = 'Strangeway_et_al_2005__DC_params--ESA_intervals.sav'

  IF FILE_TEST(outDir+hashFile) THEN BEGIN
     PRINT,"Restoring hash file ..."
     RESTORE,outDir+hashFile

     ;; CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
     ;;    -1: BEGIN
     ;;    END
     ;;    ELSE: BEGIN
     ;;    END
     ;; ENDCASE

  ENDIF ELSE BEGIN
     PRINT,'Nothing here! Returning ...'
     RETURN,-1
  ENDELSE

  maxNElems    = 1e6

  orbArr       = MAKE_ARRAY(N_ELEMENTS(swHash) ,/LONG ,VALUE=0.) 
  itvlArr      = MAKE_ARRAY(maxNElems          ,/INTEG,VALUE=0.) 
  eAlongVArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  dB_perpArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  pFAlongBArr  = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)
  jeArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      
  jeeArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)     
  jiArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      

  nCount       = 0
  orbCnt       = 0
  FOREACH value, swHash, key DO BEGIN

     nItvls    = N_ELEMENTS(value)

     FOR k=0,nItvls-1 DO BEGIN

        tmpThing  = ((value[k].(stat)).(hemi)).(side)

        nHere     = N_ELEMENTS(tmpThing.eAlongV)

        IF nHere EQ 0 THEN CONTINUE

        curInds   = [nCount:nCount+nHere-1]


        orbArr      [orbCnt ] = key
        itvlArr     [curInds] = k + 1
        eAlongVArr  [curInds] = tmpThing.eAlongV   
        dB_perpArr  [curInds] = tmpThing.dB_perp   
        pFAlongBArr [curInds] = tmpThing.pFAlongB  
        jeArr       [curInds] = tmpThing.je        
        jeeArr      [curInds] = tmpThing.jee       
        jiArr       [curInds] = tmpThing.ji        

        ;; PRINT,"NCOUNT: " + STRCOMPRESS(nCount,/REMOVE_ALL)
        nCount    += nHere

     ENDFOR

        orbCnt++
     ENDFOREACH

  finStruct = {orbit     : orbArr                   , $
               interval  : itvlArr     [0:nCount-1] , $  
               eAlongV   : eAlongVArr  [0:nCount-1] , $  
               dB_perp   : dB_perpArr  [0:nCount-1] , $  
               pFAlongB  : pFAlongBArr [0:nCount-1] , $ 
               je        : jeArr       [0:nCount-1] , $       
               jee       : jeeArr      [0:nCount-1] , $      
               ji        : jiArr       [0:nCount-1]}


  IF ~KEYWORD_SET(no_plots) THEN BEGIN
     xQuants  = [4,5,6]
     nPlots   = N_ELEMENTS(xQuants)*2 ;one extra for linear regression
     plotArr  = MAKE_ARRAY(nPlots,/OBJ)

     xTitle   = ["", $
                 "", $
                 "", $
                 "", $
                 "Poynting Flux [DC] (mW/m^2)", $
                 "Average Electron Flux (#/cm$^2$/s)", $
                 "Average Electron Energy Flux (mW/m$^2$)", $
                 "Ion Flux (#/cm!U2!N/s)"]

     xRange   = [[0.,0.], $
                 [0.,0.], $
                 [0.,0.], $
                 [0.,0.], $
                 [1e-1,1e2], $
                 [1e7,1e10], $
                 [1e-2,1e0], $
                 [1e6,1e10]]

     yTitle   = "Ion Flux (#/cm!U2!N/s)"
     yData    = (-1.)*finStruct.ji
     yRange   = [1e6,1e10]

     FOR k=0,nPlots-1,2 DO BEGIN
        datI   = xQuants[k/2]

        xDat   = finStruct.(datI)
        sDat   = SORT(xDat)
        xDat   = xDat[sDat]
        yDat   = yData[sDat]

        inds   = WHERE((xDat GT 0) AND (yDat GT 0),nInds)

        IF nInds LE 1 THEN BEGIN
           PRINT,'No good data for these plots! Outta sight!'
           CONTINUE
        ENDIF

        params = LINFIT(ALOG10(xDat[inds]),ALOG10(yDat[inds]),YFIT=yFitter)
        corr   = CORRELATE(ALOG10(xDat[inds]),ALOG10(yDat[inds]))

        ;; yFit = 10.^(params[1] * ALOG10(xDat[inds]) + params[0])

        xFit   = 10.^((INDGEN(10))/10.*(ALOG10(xRange[1,datI])-ALOG10(xRange[0,datI]))+$
                 ALOG10(xRange[0,datI]))
        yFit   = 10.^(params[1] * ALOG10(xFit) + params[0])

        yFitter = 10.^(params[1] * ALOG10(xDat[inds]) + params[0])
        tTest   = TM_TEST(yDat[inds],yFitter,/PAIRED)

        plotArr[k] = PLOT(xDat, $
                          yDat, $
                          XTITLE=xTitle[datI], $
                          YTITLE=yTitle, $
                          XLOG=1, $
                          YLOG=1, $
                          LINESTYLE='', $
                          SYMBOL='o', $
                          /SYM_FILLED, $
                          XRANGE=xRange[*,datI], $
                          YRANGE=yRange)

        plotArr[k+1] = PLOT(xFit,yFit, $
                            /OVERPLOT)

        slopeString  = STRING(FORMAT='(A-10,T15,F7.3)',"slope  =",params[1])
        corrString   = STRING(FORMAT='(A-10,T15,F7.3)',"r      =",corr)
        tString      = STRING(FORMAT='(A-10,T15,F7.3)',"t-test =",tTest[0])

        slopeText    = TEXT(0.2,0.80, $
                            slopeString, $
                            /NORMAL, $
                            FONT_NAME='Courier', $
                            TARGET=plotArr[k])
        corrText     = TEXT(0.2,0.75, $
                            corrString, $
                            /NORMAL, $
                            FONT_NAME='Courier', $
                            TARGET=plotArr[k])
        tText        = TEXT(0.2,0.70, $
                            tString, $
                            /NORMAL, $
                            FONT_NAME='Courier', $
                            TARGET=plotArr[k])

     ENDFOR
     ;; that = PLOT(finStruct.pfalongb,(-1.)*finStruct.ji, $
     ;;             XTITLE=xTitle[2], $
     ;;             YTITLE=yTitle, $
     ;;             XLOG=1, $
     ;;             YLOG=1, $
     ;;             LINESTYLE='', $
     ;;             SYMBOL='o', $
     ;;             /SYM_FILLED, $
     ;;             XRANGE=[1e-1,1e2], $
     ;;             YRANGE=[1e6,1e10])

  ENDIF

  RETURN,finStruct

END

