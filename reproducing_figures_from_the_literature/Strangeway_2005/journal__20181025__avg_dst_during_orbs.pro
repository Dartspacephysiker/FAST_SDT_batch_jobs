;2018/10/25
PRO JOURNAL__20181025__AVG_DST_DURING_ORBS

  COMPILE_OPT IDL2,STRICTARRSUBS

  strO = 8260
  stpO = 8292
  orbs = LINDGEN(stpO-strO+1)+strO

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/'
  pref = 'Orbit_'
  suff = '-rawProds.sav'

  pltExt = '.pdf'
  outDir = '/SPENCEdata/software/sdt/batch_jobs/plots/Strangeway_et_al_2005/V3/'

  LOAD_DST_AE_DBS,Dst

  nHours = 5L

  nXTicks = 2*nHours+1
  stride = 1
  WHILE nXTicks GT 8 DO BEGIN
     nXTicks = nXTicks/2
     stride = stride * 2
  ENDWHILE

  xVals = LINDGEN(2*nHours+1)-nHours
  xVals = xVals[0:-1:stride]

  fontSize = 18

  FOR bro=strO,stpO DO BEGIN

     tmpFile = STRING(FORMAT='(A0,I0,A0)',pref,bro,suff)
     IF ~FILE_TEST(dir+tmpFile) THEN BEGIN
        PRINT,"Couldn't get "+tmpFile+"!!!"
        CONTINUE
     ENDIF
     
     db = !NULL                 ;Clear
     
     RESTORE,dir+tmpFile

     PRINT,bro

     this = WHERE(db.ilat GT 60 AND db.ilat LT 85 $
                  AND db.mlt GE 6 AND db.mlt LE 18)

     tid = MEAN(db.x[this])

     dstInd = VALUE_CLOSEST2(dst.time,tid,/CONS)

     ;; t2s(dst.time[dstInd])
     ;; t2s(tid)

     ;; Plot 3 hours before and after
     minI = dstInd-nHours
     maxI = dstInd+nHours

     dstInds = [minI:maxI]

     xTickNames = T2S(dst.time[dstInds])

     xTickNames = xTickNames[0:-1:stride]

     xTickNames[0] = STRMID(xTickNames[0],2,STRLEN(xTickNames[0])-8)
     xTickNames[1:-1] = STRMID(xTickNames[1:-1],11,2)

     wind = WINDOW(DIMENSIONS=[1000,1000])

     plot = PLOT(xVals,dst.dst[dstInds], $
                 TITLE=STRING(FORMAT='("Orbit ",I0)',bro), $
                 THICK=2, $
                 NAME='Dst', $
                 YTITLE='nT', $
                 YRANGE=[-200,20], $
                 XTICKNAME=xTickNames, $
                 XTICKVALUES=xVals, $
                 FONT_SIZE=fontSize, $
                 CURRENT=wind)

     plot2 = PLOT(xVals,dst.DST_SMOOTHED_6HR[dstInds], $
                  NAME='6Hr smooth', $
                  LINESTYLE='--', $
                  THICK=2, $
                  COLOR='BLUE', $
                  XTICKNAME=xTickNames, $
                  XTICKVALUES=xVals, $
                  FONT_SIZE=fontSize, $
                  /OVERPLOT, $
                  CURRENT=wind)

     ;; things
     line1x = (REPLICATE(db.x[this[0]],2)-dst.time[dstInd])/3600.
     line1y = [-200,20]

     line2x = (REPLICATE(db.x[this[-1]],2)-dst.time[dstInd])/3600.
     line2y = [-200,20]

     plotl1 = PLOT(line1x,line1y, $
                   LINESTYLE=':', $
                   THICK=2, $
                   COLOR='RED', $
                   /OVERPLOT, $
                   CURRENT=wind)

     plotl2 = PLOT(line2x,line2y, $
                   LINESTYLE=':', $
                   THICK=2, $
                   COLOR='RED', $
                   /OVERPLOT, $
                   CURRENT=wind)

     leg = LEGEND(TARGET=[plot,plot2], $
                  FONT_SIZE=fontSize)

     outFile = STRING(FORMAT='("SWAY_DST-orb_",I0,A0)', $
                      bro, $
                      pltExt)

     wind.Save,outDir+outFile
     wind.Close


  ENDFOR

END
