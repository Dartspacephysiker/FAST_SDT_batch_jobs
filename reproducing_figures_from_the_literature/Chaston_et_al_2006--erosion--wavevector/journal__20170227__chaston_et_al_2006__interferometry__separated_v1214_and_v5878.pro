;;2017/02/27
PRO JOURNAL__20170227__CHASTON_ET_AL_2006__INTERFEROMETRY__SEPARATED_V1214_AND_V5878,TPLOT_RIGHTNOW=tPlot_rightNow, $
   WINID=winID, $
   TPLT_VARS=tPlt_vars, $
   TPLT_FREQLIMS=tPlt_freqLims, $
   T1ZOOM=t1Zoom, $
   T2ZOOM=t2Zoom

  COMPILE_OPT IDL2

  double          = 1

  store           = 1
  
  all_1           = 1
  all_2           = 2

  nAve_1          = 2
  nAve_2          = 2

  v58v78_1        = 0
  v58v78_2        = 1

  load_16k_v578_2 = 0
  load_16k_v578_2 = 1

  wavelet         = 1

  ;; crossName_1     = 
  ;; crossName_2     = 

  ;; sample_1        = 
  ;; sample_2        = 

  ;; overlap_1       = 
  ;; overlap_2       = 

  ;; nPts_1          = 
  ;; nPts_2          = 

  IF KEYWORD_SET(tPlot_rightNow) THEN BEGIN

     needed         = ["phase5878","coh5878","phase1214","coh1214"]
     nNeeded        = N_ELEMENTS(needed)

     TPLOT_NAMES,NAMES=names
     haveIt         = 0
     iGuy           = 0
     WHILE iGuy LT nNeeded DO BEGIN
        IF (N_ELEMENTS(WHERE((STRUPCASE(needed))[iGuy] EQ STRUPCASE(names),/NULL)) GT 0) THEN BEGIN
           PRINT,"Got it: ",needed[iGuy]
           haveIt++
        ENDIF

        iGuy++
     ENDWHILE

     IF haveIt EQ nNeeded THEN BEGIN

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO,FONT_SIZE=8


           ENDIF ELSE BEGIN
              WINDOW,winID,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE

        dlist = GET_DQDS(START_TIMES=start_times,END_TIMES=end_times)
        ;; this  = WHERE(STRMATCH(STRUPCASE(dlist),STRUPCASE('coh'))   OR $
        ;;               STRMATCH(STRUPCASE(dlist),STRUPCASE('phase'))    $
        ;;               ,nMatch)

        ;; IF nMatch GT 0 THEN BEGIN
        GET_DATA,needed[0],DATA=dat
        t1BKUP = dat.x[0]
        t2BKUP = dat.x[-1]
        ;; ENDIF

        IF (WHERE(STRUPCASE(dlist) EQ 'ALT'))[0] EQ -1 THEN BEGIN
           GET_FA_ORBIT,t1BKUP,t2BKUP
        ENDIF

        IF KEYWORD_SET(t1Zoom) AND KEYWORD_SET(t2Zoom) THEN BEGIN
           tLims = [t1Zoom,t2Zoom]
        ENDIF ELSE IF N_ELEMENTS(t1Zoom) GT 0 THEN BEGIN
           tLims = [t1Zoom,t2BKUP]
        ENDIF ELSE IF N_ELEMENTS(t2Zoom) GT 0 THEN BEGIN
           tLims = [t1BKUP,t2Zoom]
        ENDIF ELSE BEGIN
           tLims = [t1BKUP,t2BKUP]
        ENDELSE

        IF KEYWORD_SET(tPlt_freqLims) THEN BEGIN

           proto_freqLims  = MAKE_ARRAY(3,VALUE=0.,/FLOAT)
           FOR jj=0,N_ELEMENTS(tPlt_freqLims)-1 DO BEGIN
              proto_freqLims[jj] = tPlt_freqLims[jj]
           ENDFOR
           
           FOR jj=0,N_ELEMENTS(needed)-1 DO BEGIN

              tmp_freqLims = proto_freqLims

              CASE N_ELEMENTS(tPlt_freqLims) OF
                 1: BEGIN
                    tmp_freqLims = proto_freqLims

                    GET_DATA,needed[jj],DATA=dat
                    tmp_freqLims[1] = MAX(dat.v)
                    tmp_freqLims[2] = 1
                 END
                 2: BEGIN
                    tmp_freqLims[2] = 1
                 END
                 3: BEGIN
                 END
              ENDCASE

              YLIM,needed[jj],tmp_freqLims[0],tmp_freqLims[1],tmp_freqLims[2]
           ENDFOR
        ENDIF

        LOADCT2,40
        TPLOT,/LASTVAR,VAR=['ALT','ILAT','MLT'],TRANGE=tLims

        ;; IF KEYWORD_SET(add_timebar) THEN BEGIN
        ;;    TIMEBAR,timesBar,COLOR=!D.N_COLORS-4
        ;; ENDIF


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

        RETURN

     ENDIF


  ENDIF

  INTERFEROMETRY__CROSS_SPEC, $
     N_AVE=nAve_1, $
     NPTS=nPts_1, $
     ALL_PTS=all_1, $
     OVERLAP=overlap_1, $
     SAMPLE=sample_1, $
     CROSSNAME=crossName_1, $
     INPUTFILE=inFil_1, $
     INPUTDIR=inDir, $
     STORE=store, $
     TPLT_VARS=tPlt_vars, $
     WAVELET=wavelet, $
     WV__FAMILY=family, $
     WV__START_SCALE=start_scale, $
     WV__ORDER=order, $
     WV__DSCALE=dScale, $
     WV__NSCALE=nScale, $
     WV__PAD=pad_1, $
     WV__PHASECORRECT=phaseCorr, $
     DOUBLE=double, $
     V58V78=v58v78_1, $
     LOAD_16k_V578=load_16k_v578_1

  INTERFEROMETRY__CROSS_SPEC, $
     N_AVE=nAve_2, $
     NPTS=nPts_2, $
     ALL_PTS=all_2, $
     OVERLAP=overlap_2, $
     SAMPLE=sample_2, $
     CROSSNAME=crossName_2, $
     INPUTFILE=inFil_2, $
     INPUTDIR=inDir, $
     STORE=store, $
     TPLT_VARS=tPlt_vars, $
     WAVELET=wavelet, $
     WV__FAMILY=family, $
     WV__START_SCALE=start_scale, $
     WV__ORDER=order, $
     WV__DSCALE=dScale, $
     WV__NSCALE=nScale, $
     WV__PAD=pad_2, $
     WV__PHASECORRECT=phaseCorr, $
     DOUBLE=double, $
     V58V78=v58v78_2, $
     LOAD_16k_V578=load_16k_v578_2

  IF KEYWORD_SET(tPlot_rightNow) THEN BEGIN
     WINDOW,winID, $
            XSIZE=600, $
            YSIZE=800

     TPLOT,tPlt_vars, $
           WINDOW=winInd

  ENDIF

END
