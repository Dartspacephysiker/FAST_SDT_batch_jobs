;;2017/02/27
;;Data file made with JOURNAL__20170225__CHASTON_ET_AL_2006__INTERFEROMETRY__GET_EFIELD_DATA_FROM_SDT
;; and UIcfg.pre8_proceedings_7__Orb_6717__INTERFER__Chaston_et_al_2006__Fig_A1
PRO INTERFEROMETRY__CROSS_SPEC, $
   N_AVE=nAve, $
   NPTS=nPts, $
   ALL_PTS=all, $
   OVERLAP=overlap, $
   SAMPLE=sample, $
   CROSSNAME=crossName, $
   INPUTFILE=inFil, $
   INPUTDIR=inDir, $
   STORE=store, $
   TPLT_VARS=tPlt_vars, $
   TPLT_FREQLIMS=tPlt_freqLims, $
   WAVELET=wavelet, $
   WV__FAMILY=family, $
   WV__START_SCALE=start_scale, $
   WV__ORDER=order, $
   WV__DSCALE=dScale, $
   WV__NSCALE=nScale, $
   WV__PAD=pad, $
   WV__PHASECORRECT=phaseCorr, $
   DOUBLE=double, $
   V58V78=v58v78, $
   LOAD_16k_V578=load_16k_v578


  COMPILE_OPT IDL2

  defInDir          = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  CASE 1 OF
     KEYWORD_SET(load_16k_v578): BEGIN
        defInFil    = 'Efield_data_for_Chaston_et_al_2006__Fig_A1-16k_v578__4k_v124.sav'
     END
     ELSE: BEGIN
        defInFil    = 'Efield_data_for_Chaston_et_al_2006__Fig_A1-4k_all.sav'
     END
  ENDCASE

  defNave           = 4             ;The number of segments to divide up the time series data in sig1 and sig2 for averaging.
  defNpts           = 4096 ;To get decent statistics at least 4-8 sequential or sliding averages should be used -- so pick n_ave = 8, npts=1024, and sample = the sample time (in seconds) for the time series
  defOverlap        = 0B ;Set this keyword to slide the cross-spectral interval by one-half of an interval instead of averaging together each separate interval. For most data types this will yield a higher number of averages and less error per data point than straight sequential averaging. Note that N_AVE specifies how many sequential averages are taken without overlap. Thus N_AVE=4 with /OVERLAP yields a total of 7 averages, etc.
  ;; defSample      = ;The sample time for each point in the time series data in sig1 and sig2.
  defCross124Name   = '1214'
  defCross578Name   = '5878'

  IF N_ELEMENTS(nAve) EQ 0 THEN BEGIN
     nAve           = defnAve
  END

  IF N_ELEMENTS(nPts) EQ 0 THEN BEGIN
     nPts           = defnPts
  END

  IF N_ELEMENTS(overlap) EQ 0 THEN BEGIN
     overlap        = defoverlap
  END

  IF N_ELEMENTS(inDir) EQ 0 THEN BEGIN
     inDir          = defInDir
  END

  IF N_ELEMENTS(inFil) EQ 0 THEN BEGIN
     inFil          = defInFil
  END

  RESTORE,inDir+inFil

  CASE 1 OF
     KEYWORD_SET(load_16k_v578): BEGIN
        CASE 1 OF
           KEYWORD_SET(v58v78): BEGIN

              time    = fields.time578

           END
           ELSE: BEGIN
              time    = fields.time124
           END
        ENDCASE
     END
     ELSE: BEGIN
        ;; CASE 1 OF
        ;;    KEYWORD_SET(v58v78): BEGIN
              time = fields.time
        ;;    END
        ;;    ELSE: BEGIN
        ;;       time = fields.time
        ;;    END
        ;; ENDCASE
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(v58v78): BEGIN
        fields1 = fields.v5v8
        fields2 = fields.v7v8

        IF ~KEYWORD_SET(phaseCorr) THEN BEGIN
           phaseCorr = 0.9*!PI
        ENDIF

        IF N_ELEMENTS(crossName) EQ 0 THEN BEGIN
           crossName = defCross578Name
        ENDIF

     END
     ELSE: BEGIN
        fields1 = fields.v1v2
        fields2 = fields.v1v4

        IF ~KEYWORD_SET(phaseCorr) THEN BEGIN
           phaseCorr = 0.8*!PI
        ENDIF
        
        IF N_ELEMENTS(crossName) EQ 0 THEN BEGIN
           crossName = defCross124Name
        ENDIF

     END
  ENDCASE

  IF KEYWORD_SET(all) THEN BEGIN
     nPts           = N_ELEMENTS(time)
     PRINT,"Doing all " + STRCOMPRESS(nPts,/REMOVE_ALL) + " points (padding set, and no averaging)"
     nAve           = 1
     pad            = 1

     IF ((FLOOR(ALOG2(nPts)) MOD 2) EQ 0) AND (((ALOG2(nPts)) MOD 2) LT 1) THEN BEGIN
        nPts        = nPts
        pad         = 0
     ENDIF ELSE BEGIN
        nPts        = nPts
        pad         = 1
     ENDELSE

     delta          = (time[1]-time[0])/2.
     decimal_place  = FLOOR(ALOG10(time[1]-time[0]))
     GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,time,decimal_place, $
                                        N=nMin, $
                                        DELTA=delta, $
                                        START_I=start_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        OUT_RATES=rates
  ENDIF ELSE BEGIN
     FA_FIELDS_BUFS,time,nPts, $
                    BUF_STARTS=start_i, $
                    BUF_ENDS=stop_i

  ENDELSE

  ;; IF N_ELEMENTS(sample) EQ 0 THEN BEGIN
  ;;    sample  = defsample
  ;; END

  ;; sample     = 1D0/(time[1]-time[0])

  nStreaks      = N_ELEMENTS(start_i)

  NptsPerT      = nAve*nPts

  ;;Calculate number of times we're going to get from this little diamond
  CASE 1 OF
     KEYWORD_SET(all): BEGIN

        T_per_strk = 1

     END
     ELSE: BEGIN

        T_per_strk    = !NULL

        FOR k=0,nStreaks-1 DO BEGIN

           strti      = start_i[k]
           stopi      = stop_i[k]
           tmpInds    = [strti:stopi]

           T_per_strk = [T_per_strk,(stopi-strti)/NptsPerT]

        ENDFOR
     END
  ENDCASE


  CASE 1 OF
     KEYWORD_SET(wavelet): BEGIN


        IF ~KEYWORD_SET(family) THEN BEGIN
           family = 'Morlet'
        ENDIF

        IF ~KEYWORD_SET(order) THEN BEGIN
           order  = 6
        ENDIF

        IF KEYWORD_SET(start_frequency) OR ~KEYWORD_SET(start_scale) THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(start_frequency): BEGIN
                 
              END
              ELSE: BEGIN
                 start_scale = 2
              END
           ENDCASE
        ENDIF

        ;; IF ~KEYWORD_SET(start_scale) THEN BEGIN
        ;;    start_scale = 2
        ;; ENDIF

        IF ~KEYWORD_SET(dScale) THEN BEGIN
           dScale = 0.25
        ENDIF

        IF ~KEYWORD_SET(nScale) OR (KEYWORD_SET(dScale) AND KEYWORD_SET(start_scale)) THEN BEGIN
           nScale = FLOOR((ALOG2(nPts/start_scale))/dScale+1)
        ENDIF

        IF N_ELEMENTS(pad) EQ 0THEN BEGIN
           pad        = 1B
        ENDIF

        nTimes        = N_ELEMENTS(time)/nAve
        timeArr       = MAKE_ARRAY(nTimes,/DOUBLE)
        cohvArr    = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        phasevArr  = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        freqvArr   = MAKE_ARRAY(nTimes,nScale,/FLOAT)

        baseii        = [0:(nPtsPerT-1)]
        baseiiStr     = [0:(nPtsPerT-1):nAve]
        FOR k=0,nStreaks-1 DO BEGIN

           strti      = start_i[k]
           stopi      = stop_i[k]
           inds       = [strti:stopi]

           sample     = 1D0/(time[strti[0]+1]-time[strti[0]])

           ;;Loop over times within streak
           FOR kk=0,T_per_strk[k]-1 DO BEGIN

              ;; tmp_ii  = [(kk*NptsPerT):((kk+1)*NptsPerT-1)]
              ;; tmpStri = [(kk*NptsPerT):((kk+1)*NptsPerT-1):nAve]
              tmp_ii  = kk*nPtsPerT + baseii
              tmpStri = kk*nPtsPerT + baseiiStr
              tmpInds = inds[tmp_ii]
              tmpSti  = inds[tmpStri]
              tmpNi   = tmpsti/nAve

              ;; torI    = tCount*nPts + tmp
              ;; sampleF    = [1D0/(time[tmpInds[1]]-time[tmpInds[0]]),1D0/(time[tmpInds[1:-1]]-time[tmpInds[0:-2]])]
              sampleDT   = [(time[tmpInds[1]]-time[tmpInds[0]]),(time[tmpInds[1:-1]]-time[tmpInds[0:-2]])]

              CROSS_SPEC_WAVELET, $
                 fields1[tmpInds],fields2[tmpInds], $
                 cohv,phasev,freqv, $
                 TIMES=time[tmpInds], $
                 N_AVE=nAve, $
                 NPTS=nPts, $
                 OVERLAP=overlap, $
                 SAMPLE=sampleDT, $
                 FAMILY=family, $
                 START_SCALE=start_scale, $
                 ORDER=order, $
                 DSCALE=dScale, $
                 NSCALE=nScale, $
                 PAD=pad, $
                 PHASE_CORRECT=phaseCorr, $
                 DOUBLE=double

              ;;Update arrays
              nCoh                = N_ELEMENTS(cohv[*,0])
              timeArr[tmpNi]      = time[tmpSti]
              cohvArr[tmpNi,*]    = TEMPORARY(cohv)
              phasevArr[tmpNi,*]  = TEMPORARY(phasev)
              freqvArr[tmpNi,*]   = TEMPORARY(freqv)

              PRINT,tmpSti[0],' ',TIME_TO_STR(timeArr[tmpNi[0]],/MS),' ',nCoh,' ',' ',tmpNi[0],' ',tmpNi[-1]

           ENDFOR

        ENDFOR

        lastInd       = MAX(WHERE(timeArr NE 0.D))

        finalInds     = [0L:lastInd]
        timeArr       = timeArr[finalInds]
        cohvArr       = cohvArr  [finalInds,*]
        phasevArr     = phasevArr[finalInds,*]
        freqvArr      = freqvArr [finalInds,*]

     END
     ELSE: BEGIN

        nTimes        = TOTAL(T_per_strk)
        nFreqs        = nPts/2+1
        timeArr       = MAKE_ARRAY(nTimes,/DOUBLE)
        cohvArr       = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        phasevArr     = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        freqvArr      = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)

        tCount        = 0
        FOR k=0,nStreaks-1 DO BEGIN

           strti      = start_i[k]
           stopi      = stop_i[k]
           inds       = [strti:stopi]

           sample     = 1D0/(time[strti[0]+1]-time[strti[0]])

           ;;Loop over times within streak
           FOR kk=0,T_per_strk[k]-1 DO BEGIN

              tmp_ii  = [(kk*NptsPerT):((kk+1)*NptsPerT-1)]
              tmpInds = inds[tmp_ii]

              ;; sampleF    = [1D0/(time[tmpInds[1]]-time[tmpInds[0]]),1D0/(time[tmpInds[1:-1]]-time[tmpInds[0:-2]])]
              sampleDT   = [(time[tmpInds[1]]-time[tmpInds[0]]),(time[tmpInds[1:-1]]-time[tmpInds[0:-2]])]

              CROSS_SPEC,fields1[tmpInds],fields2[tmpInds],cohv,phasev,freqv, $
                         N_AVE=nAve, $
                         NPTS=nPts, $
                         OVERLAP=overlap, $
                         SAMPLE=sampleDT

              ;; IF N_ELEMENTS(cohv) NE N_ELEMENTS(cohv578) OR $
              ;;    ~ARRAY_EQUAL(freqv,freqv578)            OR $
              ;;    N_ELEMENTS(cohv) NE nFreqs                 $
              ;; THEN STOP

              PRINT,tCount,' ',TIME_TO_STR(timeArr[tCount],/MS),' ',N_ELEMENTS(cohv),' '

              ;;Update arrays
              timeArr[tCount]         = time[tmpInds[0]]
              cohvArr[tCount,*]    = TEMPORARY(cohv)
              phasevArr[tCount,*]  = TEMPORARY(phasev)
              freqvArr[tCount,*]   = TEMPORARY(freqv)

              tCount++

           ENDFOR

        ENDFOR
     END
  ENDCASE

  IF KEYWORD_SET(store) THEN BEGIN

     IF N_ELEMENTS(tPlt_vars) EQ 0 THEN BEGIN
        tPlt_vars  = !NULL
     ENDIF

     phaseTitle    = 'Phase'
     freqTitle     = 'Frequency (Hz)'
     cohTitle      = 'Coherence'

     freqLog       = 1
     freqLims      = [MIN([freqvArr[WHERE(freqvArr GT 0)],freqvArr[WHERE(freqvArr GT 0)]]),MAX([freqvArr,freqvArr]),freqLog]
     cohLims       = [0,1,0]
     phaseLims     = [(-1.)*!PI,!PI,0]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Phase

     varName       = 'phase'+crossName

     STORE_DATA,varName,DATA={x:timeArr, y:phasevArr, v:freqvArr}
     OPTIONS,varName,'spec',1
     OPTIONS,varName,'ytitle',freqTitle
     OPTIONS,varName,'ztitle',phaseTitle
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',2
     YLIM,varName,freqLims[0],freqLims[1],freqLims[2]
     ZLIM,varName,phaseLims[0],phaseLims[1],phaseLims[2]
     tPlt_vars     = N_ELEMENTS(tPlt_vars) GT 0 ? $
                     (N_ELEMENTS(WHERE(STRMATCH(STRUPCASE(tPlt_vars),STRUPCASE(varName)),/NULL)) GT 0 ? tPlt_vars : [tPlt_vars,varName]) : $
                     varName


     IF KEYWORD_SET(tPlt_freqLims) THEN BEGIN

        tmp_freqLims = freqLims

        CASE N_ELEMENTS(tPlt_freqLims) OF
           1: BEGIN
              tmp_freqLims[1] = MAX(freqvArr)
              tmp_freqLims[2] = 1
           END
           2: BEGIN
              tmp_freqLims[2] = 1
           END
           3: BEGIN
           END
        ENDCASE

        YLIM,varName,tmp_freqLims[0],tmp_freqLims[1],tmp_freqLims[2]
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Coherence

     varName       = 'coh'+crossName

     STORE_DATA,varName,DATA={x:timeArr, y:cohvArr, v:freqvArr}
     OPTIONS,varName,'spec',1
     ;; ZLIM,varName,4,9,0
     ;; YLIM,varName,-90,270,0
     OPTIONS,varName,'ytitle',freqTitle
     OPTIONS,varName,'ztitle',cohTitle
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',2
     YLIM,varName,freqLims[0],freqLims[1],freqLims[2]
     ZLIM,varName,cohLims[0],cohLims[1],cohLims[2]
     tPlt_vars     = N_ELEMENTS(WHERE(STRMATCH(STRUPCASE(tPlt_vars),STRUPCASE(varName)),/NULL)) GT 0 ? tPlt_vars : [tPlt_vars,varName]

  ENDIF

END
