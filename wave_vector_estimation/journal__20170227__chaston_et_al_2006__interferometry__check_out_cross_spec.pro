;;2017/02/27
;;Data file made with JOURNAL__20170225__CHASTON_ET_AL_2006__INTERFEROMETRY__GET_EFIELD_DATA_FROM_SDT
;; and UIcfg.pre8_proceedings_7__Orb_6717__INTERFER__Chaston_et_al_2006__Fig_A1
PRO JOURNAL__20170227__CHASTON_ET_AL_2006__INTERFEROMETRY__CHECK_OUT_CROSS_SPEC, $
   N_AVE=nAve, $
   NPTS=nPts, $
   ALL_PTS=all, $
   OVERLAP=overlap, $
   SAMPLE=sample, $
   CROSS1NAME=cross1Name, $
   CROSS2NAME=cross2Name, $
   INPUTFILE=inFil, $
   INPUTDIR=inDir, $
   STORE=store, $
   TPLT_VARS=tPlt_vars, $
   WAVELET=wavelet, $
   WV__FAMILY=family, $
   WV__START_SCALE=start_scale, $
   WV__ORDER=order, $
   WV__DSCALE=dScale, $
   WV__NSCALE=nScale, $
   WV__PAD=pad, $
   WV__PHASECORRECT_124=phaseCorr124, $
   WV__PHASECORRECT_578=phaseCorr578, $
   DOUBLE=double, $
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
  defCross1Name     = '1214'
  defCross2Name     = '5878'

  IF N_ELEMENTS(nAve) EQ 0 THEN BEGIN
     nAve           = defnAve
  END

  IF N_ELEMENTS(nPts) EQ 0 THEN BEGIN
     nPts           = defnPts
  END

  IF N_ELEMENTS(overlap) EQ 0 THEN BEGIN
     overlap        = defoverlap
  END

  IF N_ELEMENTS(cross1Name) EQ 0 THEN BEGIN
     cross1Name     = defcross1Name
  END

  IF N_ELEMENTS(cross2Name) EQ 0 THEN BEGIN
     cross2Name     = defcross2Name
  END

  IF N_ELEMENTS(inDir) EQ 0 THEN BEGIN
     inDir          = defInDir
  END

  IF N_ELEMENTS(inFil) EQ 0 THEN BEGIN
     inFil          = defInFil
  END

  RESTORE,inDir+inFil

  IF KEYWORD_SET(all) THEN BEGIN
     nPts           = N_ELEMENTS(fields.time)
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

     delta          = (fields.time[1]-fields.time[0])/2.
     decimal_place  = FLOOR(ALOG10(fields.time[1]-fields.time[0]))
     GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,fields.time,decimal_place, $
                                        N=nMin, $
                                        DELTA=delta, $
                                        START_I=start_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        OUT_RATES=rates
  ENDIF ELSE BEGIN
     FA_FIELDS_BUFS,fields.time,nPts, $
                    BUF_STARTS=start_i, $
                    BUF_ENDS=stop_i

  ENDELSE

  ;; IF N_ELEMENTS(sample) EQ 0 THEN BEGIN
  ;;    sample  = defsample
  ;; END

  ;; sample     = 1D0/(fields.time[1]-fields.time[0])

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

        IF ~KEYWORD_SET(phaseCorr124) THEN BEGIN
           phaseCorr124 = 0.8*!PI
        ENDIF
        
        IF ~KEYWORD_SET(phaseCorr578) THEN BEGIN
           phaseCorr578 = 0.9*!PI
        ENDIF
        
        IF N_ELEMENTS(pad) EQ 0THEN BEGIN
           pad        = 1B
        ENDIF

        nTimes        = N_ELEMENTS(fields.time)/nAve
        timeArr       = MAKE_ARRAY(nTimes,/DOUBLE)
        cohv124Arr    = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        phasev124Arr  = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        freqv124Arr   = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        cohv578Arr    = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        phasev578Arr  = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        freqv578Arr   = MAKE_ARRAY(nTimes,nScale,/FLOAT)
        baseii        = [0:(nPtsPerT-1)]
        baseiiStr     = [0:(nPtsPerT-1):nAve]
        FOR k=0,nStreaks-1 DO BEGIN

           strti      = start_i[k]
           stopi      = stop_i[k]
           inds       = [strti:stopi]

           sample     = 1D0/(fields.time[strti[0]+1]-fields.time[strti[0]])

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
              ;; sampleF    = [1D0/(fields.time[tmpInds[1]]-fields.time[tmpInds[0]]),1D0/(fields.time[tmpInds[1:-1]]-fields.time[tmpInds[0:-2]])]
              sampleDT   = [(fields.time[tmpInds[1]]-fields.time[tmpInds[0]]),(fields.time[tmpInds[1:-1]]-fields.time[tmpInds[0:-2]])]

              CROSS_SPEC_WAVELET, $
                 fields.v1v2[tmpInds],fields.v1v4[tmpInds], $
                 cohv124,phasev124,freqv124, $
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
                 PHASE_CORRECT=phaseCorr124, $
                 DOUBLE=double


              CROSS_SPEC_WAVELET, $
                 fields.v5v8[tmpInds],fields.v7v8[tmpInds], $
                 cohv578,phasev578,freqv578, $
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
                 PHASE_CORRECT=phaseCorr578, $
                 DOUBLE=double

              IF N_ELEMENTS(cohv124) NE N_ELEMENTS(cohv578) OR $
                 ~ARRAY_EQUAL(freqv124,freqv578)               $ ; OR $
                 ;; N_ELEMENTS(cohv124) NE nFreqs                 $
              THEN STOP

              ;;Update arrays
              nCoh124                = N_ELEMENTS(cohv124[*,0])
              nCoh578                = N_ELEMENTS(cohv578[*,0])
              timeArr[tmpNi]         = fields.time[tmpSti]
              cohv124Arr[tmpNi,*]    = TEMPORARY(cohv124)
              phasev124Arr[tmpNi,*]  = TEMPORARY(phasev124)
              freqv124Arr[tmpNi,*]   = TEMPORARY(freqv124)
              cohv578Arr[tmpNi,*]    = TEMPORARY(cohv578)
              phasev578Arr[tmpNi,*]  = TEMPORARY(phasev578)
              freqv578Arr[tmpNi,*]   = TEMPORARY(freqv578)

              PRINT,tmpSti[0],' ',TIME_TO_STR(timeArr[tmpNi[0]],/MS),' ',nCoh124,' ',nCoh578,' ',tmpNi[0],' ',tmpNi[-1]

              ;; tCount++

           ENDFOR

        ENDFOR

        lastInd       = MAX(WHERE(timeArr NE 0.D))

        finalInds     = [0L:lastInd]
        timeArr       = timeArr[finalInds]
        cohv124Arr    = cohv124Arr  [finalInds,*]
        phasev124Arr  = phasev124Arr[finalInds,*]
        freqv124Arr   = freqv124Arr [finalInds,*]
        cohv578Arr    = cohv578Arr  [finalInds,*]
        phasev578Arr  = phasev578Arr[finalInds,*]
        freqv578Arr   = freqv578Arr [finalInds,*]
     END
     ELSE: BEGIN

        nTimes        = TOTAL(T_per_strk)
        nFreqs        = nPts/2+1
        timeArr       = MAKE_ARRAY(nTimes,/DOUBLE)
        cohv124Arr    = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        phasev124Arr  = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        freqv124Arr   = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        cohv578Arr    = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        phasev578Arr  = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        freqv578Arr   = MAKE_ARRAY(nTimes,nFreqs,/FLOAT)
        tCount        = 0
        FOR k=0,nStreaks-1 DO BEGIN

           strti      = start_i[k]
           stopi      = stop_i[k]
           inds       = [strti:stopi]

           sample     = 1D0/(fields.time[strti[0]+1]-fields.time[strti[0]])

           ;;Loop over times within streak
           FOR kk=0,T_per_strk[k]-1 DO BEGIN

              tmp_ii  = [(kk*NptsPerT):((kk+1)*NptsPerT-1)]
              tmpInds = inds[tmp_ii]

              ;; sampleF    = [1D0/(fields.time[tmpInds[1]]-fields.time[tmpInds[0]]),1D0/(fields.time[tmpInds[1:-1]]-fields.time[tmpInds[0:-2]])]
              sampleDT   = [(fields.time[tmpInds[1]]-fields.time[tmpInds[0]]),(fields.time[tmpInds[1:-1]]-fields.time[tmpInds[0:-2]])]

              CROSS_SPEC,fields.v1v2[tmpInds],fields.v1v4[tmpInds],cohv124,phasev124,freqv124, $
                         N_AVE=nAve, $
                         NPTS=nPts, $
                         OVERLAP=overlap, $
                         SAMPLE=sampleDT

              CROSS_SPEC,fields.v5v8[tmpInds],fields.v7v8[tmpInds],cohv578,phasev578,freqv578, $
                         N_AVE=nAve, $
                         NPTS=nPts, $
                         OVERLAP=overlap, $
                         SAMPLE=sampleDT

              IF N_ELEMENTS(cohv124) NE N_ELEMENTS(cohv578) OR $
                 ~ARRAY_EQUAL(freqv124,freqv578)            OR $
                 N_ELEMENTS(cohv124) NE nFreqs                 $
              THEN STOP

              PRINT,tCount,' ',TIME_TO_STR(timeArr[tCount],/MS),' ',N_ELEMENTS(cohv124),' ',N_ELEMENTS(cohv578)

              ;;Update arrays
              timeArr[tCount]         = fields.time[tmpInds[0]]
              cohv124Arr[tCount,*]    = TEMPORARY(cohv124)
              phasev124Arr[tCount,*]  = TEMPORARY(phasev124)
              freqv124Arr[tCount,*]   = TEMPORARY(freqv124)
              cohv578Arr[tCount,*]    = TEMPORARY(cohv578)
              phasev578Arr[tCount,*]  = TEMPORARY(phasev578)
              freqv578Arr[tCount,*]   = TEMPORARY(freqv578)

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
     freqLims      = [MIN([freqv124Arr[WHERE(freqV124Arr GT 0)],freqv578Arr[WHERE(freqV124Arr GT 0)]]),MAX([freqv124Arr,freqv578Arr]),freqLog]
     cohLims       = [0,1,0]
     phaseLims     = [(-1.)*!PI,!PI,0]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Phases
     varName       = 'phase'+cross1Name


     STORE_DATA,varName,DATA={x:timeArr, y:phasev124Arr, v:freqv124Arr}
     OPTIONS,varName,'spec',1
     ;; ZLIM,varName,4,9,0
     ;; YLIM,varName,-90,270,0
     OPTIONS,varName,'ytitle',freqTitle
     OPTIONS,varName,'ztitle',phaseTitle
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',2
     YLIM,varName,freqLims[0],freqLims[1],freqLims[2]
     ZLIM,varName,phaseLims[0],phaseLims[1],phaseLims[2]
     tPlt_vars     = N_ELEMENTS(tPt_vars) GT 0 ? $
                     (N_ELEMENTS(WHERE(STRMATCH(STRUPCASE(tPlt_vars),STRUPCASE(varName)),/NULL)) GT 0 ? tPlt_vars : [tPlt_vars,varName]) : $
                     varName

     varName       = 'phase'+cross2Name

     STORE_DATA,varName,DATA={x:timeArr, y:phasev578Arr, v:freqv578Arr}
     OPTIONS,varName,'spec',1
     ;; ZLIM,varName,4,9,0
     ;; YLIM,varName,-90,270,0
     OPTIONS,varName,'ytitle',freqTitle
     OPTIONS,varName,'ztitle',phaseTitle
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',2
     YLIM,varName,freqLims[0],freqLims[1],freqLims[2]
     ZLIM,varName,phaseLims[0],phaseLims[1],phaseLims[2]
     tPlt_vars     = N_ELEMENTS(WHERE(STRMATCH(STRUPCASE(tPlt_vars),STRUPCASE(varName)),/NULL)) GT 0 ? tPlt_vars : [tPlt_vars,varName]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Cohs
     varName       = 'coh'+cross1Name

     STORE_DATA,varName,DATA={x:timeArr, y:cohv124Arr, v:freqv124Arr}
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

     varName       = 'coh'+cross2Name

     STORE_DATA,varName,DATA={x:timeArr, y:cohv578Arr, v:freqv578Arr}
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

  STOP

END
