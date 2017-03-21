;2017/03/21
FUNCTION GET_INDS_FOR_PLOT_THEORIE,JVPlotData, $
                                   USEINDS__INCLUDE_POSCURRENT=useInds__include_posCurrent, $
                                   USEINDS__RELCHANGE=useInds__relChange, $
                                   FRACCHANGE_TDOWN=fracChange_TDown, $
                                   FRACCHANGE_NDOWN=fracChange_NDown, $
                                   FRACERROR_TDOWN=fracError_TDown, $
                                   FRACERROR_NDOWN=fracError_NDown, $
                                   USEINDS__TWOLUMPS=useInds__twoLumps, $
                                   MAX_TDOWN=max_TDown, $
                                   MIN_TDOWN=min_TDown, $
                                   MAX_NDOWN=max_NDown, $
                                   MIN_NDOWN=min_NDown, $
                                   TRANGES=tRanges

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE 1 OF
     KEYWORD_SET(useInds__relChange): BEGIN

        relChange_TDown = (JVPlotData.TDown [1:-1]-JVPlotData.TDown [0:-2])/JVPlotData.TDown [0:-2]
        relChange_NDown = (JVPlotData.NDown [1:-1]-JVPlotData.NDown [0:-2])/JVPlotData.NDown [0:-2]

        smochange_TDown = WHERE(ABS(relChange_TDown) LE fracChange_TDown)
        smochange_NDown = WHERE(ABS(relChange_NDown) LE fracChange_NDown)

        useInds1        = CGSETINTERSECTION(smochange_NDown,smochange_TDown,COUNT=nUsers)

        ;;Any otras condiciones?
        useInds2        = WHERE((ABS(JVPlotData.TDownErr/JVPlotData.TDown) LE fracError_TDown) AND $
                                (ABS(JVPlotData.NDownErr/JVPlotData.NDown) LE fracError_NDown))

        useInds         = CGSETINTERSECTION(TEMPORARY(useInds1),TEMPORARY(useInds2),COUNT=nUsers)

     END
     KEYWORD_SET(useInds__twoLumps): BEGIN

        nTRanges = N_ELEMENTS(tRanges[0,*])
        useInds  = !NULL
        FOR k=0,nTRanges-1 DO BEGIN

           tmpInds = WHERE(JVPlotData.time GE STR_TO_TIME(tRanges[0,k]) AND $
                           JVPlotData.time LE STR_TO_TIME(tRanges[1,k]),nTmp)

           IF nTmp GT 0 THEN BEGIN
              useInds = [useInds,tmpInds]
           ENDIF ELSE BEGIN
              PRINT,"Ingenting her!"
              STOP
           ENDELSE
           
        ENDFOR

        nUsers = N_ELEMENTS(useInds)

     END
     ELSE: useInds = !NULL
  ENDCASE

  IF KEYWORD_SET(max_TDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.TDown LE max_TDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(min_TDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.TDown GE min_TDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(max_NDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.NDown LE max_NDown), $
                                 COUNT=nUsers)
  ENDIF

  IF KEYWORD_SET(min_NDown) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.NDown GE min_NDown), $
                                 COUNT=nUsers)
  ENDIF

  IF ~KEYWORD_SET(useInds__include_posCurrent) THEN BEGIN
     useInds = CGSETINTERSECTION(useInds, $
                                 WHERE(JVPlotData.cur LE 0.), $
                                 COUNT=nUsers)
  ENDIF


  IF nUsers LE 1 THEN STOP

  RETURN,useInds

END
