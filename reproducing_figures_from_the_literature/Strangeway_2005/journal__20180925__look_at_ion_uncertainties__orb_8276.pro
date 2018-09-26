;2018/09/25
PRO JOURNAL__20180925__LOOK_AT_ION_UNCERTAINTIES__ORB_8276

  COMPILE_OPT IDL2,STRICTARRSUBS

  RESTORE,'/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/20180925-orb_8276-ionMomStruct.sav'

  flipMe = WHERE(FINITE(ionMomStruct.j),nFlip,/NULL)

  IF nFlip GT 0 THEN BEGIN

     ionMomStruct.j[flipMe] = -1. * ionMomStruct.j[flipMe]

     ;; IF KEYWORD_SET(only_leeward_ions) THEN BEGIN
     ;;    ionupJ.y[flipMe] *= 2.
     ;; ENDIF

  ENDIF

  upflow_i = WHERE(FINITE(ionMomStruct.j) AND ionMomStruct.j GT 0,nUpflow)

  LOCALE_ADJUSTMENTS,ionMomStruct.time,upflow_i,nUpflow,/DAY,MINILAT=60,/NORTH

  CGHISTOPLOT,ALOG10(ionMomStruct.j[upflow_i])

  meanUp = MEAN(ionMomStruct.j[upflow_i])

  weights1 = 1.D/ABS(ionMomStruct.jErr)
  weights2 = 1.D/ABS(ionMomStruct.jErr)^2.D
  weights3 = 1.D/ALOG(ABS(ionMomStruct.jErr))
  wtMean1 = TOTAL(weights1[upflow_i]*ionMomStruct.j[upflow_i])/TOTAL(weights1[upflow_i])
  wtMean2 = TOTAL(weights2[upflow_i]*ionMomStruct.j[upflow_i])/TOTAL(weights2[upflow_i])
  wtMean3 = TOTAL(weights3[upflow_i]*ionMomStruct.j[upflow_i])/TOTAL(weights3[upflow_i])

  unc1 = SQRT(TOTAL((ionMomStruct.jErr[upflow_i])^2.D))

  STOP

  this = PLOT(ALOG10(ionMomStruct.j[upflow_i]),ALOG10(ionMomStruct.jErr[upflow_i]),LINESTYLE='',SYMBOL='*',XTITLE='log upflow',YTITLE='log error')


END
