;2017/09/21
PRO JOURNAL__20170921__TRY_THE_STEVE_ON_ORB_1773

  COMPILE_OPT IDL2,STRICTARRSUBS

  
  fitDir  = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  fitFile = '20170922_kappaGaussFits_orig__for_kappaManuscript_Fig2.sav'

  ;; It turns out that these fitFiles don't have the original data. That's
  ;; OK--uncomment the lines below the comment "Temporary 2017/09/22" in
  ;; JOURNAL__20170814__THE_CLASSICS_PLUSMINUS_CHARE__JIM_IDEA to skip to
  ;; majicTime. I then set a break inside KAPPA__GET_FITS__MPFIT1D at the end of
  ;; the routine, manually added the struct 'orig' to kappaFits, and then
  ;; executed
  ;; SAVE,kappaFit,gaussFit,FILENAME='/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/20170922_kappaGaussFits_orig__for_kappaManuscript_Fig2.sav'

  ;; BEGIN Spence learns his lesson

  ;; fitFile = '20170815-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-only_fit_peak_eRange-avg_itvl2.sav'
  ;; fitFile = '20170614-orb_1773-Kappa_fits_and_Gauss_fits-ees-horseshoe2d-classics-3-Elphic_et_al_1998-only_fit_peak_eRange-avg_itvl2.sav'

  majic = '1997-02-01/09:27:01.57' ;Time of plot in latest kappa manuscript
  majicT = STR_TO_TIME(majic)

  IF ~FILE_TEST(fitDir+fitFile) THEN BEGIN
     PRINT,"Say what!?!"
     STOP
  ENDIF

  RESTORE,fitDir+fitFile

  STOP
  
  ;; fitKT   = !NULL
  ;; fitGT   = !NULL
  ;; fit2DKT = !NULL
  ;; fit2DGT = !NULL
  ;; FOR k=0,N_ELEMENTS(kappaFits)-1 DO BEGIN
  ;;    fitKT = [fitKT,kappaFits[k].time]
  ;; ENDFOR
  ;; FOR k=0,N_ELEMENTS(gaussFits)-1 DO BEGIN
  ;;    fitGT = [fitGT,gaussFits[k].time]
  ;; ENDFOR
  ;; FOR k=0,N_ELEMENTS(fit2DKappa_inf_list)-1 DO BEGIN
  ;;    fit2DKT = [fit2DKT,fit2DKappa_inf_list[k].sdt.time]
  ;; ENDFOR
  ;; FOR k=0,N_ELEMENTS(fit2DGauss_inf_list)-1 DO BEGIN
  ;;    fit2DGT = [fit2DGT,fit2DGauss_inf_list[k].sdt.time]
  ;; ENDFOR

  ;; kFitInd = VALUE_CLOSEST2(fitKT,majicT,/CONSTRAINED)
  ;; gFitInd = VALUE_CLOSEST2(fitGT,majicT,/CONSTRAINED)

  ;; fit2DKInd = VALUE_CLOSEST2(fit2DKT,majicT,/CONSTRAINED)
  ;; fit2DGInd = VALUE_CLOSEST2(fit2DGT,majicT,/CONSTRAINED)

  ;; PRINT,kFitInd,gFitInd,fit2DKInd,fit2DGInd

  ;; PRINT,FORMAT='(A0,T25,": ",I4,T35,A0)',"kappaFit ind,Time",kFitInd,TIME_TO_STR(fitKT[kFitInd],/MS)
  ;; PRINT,FORMAT='(A0,T25,": ",I4,T35,A0)',"gaussFit ind,Time",gFitInd,TIME_TO_STR(fitGT[gFitInd],/MS)
  ;; PRINT,FORMAT='(A0,T25,": ",I4,T35,A0)',"k2DFit   ind,Time",fit2DKInd,TIME_TO_STR(fit2DKT[fit2DKInd],/MS)
  ;; PRINT,FORMAT='(A0,T25,": ",I4,T35,A0)',"g2DFit   ind,Time",fit2DGInd,TIME_TO_STR(fit2DGT[fit2DGInd],/MS)
  ;; PRINT,""

  ;; kFitMajic = kappaFits[kFitInd]
  ;; gFitMajic = gaussFits[gFitInd]
  
  ;; plot = PLOT(kFitMajic.x,kFitMajic.y,/XLOG,/YLOG,SYMBOL='+',LINESTYLE='')
  ;; plot = PLOT(kFitMajic.xFull,kFitMajic.yFull,/XLOG,/YLOG,/OVERPLOT,COLOR='RED')

  ;; STOP 
  ;; END Spence learns his lesson



END
