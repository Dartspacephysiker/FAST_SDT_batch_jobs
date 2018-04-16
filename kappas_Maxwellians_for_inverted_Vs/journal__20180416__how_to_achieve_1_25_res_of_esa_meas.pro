;2018/04/16
PRO JOURNAL__20180416__HOW_TO_ACHIEVE_1_25_RES_OF_ESA_MEAS

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/'
  filename = '20180416-tmpTimes_1694.sav'
  RESTORE,dir+filename

  ;; cumulT = TOTAL(times[1:-1]-times[0:-2],/CUMULATIVE)
  ;; CHECK_SORTED,cumulT,is_sorted,/QUIET
  ;; IF ~is_sorted THEN BEGIN
  ;;    PRINT,"Not sorted!"
  ;;    STOP
  ;; ENDIF

  nInds  = N_ELEMENTS(times)-1
  curInd = 0
  startIndArr = !NULL
  endIndArr   = !NULL
  ;; cumulT      = times
  WHILE curInd LT nInds DO BEGIN
     nextInd = VALUE_CLOSEST2(times-times[curInd],1.25,/CONSTRAINED)

     ;; PRINT,FORMAT='(A25,TR5,I06,":",F8.3)',T2S(times[curInd],/MS),curInd,times[nextInd]-times[curInd]

     startIndArr = [startIndArr,curInd   ]
     endIndArr   = [endIndArr  ,nextInd-1]
     curInd      = nextInd
     
     IF (times[-1] - times[curInd]) LT 1.5 THEN BREAK

  ENDWHILE

  thisMany = N_ELEMENTS(startIndArr)
  FOR k=0,thisMany-1 DO BEGIN

     ind1 = startIndArr[k]
     ind2 = endIndArr[k]
     
     ;; PRINT,FORMAT='(I3,"–",I-3,TR5,":",F0.4,TR5,"(",A25,"–",A25,")")', $
     ;;       ind1,ind2, $
     ;;       times[ind2]-times[ind1], $
     ;;       T2S(times[ind1],/MS), $
     ;;       T2S(times[ind2],/MS)

     PRINT,FORMAT='(I3,"–",I-3,TR5,":",F0.4,TR5,"(",A25,"–",A25,")")', $
           ind1,ind2, $
           dat_eFlux[ind2].end_time-dat_eFlux[ind1].time, $
           T2S(dat_eFlux[ind1].time,/MS), $
           T2S(dat_eFlux[ind2].end_time,/MS)

  ENDFOR
    
  STOP

END
