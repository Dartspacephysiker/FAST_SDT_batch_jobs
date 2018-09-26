;2018/09/26
FUNCTION SWAY_STATS__ADD_SW_INFO,t1, $
                                 MINUTESBEFORE=minutesBefore, $
                                 MINUTESAVERAGE=minutesAverage, $
                                 INITIALIZE_STRUCT=initialize_struct

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__omni_structs.pro
  
  COMMON SWSTATS_SWINFO,SWStat_SW_tmplt

  minutesBef = KEYWORD_SET(minutesBefore ) ? minutesBefore  : 30 ;How many minutes before interval start should I grab SW data?
  minutesAvg = KEYWORD_SET(minutesAverage) ? minutesAverage : 30 ;Over how many minutes of data shall I average?
  ;; minutesBef = 60 ;How many minutes before interval start should I grab SW data?
  ;; minutesAvg = 60 ;Over how many minutes of data shall I average?

  IF N_ELEMENTS(SWStat_SW_tmplt) EQ 0 THEN BEGIN

     nEntries = 1
     blankL = MAKE_ARRAY(nEntries,/LONG,VALUE=-999)
     blankD = MAKE_ARRAY(nEntries,/DOUBLE,VALUE=-999.D)
     tripleBlankD = TRANSPOSE([[blankD],[blankD],[blankD]])
     ;; BlankD12 = TRANSPOSE([[[blankD],[blankD],[blankD]], $
     ;;                       [[blankD],[blankD],[blankD]], $
     ;;                       [[blankD],[blankD],[blankD]], $
     ;;                       [[blankD],[blankD],[blankD]]])
     SWStat_SW_tmplt = { $ ;; orbit : blankL, $
                       ;; itvl  : blankL, $
                       imf   : tripleBlankD, $
                       clkAngle : blankD, $
                       n     : blankD, $
                       p     : blankD, $
                       t     : blankD, $
                       v_sw  : tripleBlankD, $
                       vSpeed : blankD, $
                       Dst   : blankD, $
                       AE    : blankD, $
                       Kp    : blankD, $
                       var : {imf   : tripleBlankD, $
                              n     : blankD, $
                              p     : blankD, $
                              t     : blankD, $
                              v_sw  : tripleBlankD, $
                              vSpeed : blankD, $
                              Dst   : blankD, $
                              AE    : blankD, $
                              Kp    : blankD} $
                       }
     
  ENDIF

  IF KEYWORD_SET(initialize_struct) THEN RETURN,0

  struct = SWStat_SW_tmplt

  ;; How to use mean with multiple-dim quantities:
  ;; testArray=[[10,10,10],[0,0,0],[5,5,5],[100,100,100]]
  ;; PRINT,MEAN(testArray)
  ;; ;;       28.750000
  ;; PRINT,MEAN(testArray,DIMENSION=1)
  ;; ;;       10.000000       0.0000000       5.0000000       100.00000

  ;; FOR k=0,N_ELEMENTS(thing[0,*])-1 DO BEGIN

  tee1 = DOUBLE(t1)-minutesBef*60.D
  tee2 = tee1+minutesAvg*60.D

  conds = GET_SW_CONDS_UTC(tee1,tee2, $
                           ;; TIME_ARRAY=time_array, $
                           /GET_ALL_VALUES_BETWEEN_T1_AND_T2, $
                           REABERRATE_VY=reaberrate_Vy)

  IF N_ELEMENTS(conds.imf) GT 3 THEN BEGIN
     struct.imf[*]   = MEAN(conds.imf  ,/NAN,DIMENSION=2)
  ENDIF ELSE BEGIN
     struct.imf[*]   = conds.imf
  ENDELSE

  IF N_ELEMENTS(conds.n) GT 1 THEN BEGIN
     struct.n      = MEAN(conds.n,/NAN)
     struct.p      = MEAN(conds.p,/NAN)
     struct.t      = MEAN(conds.t,/NAN)
     struct.var.n  = VARIANCE(conds.n,/NAN)
     struct.var.p  = VARIANCE(conds.p,/NAN)
     struct.var.t  = VARIANCE(conds.t,/NAN)
  ENDIF ELSE BEGIN
     struct.n      = conds.n[0]
     struct.p      = conds.p[0]
     struct.t      = conds.t[0]
     struct.var.n  = 0.
     struct.var.p  = 0.
     struct.var.t  = 0.
  ENDELSE

  IF N_ELEMENTS(conds.vSpeed) GT 1 THEN BEGIN
     struct.vSpeed       = MEAN(conds.vSpeed,/NAN)
     struct.v_sw[*]      = MEAN(conds.v_sw  ,/NAN,DIMENSION=2)
     struct.var.vSpeed   = VARIANCE(conds.vSpeed,/NAN)
     struct.var.v_sw[*]  = VARIANCE(conds.v_sw  ,/NAN,DIMENSION=2)
  ENDIF ELSE BEGIN
     struct.vSpeed       = conds.vSpeed[0]
     struct.v_sw[*]      = conds.v_sw
     struct.var.vSpeed   = 0.
     struct.var.v_sw[*]  = 0.
  ENDELSE

  IF N_ELEMENTS(conds.Dst) GT 1 THEN BEGIN
     struct.Dst      = MEAN(conds.Dst,/NAN)
     struct.var.Dst  = VARIANCE(conds.Dst,/NAN)
  ENDIF ELSE BEGIN
     struct.Dst      = conds.Dst[0]
     struct.var.Dst  = 0.
  ENDELSE
  
  IF N_ELEMENTS(conds.AE) GT 1 THEN BEGIN
     struct.AE      = MEAN(conds.AE,/NAN)
     struct.var.AE  = VARIANCE(conds.AE,/NAN)
  ENDIF ELSE BEGIN
     struct.AE      = conds.AE[0]
     struct.var.AE  = 0.
  ENDELSE
  
  IF N_ELEMENTS(conds.Kp) GT 1 THEN BEGIN
     struct.Kp      = MEAN(conds.Kp,/NAN)
     struct.var.Kp  = VARIANCE(conds.Kp,/NAN)
  ENDIF ELSE BEGIN
     struct.Kp      = conds.Kp[0]
     struct.var.Kp  = 0.
  ENDELSE
     
  RETURN,struct

  ;; plot = PLOT(struct.orbit,struct.p, $
  ;;             LINESTYLE='', $
  ;;             SYMBOL='*', $
  ;;             XTITLE='Orbit', $
  ;;             YTITLE='Pressure (nPa)')

  ;; plot = PLOT(struct.orbit,SQRT(struct.var.p), $
  ;;             LINESTYLE='', $
  ;;             SYMBOL='*', $
  ;;             XTITLE='Orbit', $
  ;;             YTITLE='Pressure variance (nPa)')

END
