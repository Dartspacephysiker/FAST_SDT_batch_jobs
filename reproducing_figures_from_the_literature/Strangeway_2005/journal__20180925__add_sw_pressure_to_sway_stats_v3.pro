;2018/09/25
PRO JOURNAL__20180925__ADD_SW_PRESSURE_TO_SWAY_STATS_V3

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__omni_structs.pro

  
  minutesBef = 30 ;How many minutes before interval start should I grab SW data?
  minutesAvg = 30 ;Over how many minutes of data shall I average?
  ;; minutesBef = 60 ;How many minutes before interval start should I grab SW data?
  ;; minutesAvg = 60 ;Over how many minutes of data shall I average?

  thing =   [ $
            [8260,  0, 906553947L], $
            [8261,  0, 906561747L], $
            [8262,  0, 906569630L], $
            [8263,  0, 906577608L], $
            [8264,  0, 906585689L], $
            [8265,  0, 906593823L], $
            [8266,  0, 906601935L], $
            [8267,  0, 906610004L], $
            [8267,  1, 906611278L], $
            [8268,  0, 906617995L], $
            [8269,  0, 906625898L], $
            [8270,  0, 906633727L], $
            [8271,  0, 906641513L], $
            [8272,  0, 906649326L], $
            [8275,  0, 906673322L], $
            [8276,  0, 906681459L], $
            [8277,  0, 906689569L], $
            [8278,  0, 906697622L], $
            [8279,  0, 906705604L], $
            [8280,  0, 906713486L], $
            [8280,  1, 906713760L], $
            [8280,  2, 906715319L], $
            [8281,  0, 906721300L], $
            [8281,  1, 906721740L], $
            [8282,  0, 906729079L], $
            [8283,  0, 906736901L], $
            [8284,  0, 906744861L], $
            [8285,  0, 906752851L], $
            [8286,  0, 906760961L], $
            [8287,  0, 906769091L], $
            [8288,  0, 906777197L], $
            [8289,  0, 906785236L], $
            [8290,  0, 906793194L], $
            [8291,  0, 906801062L], $
            [8292,  0, 906808862L] $
            ]

  nEntries = N_ELEMENTS(thing[0,*])
  blankL = MAKE_ARRAY(nEntries,/LONG,VALUE=-999)
  blankD = MAKE_ARRAY(nEntries,/DOUBLE,VALUE=-999.D)
  tripleBlankD = TRANSPOSE([[blankD],[blankD],[blankD]])
  BlankD12 = TRANSPOSE([[[blankD],[blankD],[blankD]], $
                        [[blankD],[blankD],[blankD]], $
                        [[blankD],[blankD],[blankD]], $
                        [[blankD],[blankD],[blankD]]])
  struct = {orbit : blankL, $
            itvl  : blankL, $
            imf   : tripleBlankD, $
            n     : blankD, $
            p     : blankD, $
            t     : blankD, $
            v_sw  : tripleBlankD, $
            vSpeed : blankD, $
            Dst   : blankD, $
            AE    : blankD, $
            Kp    : blankD, $
            variance : {imf   : tripleBlankD, $
                        n     : blankD, $
                        p     : blankD, $
                        t     : blankD, $
                        v_sw  : tripleBlankD, $
                        vSpeed : blankD, $
                        Dst   : blankD, $
                        AE    : blankD, $
                        Kp    : blankD} $
           }
            
  ;; How to use mean with multiple-dim quantities:
  ;; testArray=[[10,10,10],[0,0,0],[5,5,5],[100,100,100]]
  ;; PRINT,MEAN(testArray)
  ;; ;;       28.750000
  ;; PRINT,MEAN(testArray,DIMENSION=1)
  ;; ;;       10.000000       0.0000000       5.0000000       100.00000

  FOR k=0,N_ELEMENTS(thing[0,*])-1 DO BEGIN

     tee1 = DOUBLE(thing[2,k])-minutesBef*60.D
     tee2 = tee1+minutesAvg*60.D

     conds = GET_SW_CONDS_UTC(tee1,tee2, $
                              ;; TIME_ARRAY=time_array, $
                              /GET_ALL_VALUES_BETWEEN_T1_AND_T2, $
                              REABERRATE_VY=reaberrate_Vy)

     struct.orbit[k]     = thing[0,k]
     struct.itvl[k]      = thing[1,k]

     IF N_ELEMENTS(conds.imf) GT 3 THEN BEGIN
        struct.imf[*,k]   = MEAN(conds.imf  ,/NAN,DIMENSION=2)
     ENDIF ELSE BEGIN
        struct.imf[*,k]   = conds.imf
     ENDELSE

     IF N_ELEMENTS(conds.n) GT 1 THEN BEGIN
        struct.n[k]           = MEAN(conds.n,/NAN)
        struct.p[k]           = MEAN(conds.p,/NAN)
        struct.t[k]           = MEAN(conds.t,/NAN)
        struct.variance.n[k]  = VARIANCE(conds.n,/NAN)
        struct.variance.p[k]  = VARIANCE(conds.p,/NAN)
        struct.variance.t[k]  = VARIANCE(conds.t,/NAN)
     ENDIF ELSE BEGIN
        struct.n[k]           = conds.n[0]
        struct.p[k]           = conds.p[0]
        struct.t[k]           = conds.t[0]
        struct.variance.n[k]  = 0.
        struct.variance.p[k]  = 0.
        struct.variance.t[k]  = 0.
     ENDELSE

     IF N_ELEMENTS(conds.vSpeed) GT 1 THEN BEGIN
        struct.vSpeed[k]           = MEAN(conds.vSpeed,/NAN)
        struct.v_sw[*,k]           = MEAN(conds.v_sw  ,/NAN,DIMENSION=2)
        struct.variance.vSpeed[k]  = VARIANCE(conds.vSpeed,/NAN)
        struct.variance.v_sw[*,k]  = VARIANCE(conds.v_sw  ,/NAN,DIMENSION=2)
     ENDIF ELSE BEGIN
        struct.vSpeed[k]           = conds.vSpeed[0]
        struct.v_sw[*,k]           = conds.v_sw
        struct.variance.vSpeed[k]  = 0.
        struct.variance.v_sw[*,k]  = 0.
     ENDELSE

     IF N_ELEMENTS(conds.Dst) GT 1 THEN BEGIN
        struct.Dst[k]        = MEAN(conds.Dst,/NAN)
        struct.variance.Dst[k] = VARIANCE(conds.Dst,/NAN)
     ENDIF ELSE BEGIN
        struct.Dst[k]           = conds.Dst[0]
        struct.variance.Dst[k] = 0.
     ENDELSE
     
     IF N_ELEMENTS(conds.AE) GT 1 THEN BEGIN
        struct.AE[k]           = MEAN(conds.AE,/NAN)
        struct.variance.AE[k]  = VARIANCE(conds.AE,/NAN)
     ENDIF ELSE BEGIN
        struct.AE[k]           = conds.AE[0]
        struct.variance.AE[k]  = 0.
     ENDELSE
     
     IF N_ELEMENTS(conds.Kp) GT 1 THEN BEGIN
        struct.Kp[k]           = MEAN(conds.Kp,/NAN)
        struct.variance.Kp[k]  = VARIANCE(conds.Kp,/NAN)
     ENDIF ELSE BEGIN
        struct.Kp[k]           = conds.Kp[0]
        struct.variance.Kp[k]  = 0.
     ENDELSE
     
  ENDFOR

  plot = PLOT(struct.orbit,struct.p, $
              LINESTYLE='', $
              SYMBOL='*', $
              XTITLE='Orbit', $
              YTITLE='Pressure (nPa)')

  plot = PLOT(struct.orbit,SQRT(struct.variance.p), $
              LINESTYLE='', $
              SYMBOL='*', $
              XTITLE='Orbit', $
              YTITLE='Pressure variance (nPa)')

  STOP

  FOR k=0,N_ELEMENTS(thing[0,*])-1 DO BEGIN

     PRINT,thing[0,k]

  ENDFOR
END
