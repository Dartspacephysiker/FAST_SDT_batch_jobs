PRO JOURNAL__20160519__CALC_ENERGY_CHANNEL_SPACING_FOR_DMSP_AND_FAST

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;What are the logarithmic spacings between ESA energy channels for DMSP?
  minE_1            = 30        ; eV
  maxE_1            = 1000      ; eV
  steps_1           = 10
  
  minE_2            = 1000      ; eV
  maxE_2            = 300000
  steps_2           = 10
  
  Erange_1          = [minE_1,maxE_1]
  Erange_2          = [minE_2,maxE_2]
  logErange_1       = ALOG10(Erange_1)
  logErange_2       = ALOG10(Erange_2)

  ;;Note, these are not equally spaced
  delta_E1          = (Erange_1[1]-Erange_1[0])/steps_1       ; 0.152288
  delta_E2          = (Erange_2[1]-Erange_2[0])/steps_2       ; 0.247712
  LOGdelta_E1       = (logErange_1[1]-logErange_1[0])/steps_1 ; 0.152288
  LOGdelta_E2       = (logErange_2[1]-logErange_2[0])/steps_2 ; 0.247712
  
  logElevels_1      = FLOAT(INDGEN(steps_1)*delta_E1)+ logErange_1[0]
  logElevels_2      = FLOAT(INDGEN(steps_2)*delta_E2)+ logErange_2[0]

  Elevels_1         = 10.^(logElevels_1)
  Elevels_2         = 10.^(logElevels_2)

  ;;check
  PRINT,10.^(logElevels_1[-1]+delta_E1)
  PRINT,10.^(logElevels_2[-1]+delta_E2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;What are the logarithmic spacings between ESA energy channels for FAST?
  restore,'../20160420--fit_Maxwellians_kappas_for_inverted_Vs/Orbit_10000/nFlux_and_eSpec--orb_10000__18_08_36-18_09_00.sav'
  FAST_en           = REVERSE(TRANSPOSE(espec.v[0,*]))
  logF_en           = ALOG10(FAST_en)
  nFAST_en          = N_ELEMENTS(logF_en)

  deltaEn_F_Arr       = (SHIFT(FAST_en,-1)-FAST_en)[0:-2]
  LOGdeltaEn_F_Arr    = (SHIFT(logF_en,-1)-logF_en)[0:-2]
  UNLOGdeltaEn_F_Arr  = 10.^LOGdeltaEn_F_Arr
  UNLOG_2STEP         = UNLOGdeltaEn_F_Arr*SHIFT(UNLOGdeltaEn_F_Arr,-1)

END
