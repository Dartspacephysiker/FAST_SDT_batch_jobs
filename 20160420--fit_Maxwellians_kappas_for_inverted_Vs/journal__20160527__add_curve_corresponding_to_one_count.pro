;;05/27/16 Chris wants me to add a one-count curve to our stuff
;;Doing it with orb 1843 for starters
PRO JOURNAL__20160527__ADD_CURVE_CORRESPONDING_TO_ONE_COUNT

  COMPILE_OPT IDL2

  eeb_or_ees             = 'eeb'
  
  orb_num                = 1843
  orb                    = STRING(FORMAT='(I0)',orb_num)
  t1Str                  = '97-02-07/20:49:31'
  t2Str                  = '97-02-07/20:50:19'
  
  t1                     = STR_TO_TIME(t1Str)
  t2                     = STR_TO_TIME(t2Str)


  func                   = STRING(FORMAT='("GET_FA_",A0)',STRUPCASE(eeb_or_ees))
  dat                    = CALL_FUNCTION(func,t1,INDEX=idx)

  IF ~dat.valid THEN BEGIN
     PRINT,'Invalid data from SDT!'
     RETURN
  ENDIF

  ;;Convert to one count
  
  oneCountSpec           = J_2D_B_EN(dat,

END
