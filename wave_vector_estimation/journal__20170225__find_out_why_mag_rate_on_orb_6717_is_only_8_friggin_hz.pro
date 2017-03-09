;;2017/02/25
;;See, it's because the silly fluxgate mag was only sampling at 8 Hz at the time. What a travesty!
PRO JOURNAL__20170225__FIND_OUT_WHY_MAG_RATE_ON_ORB_6717_IS_ONLY_8_FRIGGIN_HZ

  COMPILE_OPT IDL2

  dato           = '98-05-04/'
  t1             = dato + '06:44:31'  
  t2             = dato + '06:45:00'

  timeBar_times  = dato + ['06:44:48','06:44:56.5']

  ;;Snippet from ucla_mag_lib
  has_mag        = get_mag_dqis()

  magxyz         = {valid:has_mag.magxyz}
  if (magxyz.valid) then magxyz=get_fa_fields('MagXYZ',/all,repair=repair)
  ;;End snippet
  
  tt             = STR_TO_TIME(timebar_times)
  GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,magxyz.time,-2, $
                                     START_I=start_i, $
                                     STOP_I=stop_i, $
                                     OUT_RATES=rates, $
                                     STREAKLENS=streaklens, $
                                     DELTA=0.0055
  those        = VALUE_CLOSEST2(magxyz.time,tt)
  whereInStart = VALUE_LOCATE(start_i, $
                              VALUE_CLOSEST2(magxyz.time,tt))

  IF whereInStart[0] NE whereInStart[1] THEN STOP

  PRINT,magxyz.time[start_iwhereInStart]-tt[0]
  PRINT,magxyz.time[start_i[whereInStart+1]]-tt[0]
  PRINT,magxyz.time[stop_iwhereInStart]-tt[0]
  PRINT,magxyz.time[stop_iwhereInStart]-tt[1]
  PRINT,rateswhereInStart
  PRINT,1/rateswhereInStart

END
