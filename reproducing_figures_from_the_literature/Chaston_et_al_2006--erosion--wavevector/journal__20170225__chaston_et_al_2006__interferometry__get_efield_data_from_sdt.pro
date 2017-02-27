;;2017/02/25
PRO JOURNAL__20170225__CHASTON_ET_AL_2006__INTERFEROMETRY__GET_EFIELD_DATA_FROM_SDT, $
   SAVE_16K_V578=save_16k_v578

  orig_rtine  = 'JOURNAL__20170225__CHASTON_ET_AL_2006__INTERFEROMETRY__GET_EFIELD_DATA_FROM_SDT'
  saveDir     = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  saveFil     = 'Efield_data_for_Chaston_et_al_2006__Fig_A1'

  ;; V1-V2_4k
  ;; V1-V4_16k
  ;; V5-V8_16k
  ;; V7-V8_16k

  dato        = '1998-05-04/'
  t1Str       = dato + '06:44:10'
  t2Str       = dato + '06:44:30'

  t1          = STR_TO_TIME(t1Str)
  t2          = STR_TO_TIME(t2Str)

  v1v2_4k     = GET_FA_FIELDS('V1-V2_4k',t1,t2,/CALIBRATE)
  v1v4        = GET_FA_FIELDS('V1-V4_16k',t1,t2,/CALIBRATE)
  v5v8        = GET_FA_FIELDS('V5-V8_16k',t1,t2,/CALIBRATE)
  v7v8        = GET_FA_FIELDS('V7-V8_16k',t1,t2,/CALIBRATE)

  
  FA_FIELDS_COMBINE,v1v2_4k,v1v4,RESULT=v1v4_4k,/TALK

  CASE 1 OF
     KEYWORD_SET(save_16k_v578): BEGIN

        FA_FIELDS_COMBINE,v5v8,v7v8,RESULT=v7v8_16k,/TALK

        fields      = {time124              : v1v2_4k.time  , $
                       time578              : v5v8.time     , $
                       v1v2                 : v1v2_4k.comp1 , $
                       v1v4                 : v1v4_4k       , $
                       v5v8                 : v5v8.comp1    , $
                       v7v8                 : v7v8_16k      , $
                       originating_routine  : orig_rtine    , $
                       date                 : GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                       info                 : 'Created with calls to GET_FA_FIELDS for V1-V2_4k V1-V4_16k, and V{5,7}-V8_16k, as well as FA_FIELDS_COMBINE'}

        suff = '-16k_v578__4k_v124'

        IF N_ELEMENTS(UNIQ(fields.time124,SORT(fields.time124))) NE N_ELEMENTS(fields.time124) THEN STOP

        IF N_ELEMENTS(UNIQ(fields.time578,SORT(fields.time578))) NE N_ELEMENTS(fields.time578) THEN STOP

     END
     ELSE: BEGIN

        FA_FIELDS_COMBINE,v1v2_4k,v5v8,RESULT=v5v8_4k,/TALK
        FA_FIELDS_COMBINE,v1v2_4k,v7v8,RESULT=v7v8_4k,/TALK

        fields      = {time                 : v1v2_4k.time  , $
                       v1v2                 : v1v2_4k.comp1 , $
                       v1v4                 : v1v4_4k       , $
                       v5v8                 : v5v8_4k       , $
                       v7v8                 : v7v8_4k       , $
                       originating_routine  : orig_rtine    , $
                       date                 : GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                       info                 : 'Created with calls to GET_FA_FIELDS for V1-V2_4k V1-V4_16k, and V{5,7}-V8_16k, as well as FA_FIELDS_COMBINE'}

        IF N_ELEMENTS(UNIQ(fields.time,SORT(fields.time))) NE N_ELEMENTS(fields.time) THEN STOP

        suff = '-4k_all'
     END
  ENDCASE

  saveFil += suff + '.sav'
  PRINT,"Saving " + saveFil + ' ...'

  ;; SAVE,v1v2_4k,v1v4,v5v8,v7v8,FILENAME=saveDir+saveFil
  SAVE,fields,FILENAME=saveDir+saveFil


  STOP

END
