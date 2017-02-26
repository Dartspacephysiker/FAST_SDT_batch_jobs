;;2017/02/25
PRO JOURNAL__20170225__CHASTON_ET_AL_2006__INTERFEROMETRY__GET_EFIELD_DATA_FROM_SDT

  saveDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  saveFil = 'Efield_data_for_Chaston_et_al_2006__Fig_A1.sav'

  ;; V1-V2_4k
  ;; V1-V4_16k
  ;; V5-V8_16k
  ;; V7-V8_16k

  dato    = '1998-05-04/'
  t1Str   = dato + '06:44:14'
  t2Str   = dato + '06:44:27'

  t1      = STR_TO_TIME(t1Str)
  t2      = STR_TO_TIME(t2Str)

  v1v2_4k = GET_FA_FIELDS('V1-V2_4k',t1,t2,/CALIBRATE)
  v1v4    = GET_FA_FIELDS('V1-V4_16k',t1,t2,/CALIBRATE)
  v5v8    = GET_FA_FIELDS('V5-V8_16k',t1,t2,/CALIBRATE)
  v7v8    = GET_FA_FIELDS('V7-V8_16k',t1,t2,/CALIBRATE)

  
  FA_FIELDS_COMBINE,v1v2_4k,v1v4,RESULT=v1v4_4k,/TALK
  FA_FIELDS_COMBINE,v1v2_4k,v5v8,RESULT=v5v8_4k,/TALK
  FA_FIELDS_COMBINE,v1v2_4k,v7v8,RESULT=v7v8_4k,/TALK

  fields  = {time:v1v2_4k.time, $
             v1v2:v1v2_4k.comp1, $
             v1v4:v1v4_4k, $
             v5v8:v5v8_4k, $
             v7v8:v7v8_4k}

  IF N_ELEMENTS(UNIQ(fields.time,SORT(fields.time))) NE N_ELEMENTS(fields.time) THEN STOP

  PRINT,"Saving " + saveFil + ' ...'

  ;; SAVE,v1v2_4k,v1v4,v5v8,v7v8,FILENAME=saveDir+saveFil
  SAVE,fields,FILENAME=saveDir+saveFil


  STOP

END
