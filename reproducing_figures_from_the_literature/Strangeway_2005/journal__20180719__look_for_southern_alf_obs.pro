;2018/07/19
PRO JOURNAL__20180719__LOOK_FOR_SOUTHERN_ALF_OBS

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_MAXIMUS_AND_CDBTIME,maximus,/NO_MEMORY_LOAD

  RESTORE,"/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/journal__20180719__alfvenwave_obs_inds__cleaned.sav"
  
  south_good_i = WHERE(maximus.ilat[good_i] LT 0)

  STOP

END
