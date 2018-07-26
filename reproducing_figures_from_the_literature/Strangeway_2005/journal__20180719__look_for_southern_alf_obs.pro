;2018/07/19
PRO JOURNAL__20180719__LOOK_FOR_SOUTHERN_ALF_OBS

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__maximus_vars.pro

  LOAD_MAXIMUS_AND_CDBTIME

  maximus = MAXIMUS__maximus

  RESTORE,"/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/journal__20180719__alfvenwave_obs_inds__cleaned.sav"
  
  south_good_i = good_i[WHERE(maximus.ilat[good_i] LT 0)]

  uniqOrb      = maximus.orbit[south_good_i[UNIQ(maximus.orbit[south_good_i],SORT(maximus.orbit[south_good_i]))]]

  alt = uniqOrb * 0.
  FOREACH orb,uniqOrb,ind DO BEGIN
     tmpI = WHERE((maximus.orbit EQ orb) AND $
                  (maximus.ilat LT 0) AND $
                  (maximus.mlt GE 9) AND $
                  (maximus.mlt LE 16),nTmp)

     IF nTmp GT 0 THEN BEGIN
        alt[ind] = MAX(maximus.alt[tmpI])
     ENDIF
  ENDFOREACH

  STOP

END
