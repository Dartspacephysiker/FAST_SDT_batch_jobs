;2016/06/11 Well, I suppose it's better to find out now than later, or maybe even never
PRO GET_MAPPED_BFIELDVALS_FOR_ALFVENDB_TIMES__UCLA_DESPIN__201605_UPDATES,orbit_num

  COMPILE_OPT idl2

  thisDir    = '/SPENCEdata/software/sdt/batch_jobs/map_Poyntingflux__20151217/'

  ;; alfFile    = 'alfTimes_and_alfOrbits--20160107_despun_DB.sav'
  alfFile    = 'orbs_and_times--201605_despun_maximus_db.sav'

  outDir     = thisDir + 'output__despundb_20160611/'

  ;;Get the Alfven DB stuff
  restore,thisDir+alfFile

  ;;For skipping the "get interval times" bit
  as5_dir                                = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  indDir                                 = as5_dir + 'je_time_ind_dir/'
  indFilePref                            = "je_and_cleaned_time_range_indices--orbit_"
  intervalArrFile                        = "orb_and_num_intervals--0-16361.sav" ;;Use it to figure out which file to restore

  ;;How I got the orbs and tims
  ;; LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbtime,/DO_DESPUNDB
  ;; orbit = maximus.orbit
  ;; SAVE,orbit,cdbtime,filename='~/software/sdt/batch_jobs/map_Poyntingflux__20151217/orbs_and_times--201605_despun_maximus_db.sav'

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                                 = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  ;; dat                                    = GET_FA_EES(0.0D, EN=1)
  ;; IF dat.valid EQ 0 THEN BEGIN
  ;;    print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
  ;;   RETURN
  ;; ENDIF ELSE BEGIN
  ;;    n_EESA_spectra                      = dat.index+1
  ;;    last_index                          = LONG(dat.index)

  ;;    PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ;; ENDELSE

  ;; t2                                     = 0.0D
  ;; temp                                   = GET_FA_EES(t2,INDEX=0.0D)
  ;; t1                                     = t2
  ;; temp                                   = GET_FA_EES(t2,/ADV)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First, see that we are able to match all points in this orb
  RESTORE,as5_dir+intervalArrFile

  ;; GET_FA_ORBIT,t1,t2
  ;; ;;now get orbit quantities
  ;; GET_DATA,'ORBIT',DATA=orb
  ;; orbit_num                              = orb.y[0]
  orbStr                                 = STRCOMPRESS(orbit_num,/REMOVE_ALL)
  number_of_intervals                    = intervalArr[orbit_num]
  print,'number_of_intervals',number_of_intervals

  indFile                                = STRING(FORMAT='(A0,I0,"--",I0,"_intervals.sav")', $
                                                  indFilePref,orbit_num,number_of_intervals)

  ;;This file gives us je,orbit_num,time_range_indices, and time_range
  IF FILE_TEST(indDir+indFile) THEN BEGIN
     PRINT,'Restoring indFile ' + indFile + ' ...'
     RESTORE,indDir+indFile
  ENDIF ELSE BEGIN
     PRINT,"File doesn't exist: " + indFile
     PRINT,"Returning ..."
     RETURN
  ENDELSE

  outFile = 'mapping_ratio--orb_'+orbStr+'.sav'

  IF FILE_TEST(outDir+outFile) THEN BEGIN
     PRINT,"File exists: " + outFile
     PRINT,'Skipping ...'
     RETURN
  ENDIF

  these_i          = where(orbit EQ orbit_num,nMatch) ;relevant indices

  IF these_i[0] NE -1 THEN BEGIN

     times_i       = MAKE_ARRAY(N_ELEMENTS(these_i),/LONG)
     times         = MAKE_ARRAY(N_ELEMENTS(these_i),/DOUBLE)
     ratios        = MAKE_ARRAY(N_ELEMENTS(these_i),/DOUBLE)

     GET_FA_ORBIT,cdbTime[these_i],/TIME_ARRAY,/ALL
     get_data,'ILAT',data=tmp
     ;; GET_DATA,'ORBIT',data=tmp

     FOR j=0,nMatch-1 DO BEGIN
        tempMin    = MIN(ABS(tmp.x-cdbTime[these_i[j]]),tempMin_i)
        ;; PRINT,"Min time difference: " + STRCOMPRESS(tempMin,/REMOVE_ALL)
        times[j]   = tmp.x[tempMin_i]
        times_i[j] = tempMin_i
     ENDFOR

     ;;Scale electron energy flux to 100km, pos flux earthward
     ;; get_data,'ILAT',data=tmp
     get_data,'B_model',data=tmp1
     get_data,'BFOOT',data=tmp2
     ;; mag1 = (tmp1.y(*,0)*tmp1.y(*,0)+tmp1.y(*,1)*tmp1.y(*,1)+tmp1.y(*,2)*tmp1.y(*,2))^0.5
     ;; mag2 = (tmp2.y(*,0)*tmp2.y(*,0)+tmp2.y(*,1)*tmp2.y(*,1)+tmp2.y(*,2)*tmp2.y(*,2))^0.5
     mag1          = (tmp1.y[times_i,0]*tmp1.y[times_i,0]+tmp1.y[times_i,1]*tmp1.y[times_i,1]+tmp1.y[times_i,2]*tmp1.y[times_i,2])^0.5
     mag2          = (tmp2.y[times_i,0]*tmp2.y[times_i,0]+tmp2.y[times_i,1]*tmp2.y[times_i,1]+tmp2.y[times_i,2]*tmp2.y[times_i,2])^0.5
     ratio         = (mag2/mag1)

     PRINT,'Saving ' + outFile
     SAVE,times,ratio,mag1,mag2,FILENAME=outDir+outFile


  ENDIF ELSE BEGIN
     PRINT,'No data for orbit ' + orbStr + '!!!'
  ENDELSE

END