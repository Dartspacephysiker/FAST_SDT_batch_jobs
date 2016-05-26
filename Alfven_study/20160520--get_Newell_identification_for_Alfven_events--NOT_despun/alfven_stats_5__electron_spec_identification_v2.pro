PRO ALFVEN_STATS_5__ELECTRON_SPEC_IDENTIFICATION_V2, $
   ENERGY_ELECTRONS=energy_electrons,ENERGY_IONS=energy_ions, $
   T1=t1,T2=t2, $
   BATCH_MODE=batch_mode, $
   INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  dbDir                                  = '/SPENCEdata/Research/database/FAST/dartdb/saves/'
  dbTFile                                = 'Dartdb_20150814--500-16361_inc_lower_lats--burst_1000-16361--cdbtime.sav'
  dbOrbFile                              = 'Dartdb_20151222--500-16361_inc_lower_lats--burst_1000-16361--orbits.sav'
  as5_dir                                = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  alfven_startstop_maxJ_file             = './alfven_startstop_maxJ_times--500-16361_inc_lower_lats--burst_1000-16361.sav'

  todayStr                               = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  outDir                                 = as5_dir + 'batch_output/'
  outNewellDir                           = as5_dir + 'Newell_batch_output/'
  outFile_pref                           = 'Dartdb--Alfven--Newell_identification_of_electron_spectra--Orbit_'
  IF KEYWORD_SET(include_ions) THEN BEGIN
     newellStuff_pref                    = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'
  ENDIF ELSE BEGIN
     newellStuff_pref                    = 'Newell_et_al_identification_of_electron_spectra--Orbit_'
  ENDELSE
  noEventsFile                           = 'Orbs_without_Alfven_events--'+todayStr+'.txt'
  badFile                                = 'Orbs_with_other_issues--'+todayStr+'.txt'

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons                    = [0.,30000.]                           ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF
  IF NOT KEYWORD_SET(energy_ions) THEN BEGIN
     energy_ions                         = [0.,500.]     ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                                 = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                                    = GET_FA_EES(0.0, EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
    RETURN
  ENDIF ELSE BEGIN
     n_EESA_spectra                      = dat.index+1
     last_index                          = LONG(dat.index)
  
     PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t1                                     = 0.0D
  t2                                     = 0.0D
  temp                                   = GET_FA_EES(t1,INDEX=0.0D)
  temp                                   = GET_FA_EES(t2,INDEX=DOUBLE(last_index))
  GET_2DT_TS,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=ENERGY_ELECTRONS

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Welp, here we go!

  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep                                   = WHERE(FINITE(tmpj.y))
  tmpj.x                                 = tmpj.x[keep]
  tmpj.y                                 = tmpj.y[keep]
  keep                                   = WHERE(ABS(tmpj.y) GT 0.0)
  tx                                     = tmpj.x[keep]
  ty                                     = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order                             = SORT(tx)
  tx                                     = tx[time_order]
  ty                                     = ty[time_order]
  
  ;;throw away the first 10  points since they are often corrupted
  STORE_DATA,'Je',DATA={x:tx[10:N_ELEMENTS(tx)-1],y:ty[10:N_ELEMENTS(tx)-1]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je                            = MAKE_ARRAY(N_ELEMENTS(Je.x),/double)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j]                      = ABS(Je.x[j]-Je.x[j-1])
  ENDFOR
  part_res_Je[0]                         = part_res_Je[1]
  gap                                    = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start                      = [0,where(part_res_je GT 10.0)]
     separate_stop                       = [where(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start                      = [0]
     separate_stop                       = [N_ELEMENTS(Je.x)-1]
  ENDELSE

  ;;remove esa burp when switched on
  turn_on                                = WHERE(part_res_je GT 300.0)
  IF turn_on[0] NE -1 THEN BEGIN
     turn_on_separate                    = MAKE_ARRAY(N_ELEMENTS(turn_on),/DOUBLE)
     FOR j=0,N_ELEMENTS(turn_on)-1 DO BEGIN
        turn_on_separate[j]              = WHERE(separate_start EQ turn_on[j])
     ENDFOR
     separate_start[turn_on_separate+1]  = separate_start[turn_on_separate+1]+5
  ENDIF

  ;; loop through all of the EES distributions in SDT:
  ;; for I                               = 0L, last_index do begin
  ;;    didx                             = double(I)
  ;;    data                             = get_fa_ees(0.0, INDEX=didx)
  ;; endfor

  ;;identify time indices for each interval
  count                                  = 0.0
  FOR j=0,N_ELEMENTS(separate_start)-1 DO BEGIN
     IF (separate_stop[j]-separate_start[j]) GT 10 THEN BEGIN
        count                            = count+1
        IF count EQ 1.0 THEN BEGIN
           time_range_indices            = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        ENDIF ELSE BEGIN
           time_range_indices            = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        ENDELSE
     ENDIF
  ENDFOR
  
  ;;identify interval times
  time_ranges                            = je.x[time_range_indices]
  number_of_intervals                    = N_ELEMENTS(time_ranges[*,0])
  
  print,'number_of_intervals',number_of_intervals
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First, see that we are able to match all points in this orb
  RESTORE,dbDir+dbOrbFile

  GET_FA_ORBIT,je.x[time_range_indices[0,0]],je.x[time_range_indices[0,1]]
  ;;now get orbit quantities
  GET_DATA,'ORBIT',DATA=orb
  orbit_num                              = orb.y[0]
  orbStr                                 = STRCOMPRESS(orbit_num,/REMOVE_ALL)
  match_i                                = WHERE(alfven_orblist EQ orbit_num,nMatch)
  IF nMatch EQ 0 THEN BEGIN
     PRINT,'No Alfvén matches!'
     WRITE_MESSAGE_TO_LOGFILE,noEventsFile, $
                              STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No Alfs'), $
                              /APPEND

  ENDIF ELSE BEGIN
     ;;set up output
     outFile                             = outFile_pref + orbStr + '.sav'

     RESTORE,alfven_startstop_maxJ_file

     start_times                            = alfven_start_time[match_i]
     stop_times                             = alfven_stop_time[match_i]
     center_times                           = cdbTime[match_i]
     matched                                = MAKE_ARRAY(nMatch,VALUE=255,/BYTE) ;If well-matched, matched[i] = jjj
                                ;If loosely matched, matched[i] = 255-jjj-1
     nHits                                          = MAKE_ARRAY(nMatch,VALUE=0,/INTEGER)
     nSpectra                                       = 0
     hit_nSpectra                                   = !NULL
     alfvenDBindices_for_spectra                    = !NULL
     orbInterval_for_spectra                        = !NULL
     electron_startstop_alfven_ind_list             = LIST()
     electron_startstop_alfven_time_list            = LIST()
     temp_last_closest                              = MAKE_ARRAY(nMatch,VALUE=250,/FLOAT)
  ENDELSE

  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN

     ;;We're going to make output in any case. We're already here, after all!
     out_newell_file                             = newellStuff_pref + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     PRINT,'time_range',time_to_str(time_ranges[jjj,0]),time_to_str(time_ranges[jjj,1])
     
     je_tmp_time                                 = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data                                 = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]

     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}

     ;;get_orbit data
     GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
     
     ;;define loss cone angle
     GET_DATA,'ALT',DATA=alt
     loss_cone_alt                               = alt.y[0]*1000.0
     lcw                                         = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
     GET_DATA,'ILAT',DATA=ilat
     north_south                                 = ABS(ilat.y[0])/ilat.y[0]
     
     if north_south EQ -1 then begin
        e_angle                                  = [180.-lcw,180+lcw] ; for Southern Hemis.
        ;;i_angle=[270.0,90.0]	
        ;;elimnate ram from data
        i_angle                                  = [180.0,360.0]
        i_angle_up                               = [270.0,360.0]
        
     endif else begin
        e_angle                                  = [360.-lcw,lcw]  ;	for Northern Hemis.
        ;;i_angle=[90.,270.0]
        ;;eliminate ram from data
        i_angle                                  = [0.0,180.0]
        i_angle_up                               = [90.0,180.0]
        
     endelse
     
     ;;get fields mode
     fields_mode=GET_FA_FIELDS('DataHdr_1032',time_ranges[jjj,0],time_ranges[jjj,1])
     
     GET_SC_POTENTIAL,T1=t1,T2=t2,DATA=sc_pot

     ;;get moments/integrals of various fluxes
     GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEe_lc',ANGLE=e_angle,ENERGY=energy_electrons,SC_POT=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot, $
                    OUT_SC_TIME=out_sc_time, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='eSpec_lc',ANGLE=e_angle,RETRACE=1, $
                 T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB
     IF KEYWORD_SET(include_ions) THEN BEGIN
        GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                       NAME='JEi_up',ENERGY=energy_ions,ANGLE=i_angle_up,SC_POT=sc_pot,/CALIB
        GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                       NAME='Ji_up',ENERGY=energy_ions,ANGLE=i_angle_up,SC_POT=sc_pot,/CALIB, $
                       OUT_SC_POT=out_sc_pot_i, $
                       OUT_SC_TIME=out_sc_time_i, $
                       OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i
        GET_EN_SPEC,"fa_ies_c",UNITS='eflux',NAME='iSpec_up',ANGLE=i_angle_up,RETRACE=1, $
                    T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB
     ENDIF     

     ;;Now get 'em all, see what we gots
     GET_DATA,'JEe_lc',DATA=tmpjee_lc
     GET_DATA,'Je_lc',DATA=tmpje_lc
     GET_DATA,'eSpec_lc', DATA=tmpeSpec_lc
     IF KEYWORD_SET(include_ions) THEN BEGIN
        GET_DATA,'JEi_up',DATA=tmpjei_up
        GET_DATA,'Ji_up',DATA=tmpji_up
        GET_DATA,'iSpec_up', DATA=tmpiSpec_up
     ENDIF

     ;;Check for dupes and/or sort
     CHECK_DUPES,tmpjee_lc.x,HAS_DUPES=jee_has_dupes,OUT_UNIQ_I=jee_uniq_i,IS_SORTED=is_jee_sorted,/QUIET
     IF jee_has_dupes OR ~is_jee_sorted THEN BEGIN
        tmpjee_lc                                   = {x:tmpjee_lc.x[jee_uniq_i],y:tmpjee_lc.y[jee_uniq_i]}
     ENDIF
     CHECK_DUPES,tmpje_lc.x,HAS_DUPES=je_lc_has_dupes,OUT_UNIQ_I=je_lc_uniq_i,IS_SORTED=is_je_lc_sorted,/QUIET
     IF je_lc_has_dupes OR ~is_je_lc_sorted THEN BEGIN
        tmpje_lc                                 = {x:tmpje_lc.x[je_lc_uniq_i],y:tmpje_lc.y[je_lc_uniq_i]}
     ENDIF
     CHECK_DUPES,tmpeSpec_lc.x,HAS_DUPES=eSpec_has_dupes,OUT_UNIQ_I=eSpec_uniq_i,IS_SORTED=is_eSpec_sorted,/QUIET
     IF eSpec_has_dupes OR ~is_eSpec_sorted THEN BEGIN
        tmpeSpec_lc                                 = {x:tmpeSpec_lc.x[eSpec_uniq_i],y:tmpeSpec_lc.y[eSpec_uniq_i,*],v:tmpeSpec_lc.v[eSpec_uniq_i,*]}
     ENDIF

     ;;remove junk first--all have to be finite (i.e., not NANs and such)
     keep1                                       = WHERE(FINITE(tmpjee_lc.y))
     tmpjee_lc.x                                 = tmpjee_lc.x[keep1]
     tmpjee_lc.y                                 = tmpjee_lc.y[keep1]

     keep1                                       = WHERE(FINITE(tmpje_lc.y))
     tmpje_lc.x                                  = tmpje_lc.x[keep1]
     tmpje_lc.y                                  = tmpje_lc.y[keep1]
     out_sc_pot                                  = out_sc_pot[keep1]
     out_sc_time                                 = out_sc_time[keep1]
     out_sc_min_energy_ind                       = out_sc_min_energy_ind[keep1]

     keep1                                       = FINITE(tmpeSpec_lc.y)
     nTimes                                      = N_ELEMENTS(tmpeSpec_lc.y[*,0])
     nEnergies                                   = N_ELEMENTS(tmpeSpec_lc.y[0,*])
     keepRow                                     = MAKE_ARRAY(nTimes,/BYTE,VALUE=1)
     FOR i=0,N_ELEMENTS(tmpeSpec_lc.y[*,0])-1 DO BEGIN
        test                                     = WHERE(keep1[i,*],tCount)
        keepRow[i]                               = tCount EQ nEnergies ? 1 : 0
     ENDFOR
     tmpeSpec_lc.x                               = tmpeSpec_lc.x[WHERE(keepRow)]
     tmpeSpec_lc.y                               = tmpeSpec_lc.y[WHERE(keepRow),*]
     tmpeSpec_lc.v                               = tmpeSpec_lc.v[WHERE(keepRow),*]

     ;;Now check for zeroes
     keep2                                       = WHERE(ABS(tmpjee_lc.y) GT 0.0)
     jee_tmp_time                                = tmpjee_lc.x[keep2]
     jee_tmp_data                                = tmpjee_lc.y[keep2]

     keep2                                       = WHERE(ABS(tmpje_lc.y) GT 0.0)
     je_lc_tmp_time                              = tmpje_lc.x[keep2]
     je_lc_tmp_data                              = tmpje_lc.y[keep2]
     out_sc_pot                                  = out_sc_pot[keep2]
     out_sc_time                                 = out_sc_time[keep2]
     out_sc_min_energy_ind                       = out_sc_min_energy_ind[keep2]


     success = ALIGN_FLUX_EFLUX_AND_ESPEC(je_lc_tmp_time,je_lc_tmp_data, $
                                          jee_tmp_time,jee_tmp_data, $
                                          tmpeSpec_lc.x, $
                                          OUT_SC_POT=out_sc_pot, $
                                          OUT_SC_TIME=out_sc_time, $
                                          OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind, $
                                          ORBSTR=orbStr, $
                                          FLUXSTRARR=['Je_lc','JEe_lc','eSpec_lc'], $
                                          LOGFILE=badFile, $
                                          BATCH_MODE=batch_mode, $
                                          /QUIET)
     IF ~success THEN RETURN

     STORE_DATA,'JEe_lc',DATA={x:jee_tmp_time,y:jee_tmp_data}
     STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:je_lc_tmp_data}
     STORE_DATA,'eSpec_lc',DATA={x:tmpeSpec_lc.x,y:tmpeSpec_lc.y,v:tmpeSpec_lc.v}

     ;;Now get 'em and send 'em packing!
     GET_DATA,'JEe_lc',DATA=tmpjee_lc
     GET_DATA,'Je_lc',DATA=tmpje_lc
     GET_DATA,'eSpec_lc',DATA=tmpeSpec_lc
     ;;Because we need MLT
     GET_FA_ORBIT,tmpeSpec_lc.x,/TIME_ARRAY
     GET_DATA,'MLT',DATA=mlt
     mlt                                         = FLOAT(mlt.y)
     GET_DATA,'ILAT',DATA=ilat
     ilat                                        = FLOAT(ilat.y)

     IF KEYWORD_SET(include_ions) THEN BEGIN
        CHECK_DUPES,tmpjei_up.x,HAS_DUPES=jei_up_has_dupes,OUT_UNIQ_I=jei_up_uniq_i,IS_SORTED=is_jei_up_sorted,/QUIET
        IF jei_up_has_dupes OR ~is_jei_up_sorted THEN BEGIN
           tmpjei_up                                   = {x:tmpjei_up.x[jei_up_uniq_i],y:tmpjei_up.y[jei_up_uniq_i]}
        ENDIF
        CHECK_DUPES,tmpji_up.x,HAS_DUPES=ji_up_has_dupes,OUT_UNIQ_I=ji_up_uniq_i,IS_SORTED=is_ji_up_sorted,/QUIET
        IF ji_up_has_dupes OR ~is_ji_up_sorted THEN BEGIN
           tmpji_up                                 = {x:tmpji_up.x[ji_up_uniq_i],y:tmpji_up.y[ji_up_uniq_i]}
        ENDIF
        CHECK_DUPES,tmpiSpec_up.x,HAS_DUPES=iSpec_has_dupes,OUT_UNIQ_I=iSpec_uniq_i,IS_SORTED=is_iSpec_sorted,/QUIET
        IF iSpec_has_dupes OR ~is_iSpec_sorted THEN BEGIN
           tmpiSpec_up                                 = {x:tmpiSpec_up.x[iSpec_uniq_i],y:tmpiSpec_up.y[iSpec_uniq_i,*],v:tmpiSpec_up.v[iSpec_uniq_i,*]}
        ENDIF

     ;;remove junk first--all have to be finite (i.e., not NANs and such)
        keep1                                    = WHERE(FINITE(tmpjei_up.y))
        tmpjei_up.x                              = tmpjei_up.x[keep1]
        tmpjei_up.y                              = tmpjei_up.y[keep1]

        keep1                                    = WHERE(FINITE(tmpji_up.y))
        tmpji_up.x                               = tmpji_up.x[keep1]
        tmpji_up.y                               = tmpji_up.y[keep1]
        out_sc_pot_i                             = out_sc_pot_i[keep1]
        out_sc_time_i                            = out_sc_time_i[keep1]
        out_sc_min_energy_ind_i                  = out_sc_min_energy_ind_i[keep1]

        keep1                                    = FINITE(tmpiSpec_up.y)
        nTimes                                   = N_ELEMENTS(tmpiSpec_up.y[*,0])
        nEnergies                                = N_ELEMENTS(tmpiSpec_up.y[0,*])
        keepRow                                  = MAKE_ARRAY(nTimes,/BYTE,VALUE=1)
        FOR i=0,N_ELEMENTS(tmpiSpec_up.y[*,0])-1 DO BEGIN
           test                                  = WHERE(keep1[i,*],tCount)
           keepRow[i]                            = tCount EQ nEnergies ? 1 : 0
        ENDFOR
        tmpiSpec_up.x                               = tmpiSpec_up.x[WHERE(keepRow)]
        tmpiSpec_up.y                               = tmpiSpec_up.y[WHERE(keepRow),*]
        tmpiSpec_up.v                               = tmpiSpec_up.v[WHERE(keepRow),*]

        ;;Now check for zeroes
        keep2                                    = WHERE(ABS(tmpjei_up.y) GT 0.0)
        jei_up_tmp_time                          = tmpjei_up.x[keep2]
        jei_up_tmp_data                          = tmpjei_up.y[keep2]
        
        keep2                                    = WHERE(ABS(tmpji_up.y) GT 0.0)
        ji_up_tmp_time                           = tmpji_up.x[keep2]
        ji_up_tmp_data                           = tmpji_up.y[keep2]
        out_sc_pot_i                             = out_sc_pot_i[keep2]
        out_sc_time_i                            = out_sc_time_i[keep2]
        out_sc_min_energy_ind_i                  = out_sc_min_energy_ind_i[keep2]


        success = ALIGN_FLUX_EFLUX_AND_ESPEC(ji_up_tmp_time,ji_up_tmp_data, $
                                             jei_up_tmp_time,jei_up_tmp_data, $
                                             tmpiSpec_up.x, $
                                             OUT_SC_POT=out_sc_pot_i, $
                                             OUT_SC_TIME=out_sc_time_i, $
                                             OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                                             ORBSTR=orbStr, $
                                             FLUXSTRARR=['Ji_up','JEi_up','iSpec_up'], $
                                             LOGFILE=badFile, $
                                             BATCH_MODE=batch_mode, $
                                             /QUIET)
        
        STORE_DATA,'JEi_up',DATA={x:jei_up_tmp_time,y:jei_up_tmp_data}
        STORE_DATA,'Ji_up',DATA={x:ji_up_tmp_time,y:ji_up_tmp_data}
        STORE_DATA,'iSpec_up',DATA={x:tmpiSpec_up.x,y:tmpiSpec_up.y,v:tmpiSpec_up.v}
        
        ;;Now get 'em and send 'em packing!
        ;; GET_DATA,'JEi_up',DATA=tmpjei_up
        ;; GET_DATA,'Ji_up',DATA=tmpji_up
        ;; GET_DATA,'iSpec_up',DATA=tmpiSpec_up
        GET_DATA,'JEi_up',DATA=jei_up
        GET_DATA,'Ji_up',DATA=ji_up
        GET_DATA,'iSpec_up',DATA=iSpec_up
        ;;Because we need MLT
        ;; GET_FA_ORBIT,tmpiSpec_up.x,/TIME_ARRAY
        ;; GET_DATA,'MLT',DATA=mlt
        ;; mlt                                         = FLOAT(mlt.y)
        ;; GET_DATA,'ILAT',DATA=ilat
        ;; ilat                                        = FLOAT(ilat.y)
        
     ENDIF
     ;;Are we safe?
     ;; nJee                                        = N_ELEMENTS(jee_tmp_time)
     ;; nJe_lc                                      = N_ELEMENTS(je_lc_tmp_time)
     ;; nESpec                                      = N_ELEMENTS(tmpeSpec_lc.x)
     ;; IF ( nJee   NE nJe_lc ) OR $
     ;;    ( nJe_lc NE nESpec ) OR $
     ;;    ( nJe_lc NE nESpec ) $
     ;; THEN BEGIN
     ;;    all_bad                                  = 0 ;Not throwing in the towel yet
     ;;    WRITE_MESSAGE_TO_LOGFILE,badFile, $
     ;;                             STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'Unequal # of Je/Jee/eSpec inds'), $
     ;;                             /APPEND
     ;;    ;;We'll handle Jee first. eSpec is the gold standard
     ;;    tmpClosest                               = VALUE_CLOSEST(tmpeSpec_lc.x,jee_tmp_time,diffs,/QUIET,BATCH_MODE=batch_mode)
     ;;    keep                                     = WHERE(ABS(diffs) LT 0.05)
     ;;    IF keep[0] NE -1 THEN BEGIN
     ;;       jee_tmp_time                          = jee_tmp_time[keep]
     ;;       jee_tmp_data                          = jee_tmp_data[keep]
     ;;    ENDIF ELSE BEGIN
     ;;       WRITE_MESSAGE_TO_LOGFILE,badFile, $
     ;;                                STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No JEe inds within 0.05 s of eSpec'), $
     ;;                                /APPEND
     ;;       all_bad                               = 1
     ;;    ENDELSE

     ;;    ;;Now handle je_lc
     ;;    tmpClosest                               = VALUE_CLOSEST(tmpeSpec_lc.x,je_lc_tmp_time,diffs,/QUIET,BATCH_MODE=batch_mode)
     ;;    keep                                     = WHERE(ABS(diffs) LT 0.05)
     ;;    IF keep[0] NE -1 THEN BEGIN
     ;;       je_lc_tmp_time                        = je_lc_tmp_time[keep]
     ;;       je_lc_tmp_data                        = je_lc_tmp_data[keep]
     ;;       out_sc_pot                            = out_sc_pot[keep]
     ;;       out_sc_time                           = out_sc_time[keep]
     ;;       out_sc_min_energy_ind                 = out_sc_min_energy_ind[keep]
     ;;    ENDIF ELSE BEGIN
     ;;       WRITE_MESSAGE_TO_LOGFILE,badFile, $
     ;;                                STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No Je_lc inds within 0.05 s of eSpec'), $
     ;;                                /APPEND
     ;;       all_bad                               = 1
     ;;    ENDELSE

     ;;    IF KEYWORD_SET(all_bad) THEN RETURN
     ;; ENDIF

     ;;Now make 'em cry
     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                                             mlt,ilat, $
                                             eSpecs_parsed, $
                                             SC_POT=out_sc_pot, $
                                             /QUIET, $
                                             BATCH_MODE=batch_mode, $
                                             ORBSTR=orbStr, $
                                             ERRORLOGFILE=badFile
     

     IF KEYWORD_SET(include_ions) THEN BEGIN
        ;;Save the electron stuff
        PRINT,'Saving Newell file with ions: ' + out_newell_file
        SAVE,eSpecs_parsed,tmpeSpec_lc, $
             jei_up,ji_up,iSpec_up, $
             out_sc_pot,out_sc_time, $
             out_sc_pot_i,out_sc_time_i,out_sc_min_energy_ind_i, $
             FILENAME=outNewellDir+out_newell_file
     ENDIF ELSE BEGIN
        ;;Save the electron stuff
        PRINT,'Saving Newell file: ' + out_newell_file
        SAVE,eSpecs_parsed,tmpeSpec_lc, $
             out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
             FILENAME=outNewellDir+out_newell_file
     ENDELSE
     IF nMatch EQ 0 THEN CONTINUE ;Leave if there are no Alfvén events here

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now figure out Alfvéns
     ;;Reset the inds
     electron_startstop_alfven_inds              = !NULL
     keep_these_spectra_i                        = !NULL
     ;;get the relevant time range
     ;; n_tmp_times = N_ELEMENTS(je_tmp_time)
     n_tmp_times = N_ELEMENTS(eSpecs_parsed.x)
     FOR iMatch=0,nMatch-1 DO BEGIN
        tempCount                                = 0 ;assume there are no matches
        IF (matched[iMatch] NE 255) THEN CONTINUE ;Don't get the same one twice

        temp_inds                                = WHERE(eSpecs_parsed.x GE start_times[iMatch] AND eSpecs_parsed.x LE stop_times[iMatch],tempCount)
        IF tempCount GT 0 THEN BEGIN
           keep_these_spectra_i                  = [keep_these_spectra_i,temp_inds]
           hit_nSpectra                          = [hit_nSpectra,tempCount]
           matched[iMatch]                       = jjj
           nHits[iMatch]++
           alfvenDBindices_for_spectra           = [alfvenDBindices_for_spectra,REPLICATE(match_i[iMatch],tempCount)]
           orbInterval_for_spectra               = [orbInterval_for_spectra,REPLICATE(jjj,tempCount)]
        ENDIF ELSE BEGIN
           minstartdiff                          = MIN(ABS(eSpecs_parsed.x-start_times[iMatch]),temp_i_start)
           minstopdiff                           = MIN(ABS(eSpecs_parsed.x-stop_times[iMatch]),temp_i_stop)
           minDiff                               = MIN([minstartdiff,minstopdiff],temp_ii_min)
           IF minDiff LT temp_last_closest[iMatch] THEN BEGIN
              temp_last_closest[iMatch]          = minDiff

              IF temp_ii_min EQ 0 THEN BEGIN
                 temp_i                          = temp_i_start
                 minitime                        = minstartdiff
              ENDIF ELSE BEGIN
                 temp_i                          = temp_i_stop
                 minitime                        = minstopdiff
              ENDELSE
              ;; CASE temp_i OF
              ;;    0: je_sampPeriod                = (n_tmp_times GT 1) ? eSpecs_parsed.x[1]-eSpecs_parsed.x[0] : 2.5
              ;;    n_tmp_times-1: je_sampPeriod    = eSpecs_parsed.x[-1]-eSpecs_parsed.x[-2]
              ;;    ELSE: je_sampPeriod             = eSpecs_parsed.x[temp_i]-eSpecs_parsed.x[temp_i-1]
              ;; ENDCASE
              IF minitime LE 10. THEN BEGIN
                 tempCount                       = 1
                 keep_these_spectra_i            = [keep_these_spectra_i,temp_i]
                 hit_nSpectra                    = [hit_nSpectra,tempCount]
                 electron_startstop_alfven_inds  = [[electron_startstop_alfven_inds],[temp_i,temp_i]]
                 ;; electron_start_times         = [electron_start_times,eSpecs_parsed.x[temp_i]]
                 ;; electron_stop_times          = [electron_stop_times,eSpecs_parsed.x[temp_i]]
                 matched[iMatch]                 = 255-jjj-1
                 nHits[iMatch]++
                 alfvenDBindices_for_spectra     = [alfvenDBindices_for_spectra,match_i[iMatch]]
                 orbInterval_for_spectra         = [orbInterval_for_spectra,REPLICATE(jjj,tempCount)]
              ENDIF
           ENDIF;;  ELSE BEGIN
           ;;    PRINT,'Last match was better ...'
           ;; ENDELSE
        ENDELSE
        nSpectra        += tempCount
     ENDFOR
     tmp_n_Alfvens_this_interval = N_ELEMENTS(keep_these_spectra_i)
     IF tmp_n_Alfvens_this_interval EQ 0 THEN BEGIN
        PRINT,"No Alfven events here!"
        tmp_n_Alfvens_this_interval              = 0
     ENDIF ELSE BEGIN
        CAT_EVENTS_FROM_PARSED_SPECTRAL_STRUCT,alf_spectra,eSpecs_parsed,keep_these_spectra_i
     ENDELSE        

  ENDFOR

  ;;Now stitch together what we have
  IF nMatch GT 0 THEN BEGIN
     alf_eSpec                                      = MAKE_ELECTRON_SPECTRA_STRUCT_FOR_ALFVEN_EVENTS_V2(orb.y[0],nSpectra,alfvenDBindices_for_spectra, $
                                                                                                        center_times,matched,nHits,hit_nSpectra,alf_spectra)
     
     PRINT,'Saving Alfven electron spectra to ' + outFile + '...'
     SAVE,alf_eSpec,FILENAME=outDir+outFile
  ENDIF

  RETURN 
END
