;;2016/06/08

;;TO BE DONE: NEED TO FIGURE OUT WHAT WE'RE SAVING (2016/06/08, 07:43 EDT)

PRO ELECTRON_SPEC_IDENTIFICATION_V2__DOWNGOING_IONS, $
   SKIP_IF_FILE_EXISTS=skip_if_file_exists, $
   ENERGY_ELECTRONS=energy_electrons,ENERGY_IONS=energy_ions, $
   T1=t1,T2=t2, $
   BATCH_MODE=batch_mode;; , $
   ;; INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  todayStr                 = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  ;;For skipping the "get interval times" bit
  savesDir                 = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/eesa_time_intervals/'

  outNewellDir             = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/Newell_batch_output/'
  out_sc_pot_dir           = savesDir + 'just_potential/'
  outFile_pref             = 'Dartdb--Alfven--Newell_identification_of_electron_spectra--Orbit_'

  newellStuff_pref_sc_pot  = 'Newell_et_al_identification_of_electron_spectra--just_sc_pot--Orbit_'

  ;; IF KEYWORD_SET(include_ions) THEN BEGIN
     newellStuff_pref      = 'Newell_et_al_identification_of_electron_spectra--downgoing_ions_upgoing_electrons--Orbit_'
  ;; ENDIF ELSE BEGIN
  ;;    newellStuff_pref   = 'Newell_et_al_identification_of_electron_spectra--Orbit_'
  ;; ENDELSE
  ;; noEventsFile          = 'Orbs_without_Alfven_events--'+todayStr+'.txt'
  badFile                  = 'Orbs_with_other_issues--'+todayStr+'.txt'

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons      = [0.,30000.]                           ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF
  IF NOT KEYWORD_SET(energy_ions) THEN BEGIN
     energy_ions           = [0.,500.]     ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                   = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                      = GET_FA_EES(0.0D, EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
    RETURN
  ENDIF ELSE BEGIN
     n_EESA_spectra        = dat.index+1
     last_index            = LONG(dat.index)
  
     PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t2                       = 0.0D
  temp                     = GET_FA_EES(t2,INDEX=0.0D)
  t1                       = t2
  temp                     = GET_FA_EES(t2,/ADV)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First, see that we are able to match all points in this orb
  GET_FA_ORBIT,t1,t2
  ;;now get orbit quantities
  GET_DATA,'ORBIT',DATA=orb
  orbit_num                = orb.y[0]
  orbStr                   = STRCOMPRESS(orbit_num,/REMOVE_ALL)

  this                     = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                                          /USE_DUPELESS_FILES, $
                                                          JE_OUT=je, $
                                                          NINTERVALS_OUT=number_of_intervals, $
                                                          TIME_RANGE_INDICES_OUT=time_range_indices, $
                                                          TIME_RANGES_OUT=time_ranges)

  IF ~this THEN BEGIN
     PRINT,"Couldn't load eesa tInterval stuff for orbit " + orbStr + "!!"
     RETURN
  ENDIF

  STORE_DATA,'Je',DATA=je

  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN

     ;;We're going to make output in any case. We're already here, after all!
     out_newell_file                             = newellStuff_pref + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     IF KEYWORD_SET(skip_if_file_exists) AND FILE_TEST(outNewellDir+out_newell_file) THEN BEGIN
        PRINT,'Skipping ' + out_newell_file + '...'
        alfven_skip_this_orb                     = 1
        CONTINUE
     ENDIF

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
        ;; e_angle                                  = [180.-lcw,180+lcw] ; for Southern Hemis.
        e_angle_up                               = [270.,90.]            ; for Southern Hemis.
        e_angle_up_lc                            = [360.-lcw,lcw]        ; for Southern Hemis.
        ;;i_angle=[270.0,90.0]	
        ;;elimnate ram from data
        ;; i_angle                                  = [180.0,360.0]
        ;; i_angle_up                               = [270.0,360.0]
        i_angle_down                             = [180.0    ,270.     ]
        i_angle_down_lc                          = [180.0    ,180.0+lcw]
        i_angle_down_lc_ram                      = [180.0-lcw,180.0+lcw]
        
     endif else begin
        ;; e_angle                                  = [360.-lcw,lcw]     ;	for Northern Hemis.
        e_angle_up                               = [90.0,270.0]          ;	for Northern Hemis.
        e_angle_up_lc                            = [180.-lcw,180.+lcw]  ;	for Northern Hemis.
        ;;i_angle=[90.,270.0]
        ;;eliminate ram from data
        ;; i_angle                                  = [0.0,180.0]
        ;; i_angle_up                               = [90.0,180.0]
        i_angle_down                             = [0.,90.]
        i_angle_down_lc                          = [0.,lcw]
        i_angle_down_ram                         = [0.-lcw,lcw]
        
     endelse
     
     ;;get fields mode
     fields_mode=GET_FA_FIELDS('DataHdr_1032',time_ranges[jjj,0],time_ranges[jjj,1])
     
     out_newell_file_sc_pot                   = newellStuff_pref_sc_pot + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     IF FILE_TEST(out_sc_pot_dir+out_newell_file_sc_pot) THEN BEGIN
        PRINT,"Restoring S/C pot file: " + out_newell_file_sc_pot
        RESTORE,out_sc_pot_dir+out_newell_file_sc_pot
        IF N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN
           get_potential = 1
        ENDIF ELSE BEGIN
           get_potential = 0
        ENDELSE
     ENDIF ELSE BEGIN
        get_potential = 1
     ENDELSE

     IF KEYWORD_SET(get_potential) THEN BEGIN
        GET_SC_POTENTIAL,T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],DATA=sc_pot
        PRINT,'Saving potential to ' + out_newell_file_sc_pot
        SAVE,sc_pot,FILENAME=out_sc_pot_dir+out_newell_file_sc_pot
     ENDIF

     ;;get moments/integrals of various fluxes
     GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEe_up',ANGLE=e_angle_up,ENERGY=energy_electrons,SC_POT=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Je_up',ENERGY=energy_electrons,ANGLE=e_angle_up,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot, $
                    OUT_SC_TIME=out_sc_time, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='eSpec_up',ANGLE=e_angle_up, $ ;RETRACE=1, $
                 T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

     GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEe_up_lc',ANGLE=e_angle_up_lc,ENERGY=energy_electrons,SC_POT=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Je_up_lc',ENERGY=energy_electrons,ANGLE=e_angle_up_lc,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot, $
                    OUT_SC_TIME=out_sc_time, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='eSpec_up_lc',ANGLE=e_angle_up_lc, $ ;RETRACE=1, $
                 T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

     ;; IF KEYWORD_SET(include_ions) THEN BEGIN
        GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                       NAME='JEi_down',ENERGY=energy_ions,ANGLE=i_angle_down,SC_POT=sc_pot,/CALIB
        GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                       NAME='Ji_down',ENERGY=energy_ions,ANGLE=i_angle_down,SC_POT=sc_pot,/CALIB, $
                       OUT_SC_POT=out_sc_pot_i, $
                       OUT_SC_TIME=out_sc_time_i, $
                       OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i
        GET_EN_SPEC,"fa_ies_c",UNITS='eflux',NAME='iSpec_down',ANGLE=i_angle_down,RETRACE=1, $
                    T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

        ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
        ;;                NAME='JEi_down_lc',ENERGY=energy_ions,ANGLE=i_angle_down_lc,SC_POT=sc_pot,/CALIB
        ;; GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
        ;;                NAME='Ji_down_lc',ENERGY=energy_ions,ANGLE=i_angle_down_lc,SC_POT=sc_pot,/CALIB, $
        ;;                OUT_SC_POT=out_sc_pot_i, $
        ;;                OUT_SC_TIME=out_sc_time_i, $
        ;;                OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i
        ;; GET_EN_SPEC,"fa_ies_c",UNITS='eflux',NAME='iSpec_down_lc',ANGLE=i_angle_down_lc,RETRACE=1, $
        ;;             T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

        ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
        ;;                NAME='JEi_down_lc_ram',ENERGY=energy_ions,ANGLE=i_angle_down_lc_ram,SC_POT=sc_pot,/CALIB
        ;; GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
        ;;                NAME='Ji_down_lc_ram',ENERGY=energy_ions,ANGLE=i_angle_down_lc_ram,SC_POT=sc_pot,/CALIB, $
        ;;                OUT_SC_POT=out_sc_pot_i, $
        ;;                OUT_SC_TIME=out_sc_time_i, $
        ;;                OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i
        ;; GET_EN_SPEC,"fa_ies_c",UNITS='eflux',NAME='iSpec_down_lc_ram',ANGLE=i_angle_down_lc_ram,RETRACE=1, $
        ;;             T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB
     ;; ENDIF     

     ;;Now get 'em all, see what we gots
     GET_DATA,'JEe_up',DATA=tmpjee_up
     GET_DATA,'Je_up',DATA=tmpje_up
     GET_DATA,'eSpec_up', DATA=tmpeSpec_up

     ;; GET_DATA,'JEe_up_lc',DATA=tmpjee_up_lc
     ;; GET_DATA,'Je_up_lc',DATA=tmpje_up_lc
     ;; GET_DATA,'eSpec_up_lc', DATA=tmpeSpec_up_lc

     ;; IF KEYWORD_SET(include_ions) THEN BEGIN
        GET_DATA,'JEi_down',DATA=tmpjei_down
        GET_DATA,'Ji_down',DATA=tmpji_down
        GET_DATA,'iSpec_down', DATA=tmpiSpec_down

        ;; GET_DATA,'JEi_down_lc',DATA=tmpjei_down_lc
        ;; GET_DATA,'Ji_down_lc',DATA=tmpji_down_lc
        ;; GET_DATA,'iSpec_down_lc', DATA=tmpiSpec_down_lc

        ;; GET_DATA,'JEi_down_lc_ram',DATA=tmpjei_down_lc_ram
        ;; GET_DATA,'Ji_down_lc_ram',DATA=tmpji_down_lc_ram
        ;; GET_DATA,'iSpec_down_lc_ram', DATA=tmpiSpec_down_lc_ram
     ;; ENDIF

     ;;Check for dupes and/or sort
     CHECK_DUPES,tmpjee_up.x,HAS_DUPES=jee_has_dupes,OUT_UNIQ_I=jee_uniq_i,IS_SORTED=is_jee_sorted,/QUIET
     IF jee_has_dupes OR ~is_jee_sorted THEN BEGIN
        tmpjee_up                                   = {x:tmpjee_up.x[jee_uniq_i],y:tmpjee_up.y[jee_uniq_i]}
     ENDIF
     CHECK_DUPES,tmpje_up.x,HAS_DUPES=je_up_has_dupes,OUT_UNIQ_I=je_up_uniq_i,IS_SORTED=is_je_up_sorted,/QUIET
     IF je_up_has_dupes OR ~is_je_up_sorted THEN BEGIN
        tmpje_up                                 = {x:tmpje_up.x[je_up_uniq_i],y:tmpje_up.y[je_up_uniq_i]}
     ENDIF
     CHECK_DUPES,tmpeSpec_up.x,HAS_DUPES=eSpec_has_dupes,OUT_UNIQ_I=eSpec_uniq_i,IS_SORTED=is_eSpec_sorted,/QUIET
     IF eSpec_has_dupes OR ~is_eSpec_sorted THEN BEGIN
        tmpeSpec_up                                 = {x:tmpeSpec_up.x[eSpec_uniq_i],y:tmpeSpec_up.y[eSpec_uniq_i,*],v:tmpeSpec_up.v[eSpec_uniq_i,*]}
     ENDIF

     ;;remove junk first--all have to be finite (i.e., not NANs and such)
     keep1                                       = WHERE(FINITE(tmpjee_up.y))
     tmpjee_up.x                                 = tmpjee_up.x[keep1]
     tmpjee_up.y                                 = tmpjee_up.y[keep1]

     keep1                                       = WHERE(FINITE(tmpje_up.y))
     tmpje_up.x                                  = tmpje_up.x[keep1]
     tmpje_up.y                                  = tmpje_up.y[keep1]
     out_sc_pot                                  = out_sc_pot[keep1]
     out_sc_time                                 = out_sc_time[keep1]
     out_sc_min_energy_ind                       = out_sc_min_energy_ind[keep1]

     keep1                                       = FINITE(tmpeSpec_up.y)
     nTimes                                      = N_ELEMENTS(tmpeSpec_up.y[*,0])
     nEnergies                                   = N_ELEMENTS(tmpeSpec_up.y[0,*])
     keepRow                                     = MAKE_ARRAY(nTimes,/BYTE,VALUE=1)
     FOR i=0,N_ELEMENTS(tmpeSpec_up.y[*,0])-1 DO BEGIN
        test                                     = WHERE(keep1[i,*],tCount)
        keepRow[i]                               = tCount EQ nEnergies ? 1 : 0
     ENDFOR
     tmpeSpec_up.x                               = tmpeSpec_up.x[WHERE(keepRow)]
     tmpeSpec_up.y                               = tmpeSpec_up.y[WHERE(keepRow),*]
     tmpeSpec_up.v                               = tmpeSpec_up.v[WHERE(keepRow),*]

     ;;Now check for zeroes
     keep2                                       = WHERE(ABS(tmpjee_up.y) GT 0.0)
     jee_tmp_time                                = tmpjee_up.x[keep2]
     jee_tmp_data                                = tmpjee_up.y[keep2]

     keep2                                       = WHERE(ABS(tmpje_up.y) GT 0.0)
     je_up_tmp_time                              = tmpje_up.x[keep2]
     je_up_tmp_data                              = tmpje_up.y[keep2]
     out_sc_pot                                  = out_sc_pot[keep2]
     out_sc_time                                 = out_sc_time[keep2]
     out_sc_min_energy_ind                       = out_sc_min_energy_ind[keep2]


     success = ALIGN_FLUX_EFLUX_AND_ESPEC(je_up_tmp_time,je_up_tmp_data, $
                                          jee_tmp_time,jee_tmp_data, $
                                          tmpeSpec_up.x, $
                                          OUT_SC_POT=out_sc_pot, $
                                          OUT_SC_TIME=out_sc_time, $
                                          OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind, $
                                          ORBSTR=orbStr, $
                                          FLUXSTRARR=['Je_up','JEe_up','eSpec_up'], $
                                          LOGFILE=badFile, $
                                          BATCH_MODE=batch_mode, $
                                          /QUIET)
     ;; IF ~success THEN RETURN

     STORE_DATA,'JEe_up',DATA={x:jee_tmp_time,y:jee_tmp_data}
     STORE_DATA,'Je_up',DATA={x:je_up_tmp_time,y:je_up_tmp_data}
     STORE_DATA,'eSpec_up',DATA={x:tmpeSpec_up.x,y:tmpeSpec_up.y,v:tmpeSpec_up.v}

     ;;Now get 'em and send 'em packing!
     GET_DATA,'JEe_up',DATA=tmpjee_up
     GET_DATA,'Je_up',DATA=tmpje_up
     GET_DATA,'eSpec_up',DATA=tmpeSpec_up
     ;;Because we need MLT
     GET_FA_ORBIT,tmpeSpec_up.x,/TIME_ARRAY
     GET_DATA,'MLT',DATA=mlt
     mlt                                         = FLOAT(mlt.y)
     GET_DATA,'ILAT',DATA=ilat
     ilat                                        = FLOAT(ilat.y)
     GET_DATA,'ALT',DATA=alt
     alt                                         = FLOAT(alt.y)
     GET_DATA,'ORBIT',DATA=orbit
     orbit                                       = FLOAT(orbit.y)

     ;; IF KEYWORD_SET(include_ions) THEN BEGIN
        CHECK_DUPES,tmpjei_down.x,HAS_DUPES=jei_down_has_dupes,OUT_UNIQ_I=jei_down_uniq_i,IS_SORTED=is_jei_down_sorted,/QUIET
        IF jei_down_has_dupes OR ~is_jei_down_sorted THEN BEGIN
           tmpjei_down                                   = {x:tmpjei_down.x[jei_down_uniq_i],y:tmpjei_down.y[jei_down_uniq_i]}
        ENDIF
        CHECK_DUPES,tmpji_down.x,HAS_DUPES=ji_down_has_dupes,OUT_UNIQ_I=ji_down_uniq_i,IS_SORTED=is_ji_down_sorted,/QUIET
        IF ji_down_has_dupes OR ~is_ji_down_sorted THEN BEGIN
           tmpji_down                                 = {x:tmpji_down.x[ji_down_uniq_i],y:tmpji_down.y[ji_down_uniq_i]}
        ENDIF
        CHECK_DUPES,tmpiSpec_down.x,HAS_DUPES=iSpec_has_dupes,OUT_UNIQ_I=iSpec_uniq_i,IS_SORTED=is_iSpec_sorted,/QUIET
        IF iSpec_has_dupes OR ~is_iSpec_sorted THEN BEGIN
           tmpiSpec_down                                 = {x:tmpiSpec_down.x[iSpec_uniq_i],y:tmpiSpec_down.y[iSpec_uniq_i,*],v:tmpiSpec_down.v[iSpec_uniq_i,*]}
        ENDIF

     ;;remove junk first--all have to be finite (i.e., not NANs and such)
        keep1                                    = WHERE(FINITE(tmpjei_down.y))
        tmpjei_down.x                              = tmpjei_down.x[keep1]
        tmpjei_down.y                              = tmpjei_down.y[keep1]

        keep1                                    = WHERE(FINITE(tmpji_down.y))
        tmpji_down.x                               = tmpji_down.x[keep1]
        tmpji_down.y                               = tmpji_down.y[keep1]
        out_sc_pot_i                             = out_sc_pot_i[keep1]
        out_sc_time_i                            = out_sc_time_i[keep1]
        out_sc_min_energy_ind_i                  = out_sc_min_energy_ind_i[keep1]

        keep1                                    = FINITE(tmpiSpec_down.y)
        nTimes                                   = N_ELEMENTS(tmpiSpec_down.y[*,0])
        nEnergies                                = N_ELEMENTS(tmpiSpec_down.y[0,*])
        keepRow                                  = MAKE_ARRAY(nTimes,/BYTE,VALUE=1)
        FOR i=0,N_ELEMENTS(tmpiSpec_down.y[*,0])-1 DO BEGIN
           test                                  = WHERE(keep1[i,*],tCount)
           keepRow[i]                            = tCount EQ nEnergies ? 1 : 0
        ENDFOR
        tmpiSpec_down.x                               = tmpiSpec_down.x[WHERE(keepRow)]
        tmpiSpec_down.y                               = tmpiSpec_down.y[WHERE(keepRow),*]
        tmpiSpec_down.v                               = tmpiSpec_down.v[WHERE(keepRow),*]

        ;;Now check for zeroes
        keep2                                    = WHERE(ABS(tmpjei_down.y) GT 0.0)
        jei_down_tmp_time                          = tmpjei_down.x[keep2]
        jei_down_tmp_data                          = tmpjei_down.y[keep2]
        
        keep2                                    = WHERE(ABS(tmpji_down.y) GT 0.0)
        ji_down_tmp_time                           = tmpji_down.x[keep2]
        ji_down_tmp_data                           = tmpji_down.y[keep2]
        out_sc_pot_i                             = out_sc_pot_i[keep2]
        out_sc_time_i                            = out_sc_time_i[keep2]
        out_sc_min_energy_ind_i                  = out_sc_min_energy_ind_i[keep2]


        success = ALIGN_FLUX_EFLUX_AND_ESPEC(ji_down_tmp_time,ji_down_tmp_data, $
                                             jei_down_tmp_time,jei_down_tmp_data, $
                                             tmpiSpec_down.x, $
                                             OUT_SC_POT=out_sc_pot_i, $
                                             OUT_SC_TIME=out_sc_time_i, $
                                             OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                                             ORBSTR=orbStr, $
                                             FLUXSTRARR=['Ji_down','JEi_down','iSpec_down'], $
                                             LOGFILE=badFile, $
                                             BATCH_MODE=batch_mode, $
                                             /QUIET)
        
        STORE_DATA,'JEi_down',DATA={x:jei_down_tmp_time,y:jei_down_tmp_data}
        STORE_DATA,'Ji_down',DATA={x:ji_down_tmp_time,y:ji_down_tmp_data}
        STORE_DATA,'iSpec_down',DATA={x:tmpiSpec_down.x,y:tmpiSpec_down.y,v:tmpiSpec_down.v}
        
        ;;Now get 'em and send 'em packing!
        ;; GET_DATA,'JEi_down',DATA=tmpjei_down
        ;; GET_DATA,'Ji_down',DATA=tmpji_down
        ;; GET_DATA,'iSpec_down',DATA=tmpiSpec_down
        GET_DATA,'JEi_down',DATA=jei_down
        GET_DATA,'Ji_down',DATA=ji_down
        GET_DATA,'iSpec_down',DATA=iSpec_down
        ;;Because we need MLT
        ;; GET_FA_ORBIT,tmpiSpec_down.x,/TIME_ARRAY
        ;; GET_DATA,'MLT',DATA=mlt
        ;; mlt                                         = FLOAT(mlt.y)
        ;; GET_DATA,'ILAT',DATA=ilat
        ;; ilat                                        = FLOAT(ilat.y)
        
     ;; ENDIF

     ;;Now make 'em cry
     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_up,tmpjee_up,tmpje_up, $
                                             mlt,ilat,alt,orbit, $
                                             eSpecs_parsed, $
                                             SC_POT=out_sc_pot, $ ;The reason for no negative is that the sign gets flipped get_2d_ts_pot
                                             /QUIET, $
                                             BATCH_MODE=batch_mode, $
                                             ORBSTR=orbStr, $
                                             ERRORLOGFILE=badFile
     

     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,iSpec_down,jei_down,ji_down, $
                                             mlt,ilat,alt,orbit, $
                                             iSpecs_parsed, $
                                             SC_POT=(-1.)*out_sc_pot, $ ;The reason for the negative is that that's what we actually get from V8_S
                                             /QUIET, $
                                             BATCH_MODE=batch_mode, $
                                             ORBSTR=orbStr, $
                                             ERRORLOGFILE=badFile

     ;; IF KEYWORD_SET(include_ions) THEN BEGIN
        ;;Save the electron stuff
        PRINT,'Saving Newell file with ions: ' + out_newell_file
        SAVE,eSpecs_parsed,tmpeSpec_up,tmpjee_up,tmpje_up, $
             out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
             iSpecs_parsed,iSpec_down,jei_down,ji_down, $
             out_sc_pot_i,out_sc_time_i,out_sc_min_energy_ind_i, $
             FILENAME=outNewellDir+out_newell_file
     ;; ENDIF ELSE BEGIN
     ;;    ;;Save the electron stuff
     ;;    PRINT,'Saving Newell file: ' + out_newell_file
     ;;    SAVE,eSpecs_parsed,tmpeSpec_up,tmpjee_up,tmpje_up, $
     ;;         out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
     ;;         FILENAME=outNewellDir+out_newell_file
     ;; ENDELSE
  ENDFOR

  RETURN 
END
