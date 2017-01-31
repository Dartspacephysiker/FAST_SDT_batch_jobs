;;2017/01/31
;;The only reason I (DOWNGOING_IONS__V1__GET_ION_TIME_SERIES, that is) exist is that Spence is too dumb to have remembered to save the
;;time series

PRO DOWNGOING_IONS__V1__GET_ION_TIME_SERIES, $
   SKIP_IF_FILE_EXISTS=skip_if_file_exists, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   T1=t1,T2=t2, $
   BATCH_MODE=batch_mode;; , $
   ;; INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  as5_saveDir              = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  Newell_dir               = '/SPENCEdata/software/sdt/batch_jobs/do_the_Newell_2009/'
  Newell_saveDir           = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/'

  todayStr                 = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  ;;For skipping the "get interval times" bit
  ;; indDir                = as5_dir + 'je_time_ind_dir/'
  ;; indFilePref           = "je_and_cleaned_time_range_indices--orbit_"
  ;; intervalArrFile       = "orb_and_num_intervals--0-16361.sav" ;;Use it to figure out which file to restore

  outNewellDir             = Newell_saveDir + 'downgoing_ions__v1_output/'
  out_sc_pot_dir           = as5_saveDir + 'just_potential/'
  outFile_pref             = 'downgoing_ions__v1__time--orbit_'

  newellStuff_pref_sc_pot  = 'Newell_et_al_identification_of_electron_spectra--just_sc_pot--Orbit_'

  ;; badFile                  = 'downgoing_ions__v1__time--orbs_with_issues--'+todayStr+'.txt'

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons            = [0.,30000.]                           ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF
  IF NOT KEYWORD_SET(energy_ions) THEN BEGIN
     energy_ions_high            = [300,2.4e4]
  ENDIF

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                                 = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                                    = GET_FA_IES(0.0D,EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST ion survey data -- GET_FA_IES(0.0, EN=1) returned invalid data'
    RETURN
  ENDIF ELSE BEGIN
     n_IESA_spectra                      = dat.index+1
     last_index                          = LONG(dat.index)
  
     PRINT,'There are ' + STRCOMPRESS(n_IESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t2                                     = 0.0D
  temp                                   = GET_FA_IES(t2,INDEX=0.0D)
  t1                                     = t2
  temp                                   = GET_FA_IES(t2,/ADV)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First, see that we are able to match all points in this orb
  ;; RESTORE,intervalArrFile 

  GET_FA_ORBIT,t1,t2
  ;;now get orbit quantities
  GET_DATA,'ORBIT',DATA=orb
  orbit_num                              = orb.y[0]
  orbStr                                 = STRCOMPRESS(orbit_num,/REMOVE_ALL)

  ;;The loader gives us je,orbit_num,time_range_indices, and time_range
  ;; PRINT,'Restoring indFile ' + indFile + ' ...'
  ;; RESTORE,indDir+indFile
  this = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                      /USE_DUPELESS_FILES, $
                                      JE_OUT=je, $
                                      NINTERVALS_OUT=number_of_intervals, $
                                      TIME_RANGES_OUT=time_ranges, $
                                      TIME_RANGE_INDICES_OUT=time_range_indices)
  IF this EQ -1 THEN BEGIN
     PRINT,"Can't get ESA time ranges/time range indices for orbit " + orbStr + '! Returning ...'
     RETURN
  ENDIF
  PRINT,'orb, nIntervals: ',orbit_num,number_of_intervals

  STORE_DATA,'Je',DATA=je

  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN

     ;;We're going to make output in any case. We're already here, after all!
     out_newell_file                             = outFile_pref + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     IF KEYWORD_SET(skip_if_file_exists) AND FILE_TEST(outNewellDir+out_newell_file) THEN BEGIN
        PRINT,'Skipping ' + out_newell_file + '...'
        alfven_skip_this_orb                     = 1
        CONTINUE
     ENDIF

     PRINT,'time_range: ',time_to_str(time_ranges[jjj,0]),' - ',time_to_str(time_ranges[jjj,1])
     
     je_tmp_time                                 = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data                                 = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]

     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}

     ;;get_orbit data
     GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
     
     ;;define loss cone angle
     GET_DATA,'ALT',DATA=alt
     loss_cone_alt           = alt.y[0]*1000.0
     lcw                     = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
     GET_DATA,'ILAT',DATA=ilat
     north_south             = ABS(ilat.y[0])/ilat.y[0]
     
     if north_south EQ -1 then begin
        ;; e_angle           = [180.-lcw,180+lcw] ; for Southern Hemis.
        ;; e_angle_up        = [270.,90.]            ; for Southern Hemis.
        ;; e_angle_up_lc     = [360.-lcw,lcw]        ; for Southern Hemis.
        ;;i_angle=[270.0,90.0]	
        ;;elimnate ram from data
        ;; i_angle           = [180.0,360.0]
        ;; i_angle_up        = [270.0,360.0]
        i_angle_down         = [180.0    ,270.     ]
        i_angle_down_lc      = [180.0    ,180.0+lcw]
        i_angle_down_lc_ram  = [180.0-lcw,180.0+lcw]
        
     endif else begin
        ;; e_angle           = [360.-lcw,lcw]     ;	for Northern Hemis.
        ;; e_angle_up        = [90.0,270.0]          ;	for Northern Hemis.
        ;; e_angle_up_lc     = [180.-lcw,180.+lcw]  ;	for Northern Hemis.
        ;;i_angle=[90.,270.0]
        ;;eliminate ram from data
        ;; i_angle           = [0.0,180.0]
        ;; i_angle_up        = [90.0,180.0]
        i_angle_down         = [0.,90.]
        i_angle_down_lc      = [0.,lcw]
        i_angle_down_lc_ram  = [0.-lcw,lcw]
        
     endelse
     
     ;;get fields mode
     ;; fields_mode             = GET_FA_FIELDS('DataHdr_1032',time_ranges[jjj,0],time_ranges[jjj,1])
     
     ;; out_newell_file_sc_pot  = newellStuff_pref_sc_pot + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     ;; IF FILE_TEST(out_sc_pot_dir+out_newell_file_sc_pot) THEN BEGIN
     ;;    PRINT,"Restoring S/C pot file: " + out_newell_file_sc_pot
     ;;    RESTORE,out_sc_pot_dir+out_newell_file_sc_pot
     ;;    IF N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN
     ;;       get_potential = 1
     ;;    ENDIF ELSE BEGIN
     ;;       get_potential = 0
     ;;    ENDELSE
     ;; ENDIF ELSE BEGIN
     ;;    get_potential = 1
     ;; ENDELSE

     ;; IF KEYWORD_SET(get_potential) THEN BEGIN
     ;;    GET_SC_POTENTIAL,T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],DATA=sc_pot
     ;;    PRINT,'Saving potential to ' + out_newell_file_sc_pot
     ;;    SAVE,sc_pot,FILENAME=out_sc_pot_dir+out_newell_file_sc_pot
     ;; ENDIF

     ;;get moments/integrals of various fluxes
     ;; GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     ;;                NAME='JEe_up',ANGLE=e_angle_up,ENERGY=energy_electrons,SC_POT=sc_pot
     ;; GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     ;;                NAME='Je_up',ENERGY=energy_electrons,ANGLE=e_angle_up,SC_POT=sc_pot, $
     ;;                OUT_SC_POT=out_sc_pot, $
     ;;                OUT_SC_TIME=out_sc_time, $
     ;;                OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     ;; GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='eSpec_up',ANGLE=e_angle_up,RETRACE=1, $
     ;;             T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

     ;; GET_2DT_TS_POT,'je_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     ;;                NAME='JEe_up_lc',ANGLE=e_angle_up_lc,ENERGY=energy_electrons,SC_POT=sc_pot
     ;; GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     ;;                NAME='Je_up_lc',ENERGY=energy_electrons,ANGLE=e_angle_up_lc,SC_POT=sc_pot, $
     ;;                OUT_SC_POT=out_sc_pot, $
     ;;                OUT_SC_TIME=out_sc_time, $
     ;;                OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     ;; GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='eSpec_up_lc', $
     ;;             ANGLE=e_angle_up_lc,RETRACE=1, $
     ;;             T1=time_ranges[jjj,0],T2=time_ranges[jjj,1],/CALIB

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;High energy
     ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='JEi_down_highE',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down, $
                ;; SC_POT=sc_pot, $
                /CALIB

     GET_DATA,'JEi_down_highE',DATA=tmpJEi_down_highE
     ;;uniq 'em
     uniq_i                    = UNIQ(tmpJEi_down_highE.x,SORT(tmpJEi_down_highE.x))
     tmpJEi_down_highE         = {x:tmpJEi_down_highE.x[uniq_i],y:tmpJEi_down_highE.y[uniq_i]}

     x = (TEMPORARY(tmpJEi_down_highE)).x

     PRINT,'Saving downgoing_ion time: ' + out_newell_file
     SAVE,x,FILENAME=outNewellDir+out_newell_file
  ENDFOR

  RETURN 
END

