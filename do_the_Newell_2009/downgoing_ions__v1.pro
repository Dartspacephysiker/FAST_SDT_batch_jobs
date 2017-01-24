;;2017/01/24

;;TO BE DONE: NEED TO FIGURE OUT WHAT WE'RE SAVING (2016/06/08, 07:43 EDT)

PRO DOWNGOING_IONS__V1, $
   SKIP_IF_FILE_EXISTS=skip_if_file_exists, $
   ENERGY_ELECTRONS=energy_electrons,ENERGY_IONS=energy_ions, $
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
  outFile_pref             = 'downgoing_ions__v1--orbit_'

  newellStuff_pref_sc_pot  = 'Newell_et_al_identification_of_electron_spectra--just_sc_pot--Orbit_'

  badFile                  = 'downgoing_ions__v1--orbs_with_issues--'+todayStr+'.txt'

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons            = [0.,30000.]                           ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF
  IF NOT KEYWORD_SET(energy_ions) THEN BEGIN
     energy_ions_low             = [0.,300.]     ;use 3.0 for lower bound since the sc_pot is used to set this
     energy_ions_high            = [300,2.4e4]
  ENDIF

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                                 = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                                    = GET_FA_EES(0.0D,EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
    RETURN
  ENDIF ELSE BEGIN
     n_EESA_spectra                      = dat.index+1
     last_index                          = LONG(dat.index)
  
     PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t2                                     = 0.0D
  temp                                   = GET_FA_EES(t2,INDEX=0.0D)
  t1                                     = t2
  temp                                   = GET_FA_EES(t2,/ADV)

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
     fields_mode             = GET_FA_FIELDS('DataHdr_1032',time_ranges[jjj,0],time_ranges[jjj,1])
     
     out_newell_file_sc_pot  = newellStuff_pref_sc_pot + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

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
     ;;Low energy
     GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEi_down_lowE',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down, $
                    SC_POT=sc_pot, $
                    /CALIB

     GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Ji_down_lowE',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down, $
                    SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot_i, $
                    OUT_SC_TIME=out_sc_time_i, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                    /CALIB


     ;;Low energy--loss cone
     GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEi_down_lowE_lc',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down_lc, $
                    SC_POT=sc_pot, $
                    /CALIB

     GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Ji_down_lowE_lc',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down_lc,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot_i, $
                    OUT_SC_TIME=out_sc_time_i, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                    /CALIB

     ;;Low energy--loss cone + ram
     GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='JEi_down_lowE_lc_ram',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down_lc_ram,SC_POT=sc_pot,/CALIB

     GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                    NAME='Ji_down_lowE_lc_ram',ENERGY=energy_ions_low, $
                    ANGLE=i_angle_down_lc_ram, $
                    SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot_i, $
                    OUT_SC_TIME=out_sc_time_i, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                    /CALIB


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;High energy
     ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='JEi_down_highE',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down, $
                ;; SC_POT=sc_pot, $
                /CALIB

     ;; GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='Ji_down_highE',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down, $
                ;; SC_POT=sc_pot, $
                ;; OUT_SC_POT=out_sc_pot_i, $
                ;; OUT_SC_TIME=out_sc_time_i, $
                ;; OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                /CALIB

     ;;High energy--loss cone
     ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='JEi_down_highE_lc',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down_lc, $
                ;; SC_POT=sc_pot, $
                /CALIB

     ;; GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='Ji_down_highE_lc',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down_lc, $
                ;; SC_POT=sc_pot, $
                ;; OUT_SC_POT=out_sc_pot_i, $
                ;; OUT_SC_TIME=out_sc_time_i, $
                ;; OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                /CALIB

     ;;High energy--loss cone + ram
     ;; GET_2DT_TS_POT,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'je_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='JEi_down_highE_lc_ram',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down_lc_ram, $
                ;; SC_POT=sc_pot, $
                /CALIB

     ;; GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_2DT_TS,'j_2d_b','fa_ies',T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                NAME='Ji_down_highE_lc_ram',ENERGY=energy_ions_high, $
                ANGLE=i_angle_down_lc_ram, $
                ;; SC_POT=sc_pot, $
                ;; OUT_SC_POT=out_sc_pot_i, $
                ;; OUT_SC_TIME=out_sc_time_i, $
                ;; OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind_i, $
                /CALIB


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Spectrum
     GET_EN_SPEC,"fa_ies_c", $
                 UNITS='eflux', $
                 NAME='iSpec_down', $
                 ANGLE=i_angle_down, $
                 RETRACE=1, $
                 T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                 /CALIB

     GET_EN_SPEC,"fa_ies_c", $
                 UNITS='eflux',NAME='iSpec_down_lc_ram', $
                 ANGLE=i_angle_down_lc_ram, $
                 RETRACE=1, $
                 T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
                 /CALIB

     ;; NAME='JEi_down_lowE'         ,ENERGY=energy_ions_low
     ;; NAME='Ji_down_lowE'          ,ENERGY=energy_ions_low
     ;; NAME='JEi_down_lowE_lc'      ,ENERGY=energy_ions_low
     ;; NAME='Ji_down_lowE_lc'       ,ENERGY=energy_ions_low
     ;; NAME='JEi_down_lowE_lc_ram'  ,ENERGY=energy_ions_low
     ;; NAME='Ji_down_lowE_lc_ram'   ,ENERGY=energy_ions_low
     ;; NAME='JEi_down_highE'        ,ENERGY=energy_ions_high
     ;; NAME='Ji_down_highE'         ,ENERGY=energy_ions_high
     ;; NAME='JEi_down_highE_lc'     ,ENERGY=energy_ions_high
     ;; NAME='Ji_down_highE_lc'      ,ENERGY=energy_ions_high
     ;; NAME='JEi_down_highE_lc_ram' ,ENERGY=energy_ions_high
     ;; NAME='Ji_down_highE_lc_ram'  ,ENERGY=energy_ions_high

     GET_DATA,'JEi_down_lowE'         ,DATA=tmpJEi_down_lowE
     GET_DATA,'Ji_down_lowE'          ,DATA=tmpJi_down_lowE
     GET_DATA,'JEi_down_lowE_lc'      ,DATA=tmpJEi_down_lowE_lc
     GET_DATA,'Ji_down_lowE_lc'       ,DATA=tmpJi_down_lowE_lc
     GET_DATA,'JEi_down_lowE_lc_ram'  ,DATA=tmpJEi_down_lowE_lc_ram
     GET_DATA,'Ji_down_lowE_lc_ram'   ,DATA=tmpJi_down_lowE_lc_ram
     GET_DATA,'JEi_down_highE'        ,DATA=tmpJEi_down_highE
     GET_DATA,'Ji_down_highE'         ,DATA=tmpJi_down_highE
     GET_DATA,'JEi_down_highE_lc'     ,DATA=tmpJEi_down_highE_lc
     GET_DATA,'Ji_down_highE_lc'      ,DATA=tmpJi_down_highE_lc
     GET_DATA,'JEi_down_highE_lc_ram' ,DATA=tmpJEi_down_highE_lc_ram
     GET_DATA,'Ji_down_highE_lc_ram'  ,DATA=tmpJi_down_highE_lc_ram

     ;;uniq 'em
     uniq_i                    = UNIQ(tmpJEi_down_lowE.x,SORT(tmpJEi_down_lowE.x))
     tmpJEi_down_lowE          = {x:tmpJEi_down_lowE.x[uniq_i],y:tmpJEi_down_lowE.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_lowE.x,SORT(tmpJi_down_lowE.x))
     tmpJi_down_lowE           = {x:tmpJi_down_lowE.x[uniq_i],y:tmpJi_down_lowE.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJEi_down_lowE_lc.x,SORT(tmpJEi_down_lowE_lc.x))
     tmpJEi_down_lowE_lc       = {x:tmpJEi_down_lowE_lc.x[uniq_i],y:tmpJEi_down_lowE_lc.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_lowE_lc.x,SORT(tmpJi_down_lowE_lc.x))
     tmpJi_down_lowE_lc        = {x:tmpJi_down_lowE_lc.x[uniq_i],y:tmpJi_down_lowE_lc.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJEi_down_lowE_lc_ram.x,SORT(tmpJEi_down_lowE_lc_ram.x))
     tmpJEi_down_lowE_lc_ram   = {x:tmpJEi_down_lowE_lc_ram.x[uniq_i],y:tmpJEi_down_lowE_lc_ram.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_lowE_lc_ram.x,SORT(tmpJi_down_lowE_lc_ram.x))
     tmpJi_down_lowE_lc_ram    = {x:tmpJi_down_lowE_lc_ram.x[uniq_i],y:tmpJi_down_lowE_lc_ram.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJEi_down_highE.x,SORT(tmpJEi_down_highE.x))
     tmpJEi_down_highE         = {x:tmpJEi_down_highE.x[uniq_i],y:tmpJEi_down_highE.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_highE.x,SORT(tmpJi_down_highE.x))
     tmpJi_down_highE          = {x:tmpJi_down_highE.x[uniq_i],y:tmpJi_down_highE.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJEi_down_highE_lc.x,SORT(tmpJEi_down_highE_lc.x))
     tmpJEi_down_highE_lc      = {x:tmpJEi_down_highE_lc.x[uniq_i],y:tmpJEi_down_highE_lc.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_highE_lc.x,SORT(tmpJi_down_highE_lc.x))
     tmpJi_down_highE_lc       = {x:tmpJi_down_highE_lc.x[uniq_i],y:tmpJi_down_highE_lc.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJEi_down_highE_lc_ram.x,SORT(tmpJEi_down_highE_lc_ram.x))
     tmpJEi_down_highE_lc_ram  = {x:tmpJEi_down_highE_lc_ram.x[uniq_i],y:tmpJEi_down_highE_lc_ram.y[uniq_i]}
     uniq_i                    = UNIQ(tmpJi_down_highE_lc_ram.x,SORT(tmpJi_down_highE_lc_ram.x))
     tmpJi_down_highE_lc_ram   = {x:tmpJi_down_highE_lc_ram.x[uniq_i],y:tmpJi_down_highE_lc_ram.y[uniq_i]}

     down_lowE_sameT         = ARRAY_EQUAL(tmpJEi_down_lowE.x,tmpJi_down_lowE.x)
     down_lowE_lc_sameT      = ARRAY_EQUAL(tmpJEi_down_lowE_lc.x,tmpJi_down_lowE_lc.x)
     down_lowE_lc_ram_sameT  = ARRAY_EQUAL(tmpJEi_down_lowE_lc_ram.x,tmpJi_down_lowE_lc_ram.x)
     down_lowE_sammen        = down_lowE_sameT AND down_lowE_lc_sameT  AND down_lowE_lc_ram_sameT
        
     down_highE_sameT        = ARRAY_EQUAL(tmpJEi_down_highE.x,tmpJi_down_highE.x)
     down_highE_lc_sameT     = ARRAY_EQUAL(tmpJEi_down_highE_lc.x,tmpJi_down_highE_lc.x)
     down_highE_lc_ram_sameT = ARRAY_EQUAL(tmpJEi_down_highE_lc_ram.x,tmpJi_down_highE_lc_ram.x)
     down_highE_sammen       = down_highE_sameT AND down_highE_lc_sameT  AND down_highE_lc_ram_sameT
        
     down_sammen             = down_lowE_sameT AND down_highE_sameT
     down_lc_sammen          = down_lowE_lc_sameT AND down_highE_lc_sameT
     down_lc_ram_sammen      = down_lowE_lc_ram_sameT AND down_highE_lc_ram_sameT

     down_lowHighE_sameT          = down_lowE_sameT        AND down_highE_sameT        AND $
                                    ARRAY_EQUAL(tmpJEi_down_lowE.x       ,tmpJEi_down_highE.x)
     down_lowHighE_lc_sameT       = down_lowE_lc_sameT     AND down_highE_lc_sameT     AND $
                                    ARRAY_EQUAL(tmpJEi_down_lowE_lc.x    ,tmpJEi_down_highE_lc.x)
     down_lowHighE_lc_ram_sameT   = down_lowE_lc_ram_sameT AND down_highE_lc_ram_sameT AND $
                                    ARRAY_EQUAL(tmpJEi_down_lowE_lc_ram.x,tmpJEi_down_highE_lc_ram.x)
     down_lowHighE_sammen         = down_lowHighE_sameT AND down_lowHighE_lc_sameT AND down_lowHighE_lc_ram_sameT

     down_lowHighE_alle_sameT = down_lowHighE_sammen AND $
                                    ARRAY_EQUAL(tmpJEi_down_lowE.x,tmpJEi_down_highE_lc_ram.x) AND $
                                    ARRAY_EQUAL(tmpJEi_down_lowE_lc.x,tmpJEi_down_highE_lc_ram.x)

     ;;alle_sammen = 1 betyr at alle tid er sammen
     ;;alle_sammen = 2 betyr at tid for hver types angle range  er sammen
     CASE 1 OF
        down_lowHighE_alle_sameT: BEGIN
           alle_sammen = 1

           ;;Sort 'em

           GET_FA_ORBIT,tmpJEi_down_lowE.x,/TIME_ARRAY,/ALL

           GET_DATA,'MLT',DATA=mlt
           mlt    = FLOAT(mlt.y)
           GET_DATA,'ILAT',DATA=ilat
           ilat   = FLOAT(ilat.y)
           GET_DATA,'ALT',DATA=alt
           alt    = FLOAT(alt.y)
           GET_DATA,'ORBIT',DATA=orbit
           orbit  = FLOAT(orbit.y)

           GET_DATA,'B_model',DATA=tmp1
           GET_DATA,'BFOOT',DATA=tmp2
           mag1  = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
           mag2  = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
           ratio = (mag2/mag1)
           
        END
        ;; (down_sammen AND down_lc_sammen AND down_lc_ram_sammen): BEGIN
        ;;    alle_sammen = 2
        ;;    GET_FA_ORBIT,tmpeSpec_up.x,/TIME_ARRAY
        ;;    GET_DATA,'MLT',DATA=mlt
        ;;    mlt    = FLOAT(mlt.y)
        ;;    GET_DATA,'ILAT',DATA=ilat
        ;;    ilat   = FLOAT(ilat.y)
        ;;    GET_DATA,'ALT',DATA=alt
        ;;    alt    = FLOAT(alt.y)
        ;;    GET_DATA,'ORBIT',DATA=orbit
        ;;    orbit  = FLOAT(orbit.y)
        ;; END
        ELSE: BEGIN
           OPENW,badLun,badFile,/GET_LUN,/APPEND
           PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"ikke alle sammen")',orbStr,jjj
           CLOSE,badLun
           FREE_LUN,badLun
        END
     ENDCASE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Low energy
     CLEANUP_STRUCT,tmpJEi_down_lowE, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_lowE_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_lowE")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_lowE, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_lowE_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_lowE")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJEi_down_lowE_lc, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_lowE_lc_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_lowE_lc")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_lowE_lc, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_lowE_lc_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_lowE_lc")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJEi_down_lowE_lc_ram, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_lowE_lc_ram_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_lowE_lc_ram")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_lowE_lc_ram, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_lowE_lc_ram_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_lowE_lc_ram")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;High edgery
     CLEANUP_STRUCT,tmpJEi_down_highE, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_highE_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_highE")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_highE, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_highE_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_highE")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJEi_down_highE_lc, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_highE_lc_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_highE_lc")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_highE_lc, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_highE_lc_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_highE_lc")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJEi_down_highE_lc_ram, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=JEi_down_highE_lc_ram_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJEi_down_highE_lc_ram")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF
     CLEANUP_STRUCT,tmpJi_down_highE_lc_ram, $
                    /MUY_RAPIDO, $
                    SUCCESS=success, $
                    /REMOVE_TIME, $
                    KEPT_I=Ji_down_highE_lc_ram_i
     IF ~success THEN BEGIN
        OPENW,badLun,badFile,/GET_LUN,/APPEND
        PRINTF,badLun,FORMAT='(I8,T10,I2,T20,"could not clean tmpJi_down_highE_lc_ram")',orbStr,jjj
        CLOSE,badLun
        FREE_LUN,badLun
        RETURN
     ENDIF


        ;; keep1                                    = FINITE(tmpiSpec_down.y)
        ;; nTimes                                   = N_ELEMENTS(tmpiSpec_down.y[*,0])
        ;; nEnergies                                = N_ELEMENTS(tmpiSpec_down.y[0,*])
        ;; keepRow                                  = MAKE_ARRAY(nTimes,/BYTE,VALUE=1)
        ;; FOR i=0,N_ELEMENTS(tmpiSpec_down.y[*,0])-1 DO BEGIN
        ;;    test                                  = WHERE(keep1[i,*],tCount)
        ;;    keepRow[i]                            = tCount EQ nEnergies ? 1 : 0
        ;; ENDFOR
        ;; tmpiSpec_down.x                               = tmpiSpec_down.x[WHERE(keepRow)]
        ;; tmpiSpec_down.y                               = tmpiSpec_down.y[WHERE(keepRow),*]
        ;; tmpiSpec_down.v                               = tmpiSpec_down.v[WHERE(keepRow),*]

        

     ;;Now make 'em cry
     ;; IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_up,tmpjee_up,tmpje_up, $
     ;;                                         mlt,ilat,alt,orbit, $
     ;;                                         eSpecs_parsed, $
     ;;                                         SC_POT=out_sc_pot, $ ;The reason for no negative is that the sign gets flipped get_2d_ts_pot
     ;;                                         /QUIET, $
     ;;                                         BATCH_MODE=batch_mode, $
     ;;                                         ORBSTR=orbStr, $
     ;;                                         ERRORLOGFILE=badFile
     

     ;; IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,iSpec_down,jei_down,ji_down, $
     ;;                                         mlt,ilat,alt,orbit, $
     ;;                                         iSpecs_parsed, $
     ;;                                         SC_POT=(-1.)*out_sc_pot, $ ;The reason for the negative is that that's what we actually get from V8_S
     ;;                                         /QUIET, $
     ;;                                         BATCH_MODE=batch_mode, $
     ;;                                         ORBSTR=orbStr, $
     ;;                                         ERRORLOGFILE=badFile

     GET_DATA,'iSpec_down',DATA=iSpec_down_lc_ram
     GET_DATA,'iSpec_down_lc_ram',DATA=iSpec_down_lc_ram


     ;; IF KEYWORD_SET(include_ions) THEN BEGIN
        ;;Save the electron stuff
        PRINT,'Saving downgoing_ion file: ' + out_newell_file
        ;; SAVE,eSpecs_parsed,tmpeSpec_up,tmpjee_up,tmpje_up, $
        ;;      out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
        ;;      iSpecs_parsed,iSpec_down,jei_down,ji_down, $
        ;;      out_sc_pot_i,out_sc_time_i,out_sc_min_energy_ind_i, $
        ;;      FILENAME=outNewellDir+out_newell_file
        SAVE, $
           tmpJEi_down_lowE,tmpJi_down_lowE, $
           tmpJEi_down_lowE_lc,tmpJi_down_lowE_lc, $
           tmpJEi_down_lowE_lc_ram,tmpJi_down_lowE_lc_ram, $
           tmpJEi_down_highE,tmpJi_down_highE, $
           tmpJEi_down_highE_lc,tmpJi_down_highE_lc, $
           tmpJEi_down_highE_lc_ram,tmpJi_down_highE_lc_ram, $
           JEi_down_lowE_i,Ji_down_lowE_i, $
           JEi_down_lowE_lc_i,Ji_down_lowE_lc_i, $
           JEi_down_lowE_lc_ram_i,Ji_down_lowE_lc_ram_i, $
           JEi_down_highE_i,Ji_down_highE_i, $
           JEi_down_highE_lc_i,Ji_down_highE_lc_i, $
           JEi_down_highE_lc_ram_i,Ji_down_highE_lc_ram_i, $
           mlt,ilat,alt,orbit,ratio, $
           iSpec_down,iSpec_down_lc_ram, $
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
