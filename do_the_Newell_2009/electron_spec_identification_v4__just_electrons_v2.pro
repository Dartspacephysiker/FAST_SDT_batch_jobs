;2016/08/26
;;Maybe not deserving of a new version number, but this one should load the saved Je stuff
;;much more quickly.
PRO ELECTRON_SPEC_IDENTIFICATION_V4__JUST_ELECTRONS_V2, $
   SKIP_IF_FILE_EXISTS=skip_if_file_exists, $
   ENERGY_ELECTRONS=energy_electrons, $
   EEB_OR_EES=eeb_or_ees, $
   T1=t1,T2=t2

  COMPILE_OPT idl2

  outDir               = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'

  todayStr             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  outNewellDir         = outDir + 'do_the_Newell_2009/batch_output__just_electrons/'
  outFile_pref         = 'Dartdb--e-_spectra__all_angles_energies--' + eeb_or_ees + '--Orbit_'


  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons  = [0.,35000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF
  e_angle              = [-180,180]

  ;; If no data exists, return to main
  ;; t=0
  ;; dat               = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                  = GET_FA_EES(0.0D, EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
     RETURN
  ENDIF ELSE BEGIN
     n_EESA_spectra    = dat.index+1
     last_index        = LONG(dat.index)
     
     PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t2                   = 0.0D
  temp                 = GET_FA_EES(t2,INDEX=0.0D)
  t1                   = t2
  temp                 = GET_FA_EES(t2,/ADV)

  GET_FA_ORBIT,t1,t2
  ;;now get orbit quantities
  GET_DATA,'ORBIT',DATA=orb
  orbit_num            = orb.y[0]
  orbStr               = STRCOMPRESS(orbit_num,/REMOVE_ALL)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First, see that we are able to match all points in this orb
  ;;The files inside give je, time_range_indices, and time_range
  e                    = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                                    JE_OUT=je, $
                                                    TIME_RANGES_OUT=time_ranges, $
                                                    TIME_RANGE_INDICES_OUT=time_range_indices, $
                                                    NINTERVALS_OUT=number_of_intervals)

  ;;Get us out (of the U.N.!) in case there is nothing to talk about
  IF (e EQ -1) THEN BEGIN
     PRINT,'No good data available for orbit ' + orbStr + '! Exiting ...'
     RETURN
  ENDIF

  ;; number_of_intervals  = intervalArr[orbit_num]
  PRINT,'Number of intervals : ',number_of_intervals
  nSpecRemoved_thisorbit  = n_EESA_spectra-N_ELEMENTS(je.x)

  STORE_DATA,'Je',DATA=je

  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN

     ;;We're going to make output in any case. We're already here, after all!
     out_newell_file  = outFile_pref + orbStr + '_' + STRCOMPRESS(jjj,/REMOVE_ALL) + '.sav'

     IF FILE_TEST(outNewellDir+out_newell_file) THEN BEGIN
        PRINT,'Skipping ' + out_newell_file + '...'
        CONTINUE
     ENDIF

     PRINT,'time_range',time_to_str(time_ranges[jjj,0]),time_to_str(time_ranges[jjj,1])
     
     je_tmp_time      = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data      = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]


     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}

     ;; GET_DIFF_EFLUX,T1=time_ranges[jjj,0],T2=time_ranges[jjj,1], $
     GET_DIFF_EFLUX,T1=je_tmp_time[0],T2=je_tmp_time[-1], $
                    EEB_OR_EES=eeb_or_ees, $
                    ;; SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                    /CALC_GEOM_FACTORS, $
                    NAME__DIFF_EFLUX=name__diff_eFlux, $
                    ANGLE=e_angle, $
                    /FIT_EACH_ANGLE ;, $
     ;; /FIT_EACH_ANGLE=fit_each_angle ;, $
     ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
     ;; TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct

     GET_DATA,name__diff_eFlux,DATA=diff_eFlux

     ;;get_orbit data
     GET_FA_ORBIT,je_tmp_time,/TIME_ARRAY,/ALL
     
     ;;define loss cone angle
     GET_DATA,'ALT',DATA=alt

     loss_cone_alt    = alt.y*1000.0
     lcw              = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI

     GET_DATA,'ILAT',DATA=ilat
     GET_DATA,'MLT',DATA=mlt
     ;; north_south                                 = ABS(ilat.y[0])/ilat.y[0]
     GET_DATA,'LAT',DATA=lat
     GET_DATA,'LNG',DATA=lng
     GET_DATA,'FLAT',DATA=flat
     GET_DATA,'FLNG',DATA=flng
     GET_DATA,'fa_pos',DATA=fa_pos

     ephemInfo = {x:alt.x, $
                  alt:alt.y, $
                  ilat:ilat.y, $
                  mlt:mlt.y, $
                  lc_width:lcw, $
                  lat:lat.y, $
                  lng:lng.y, $
                  flat:flat.y, $
                  flng:flng.y, $
                  fa_pos:fa_pos.y}

     ;;Save the electron stuff
     PRINT,'Saving Newell file: ' + out_newell_file
     SAVE,diff_eFlux, $
          ephemInfo, $
          nSpecRemoved_thisorbit, $
          FILENAME=outNewellDir+out_newell_file

  ENDFOR

  RETURN 
END
