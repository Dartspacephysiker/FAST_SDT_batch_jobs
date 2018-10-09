;2018/10/05
;; NOTE, that SH moment signs are not flipped!
PRO ELECTRON_MOMENTS_AND_SPECTRAL_IDENTIFICATION_V0

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; outDir            = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/electrons_2018/'
  outDir            = '/thelonious_data1/FAST_electrons_2018/'

  todayStr          = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  outFile_pref      = 'electron_moments_and_spectral_identification__Orbit_'

  energy_electrons  = [70,30400.]
  enforce_this_sample_rate = 2.5

  McFadden_diff_eFlux  = 1
  eeb_or_ees           = "ees"  

  ;; energy_electronsforSCPot = [0,30400.]

  ;; If no data exists, return to main
  ;; t=0
  ;; dat          = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  t               = 0.0D
  ;; t1=0.0D 
  t2              = 0.0D 
  dat             = GET_FA_EES(t,/ST) 
  ;; dat          = GET_FA_EES(t,EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
     RETURN
  ENDIF
  ;; ENDIF ELSE BEGIN
  t1              = t
  ;; t2           = t

  last            = GET_FA_EES(t2,/EN) 
  n_EESA_spectra  = last.index+1
  last_index      = LONG(last.index)
  
  nUniqEdgery     = N_ELEMENTS(UNIQ(FLOOR(dat.energy),SORT(FLOOR(dat.energy)))) 

  greide          = (nUniqEdgery GT 10) AND dat.valid 
  nCalls          = 1
  WHILE ~greide DO BEGIN
     dat          = GET_FA_EES(t1,/AD) 

     ;; Supposedly valid, so see if there are enough energy bins to bear this claim out
     IF dat.valid THEN BEGIN
        nUniqEdgery  = N_ELEMENTS(UNIQ(FLOOR(dat.energy),SORT(FLOOR(dat.energy)))) 
        greide       = (nUniqEdgery GT 10) 
     ENDIF

     ;; IF dat.index EQ last_index THEN BREAK

     nCalls++
     IF nCalls EQ n_EESA_spectra THEN BREAK
  ENDWHILE

  IF ~greide THEN BEGIN
     PRINT,"Only junk ESA data! Returning ..."
     RETURN
  ENDIF

  ;;Get orbit num
  GET_FA_ORBIT,t1,/TIME_ARRAY

  GET_DATA,'ORBIT',DATA=orb
  orbit_num            = orb.y[0]
  orbStr               = STRCOMPRESS(orbit_num,/REMOVE_ALL)

  PRINT,'*********************************************'
  PRINT,'Orb ' + orbStr + ': ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra loaded in SDT...'
  PRINT,'*********************************************'
  PRINT,''

  ;; load_elec_dEF_file = N_ELEMENTS(remake_diff_eFlux) GT 0 ? ~remake_diff_eFlux : 1
  ;; save_elec_dEF_file = 1
  diffEFlux__array_of_structs = 1
  elec_dEF_energy = energy_electrons
  ;; elec_dEF_energy = energy_electronsforSCPot

  ;; DIFF_EFLUX_FNAME, $
  ;;    T1=je_tBounds[0], $
  ;;    T2=je_tBounds[1], $
  ;;    ORBIT=orbit, $
  ;;    EEB_OR_EES=eeb_or_ees, $
  ;;    BONUSPREF=bonusPref ,$
  ;;    SAVE_DIFF_EFLUX_TO_FILE=elec_dEF_fileName,$
  ;;    SAVE_DIFF_EFLUX_FILE=save_elec_dEF_file,$
  ;;    LOAD_DIFF_EFLUX_FILE=load_elec_dEF_file,$
  ;;    MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
  ;;    OUT_DIFF_EFLUX_FILE=elec_dEF_file, $
  ;;    ENFORCE_DIFF_EFLUX_SRATE=enforce_this_sample_rate, $
  ;;    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
  ;;    LOADDIR=savesDir

  GET_LOSSCONE_AND_EFLUX_DATA, $
     ;; T1=je_tBounds[0], $
     ;; T2=je_tBounds[1], $
     T1=t1, $
     T2=t2, $
     LOAD_DAT_FROM_FILE=KEYWORD_SET(load_elec_dEF_file) ? elec_dEF_file : !NULL, $
     LOAD_DIR=savesDir, $
     EEB_OR_EES=eeb_or_ees, $
     DIFF_EFLUX=elec_dEF, $
     UPGOING=upgoing, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_this_sample_rate, $
     DIFFEFLUX__ARRAY_OF_STRUCTS=diffEFlux__array_of_structs, $
     DEF__INCLUDE_SC_POT=dEF__include_sc_pot, $
     SC_POT=sc_pot, $
     OUT_ORB=orb, $
     OUT_ANGLERANGE=lc_angleRange, $
     OUT_NORTHSOUTH=north_south, $
     FIT_EACH_ANGLE=fit_each_angle, $
     MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
     ALLEXCLATM_ARANGE=allExclAtm_aRange, $
     EARTHWARD_ARANGE=earthward_aRange, $
     ANGLESTR=angleStr, $
     ELECTRON_ENERGY_LIMS=elec_dEF_energy, $
     /IGNORE_MIXED_HEMISPHERE, $
     ;; SAVE_DIFF_EFLUX_TO_FILE=elec_dEF_fileName, $
     _EXTRA=e

  ;; Now time differences
  tDiffs     = elec_dEF.end_time - elec_dEF.time
  angleRange = {all : [0,360.], $
                lc  : MAKE_ARRAY(2,N_ELEMENTS(elec_dEF),/FLOAT,VALUE=0.0)}


  ;; Now get loss-cone angles, map ratio
  struc = {B_model: TRANSPOSE(elec_dEF.B_model), $
           Bfoot  : TRANSPOSE(elec_dEF.B_foot ), $
           ilat   : elec_dEF.ilat}

  GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2, $
                                          lc_angleRange, $
                                          i_angle,i_angle_up, $
                                          north_south, $
                                          ALLEXCLATM_ARANGE=allExclAtm_aRange, $
                                          EARTHWARD_ARANGE=earthward_aRange, $
                                          CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                          UPGOING=upgoing, $
                                          OUT_E_ANGLE=e_angle, $
                                          OUT_MAPRATIO=mapRatio, $
                                          ANGLESTR=angleStr, $
                                          SDTSTRUCT=struc, $
                                          JUST_ONE=just_one

  angleRange.lc = lc_angleRange

  elec_min_if_nan_scpots = 30.
  ;; minEn_if_no_sc_pot = 30.
  energy = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX( $
           elec_dEF, $
           ENERGY=elec_dEF_energy, $
           SC_POT=sc_pot, $
           EEB_OR_EES=eeb_or_ees, $
           ARRAY_OF_STRUCTS_INSTEAD=diffEFlux__array_of_structs, $
           MIN_IF_NAN_SCPOTS=elec_min_if_nan_scpots, $
           MINEN_IF_NO_SC_POT=minEn_if_no_sc_pot)

  ;; NOTE, MOMENT_SUITE_2D ensures that earthward fluxes are positive
  MOMENT_SUITE_2D,elec_dEF, $
                  ENERGY=energy, $
                  ARANGE__MOMENTS=angleRange.all, $
                  SC_POT=sc_pot, $
                  EEB_OR_EES=eeb_or_ees, $
                  /ERROR_ESTIMATES, $
                  ;; MAP_TO_100KM=map_to_100km, $ 
                  ORBIT=orbit, $
                  /NEW_MOMENT_ROUTINE, $
                  MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                  /PROVIDING_EPHEM_INFO, $
                  IN_ILAT=elec_dEF.ilat, $
                  IN_MLT=elec_dEF.mlt, $
                  IN_ALT=elec_dEF.alt, $
                  ;; /DONT_FLIP_SIGNS_OF_SOUTHHEMI_MOMENTS, $
                  QUIET=quiet, $
                  OUTTIME=time, $
                  OUT_N=n, $
                  OUT_J_=j, $
                  OUT_JE=je, $
                  OUT_T=T, $
                  OUT_CHARE=charE, $
                  OUT_CURRENT=cur, $
                  OUT_JJE_COVAR=jje_coVar, $
                  OUT_ERRORS=errors, $
                  OUT_ERR_N=nErr, $
                  OUT_ERR_J_=jErr, $
                  OUT_ERR_JE=jeErr, $
                  OUT_ERR_T=TErr, $
                  OUT_ERR_CURRENT=curErr, $
                  OUT_ERR_CHARE=charEErr, $
                  INOUT_MAPRATIO=mapRatio, $
                  OUT_STRUCT=eMomStruct_allAngle, $
                  BATCH_MODE=batch_mode

  MOMENT_SUITE_2D,elec_dEF, $
                  ENERGY=energy, $
                  ARANGE__MOMENTS=angleRange.lc, $
                  SC_POT=sc_pot, $
                  EEB_OR_EES=eeb_or_ees, $
                  /ERROR_ESTIMATES, $
                  ;; MAP_TO_100KM=map_to_100km, $ 
                  ORBIT=orbit, $
                  /NEW_MOMENT_ROUTINE, $
                  MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                  /PROVIDING_EPHEM_INFO, $
                  IN_ILAT=elec_dEF.ilat, $
                  IN_MLT=elec_dEF.mlt, $
                  IN_ALT=elec_dEF.alt, $
                  ;; /DONT_FLIP_SIGNS_OF_SOUTHHEMI_MOMENTS, $
                  /QUIET, $
                  OUTTIME=time, $
                  OUT_N=n, $
                  OUT_J_=j, $
                  OUT_JE=je, $
                  OUT_T=T, $
                  OUT_CHARE=charE, $
                  OUT_CURRENT=cur, $
                  OUT_JJE_COVAR=jje_coVar, $
                  OUT_ERRORS=errors, $
                  OUT_ERR_N=nErr, $
                  OUT_ERR_J_=jErr, $
                  OUT_ERR_JE=jeErr, $
                  OUT_ERR_T=TErr, $
                  OUT_ERR_CURRENT=curErr, $
                  OUT_ERR_CHARE=charEErr, $
                  INOUT_MAPRATIO=mapRatio, $
                  OUT_STRUCT=eMomStruct_lcAngle, $
                  BATCH_MODE=batch_mode


  ;; Now add spectral identification

  ;;Now make 'em cry

  lcSpecVarName = 'eSpecLC'
  varName = lcSpecVarName
  eSpecLC = GET_EN_SPEC__FROM_DIFF_EFLUX( $
            elec_dEF, $
            T1=t1, $
            T2=t2, $
            /RETRACE, $
            ANGLE=angleRange.lc, $
            UNITS=eSpecUnits, $
            NAME=varName, $
            OUT_AVGFACTORARR=avgFactorArr, $
            OUT_NORMARR=normArr, $
            OUT_TIME=out_time, $
            BAD_TIME=bad_time, $
            IS_MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux)
  STORE_DATA,lcSpecVarName,DATA=eSpecLC

  ;; Align eSpec times with diff_eFlux times, and shrink appropriately
  this = VALUE_CLOSEST2(eSpecLC.x,elec_dEF.time+tDiffs/2,/CONSTRAINED)
  bad_times = bad_time[this]
  eSpecLC_MOD = {x     : eSpecLC.x[this], $
                 y     :  eSpecLC.y[this,*], $
                 v     :  eSpecLC.v[this,*], $
                 yErr  :  eSpecLC.yErr[this,*], $
                 vErr  :  eSpecLC.vErr[this,*]}

  ;; Inputs for IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT
  tmpjee_lc = eMomStruct_lcAngle.je
  tmpje_lc  = eMomStruct_lcAngle.j

  mlt = elec_dEF.mlt
  ilat = elec_dEF.ilat
  alt = elec_dEF.alt
  orbit = elec_dEF.orbit

  out_sc_pot = REPLICATE(energy_electrons[0],N_ELEMENTS(mlt))

  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpecLC_MOD,tmpjee_lc,tmpje_lc, $
                                          mlt,ilat,alt,orbit, $
                                          eSpecs_parsed, $
                                          SC_POT=out_sc_pot, $ ;The reason for no negative is that the sign gets flipped get_2d_ts_pot
                                          /QUIET, $
                                          BATCH_MODE=batch_mode, $
                                          ORBSTR=orbStr, $
                                          ERRORLOGFILE=badFile
  
  eTron = { $
          valid : elec_dEF.valid, $
          time : elec_dEF.time, $
          orbit  : TEMPORARY(orbit), $
          mlt  : TEMPORARY(mlt), $
          ilat : TEMPORARY(ilat), $
          alt  : TEMPORARY(alt), $
          moments : {lc : eMomStruct_lcAngle, $
                     all : eMomStruct_allAngle}, $
          mono : eSpecs_parsed.mono, $
          broad : eSpecs_parsed.broad, $
          diffuse : eSpecs_parsed.diffuse}
  

  extra = {tDiffs    : TEMPORARY(tDiffs), $
           eSpec_bad_time : bad_times, $
           fa_pos    : elec_dEF.fa_pos, $
           fa_vel    : elec_dEF.fa_vel, $
           b_model   : elec_dEF.b_model, $
           b_foot    : elec_dEF.b_foot, $
           mapRatio  : TEMPORARY(mapRatio), $
           foot_lat  : elec_dEF.foot_lat, $
           foot_lng  : elec_dEF.foot_lng, $
           losscone  : angleRange.lc}

  outFile = outFile_pref+orbStr+'.sav'
  PRINT,'-----------------------------------------------'
  PRINT,"Saving "+outFile+' ...'
  PRINT,'-----------------------------------------------'
  SAVE,eTron,extra,FILENAME=outDir+outFile

END
