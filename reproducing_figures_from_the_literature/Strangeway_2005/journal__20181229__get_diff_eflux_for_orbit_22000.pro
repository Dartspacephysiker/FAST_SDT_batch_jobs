;2018/12/29
PRO JOURNAL__20181229__GET_DIFF_EFLUX_FOR_ORBIT_22000

  COMPILE_OPT IDL2,STRICTARRSUBS

  ieb_or_ies = 'ies'

  ;; t = 0.
  ;; this = GET_FA_IES(

  calc_geom_factors      = 1
  deFlux__array_of_structs  = 1
  save_diff_eFlux_to_file   = 1
  load_diff_eFlux_from_file = N_ELEMENTS(remake_diff_eFlux) GT 0 ? ~remake_diff_eFlux : 1
  calib = 1

  tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)

  GET_FA_ORBIT,times,/TIME_ARRAY

  avgitvlstr = ''
  threshEFluxStr = ''
  upDownRatioStr = ''
  leewardStr = ''
  minNQualEStr = ''
  
  nHere = N_ELEMENTS(times)
  GET_DATA,"ORBIT",DATA=orbit
  orbit = orbit.y[nHere/2]
  savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios"$
             +avgItvlStr+threshEFluxStr+upDownRatioStr+minNQualEStr + leewardStr
  saveSuff = ".sav"
  DIFF_EFLUX_FNAME, $
     T1=times[0], $
     T2=times[-1], $
     ORBIT=orbit, $
     EEB_OR_EES=ieb_or_ies, $
     BONUSPREF=bonusPref ,$
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
     SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file,$
     LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file,$
     MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
     OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
     ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     LOADDIR=loadDir

  GET_DIFF_EFLUX,T1=times[0],T2=times[-1], $
                 EEB_OR_EES=ieb_or_ies, $
                 SC_POT=sc_pot, $
                 ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                 CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                 CALC_GEOM_FACTORS=calc_geom_factors, $
                 ARRAY_OF_STRUCTS_INSTEAD=deFlux__array_of_structs, $
                 SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                 OVERWRITE_EXISTING=overwrite_existing, $
                 DIFF_EFLUX_FILE=diff_eFlux_file, $
                 LOAD_DAT_FROM_FILE=load_diff_eFlux_from_file, $
                 LOAD_DIR=loadDir, $
                 OUT_DIFF_EFLUX=diff_eflux

  tDiffs     = diff_eFlux.end_time - diff_eFlux.time

  ;; GET_DIFF_EFLUX,T1=t1,T2=t2, $
  ;;    EEB_OR_EES=eeb_or_ees, $
  ;;    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
  ;;    ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
  ;;    MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
  ;;    SC_POT=sc_pot, $
  ;;    NAME__DIFF_EFLUX=name__diff_eFlux, $
  ;;    CALC_GEOM_FACTORS=calc_geom_factors, $
  ;;    CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
  ;;    MCFADDEN_GAP_TIME=gap_time, $
  ;;    ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead, $
  ;;    ;; UNITS=units, $
  ;;    ;; ANGLE=angle, $
  ;;    ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
  ;;    FIT_EACH_ANGLE=fit_each_angle, $
  ;;    TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
  ;;    OUT_DIFF_EFLUX=diff_eflux, $
  ;;    SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
  ;;    OVERWRITE_EXISTING=overwrite_existing, $
  ;;    DIFF_EFLUX_FILE=diff_eFlux_file, $
  ;;    LOAD_DAT_FROM_FILE=loadFile, $
  ;;    LOAD_DIR=loadDir, $
  ;;    NO_DATA=no_data

  STOP

  print,t2s(diff_eflux[605].time)
  ;; have ram ions at 2000-03-04/02:55:00
  ;; this is 2000-03-04/02:55:04

  dat = diff_eflux[605]

  CONTOUR2D,dat, $
            ;; ANGLE=angle, $
            /POLAR, $
            /FILL, $
            ;; /OVERPLOT, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL, $
            THICK=thick

plot = PLOT(dat.energy[0:dat.nenergy-1,19],dat.data[0:dat.nenergy-1,19],/XLOG,/YLOG)

END
