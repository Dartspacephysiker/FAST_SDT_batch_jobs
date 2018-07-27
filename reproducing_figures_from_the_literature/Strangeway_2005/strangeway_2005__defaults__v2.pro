  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V2/'
  hashFile     = 'Strangeway_et_al_2005__v2'
  indivOrbPref = 'Strangeway_et_al_2005__v2--'

  outPlotName  = 'Strangeway_2005_v2'
  plotDirSuff  = '/Strangeway_et_al_2005/V2'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     hashFile    +='--eFieldFits'
     outPlotName += '--eFieldFits'
  ENDIF

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 10 
  bonusSuff    = ''

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 10 
  bonusSuff    = '-factorof5'

  hashFile    += bonusSuff
  outPlotName += bonusSuff

  hashFile += ".sav"

  ;;Update hashfile name and outPlotName
  plotPref = SETUP_STRANGEWAY_BRAMBLES_PLOTPREF($
             USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
             ONLY_FASTSRVY_DATA=only_128Ss_data, $
             INCLUDE_E_NEAR_B=include_E_near_B, $
             FULL_PFLUX_CALC=full_pFlux, $
             FIELDS_INTERP=do_fields_interp, $
             FIELDS_SPLINE=do_fields_spline)
  
  hashFile    += plotPref
  outPlotName += plotPref

  PRINT,'outDir   : ',outDir
  PRINT,'Hashfile : ',hashFile
  