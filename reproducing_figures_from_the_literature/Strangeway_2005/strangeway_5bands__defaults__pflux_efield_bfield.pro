  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_5bands/'
  hashFile     = 'Strangeway_5bands.sav'
  indivOrbPref = 'Strangeway_5bands--'

  outPlotName  = 'Strangeway_5bands'
  plotDirSuff  = '/Strangeway_5bands'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     hashFile    +='--eFieldFits'
     outPlotName += '--eFieldFits'
  ENDIF

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  bonusSuff    = ''

  hashFile    += bonusSuff
  outPlotName += bonusSuff


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
  
