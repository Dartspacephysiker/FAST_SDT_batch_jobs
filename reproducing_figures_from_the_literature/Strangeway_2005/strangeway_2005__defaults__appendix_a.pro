  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/Appendix_A/'
  hashFile     = 'Strangeway_et_al_2005__real_thing--outflow_intervals.sav'
  indivOrbPref = 'Strangeway_et_al_2005__real_thing--'

  outPlotName  = 'Strangeway_2005_Appendix_A'
  plotDirSuff  = '/Strangeway_et_al_2005/Appendix_A'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     hashFile    +='--eFieldFits'
     outPlotName += '--eFieldFits'
  ENDIF

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  ;; bonusSuff    = ''
  bonusSuff    = '_decimate_NOW'
  ;; bonusSuff    = '_hugeChange_MOTTAKER'
  bonusSuff    = '_BirkelandTry' ;2018/07/19

  IF ( ABS(energy_ions[0] - 4.)   LT 0.01 ) THEN bonusSuff += '--4eV_lower'
  IF ( ABS(energy_ions[1] - 500.) LT 0.01 ) THEN bonusSuff += '--500eV_upper'

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
  