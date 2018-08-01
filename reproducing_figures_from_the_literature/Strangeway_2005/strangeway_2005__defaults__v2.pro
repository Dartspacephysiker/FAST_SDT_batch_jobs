  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V2/'
  hashFile     = 'Strangeway_et_al_2005__v2'
  indivOrbPref = 'Strangeway_et_al_2005__v2'

  outPlotName  = 'Strangeway_2005__v2'
  plotDirSuff  = '/Strangeway_et_al_2005/V2'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     hashFile    +='--eFieldFits'
     outPlotName += '--eFieldFits'
  ENDIF

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 10 , and at least 4 energy channels
  bonusSuff    = '-upDownRatio_10-minNQualECh_4'

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 3, and at least 4 energy channels
  bonusSuff    = '-upDownRatio_3-minNQualECh_4'

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 2, and at least 3 energy channels
  bonusSuff    = '-upDownRatio_2-minNQualECh_3'

  ;; 20180727 This version gives you the DB that requires upflow to be greater than
  ;; ... "downflow"(?) by a factor of 2, and at least 3 energy channels
  bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s'

  ;; 20180801 
  bonusSuff    = '-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SOUTHERN
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; ;; 20180728 Try out some southern orbs
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-SOUTH'

  ;; ;; 20180728 Try out some southern orbs
  ;; ;; 20180730 Leeward didn't work out very well; it wipes out masse ion outflow
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-SOUTH_6000orbs-leeward'

  ;; ;; 20180728 Try out the northern orbs, leeward style
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-NORTH_leeward'

  ;; ;; 20180730 "Leeward-stil" gikk ikke så bra; det går glipp av masse ion utstømming
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-SOUTH_6000orbs'

  ;; ;; 20180730 "Leeward-stil" gikk ikke så bra; det går glipp av masse ion utstømming
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-SOUTH_6000orbs-nyemetod'

  ;; ;; 20180731 Nylige valgte vaner på 9400-taller
  ;; bonusSuff    = '-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-SOUTH_9400-taller'

  ;; ;; 20180731 Nylige valgte vaner på 9400-taller
  ;; bonusSuff    = '-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-SOUTH_9400-taller_MODDED'

  ;; 20180731 Nylige valgte vaner på 9400-taller
  ;; bonusSuff    = '-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-SOUTH'

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
  