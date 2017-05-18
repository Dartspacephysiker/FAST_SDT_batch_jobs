  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Brambles_et_al_2011/'
  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--full_pFlux--interp'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--full_pFlux--interp--128Ss'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--128Ss'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--absVals'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--absvals'

  ;; hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--absVals'
  ;; outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--absvals'

  hashFile     = 'Brambles_et_al_2011__AC_params--ESA_intervals.sav--absVals__handchecked_ionEnergies'
  outPlotName  = 'Brambles_et_al_2011__AC_ion_outflow--ESA_intervals--absvals__handchecked_ionEnergies'

  IF KEYWORD_SET(plot_north)     THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south)     THEN outPlotName += '--' + 'SOUTH'

  PRINT,'outDir   : ',outDir
  PRINT,'Hashfile : ',hashFile
  