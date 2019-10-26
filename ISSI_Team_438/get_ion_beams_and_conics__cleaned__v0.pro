;2019/05/06
;Get ion beams (according to Hatch, Chaston, LaBelle 2018) and ion conics (according to strangeway_2005__v3.pro), as well as their moments
;; Cleaned for passing along to Nilofaur
PRO GET_ION_BEAMS_AND_CONICS__CLEANED__V0, $
   TPLT_VARS=tPlt_vars, $
   DO_NOT_ENFORCE_SAMPLE_RATE=do_not_enforce_sample_rate, $
   IONSPECS_UPDOWNMINRATIO=upDownMinRatio, $
   IONSPECS_MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
   IONSPECS_THRESH_EFLUX=thresh_eFlux, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   ONLY_LEEWARD_IONS=only_leeward_ions, $
   ENFORCE_THIS_SAMPLE_RATE=enforce_this_sample_rate, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   MAKE_IONS_OXYGEN=make_ions_oxygen, $
   MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
   SAVE_PS=save_ps, $
   BATCH_MODE=batch_mode, $
   SKIP_EXISTING=skip_existing, $
   REMAKE_DIFF_EFLUX=remake_diff_eFlux

  CURVERSION = '20190508.1KLUGE'

  savesDir = '/thelonious_data1/FAST/'

  IF KEYWORD_SET(do_not_enforce_sample_rate) THEN BEGIN
     PRINT,"GET_ION_BEAMS_AND_CONICS: Not enforcing sample rate ..."
     enforce_this_sample_rate = 0
  ENDIF ELSE IF KEYWORD_SET(enforce_this_sample_rate) THEN BEGIN
     enforce_this_sample_rate = enforce_this_sample_rate
  ENDIF ELSE BEGIN
     PRINT,"enforce_this_sample_rate = 2.5 by default ..."
     enforce_this_sample_rate = 2.5
  ENDELSE

  ;; Ion options
  ;; Nei da, med diff_eFlux
  McFadden_diff_eFlux = 1
  ionBeam_energyRange = [10,2.4e4]  ;According to J2018 l724 er ion_min_if_nan_scpots = 4.

  @tplot_com ;provides data_quants variable
  @startup

  IF ~KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR,0
  ENDIF

  dEF__include_sc_pot = 1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 0a - restore ion fings, or get 'em if we ain't got 'em

  tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)

  GET_FA_ORBIT,times,/TIME_ARRAY

  nHere = N_ELEMENTS(times)
  GET_DATA,"ORBIT",DATA=orbit
  orbit = orbit.y[nHere/2]

  ;; output file name
  specAvgSuff = ''
  CASE 1 OF
     KEYWORD_SET(enforce_this_sample_rate): BEGIN
        specAvgSuff = (STRING(FORMAT='("-sRate",F0.2)', $
                              enforce_this_sample_rate)).Replace('.','_')
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        specAvgSuff = STRING(FORMAT='("-avgItvl",I0)',spectra_average_interval)
     END
     ELSE: BEGIN
        specAvgSuff = ''
     END
  ENDCASE

  outAlgFile = 'orbit_'+str(orbit)+'__outflow_algorithm_and_beam_algorithm' $
               + specAvgSuff + '.sav'

  IF KEYWORD_SET(skip_existing) THEN BEGIN

     IF FILE_TEST(savesDir+outAlgFile) THEN BEGIN
        
        RESTORE,savesDir+outAlgFile

        IF N_ELEMENTS(saveVersion) EQ 0 THEN BEGIN
           PRINT,"No saveVersion here! Remaking ..."

           ;; Gjelder CURVERSION = '20190508.1'
           ionEvents = !NULL
           ionMomStruct = !NULL
           ephemStruct = !NULL
           saveVersion = !NULL

        ENDIF ELSE BEGIN

           IF saveVersion EQ CURVERSION THEN BEGIN
              
              PRINT,"Already have " + outAlgFile + ' (V.'+CURVERSION+')! Returning ...'
              RETURN

           ENDIF ELSE BEGIN
              PRINT,"Saved version is " + saveVersion + ", but CURVERSION is " + CURVERSION + "! Remaking ..." 

              ;; Gjelder CURVERSION = '20190508.1'
              ionEvents = !NULL
              ionMomStruct = !NULL
              ephemStruct = !NULL
              saveVersion = !NULL

           ENDELSE

        ENDELSE

     ENDIF

  ENDIF

  EXAMINE_ION_CONIC_VS_ALL_FLUX_RATIOS__CLEANED_V0, $
     TIMES=times, $
     UPDOWNMINRATIO=upDownMinRatio, $
     MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
     FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
     THRESH_EFLUX=thresh_eFlux, $
     ;; /QUIT_IF_FILE_EXISTS, $
     MAKE_IONS_OXYGEN=make_ions_oxygen, $
     ONLY_LEEWARD_IONS=only_leeward_ions, $
     ENFORCE_THIS_SAMPLE_RATE=enforce_this_sample_rate, $
     DO_NOT_ENFORCE_SAMPLE_RATE=do_not_enforce_sample_rate, $
     REMAKE_DIFF_EFLUX=remake_diff_eFlux, $
     DEF__INCLUDE_SC_POT=dEF__include_sc_pot, $
     SC_POT=sc_pot, $
     ESPECALL=eSpec, $
     ESPECUP=eSpecUp, $
     ESPECDOWN=eSpecDown, $
     UPDOWNRATIOSPEC=upDownRatioSpec, $
     UPALLRATIOSPEC=upAllRatioSpec, $
     EBOUND=eBound, $
     IONMOMSTRUCT=ionMomStruct, $
     IONUPJ=ionUpJ, $
     IONDIFFEFLUX=ion_dEF, $
     UP_ARANGEN=up_aRangeN, $
     DOWN_ARANGEN=down_aRangeN, $
     UP_ARANGES=up_aRangeS, $
     DOWN_ARANGES=down_aRangeS, $
     MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
     SAVE_PS=save_ps, $
     NO_PLOTS=no_plots, $
     ORBIT=orbit, $
     OUT_ORBIT=out_orbit, $
     OUTSTRUCT_ORBIT=ephemStruct, $
     MISLYKTES=mislyktes, $
     TPLT_VARS=tplt_vars, $
     /ADD_EBOUND_INFO_TO_IONMOMSTRUCT

  IF KEYWORD_SET(mislyktes) THEN BEGIN
     PRINT,"Mislyktes under identifikasjon av ion utstrømming-perioder"
     PRINT,"Tilbake ..."
     RETURN
  ENDIF

  ;; START NEED-RESULTS-NOW KLUGE
  PRINT,"TOTAL KLUGE! SPENCE IS BANGING HIS WAY OUT OF GET_ION_BEAMS_AND_CONICS"
  PRINT,"Saving " + outAlgFile + ' ...'
  saveVersion = CURVERSION
  SAVE,ionMomStruct, $
       ephemStruct, $
       saveVersion, $
       FILENAME=savesDir+outAlgFile
  RETURN

END
