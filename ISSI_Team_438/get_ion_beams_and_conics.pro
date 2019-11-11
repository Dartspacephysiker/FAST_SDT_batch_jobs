;2019/05/06
;Get ion beams (according to Hatch, Chaston, LaBelle 2018) and ion conics (according to strangeway_2005__v3.pro), as well as their moments
PRO GET_ION_BEAMS_AND_CONICS, $
   TPLT_VARS=tPlt_vars, $
   DO_NOT_ENFORCE_SAMPLE_RATE=do_not_enforce_sample_rate, $
   IONSPECS_UPDOWNMINRATIO=upDownMinRatio, $
   IONSPECS_MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
   IONSPECS_THRESH_EFLUX=thresh_eFlux, $
   IONSPECS_THRESH_BEAM_EFLUX=thresh_beam_eFlux, $
   IONSPECS_BEAMHALFRATIO=beamHalfRatio, $
   ;; USERDEF_HASHFILE=userDef_hashFile, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   ONLY_LEEWARD_IONS=only_leeward_ions, $
   ENFORCE_THIS_SAMPLE_RATE=enforce_this_sample_rate, $
   SCREEN_PLOT=screen_plot, $
   ;; DECIMATE_EB_CALC_PFLUX=decimate_eb_calc_pFlux, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   ;; SAVE_INDIVIDUAL_ORBIT=save_individual_orbit, $
   ;; SAVE_INDIVIDUAL_DATA_PRODUCTS_AND_QUIT=save_individual_data_products_and_quit, $
   ;; SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_IONS=save_individual_data_products__only_ions, $
   ;; SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_ELECTRONS_AND_IONS=save_individual_data_products__only_electrons_and_ions, $
   ;; SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_DB_AND_IONS=save_individual_data_products__only_db_and_ions, $
   ;; SAVE_INDIVIDUAL_DATA_PRODUCTS__FSUFF=save_individual_data_products__fSuff, $
   ;; SAVE_INDIVID__CONVERT_B_GEI_TO_B_GEO=convert_B_gei_to_B_geo, $
   MAKE_IONS_OXYGEN=make_ions_oxygen, $
   NO_BLANK_PANELS=no_blank_panels, $
   STRANGEWAY_2005_FIG3_PLOT=Strangeway_2005_Fig3_plot, $
   ;; NO_HASH_UPDATE=no_hash_update, $
   MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   BATCH_MODE=batch_mode, $
   ;; SKIP_EXISTING_IN_HASH=skip_existing_in_hash, $
   SKIP_EXISTING=skip_existing, $
   REMAKE_DIFF_EFLUX=remake_diff_eFlux, $
   FORCE_SH_TBOUNDS_FOR_JE=force_SH_tBounds_for_je, $
   ENERGY_ELECTRONS_LB=energy_electrons_lb

  CURVERSION = '20190508.1'
  CURVERSION = '20190508.1KLUGE'

  save_diff_eFlux_to_file = 0B
; Program will use fac_v if E field data are available, other use fac_v
; over-ride with use_fac_v and use_fac keywords

; if no_blank_panels is not set procedure will generate tplot data for all the parameters,
; including missing data, for a uniform plot product

; Step 0 - safety measure - delete all tplot quantities if found

  ;; outflowMinLog10 = 6
  ;; ptsMinOutflow   = 60
  ;; allowableGap    = 3 ;seconds

  ;; IF N_ELEMENTS(do_not_enforce_sample_rate) EQ 0 THEN do_not_enforce_sample_rate = 1

  IF KEYWORD_SET(do_not_enforce_sample_rate) THEN BEGIN
     PRINT,"GET_ION_BEAMS_AND_CONICS: Not enforcing sample rate ..."
     enforce_this_sample_rate = 0
  ENDIF ELSE IF KEYWORD_SET(enforce_this_sample_rate) THEN BEGIN
     enforce_this_sample_rate = enforce_this_sample_rate
  ENDIF ELSE BEGIN
     PRINT,"enforce_this_sample_rate = 2.5 by default ..."
     enforce_this_sample_rate = 2.5
  ENDELSE

  ;; Ion and electron options
  ;; Nei da, med diff_eFlux
  McFadden_diff_eFlux = 1
  eeb_or_ees = "ees"
  ionBeam_energyRange = [10,2.4e4]  ;According to J2018 l724 er ion_min_if_nan_scpots = 4.

  IF N_ELEMENTS(beamHalfRatio) EQ 0 THEN beamHalfRatio = 3


  IF N_ELEMENTS(convert_B_gei_to_B_geo) EQ 0 THEN convert_B_gei_to_B_geo = 1

  CASE 1 OF
     KEYWORD_SET(use_eField_fit_variables): BEGIN
        eAV_variable = 'EFIT_ALONG_V'
        eNB_variable = 'EFIT_NEAR_B'
     END
     ELSE: BEGIN
        eAV_variable = 'E_ALONG_V'
        eNB_variable = 'E_NEAR_B'
     END
  ENDCASE

  @tplot_com ;provides data_quants variable

  @startup

  mu_0              = DOUBLE(4.0D*!PI*1e-7)

  ;;Allowable difference between t{1,2} and nearest fields data
  tBuf              = 10.

  minILAT           = 50

  ;; energy_ions       = [4,120.]
  energy_electrons  = [50,30400.]
  energy_electronsforSCPot = [0,30400.]

  IF KEYWORD_SET(energy_electrons_lb) THEN BEGIN
     energy_electronsforSCPot[0] = energy_electrons_lb
  ENDIF

  ctNum = 43

  IF ~KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR,0
  ENDIF

  ;;From UCLA_MAG_DESPIN:
  ;;"   Field-aligned coordinates defined as:
  ;;"   z-along B, y-east (BxR), x-nominally out"
  ;;    (ind 2)  , (ind 1)     , (ind 0)
  ;;    (b)      , (e)         , (o)

  ;;"   Field-aligned velocity-based coordinates defined as:
  ;;"   z-along B, y-cross track (BxV), x-along track ((BxV)xB).
  ;;    (ind 2)  , (ind 1)            , (ind 0)
  ;;    (b)      , (p)                , (v)

  ;; magInd = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

; Step 1 - DC Mag data

  ;; savesDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  savesDir = '/thelonious_data1/FAST/'
  ;; savesIndivDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/'

  ;; 'AT'strangeway_2005__defaults__v3.pro
  ;;Outputs
  ;; hashFile     = 'Strangeway_et_al_2005__v3'
  ;; indivOrbPref = 'Strangeway_et_al_2005__v3'

  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/ISSI_Team_438/'

  outPlotName  = 'Ion_beams_and_conics'
  ;; plotDirSuff  = '/Strangeway_et_al_2005/V3'
  plotDirSuff  = ''

  ;; IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
  ;;    hashFile    +='--eFieldFits'
  ;;    outPlotName += '--eFieldFits'
  ;; ENDIF

  IF KEYWORD_SET(plot_north) THEN outPlotName += '--' + 'NORTH'
  IF KEYWORD_SET(plot_south) THEN outPlotName += '--' + 'SOUTH'

  bonusSuff    = '-threshEFlux5e5-upDownRatio_2-minNQualECh_3-interp4Hz_to_1s-50eVLBforelec'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SOUTHERN
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; hashFile    += bonusSuff
  ;; outPlotName += bonusSuff

  ;; hashFile += ".sav"

  ;;Update hashfile name and outPlotName
  ;; plotPref = SETUP_STRANGEWAY_BRAMBLES_PLOTPREF(USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables,ONLY_FASTSRVY_DATA=only_128Ss_data,INCLUDE_E_NEAR_B=include_E_near_B,FULL_PFLUX_CALC=full_pFlux,FIELDS_INTERP=do_fields_interp,FIELDS_SPLINE=do_fields_spline)

  ;; SETUP_STRANGEWAY_BRAMBLES_PLOTPREF,USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
  ;;                                    ONLY_FASTSRVY_DATA=only_128Ss_data, $
  ;;                                    INCLUDE_E_NEAR_B=include_E_near_B, $
  ;;                                    FULL_PFLUX_CALC=full_pFlux, $
  ;;                                    FIELDS_INTERP=do_fields_interp, $
  ;;                                    FIELDS_SPLINE=do_fields_spline

  ;; hashFile    += plotPref
  ;; outPlotName += plotPref


  dEF__include_sc_pot = 1

  ;; IF KEYWORD_SET(userDef_hashFile) THEN BEGIN
  ;;    ;; PRINT,"ACTUALLY, userDef hashFile: ",userDef_hashFile

  ;;    hashFile = userDef_hashFile
  ;;    outPlotName = userDef_hashFile.Replace(".sav","")
  ;; ENDIF

  ;; PRINT,'outDir   : ',outDir
  ;; PRINT,'Hashfile : ',hashFile

  psym_ptcl    = 3              ;period
  symsize_ptcl = 5.0
  nn           = N_ELEMENTS(data_quants)

  if (nn GT 1) THEN for n = nn-1L,1L,-1L do STORE_DATA,data_quants(n).name,/delete

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 0a - restore ion fings, or get 'em if we ain't got em

  tRange = GET_ESA_TIMERANGES__RASKT(/IONS,OUT_TIME_ARRAY=times)

  nHere = N_ELEMENTS(times)

  IF nHere EQ 0 THEN BEGIN
     PRINT,"No times here! returning ..."
     RETURN
  ENDIF
  
  GET_FA_ORBIT,times,/TIME_ARRAY

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

     IF FILE_TEST(savesDir+'twoTypes_ion_identification/'+outAlgFile) THEN BEGIN
        
        RESTORE,savesDir+'twoTypes_ion_identification/'+outAlgFile

        IF N_ELEMENTS(saveVersion) EQ 0 THEN BEGIN
           PRINT,"No saveVersion here! Remaking ..."

           ;; saveVersion = CURVERSION &   SAVE,ionEvents, $
           ;;                                   ionMomStruct, $
           ;;                                   saveVersion, $
           ;;                                   FILENAME=savesDir+'twoTypes_ion_identification/'+outAlgFile

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

  EXAMINE_ION_CONIC_VS_ALL_FLUX_RATIOS, $
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
     SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
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

  ;; orbit = out_orbit

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
       FILENAME=savesDir+'twoTypes_ion_identification/'+outAlgFile
  RETURN

  ;; END KLUGE


  ;; Try to add other ion beam thing
  ;; ILAT data from call to GET_FA_ORBIT for ion_diff_eFlux in JOURNAL__20180720__LOOK_AT_CONIC_VS_ALL_FLUX_RATIOS


  ;; GET_DATA,'ILAT',data=ILAT
  north_southArr               = FIX(ABS(ephemStruct.ilat)/ephemStruct.ilat)

  ;; Need t1, t2
  GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
     ;; t1,t2, $
     ephemStruct.time[0],ephemStruct.time[-1], $
     ionlc_angleRange, $
     i_angle,i_angle_up, $
     north_southArr, $
     ALLEXCLATM_ARANGE=allExclAtm_aRange, $
     OUT_LCW=lcw, $
     ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
     CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
     OUT_E_ANGLE=e_angle, $
     ANGLESTR=angleStr, $
     SDTSTRUCT=ephemStruct;; , $
     ;; /JUST_ONE

  ;; Flip lc_angleRange so that it's upgoing
  ion_angleRange = (360.*((ionlc_angleRange-180)/360.-FLOOR((ionlc_angleRange-180)/360.)))

  useDiffEflux = 1
  usePeakEnergy = 1
  ;; GET_FA_IESA_ION_BEAMS,STR_TO_TIME(t1Str),STR_TO_TIME(t2Str), $
  GET_FA_IESA_ION_BEAMS,ephemStruct.time[0],ephemStruct.time[-1], $
                        USEDIFFEFLUX=useDiffEflux, $
                        IONDIFFEFLUX=ion_dEF, $
                        MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                        ORBIT=orbit, $
                        THRESH_BEAM_EFLUX=thresh_beam_eFlux, $
                        BEAMHALFRATIO=beamHalfRatio, $
                        ;; NEWELL_2009_INTERP=Newell_2009_interp, $
                        ;; ION_ANGLERANGE=curPotList[2].angles.peakEn, $
                        ION_ANGLERANGE=ion_angleRange, $
                        ;; ION_ENERGYRANGE=curPotList[2].energy, $
                        ION_ENERGYRANGE=ionBeam_energyRange, $
                        SPECTROGRAM_UNITS=spectrogram_units, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        ENFORCE_DIFF_EFLUX_SRATE=enforce_this_sample_rate, $
                        SC_POT=sc_pot, $
                        OUT_SC_POTAVG=sc_potAvg, $
                        OUT_IONEVENTS=ionEvents, $
                        BATCH_MODE=batch_mode, $
                        USEPEAKENERGY=usePeakEnergy, $
                        EPHEMSTRUCT=ephemStruct, $
                        MAKE_TPLOT=~KEYWORD_SET(no_plots), $
                        SAVE_PS=save_ps

  ;; Stick TPLOT_BEAM_VS_HALFRANGE_ION_FLUXES HERE

  PRINT,"Saving " + outAlgFile + ' ...'
  saveVersion = CURVERSION
  SAVE,ionEvents, $
       ionMomStruct, $
       ephemStruct, $
       saveVersion, $
       FILENAME=savesDir+'twoTypes_ion_identification/'+outAlgFile

  RETURN

  STOP

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 1 - DC Mag data

  IF ~(KEYWORD_SET(save_individual_data_products__only_ions)             OR $
       KEYWORD_SET(save_individual_data_products__only_electrons_and_ions)) $
  THEN BEGIN

     UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

     IF (N_ELEMENTS(orbit) EQ 0) THEN BEGIN
        PRINT,"Couldn't pick up orb info from UCLA_MAG_DESPIN. OUT!"
        RETURN
     ENDIF

  ENDIF

  orbString           = STRING(FORMAT='(I0)',orbit)
  outPlotName        += '--' + orbString

  IF KEYWORD_SET(save_individual_orbit) THEN BEGIN
     indiv_orbFile = indivOrbPref + orbString + '.sav'
  ENDIF

  save_data_and_quit = KEYWORD_SET(save_individual_data_products__only_ions)               OR $
                       KEYWORD_SET(save_individual_data_products__only_db_and_ions)        OR $
                       KEYWORD_SET(save_individual_data_products__only_electrons_and_ions) OR $
                       KEYWORD_SET(save_individual_data_products_and_quit)

  IF KEYWORD_SET(save_data_and_quit) THEN BEGIN
     IF KEYWORD_SET(save_individual_data_products__fSuff) THEN BEGIN
        indivSuff = save_individual_data_products__fSuff
     ENDIF ELSE BEGIN
        ;; indivSuff = "Orbit_"+orbString+'-rawProds.sav'
        indivSuff = '-rawProds.sav'
     ENDELSE
     indivFile = "Orbit_"+orbString+indivSuff
  ENDIF

  ;; IF KEYWORD_SET(skip_existing_in_hash) THEN BEGIN

  ;;    IF FILE_TEST(outDir+hashFile) THEN BEGIN
  ;;       PRINT,"Checking for orbit " + orbString + " in hash file " + hashFile + " ..."
  ;;       RESTORE,outDir+hashFile

  ;;       CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
  ;;          -1: BEGIN
  ;;             PRINT,'Getting stuff from orbit ' + orbString + ' ...'
  ;;          END
  ;;          ELSE: BEGIN
  ;;             PRINT,"Already got this orbit! Out ..."
  ;;             RETURN
  ;;          END
  ;;       ENDCASE

  ;;    ENDIF

  ;; ENDIF

  ;;Get time and Je info
  check =  LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit, $
                                        RETURN_STRUCT=return_struct, $
                                        /USE_DUPELESS_FILES, $
                                        JE_OUT=je_pristine, $
                                        TIME_RANGES_OUT=time_ranges, $
                                        TIME_RANGE_INDICES_OUT=time_range_indices, $
                                        NINTERVALS_OUT=number_of_intervals, $
                                        ;; OUT_JEFILENAME=jeFileName, $
                                        ;; CLEAN_DUPES=clean_dupes, $
                                        /QUIET)

  ;;Checkups
  IF check[0] EQ -1 THEN BEGIN
     PRINT,"Couldn't get Je time info for orbit " + orbString + '!!!'
     PRINT,"Out ..."
     RETURN
  ENDIF

  IF SIZE(je_pristine,/TYPE) NE 8 THEN BEGIN
     PRINT,"Apparently no ESA interval information for this orbit. Returning ..."
     RETURN
  ENDIF

  IF N_ELEMENTS(je_pristine.x) LE 1 THEN BEGIN
     PRINT,'Insufficient data to do anything awesome! Returning ...'
     RETURN
  ENDIF

  ;;Clean up based on ILAT
  GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL
  GET_DATA,'ILAT',DATA=ilat
  IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
     PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
     RETURN
  ENDIF
  IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
     PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
     RETURN
  ENDIF

  ;;Make sure we have data where we want it.
  keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
  IF nKeep LE 1 THEN BEGIN
     PRINT,'No data above min ILAT. Out!'
     RETURN
  ENDIF

  IF KEYWORD_SET(force_SH_tBounds_for_je) THEN BEGIN
     GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=struc

     north_south = LONG(ABS(struc.ilat)/struc.ilat)

     keep = WHERE(north_south EQ -1,nKeep)

     IF nKeep EQ 0 THEN BEGIN
        IF KEYWORD_SET(batch_mode) THEN BEGIN
           PRINT,"Ain't no nothin' in SH for orbit " + orbString + "! Returning ..."
           RETURN
        ENDIF ELSE BEGIN
           STOP
        ENDELSE
     ENDIF

     je_pristine = {x: je_pristine.x[keep], $
                    y: je_pristine.y[keep]}
     GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=struc
  ENDIF

  je_tBounds = [je_pristine.x[0],je_pristine.x[-1]]
  ;; je_tBounds = [ionMomStruct.x[0],ionMomStruct.x[-1]]
  PRINT,FORMAT='(A0,T35,A0,", ",A0)',"ionMomStruct beginning/end : ", $
        TIME_TO_STR(je_tBounds[0],/MSEC), $
        TIME_TO_STR(je_tBounds[1],/MSEC)

;  if orbit > 9936 return (temporary fix)

  if (orbit gt 9936) then begin

     ;; PRINT,""
     ;; PRINT,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
     ;; PRINT,""
     ;; return

  endif

; got mag data, set time limits, delete unused tplot variables, set tPlt_vars

  ;; STORE_DATA,'BDATA',/delete
  ;; STORE_DATA,'BFIT',/delete
  ;; STORE_DATA,'Bx_sp',/delete
  ;; STORE_DATA,'By_sp',/delete
  ;; STORE_DATA,'Bz_sp',/delete
  ;; STORE_DATA,'Bx_sc',/delete
  ;; STORE_DATA,'By_sc',/delete
  ;; STORE_DATA,'Bz_sc',/delete
  ;; STORE_DATA,'Bx_sp_sm',/delete
  ;; STORE_DATA,'By_sp_sm',/delete
  ;; STORE_DATA,'Bz_sp_sm',/delete
  ;; STORE_DATA,'B_gei',/delete
  ;; STORE_DATA,'B_sm',/delete
  ;; STORE_DATA,'dB_sc',/delete
  ;; STORE_DATA,'dB_gei',/delete
  ;; STORE_DATA,'spin_freq',/delete
  ;; STORE_DATA,'spin_phase',/delete
  ;; STORE_DATA,'TORQ_X',/delete
  ;; STORE_DATA,'TORQ_Y',/delete
  ;; STORE_DATA,'TORQ_Z',/delete
  ;; STORE_DATA,'BX_DEL',/delete
  ;; STORE_DATA,'BY_DEL',/delete
  ;; STORE_DATA,'BZ_DEL',/delete
  ;; STORE_DATA,'BFIX',/delete
  ;; STORE_DATA,'TW_ZX',/delete
  ;; STORE_DATA,'TW_ZY',/delete
  ;; STORE_DATA,'TW_YY',/delete
  ;; STORE_DATA,'TW_YX',/delete
  ;; STORE_DATA,'O_X',/delete
  ;; STORE_DATA,'O_Y',/delete
  ;; STORE_DATA,'B_model_old',/delete
  ;; STORE_DATA,'Delta_B_model',/delete
  ;; STORE_DATA,'despun_to_gei',/delete
  ;; STORE_DATA,'gei_to_sm',/delete
  ;; STORE_DATA,'gei_to_fac',/delete
  ;; STORE_DATA,'gei_to_fac_v',/delete

  IF ~(KEYWORD_SET(save_individual_data_products__only_ions)              OR $
       KEYWORD_SET(save_individual_data_products__only_electrons_and_ions)  ) $
  THEN BEGIN

     GET_DATA,'dB_fac_v',data=data
     t1            = data.x[0]
     t2            = data.x[N_ELEMENTS(data.x)-1L]
     magz_tBounds  = [t1,t2]

     OPTIONS,'dB_fac_v','panel_size',2
     OPTIONS,'dB_fac','panel_size',2
     OPTIONS,'dB_sm','panel_size',2

     PRINT,FORMAT='(A0,T35,A0,", ",A0)',"MAG beginning/end : ",TIME_TO_STR(t1,/MSEC),TIME_TO_STR(t2,/MSEC)


     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

     tPlt_vars = 'dB_fac_v'

     if (keyword_set(screen_plot)) then begin
        LOADCT2,ctNum
        tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     endif

     IF KEYWORD_SET(save_individual_data_products_and_quit) OR $
        KEYWORD_SET(save_individual_data_products__only_db_and_ions) $
     THEN BEGIN

        GET_DATA,'dB_fac_v',data=dB_fac_v
        GET_DATA,'dB_fac',data=dB_fac

        ;; 'B_gei'      Smoothed and deglitched field in GEI coordinates
        GET_DATA,'B_gei',data=B_gei

        ;;  'Bx_sc'      Despun Bx (in spin plane, to sun, smoothed, deglitched)
        ;;  'By_sc'      Despun By (in spin plane, perp sun, smoothed, deglitched)
        ;;  'Bz_sc'      Despun Bz (spin axis component, smoothed, deglitched)
        ;;  'dB_sc'      Detrended field in despun spacecraft coordinates
        GET_DATA,'dB_sc',data=dB_sc

        GET_FA_ORBIT,dB_fac.x,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=dBEphem
     ENDIF


; step 2 - E field

     IF ~KEYWORD_SET(save_individual_data_products__only_db_and_ions) THEN BEGIN
        ;; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
        ;; to get the SDT index for this run.  Otherwise "showDQIs" won't
        ;; return.  If this is old, single-user SDT, "sdt_idx" is returned
        ;; as 255 and we handle the call in the old way.
        sdt_idx = get_sdt_run_idx()

        prog = GETENV('FASTBIN') + '/showDQIs'
        IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
           IF (sdt_idx GE 10) THEN BEGIN
              sidstr = STRING(sdt_idx,FORMAT='(I2)')
           ENDIF ELSE BEGIN
              sidstr = STRING(sdt_idx,FORMAT='(I1)')
           ENDELSE
           SPAWN,[prog, sidstr],result,/NOSHELL
        ENDIF ELSE BEGIN
           SPAWN,prog,result,/NOSHELL
        ENDELSE


        ;;Find out if we have various eField things
        b = WHERE(STRPOS(result,'V1-V4_S') GE 0,nb4)
        IF (nb4 GT 0) THEN IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN nb4 = 0
        b = WHERE(STRPOS(result,'V1-V2_S') GE 0,nb2)
        IF (nb2 GT 0) THEN IF STRPOS(RESULT[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN nb2 = 0
        IF (nb4 GT 0) THEN v12 = GET_FA_FIELDS('V1-V4_S',/DEFAULT) $
        ELSE IF (nb2 GT 0) THEN v12 = GET_FA_FIELDS('V1-V2_S',/DEFAULT)

        b = WHERE(STRPOS(result,'V5-V8_S') GE 0,nb5)
        IF (nb5 GT 0) THEN v58 = GET_FA_FIELDS('V5-V8_S',/DEFAULT)

        got_efield = (nb4 GT 0 OR nb2 GT 0) AND nb5 GT 0

        IF (got_efield) THEN BEGIN

           ;; despin e field data
           FA_FIELDS_DESPIN,v58,v12, $
                            ;; MAG_NOTCH=mag_notch, $
                            ;; BINTERP=BInterp, $
                            ;; BNAN=BNaN, $
                            SHADOW_NOTCH=shadow_notch, $
                            SINTERP=sInterp, $
                            SNAN=sNaN


           eF_spinPlane = GET_EFIELD_A_LA_ALFVEN_STATS_5(/BURST)

        ENDIF ELSE BEGIN
           PRINT,"Couldn't get E-field data! Out ..."
           RETURN
        ENDELSE

        IF KEYWORD_SET(save_individual_data_products_and_quit) THEN BEGIN
           GET_DATA,eAV_variable,DATA=eAlongV
           GET_DATA,eNB_variable,DATA=eNearB
           GET_DATA,"EFIT_ALONG_V",DATA=eFitAlongV
           GET_DATA,"EFIT_NEAR_B",DATA=eFitNearB
        ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Step 4 - Electron junk, AND
        ;; Step 5 - Ion flux

        ;; ENDIF

        ;; IF ~KEYWORD_SET(save_individual_data_products__only_ions) THEN BEGIN


        ;;Just get them all
        ;; t           = 0.0D
        ;; tmp         = GET_FA_EES_C(t,/EN)
        ;; IF tmp.valid EQ 0 THEN BEGIN
        ;;    PRINT,'Junk electron data'
        ;;    RETURN
        ;; ENDIF
        ;; last_index  = LONG(tmp.index)
        ;; t1Ptcl      = 0.0D
        ;; t2Ptcl      = 0.0D
        ;; temp        = GET_FA_EES(t1Ptcl,INDEX=0.0D)
        ;; temp        = GET_FA_EES(t2Ptcl,INDEX=DOUBLE(last_index))

        ;; PRINT,FORMAT='(A0,T35,A0,", ",A0)',"Particle beginning/end : ",TIME_TO_STR(,/MSEC),TIME_TO_STR(t2ptcl,/MSEC)

        types_2dt = ['je_2d_fs','j_2d_fs']
        routs_2dt = ['fa_ees_c','fa_ees_c']
        names_2dt = ['JEe','Je']

        ngsgn_2dt = [0,0]
        ;; enrgy_2dt = [[energy_electrons],[energy_electrons]]
        titls_2dt = ['Electron!CEnergy Flux!CmW/(m!U2!N)', $
                     'Electron Flux!C!C#/(cm!U2!N-s)']
        lims_2dt  = [[-1.,6.,0],[-5.e9,1.5e10,0]]
        nFlux_2dt = MAKE_ARRAY(N_ELEMENTS(types_2dt),/LONG)
        of_pos    = [0,0]
        pr_pos    = [1,1]

        ;; FOR ll=0,N_ELEMENTS(types_2dt)-1 DO BEGIN

        ;;    tmpType = types_2dt[ll]
        ;;    tmpRout = routs_2dt[ll]
        ;;    tmpName = names_2dt[ll]
        ;;    tmpNrg  = enrgy_2dt[*,ll]
        ;;    tmpTitl = titls_2dt[ll]
        ;;    tmpLims = lims_2dt[*,ll]

        ;;    GET_FA_PARTICLE_2DT,tmpType,tmpRout, $
        ;;       T1=je_tBounds[0], $
        ;;       T2=je_tBounds[1], $
        ;;       NAME=tmpName, $
        ;;       ENERGY=tmpNrg, $
        ;;       ;; ERANGE=er, $
        ;;       ;; EBINS=ebins, $
        ;;       ;; ANGLE=an, $
        ;;       ;; ARANGE=ar, $
        ;;       ;; BINS=bins, $
        ;;       ;; GAP_TIME=gap_time, $
        ;;       ;; NO_DATA=no_data, $
        ;;       ;; BKG=bkg, $
        ;;       ;; MISSING=missing, $
        ;;       ;; FLOOR=floor, $
        ;;       /CALIB, $
        ;;       TITLE=tmpTitl, $
        ;;       LIMS=tmpLims, $
        ;;       OUTFLOW_POSITIVE=of_pos[ll], $
        ;;       PRECIPITATION_POSITIVE=pr_pos[ll]

        ;;    ;; tmpDatStruct = CREATE_STRUCT(tmpDatStruct,tmpName+'_time',tmp.x,tmpName,tmp.y)
        ;;    ;; tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,tmpName,doDat)

        ;; ENDFOR

        ;; Get all loss-cone angle ranges
        ;; angleRange = {all : [0,360.], $
        ;;               lc  : [0.,0.]}

        load_elec_dEF_file = N_ELEMENTS(remake_diff_eFlux) GT 0 ? ~remake_diff_eFlux : 1
        save_elec_dEF_file = 1
        diffEFlux__array_of_structs = 1
        elec_dEF_energy = energy_electronsforSCPot

        DIFF_EFLUX_FNAME, $
           T1=je_tBounds[0], $
           T2=je_tBounds[1], $
           ORBIT=orbit, $
           EEB_OR_EES=eeb_or_ees, $
           BONUSPREF=bonusPref ,$
           SAVE_DIFF_EFLUX_TO_FILE=elec_dEF_fileName,$
           SAVE_DIFF_EFLUX_FILE=save_elec_dEF_file,$
           LOAD_DIFF_EFLUX_FILE=load_elec_dEF_file,$
           MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
           OUT_DIFF_EFLUX_FILE=elec_dEF_file, $
           ENFORCE_DIFF_EFLUX_SRATE=enforce_this_sample_rate, $
           SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
           LOADDIR=savesDir

        GET_LOSSCONE_AND_EFLUX_DATA, $
           T1=je_tBounds[0], $
           T2=je_tBounds[1], $
           ;; IN_DIFF_EFLUX_FILE=elec_dEF_file, $
           LOAD_DAT_FROM_FILE=KEYWORD_SET(load_elec_dEF_file) ? elec_dEF_file : !NULL, $
           ;; MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
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
           ;; CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
           MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
           ALLEXCLATM_ARANGE=allExclAtm_aRange, $
           EARTHWARD_ARANGE=earthward_aRange, $
           ANGLESTR=angleStr, $
           ;; ESPECUNITS=KF__Curvefit_opt.units, $
           ELECTRON_ENERGY_LIMS=elec_dEF_energy, $
           SAVE_DIFF_EFLUX_TO_FILE=elec_dEF_fileName, $
           /IGNORE_MIXED_HEMISPHERE, $
           _EXTRA=e

        angleRange = {all : [0,360.], $
                      lc  : MAKE_ARRAY(2,N_ELEMENTS(elec_dEF),/FLOAT,VALUE=0.0)}

        ;; Now get loss-cone angles, map ratio
        eMomEphem = {fa_pos : TRANSPOSE(elec_dEF.fa_pos), $
                     fa_vel : TRANSPOSE(elec_dEF.fa_vel), $
                     alt    : elec_dEF.alt, $
                     mlt    : elec_dEF.mlt, $
                     ilat   : elec_dEF.ilat, $
                     B_model: TRANSPOSE(elec_dEF.B_model), $
                     Bfoot  : TRANSPOSE(elec_dEF.B_foot ), $
                     foot_lat : TRANSPOSE(elec_dEF.foot_lat ), $
                     foot_lng : TRANSPOSE(elec_dEF.foot_lng )}

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
                                                SDTSTRUCT=eMomEphem, $
                                                JUST_ONE=just_one

        angleRange.lc = lc_angleRange
        ;; angleRange.lc = lc_angleRange

        ;; flip = WHERE(lc_angleRange GT 180,nFlip)
        ;; IF nFlip GT 0 THEN BEGIN
        ;;    lc_angleRange[flip] -= 360.
        ;; ENDIF

        elec_min_if_nan_scpots = 30.
        minEn_if_no_sc_pot = 30.
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
                        ;; /ERROR_ESTIMATES, $
                        ;; MAP_TO_100KM=map_to_100km, $
                        ORBIT=orbit, $
                        /NEW_MOMENT_ROUTINE, $
                        MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                        /PROVIDING_EPHEM_INFO, $
                        IN_ILAT=elec_dEF.ilat, $
                        IN_MLT=elec_dEF.mlt, $
                        IN_ALT=elec_dEF.alt, $
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
                        ;; /ERROR_ESTIMATES, $
                        ;; MAP_TO_100KM=map_to_100km, $
                        ORBIT=orbit, $
                        /NEW_MOMENT_ROUTINE, $
                        MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                        /PROVIDING_EPHEM_INFO, $
                        IN_ILAT=elec_dEF.ilat, $
                        IN_MLT=elec_dEF.mlt, $
                        IN_ALT=elec_dEF.alt, $
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
                        OUT_STRUCT=eMomStruct_lcAngle, $
                        BATCH_MODE=batch_mode

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Step 6 - VLF data

        ;;DSP_V5-V8HG or DSP_V5-V8

        prog = GETENV('FASTBIN') + '/showDQIs'
        IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
           IF (sdt_idx GE 10) THEN BEGIN
              sidstr = STRING(sdt_idx, FORMAT='(I2)')
           ENDIF ELSE BEGIN
              sidstr = STRING(sdt_idx, FORMAT='(I1)')
           ENDELSE
           SPAWN, [prog, sidstr], result, /NOSHELL
        ENDIF ELSE BEGIN
           SPAWN, prog, result, /NOSHELL
        ENDELSE
        b = WHERE(STRPOS(result,'DspADC_V5-V8HG') GE 0,ndsphg)
        IF (ndsphg GT 0) THEN BEGIN
           IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN ndsphg = 0
        ENDIF
        b = WHERE((STRPOS(result,'DspADC_V5-V8') GE 0) AND $
                  (STRPOS(result,'DspADC_V5-V8HG') LT 0),ndsp)
        IF (ndsp GT 0) THEN BEGIN
           IF STRPOS(result[b[0]+1],'Points (cur/aloc): 0       /') GE 0 THEN ndsp = 0
        ENDIF

        if (ndsphg GT 0) THEN BEGIN
           data = GET_FA_FIELDS('DspADC_V5-V8HG',/DEFAULT)
        ENDIF else BEGIN
           IF (ndsp GT 0) THEN BEGIN
              data = GET_FA_FIELDS('DspADC_V5-V8',/DEFAULT)
           ENDIF
        ENDELSE
        ndsp = (ndsp GT 0) or (ndsphg GT 0)

        IF nDSP EQ 0 THEN BEGIN
           PRINT,'Junk DSP data'
           RETURN
        ENDIF

     ENDIF

  ENDIF

  IF KEYWORD_SET(save_data_and_quit) THEN BEGIN

     iMomEphem = {fa_pos : TRANSPOSE(ion_dEF.fa_pos), $
                  fa_vel : TRANSPOSE(ion_dEF.fa_vel), $
                  alt    : ion_dEF.alt, $
                  mlt    : ion_dEF.mlt, $
                  ilat   : ion_dEF.ilat, $
                  B_model: TRANSPOSE(ion_dEF.B_model), $
                  Bfoot  : TRANSPOSE(ion_dEF.B_foot ), $
                  foot_lat : TRANSPOSE(ion_dEF.foot_lat ), $
                  foot_lng : TRANSPOSE(ion_dEF.foot_lng )}

     iMom = CREATE_STRUCT("ionUpJ",ionUpJ,TEMPORARY(iMomEphem),TEMPORARY(ionMomStruct))

     IF KEYWORD_SET(save_individual_data_products__only_ions) THEN BEGIN
        PRINT,"Saving " + indivFile + ' ...'
        SAVE,iMom,FILENAME=savesIndivDir+indivFile
        ;; EXIT
        RETURN
     ENDIF

     GET_DATA,'MAG_FLAGS',data=mag_flags

     IF ~KEYWORD_SET(quiet) THEN PRINT,"Getting dB field buffs ..."

     FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
     dbBufs = {start : strt_i, $
               stop  : stop_i}

     IF KEYWORD_SET(convert_B_gei_to_B_geo) THEN BEGIN

        nBMaal = N_ELEMENTS(dBEphem.time)

        coordDir = '/SPENCEdata/Research/database/temps/'
        GEO_MAG_filename = "tmpGEOMAGcoords_"+orbString+".sav"

        have_GEO_MAG = 0

        ;; Med dette får du
        ;;   GEO     = {ALT:GEOSph_arr[*,2], $
        ;;              LON:GEOSph_arr[*,1], $
        ;;              LAT:GEOSph_arr[*,0]}
        ;;
        ;;   MAG     = {ALT:MAGSph_arr[*,2], $
        ;;              LON:MAGSph_arr[*,1], $
        ;;              LAT:MAGSph_arr[*,0]}
        ;;
        ;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;  ;;make struct
        ;;  coords = {TIME   : times, $
        ;;            MAG    : MAG_arr, $
        ;;            GEO    : GEO_arr, $
        ;;            CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
        ;;            R_E    : R_E, $
        ;;            INFO   : "Components of FAST position vector in this struct are CARTESIAN MAG and GEO. The self-standing structs give FAST pos vec in SPHERICAL coords (lat, lon, R-R_E)", $
        ;;            ORIGINATING_ROUTINE:''}

        IF FILE_TEST(coordDir + GEO_MAG_filename) THEN BEGIN
           RESTORE,coordDir + GEO_MAG_filename
           have_GEO_MAG = N_ELEMENTS(coords.time) EQ nBMaal

           IF have_GEO_MAG THEN BEGIN

              tNames = STRUPCASE(TAG_NAMES(coords))

              have_GEO_MAG = (WHERE(tNames EQ "GEI2GEO_COORD"))[0] NE -1

           ENDIF

        ENDIF

        IF ~have_GEO_MAG THEN BEGIN

           JOURNAL__20190206__GET_GEI_GEO_BLAH_FOR_DB_AND_ION_STUFF, $
              HERE_ARE_MY_ORBITS=[orbit], $
              HERE_IS_MY_EPHEM=dBEphem

           RESTORE,coordDir + GEO_MAG_filename

        ENDIF

        PRINT,"Getting B in GEO coords ..."

        ;; Do the next stuff
        B_GEI = B_GEI.y
        B_GEO = B_GEI *0.D
        B_GEO_rArr = MAKE_ARRAY(nBMaal,VALUE=0.D,/DOUBLE)
        B_GEO_tArr = MAKE_ARRAY(nBMaal,VALUE=0.D,/DOUBLE)
        B_GEO_pArr = MAKE_ARRAY(nBMaal,VALUE=0.D,/DOUBLE)
        FOR jjj=0,nBMaal-1 DO BEGIN
           ;; B_GEO[jjj,*] = coords.GEI2GEO_vec[*,*,jjj] # TRANSPOSE(B_GEI[jjj,*])
           B_GEO[jjj,*] = coords.GEI2GEO_coord[*,*,jjj] # TRANSPOSE(B_GEI[jjj,*])

           GEOPACK_BCARSP_08, $
              coords.GEO[0,jjj],coords.GEO[1,jjj],coords.GEO[2,jjj], $ ;pos Vec in GEO coords
              B_GEO[jjj,0],B_GEO[jjj,1],B_GEO[jjj,2], $                ;B Vec in GEO coords
              B_GEO_r,B_GEO_theta,B_GEO_phi

           B_GEO_rArr[jjj] = B_GEO_r
           B_GEO_tArr[jjj] = B_GEO_theta
           B_GEO_pArr[jjj] = B_GEO_phi

        ENDFOR

        ;; STOP

        ;; TESTS
        ;; kompNav = ['x','y','z']
        ;; testInd = 0
        ;; PRINT,T2S(dBEphem.time[testInd])
        ;; PRINT,FORMAT='(A-5,TR5,A-8,TR5,A-8)', $
        ;;       "komp","GEI","GEO"
        ;; FOR jkj=0,2 DO PRINT,FORMAT='(A-5,TR5,F8.2,TR5,F8.2)', $
        ;;                      kompNav[jkj], $
        ;;                      B_GEI[testInd,jkj], $
        ;;                      B_GEO[testInd,jkj]

        ;; ;; Z-komponenter bør stemme
        ;; PRINT,MINMAX(B_GEI[*,2]-B_GEO[*,2])
        ;; magsGEI=SQRT(TOTAL(B_GEI^2.,2))
        ;; magsGEO=SQRT(TOTAL(B_GEO^2.,2))
        ;; PRINT,MINMAX(magsGEI-magsGEO)

        R_E              = 6371.2D ;In case coords struct doesn't have it, Earth radius in km, from IGRFLIB_V2.pro

        geoCoords = [[geo.alt+r_e],[geo.lon],[geo.lat]]

        dB = CREATE_STRUCT( $ ;'x',dB_fac.x, $
             'fac',dB_fac.y, $
             'fac_v',dB_fac_v.y, $
             ;; 'sc',dB_sc.y, $
             ;; 'B_gei',B_gei.y, $
             'B_geo',B_GEO, $
             'B_geo_r',B_GEO_rArr, $
             'B_geo_theta',B_GEO_tArr, $
             'B_geo_phi',B_GEO_pArr, $
             'mag_flags',mag_flags, $
             "fa_fields_bufs",dbBufs, $
             TEMPORARY(dBEphem), $
             ;; "fa_pos_geo",TRANSPOSE(coords.GEO), $
             "fa_pos_geo",TEMPORARY(geoCoords)) ;These are spherical, line above is Cartesian

     ENDIF ELSE BEGIN

        ;; dB_info = CREATE_STRUCT('x',dB_fac.x, $
        ;;                    'fac',dB_fac.y, $
        ;;                    'fac_v',dB_fac_v.y, $
        ;;                    'sc',{x:"",y:"",z:""})
        dB = CREATE_STRUCT('x',dB_fac.x, $
                           'fac',dB_fac.y, $
                           'fac_v',dB_fac_v.y, $
                           ;; 'sc',dB_sc.y, $
                           ;; 'B_gei',B_gei.y, $
                           'mag_flags',mag_flags, $
                           "fa_fields_bufs",dbBufs, $
                           TEMPORARY(dBEphem))

     ENDELSE

     IF KEYWORD_SET(save_individual_data_products__only_db_and_ions) THEN BEGIN
        PRINT,"Saving " + indivFile + ' ...'
        SAVE,iMom,dB,FILENAME=savesIndivDir+indivFile
        ;; EXIT
        RETURN
     ENDIF

     ;; dB = {x: dB_fac.x, $
     ;;       fac: dB_fac.y, $
     ;;       fac_v: dB_fac_v.y}

     eF = CREATE_STRUCT('x',eAlongV.x, $
                        'alongV',eAlongV.y, $
                        'nearB',eNearB.y)
     eFFit = CREATE_STRUCT('x',eFitAlongV.x, $
                           'alongV',eFitAlongV.y, $
                           'nearB',eFitNearB.y)

     ;; eF_spinPlane

     eMom = CREATE_STRUCT('lc',TEMPORARY(eMomStruct_lcAngle), $
                          'all',TEMPORARY(eMomStruct_allAngle), $
                          eMomEphem)

     dsp = data
     GET_FA_ORBIT,dsp.time,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=dspEphem
     ;; dsp = CREATE_STRUCT(dsp,dspEphem)

     ;; Get rid of time tag in dspEphem
     dspEphem = {orbit : dspEphem.orbit, $
                 fa_pos : dspEphem.fa_pos, $
                 alt : dspEphem.alt, $
                 ilat : dspEphem.ilat, $
                 ilng : dspEphem.ilng, $
                 mlt : dspEphem.mlt, $
                 fa_vel : dspEphem.fa_vel, $
                 bfoot : dspEphem.bfoot, $
                 lat : dspEphem.lat, $
                 lng : dspEphem.lng, $
                 flat : dspEphem.flat, $
                 flng : dspEphem.flng, $
                 b_model : dspEphem.b_model}

     PRINT,"Saving " + indivFile + ' ...'
     SAVE,iMom,dB,eF,eFFit,eF_spinPlane,eMom,dsp,FILENAME=savesIndivDir+indivFile
     RETURN
  ENDIF

  tmp_i = WHERE((data.time GE (je_tBounds[0]-tBuf)) AND $
                (data.time LE (je_tBounds[1]+tBuf)),nTmp)
  IF nTmp GT 1 THEN BEGIN

     nDSP = nTmp

     data   = {x:data.time[tmp_i], y:data.comp1[tmp_i,*], v:data.yaxis}
     STORE_DATA,'DSP_V5-V8', DATA={x:data.x, $
                                   y:ALOG10(data.y), $
                                   v:data.v}
     dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-14,-4], $
               ytitle:'AC E 55m!C!C(kHz)', ylog:1, $
               ztitle: '(V/m)!U2!N/Hz', panel_size:2}
     STORE_DATA,'DSP_V5-V8', dlimit=dlimit
     OPTIONS,'DSP_V5-V8','x_no_interp',1
     OPTIONS,'DSP_V5-V8','y_no_interp',1

     ;;  look for big jumps in time - blank these

     dt = data.x[1:*]-data.x[0:*]
     ntimes=N_ELEMENTS(data.x)
     bg = where (dt GT 300, ng)
     if (ng GT 0) THEN BEGIN
        bbb = bg-1
        if (bbb[0] lt 0) THEN bbb[0] = 0
        add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
        flag_dat = fltarr(ng*2)+!values.f_nan
        new_tag = [data.x,add_tag]
        tsort = sort(new_tag-new_tag[0])
        nvec=N_ELEMENTS(data.y)/ntimes
        new_dat = fltarr(N_ELEMENTS(new_tag),nvec)
        for nv = 0,nvec-1 do BEGIN
           new_dat[*,nv] = [data.y[*,nv],flag_dat]
           new_dat[*,nv] = new_dat[tsort,nv]
        endfor
        DATA={x:new_tag[tsort],y:new_dat,v:data.v}
        STORE_DATA,'DSP_V5-V8',DATA=data
     endif

     if (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=['DSP_V5-V8'] else tPlt_vars=['DSP_V5-V8',tPlt_vars]

     if (keyword_set(screen_plot)) THEN BEGIN
        LOADCT2,ctNum
        tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     endif

     ;;Now integrate
     ;; data.y    = data.y
     data.v   *= 1000.

     ;; 20180728
     ;; These lines were supposed to skip the integration over the 0 Hz channel,
     ;; But the tmpF_i line does it already!
     ;; nNRG = N_ELEMENTS(data.v)
     ;; data   = {x:data.x, y:data.y[*,1:nNRG-1], v:data.v[1:nNRG-1]}

     integData = MAKE_ARRAY(N_ELEMENTS(data.x),VALUE=0.)

     tmpF_i = LINDGEN(N_ELEMENTS(data.v)-1)+1
     ;; tmpF_i = LINDGEN(N_ELEMENTS(data.v))
     FOR m=0,N_ELEMENTS(data.x)-1 DO BEGIN

        ;; 20180728
        ;; Bothering with finiteness seems to do us no good, to my surprise
        ;; finiteii = WHERE(FINITE(data.y[m,tmpF_i]),nFinite,/NULL)

        ;;"Intergrate," as some have it, and apply test
        ;; integData[m] = INT_TABULATED(data.v[tmpF_i[finiteii]],data.y[m,tmpF_i[finiteii]])
        integData[m] = INT_TABULATED(data.v[tmpF_i],data.y[m,tmpF_i])
     ENDFOR

     ;;Get rid of the square so that units are V/m
     integData = SQRT(integData)

     data      = {x:data.x, $
                  y:integData}

     PRINT,'SMOOTHDSP'
     DSP = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           data, $
           /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
           /USE_DOUBLE_STREAKER, $
           /NO_SEPARATE_DC_AC, $
           ONESEC_TS=tS_1s)

     STORE_DATA,'DSP_integ',DATA={x:DSP.x,y:DSP.y}
     ;; STORE_DATA,'DSP_integ',DATA={x:DSP.x,y:dsp.DC+dsp.AC}
     dlimit = {ystyle:1, yrange:[0.0,0.05], $
               ytitle:'ELF Amplitude (V/m)', $
               panel_size:3}
     STORE_DATA,'DSP_integ', dlimit=dlimit
     OPTIONS,'DSP_integ','x_no_interp',1
     OPTIONS,'DSP_integ','y_no_interp',1


  ENDIF ELSE BEGIN

  ENDELSE

  ;;Now loop over stuff

  structList             = LIST()
  FOR jj=0,number_of_intervals-1 DO BEGIN

     itvlString     = STRCOMPRESS(jj,/REMOVE_ALL)

     tmpPlotName    = outPlotName + '__itvl_' + itvlString

     t1 = time_ranges[jj,0]
     t2 = time_ranges[jj,1]

     tmp_je_indices = time_range_indices[jj,*]

     ;;Clean up based on ILAT
     GET_DATA,'ILAT',DATA=ilat
     IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF
     IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF

     ;;Make sure we have data where we want it.
     keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        ;; PRINT,'No data above min ILAT. Out!'
        ;; RETURN
        PRINT,'No data above min ILAT. Skipping this interval ...'
        CONTINUE
     ENDIF

     ;;Trim time series, if necessary
     IF nKeep LT N_ELEMENTS(ionMomStruct.time) THEN BEGIN

        closest1 = MIN(ABS(t1-ionMomStruct.time[keep]),tmpII_t1)
        closest2 = MIN(ABS(t2-ionMomStruct.time[keep]),tmpII_t2)

        ;;If more than 30 s from previous mark, we're in doubt
        IF (closest1 GT 600) OR (closest2 GT 600) THEN BEGIN
           PRINT,'Either t1 or t2 is more than 10 minutes from the previous mark ...'
           PRINT,'Questionable, indeed. Skipping this interval ...'
           CONTINUE
        ENDIF

        IF tmpII_t1 EQ tmpII_t2 THEN BEGIN
           PRINT,'t1 and t2 are the same!'
           PRINT,'Questionable, indeed. Skipping this interval ...'
           CONTINUE
        ENDIF

        t1             = ionMomStruct.time[keep[tmpII_t1]]
        t2             = ionMomStruct.time[keep[tmpII_t2]]
        tmp_je_indices = [keep[tmpII_t1],keep[tmpII_t2]]

     ENDIF

     je_tmp_tBounds    = [t1,t2]

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))


     ;; Step 3 - Poynting flux
     GET_DATA,'dB_fac_v',data=magData
     GET_DATA,eAV_variable,data=eAlongV

     mintime = MIN(ABS(je_tmp_tBounds[0]-magData.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-magData.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable mag data here. Skipping interval ...'
        CONTINUE
     ENDIF

     ;;   From UCLA_MAG_DESPIN:
     ;;   "Field-aligned velocity-based coordinates defined as: "
     ;;   "z (ind 2)-along B,
     ;;    y (ind 1)-cross track (BxV),
     ;;    x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)
     magB = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,2]}
     magp = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,1]}
     magv = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,0]}
     nMag = N_ELEMENTS(magp.x)

     ;;E-field trim
     mintime = MIN(ABS(je_tmp_tBounds[0]-eAlongV.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-eAlongV.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable eAlongV data here. Skipping interval ...'
        CONTINUE
     ENDIF

     eAlongV  = {x:eAlongV.x[ind1:ind2], $
                 y:eAlongV.y[ind1:ind2]}
     nEAlongV = N_ELEMENTS(eAlongV.x)

     ;;DSP trim
     ;;And DSP
     mintime = MIN(ABS(je_tmp_tBounds[0]-dsp.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-dsp.x),ind2)


     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable DSP data here. Skipping interval ...'
        CONTINUE
     ENDIF

     ;; tmpDSP = {x:DSP.x[ind1:ind2], $
     ;;           DC:DSP.DC[ind1:ind2], $
     ;;           AC:DSP.AC[ind1:ind2]}
     tmpDSP = {x:DSP.x[ind1:ind2], $
               y:DSP.y[ind1:ind2]}
     nDSP   = N_ELEMENTS(tmpDSP.x)

     ;; magx = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,0])}

     ;; magy = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,2])}

     ;; magz = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,magInd])}

     dBv = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magv, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)
     dBB = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magB, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)
     dBp = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magp, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)

     eAV = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           eAlongV, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)

     IF KEYWORD_SET(include_E_near_B) THEN BEGIN

        GET_DATA,eNB_variable,DATA=eNearB

        mintime = MIN(ABS(je_tmp_tBounds[0]-eNearB.x),ind1)
        mintime = MIN(ABS(je_tmp_tBounds[1]-eANearB.x),ind2)

        IF ind1 EQ ind2 THEN BEGIN
           PRINT,'No usable eNearB data here. Excluding eNearB ...'
           include_E_near_B = 0
           full_pFlux       = 0
        ENDIF ELSE BEGIN

           eNearB  = {x:eNearB.x[ind1:ind2], $
                      y:eNearB.y[ind1:ind2]}
           nENearB = N_ELEMENTS(eNearB.x)

           eNB     = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                     eNearB, $
                     INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                     ONESEC_TS=tS_1s)

        ENDELSE

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Poynting fluxes!

     IF KEYWORD_SET(decimate_eb_calc_pFlux) THEN BEGIN

        ;;In this case, use decimated E and B to calc pFlux

        IF KEYWORD_SET(full_pFlux) THEN BEGIN

           pFBHigh = dBp.AC*eAV.AC/mu_0 ;Poynting flux along B
           pFPHigh = (eNB.AC*dBv.AC - $
                      1.*dBB.AC*eAV.AC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVHigh = (-1.)*eNB.AC*dBp.AC/mu_0

           pFBLow  = dBp.DC*eAV.DC/mu_0 ;Poynting flux along B
           pFPLow  = (eNB.DC*dBv.DC - $
                      1.*dBB.DC*eAV.DC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVLow  = (-1.)*eNB.DC*dBp.DC/mu_0

        ENDIF ELSE BEGIN

           pFBHigh =       dBp.AC*eAV.AC/mu_0 ;Poynting flux along B
           pFPHigh = (-1.)*dBB.AC*eAV.AC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

           pFBLow =       dBp.DC*eAV.DC/mu_0 ;Poynting flux along B
           pFPLow = (-1.)*dBB.DC*eAV.DC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ENDELSE

        pFB       = {AC: TEMPORARY(pFBHigh), $
                     DC: TEMPORARY(pFBLow)}
        pFP       = {AC: TEMPORARY(pFPHigh), $
                     DC: TEMPORARY(pFPLow)}

        IF KEYWORD_SET(full_pFlux) THEN BEGIN
           pFV    = {AC: TEMPORARY(pFVHigh), $
                     DC: TEMPORARY(pFVLow)}
        ENDIF

     ENDIF ELSE BEGIN

        ;;In this case calc pFlux from E and B field before decimation, and THEN decimate

        ;;Want to waste tons of time? Keep going on this code
        ;;Time series are already trimmed, each var is prefixed by 'n'

        ;;WASTE SPENCE'S TIME BEGIN

        ;; nE_to_nB = FIX(FLOAT(nEAlongV)/nMag)
        ;; gjordet  = 1
        ;; CASE 1 OF
        ;;    (nE_to_nB LT 1): BEGIN
        ;;       mag_is_big = 1
        ;;       ;; bigVar   = nMag
        ;;       ;; lilVar   = nEAlongV
        ;;       ;; bigTInds = VALUE_CLOSEST2(magv.x,eAlongV.x,/CONSTRAINED)
        ;;       bigT     = magv.x
        ;;       lilT     = eAlongV.x
        ;;       bigV     = magv.y
        ;;       lilV     = eAlongV.y
        ;;    END
        ;;    (nE_to_nB EQ 1): BEGIN
        ;;       gjordet = 0
        ;;    END
        ;;    (nE_to_nB GT 1): BEGIN
        ;;       mag_is_big = 0
        ;;       ;; bigVar   = nEAlongV
        ;;       ;; lilVar   = nMag
        ;;       ;; bigTInds = VALUE_CLOSEST2(eAlongV.x,magv.x,/CONSTRAINED)
        ;;       bigT     = eAlongV.x
        ;;       lilT     = magv.x
        ;;       bigV     = eAlongV.y
        ;;       lilV     = magv.y
        ;;    END
        ;; ENDCASE

        ;; IF gjordet THEN BEGIN

        ;;    nBigT = N_ELEMENTS(bigT)
        ;;    nLilT = N_ELEMENTS(lilT)

        ;;    WHILE FIX(FLOAT(nBigT)/nLilT) GE 2 THEN BEGIN




        ;;       bigTInds = VALUE_CLOSEST2(bigT,lilT,/CONSTRAINED)

        ;;       nBigT = N_ELEMENTS(bigT)
        ;;       nLilT = N_ELEMENTS(lilT)

        ;;    ENDWHILE

        ;; ENDIF

        ;;WASTE SPENCE'S TIME END

        ;;But yes, we must align time series
        eAlongVtoMag = DATA_CUT(eAlongV,magp.x)

        IF KEYWORD_SET(full_pFlux) THEN BEGIN

           eNearBtoMag = DATA_CUT(eNearB,magp.x)

           pFluxB = {x:magp.x, $
                     y:magp.y*eAlongVtoMag/mu_0} ;Poynting flux along B
           pFluxP = {x:magp.x, $
                     y:(eNearBtoMag*magv.y - $
                        1.*magB.y*eAlongVtoMag)/mu_0} ;Poynting flux perp to B and to (Bxv)xB

           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFluxV = {x:magp.x, $
                     y:(-1.)*eNearBtoMag*magp.y/mu_0}

        ENDIF ELSE BEGIN

           pFluxB = {x:magp.x, $
                     y:magp.y*eAlongVtoMag/mu_0}      ;Poynting flux along B
           pFluxP = {x:magp.x, $
                     y:(-1.)*magB.y*eAlongVtoMag/mu_0} ;Poynting flux perp to B and to (Bxv)xB
           ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        ENDELSE

        ;;Now separate into low and high frequencies
        pFB       = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                    pFluxB, $
                    INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                    ONESEC_TS=tS_1s)
        pFP       = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                    pFluxP, $
                    INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                    ONESEC_TS=tS_1s)

        pFB  = {DC:pFB.DC, $
                AC:pFB.AC}
        pFP  = {DC:pFP.DC, $
                AC:pFP.AC}

        IF KEYWORD_SET(full_pFlux) THEN BEGIN

           pFV  = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                    pFluxV, $
                    INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                    ONESEC_TS=tS_1s)
           pFV  = {DC:pFV.DC, $
                   AC:pFV.AC}

        ENDIF

     ENDELSE

     pFB.AC *= 1e-9            ;Junk that nano prefix in nT
     pFP.AC *= 1e-9

     pFB.DC *= 1e-9             ;Junk that nano prefix in nT
     pFP.DC *= 1e-9

     IF KEYWORD_SET(full_pFlux) THEN BEGIN

        pFV.AC *= 1e-9
        pFV.DC  *= 1e-9

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;AC Poynting flux

     tField = dBp.x
     ;; doDat  = pFBHigh
     doDat  = pFB.AC
     ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
     ;;    tField = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.0,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     tField   = tField[good_i]
     doDat    = ALOG10(ABS(doDat[good_i]))


     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxHigh',DATA=tmp
     dLimit = {spec:0, $
               ystyle:1, $
               ytitle:'SFlux Wave!C[0.125-0.5 Hz]!C(mW/m!U2!N)', $
               yticks:4, $
               ylog:0, $
               yrange:[-5,2], $
               ytickv:[-4,-2,0,2], $
               ytickname:['10!U-4!N', $
                          '10!U-2!N','10!U0!N','10!U2!N'], $
               ;; ylog:1, $
               ;; yrange:[1e-4,1e2], $
               ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $
               ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
               ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               colors:normColorI ,$
               panel_size:3}
     STORE_DATA,'pFluxHigh',DLIMITS=dLimit
     OPTIONS,'pFluxHigh','x_no_interp',1
     OPTIONS,'pFluxHigh','y_no_interp',1

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = ['pFluxHigh'] $
     ELSE tPlt_vars = ['pFluxHigh',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,ctNum
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;DC Poynting flux

     tField = dBp.x
     ;; doDat  = pFBLow
     doDat  = pFB.DC
     ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
     ;;    tField = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     tField   = tField[good_i]
     ;; doDat  = ABS(doDat[good_i])
     doDat  = ALOG10(ABS(doDat[good_i]))

     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxLow',DATA=tmp

     dLimit = {spec:0, $
               ystyle:1, $
               ytitle:'SFlux Wave!C[< 0.125 Hz]!C(mW/m!U2!N)', $
               yticks:7, $
               ylog:0, $
               yrange:[-4,2], $
               ytickv:[-4,-3,-2,-1,0,1,2], $
               ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
                          '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               ;; ylog:1, $
               ;; yrange:[1e-4,1e2], $
               ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $
               ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
               ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               colors:normColorI,$
               panel_size:3}
     STORE_DATA,'pFluxLow',DLIMITS=dLimit
     OPTIONS,'pFluxLow','x_no_interp',1
     OPTIONS,'pFluxLow','y_no_interp',1

     IF (n_elements(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxLow'] $
     ELSE tPlt_vars=['pFluxLow',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,ctNum
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     ENDIF

; Step 4 - Electron junk

     ;; GET_DATA,'Je',DATA=tmp &
     tmp = {x:eMomStruct_allAngle.time,y:eMomStruct_allAngle.j}
     STORE_DATA,'Je',DATA=tmp

     ll = 1
     name = names_2dt[ll]
     title = titls_2dt[ll]
     lims = lims_2dt[*,ll]

     CASE N_ELEMENTS(lims) OF
        2: BEGIN
           YLIM,name,lims[0],lims[1]
        END
        3: BEGIN
           YLIM,name,lims[0],lims[1],lims[2] ; set y limits
        END
        ELSE:
     ENDCASE

     OPTIONS,name,'ytitle',title ; set y title
     OPTIONS,name,'panel_size',3 ; set panel size
     OPTIONS,name,'x_no_interp',1
     OPTIONS,name,'y_no_interp',1

     ;; tFlux    = tmp.x
     ;; doDat    = tmp.y
     ;; IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmpJe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
              tmp, $
              /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
              /NO_SEPARATE_DC_AC, $
              /USE_DOUBLE_STREAKER, $
              ONESEC_TS=tS_1s)
        ;; tmp.y = SMOOTH(tmp.y,5)
        ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
        ;; tFlux = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(tmpJe.y) AND tmpJe.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     ;; tmpJe    = {x:tmpJe.x[good_i],y:ALOG10(tmpJe.y[good_i])}
     ;; tFlux    = tFlux[good_i]
     ;; doDat    = ALOG10(doDat[good_i])

     ;; STORE_DATA,'Je_tmp',DATA={x:tFlux,y:doDat}
     STORE_DATA,'Je_tmp',DATA={x:tmpJe.x[good_i],y:ALOG10(tmpJe.y[good_i])}
     YLIM,'Je_tmp',6,12,0                                                  ; set y limits
     OPTIONS,'Je_tmp','ytitle','Downgoing Elec.!CFlux!C#/cm!U2!N-s)' ; set y title
     OPTIONS,'Je_tmp','panel_size',3                                       ; set panel size
     OPTIONS,'Je_tmp','yticks',6                                           ; set y-axis labels
     OPTIONS,'Je_tmp','ytickv',[6,7,8,9,10,11,12]                         ; set y-axis labels
     OPTIONS,'Je_tmp','ytickname',['10!U6!N','10!U7!N','10!U8!N','10!U9!N','10!U10!N', $
                                   '10!U11!N','10!U12!N'] ; set y-axis labels
     OPTIONS,'Je_tmp','ynozero',1
     OPTIONS,'Je_tmp','ystyle',1
     ;; dLimit = {spec:0, $
     ;;           ystyle:1, $
     ;;           ytitle:'Downgoing Elec.!CFlux!C#/cm!U2!N-s)', $
     ;;           yticks:6, $
     ;;           ylog:0, $
     ;;           yrange:[6,12], $
     ;;           ytickv:[6,7,8,9,10,11,12], $ ; set y-axis labels
     ;;           ytickname:['10!U6!N','10!U7!N','10!U8!N','10!U9!N','10!U10!N', $
     ;;                      '10!U11!N','10!U12!N'], $
     ;;           colors:normColorI,$
     ;;           panel_size:3}
     ;; STORE_DATA,'Je_tmp',DLIMITS=dLimit
     OPTIONS,'Je_tmp','psym',psym_ptcl ;period symbol
     OPTIONS,'Je_tmp','symsize',symsize_ptcl ;period symbol

; Electron energy flux

     ;; t           = 0.0D
     ;; tmp         = GET_FA_EES_C(t,/EN)
     ;; IF tmp.valid EQ 0 THEN BEGIN
     ;;    PRINT,'Junk'
     ;;    RETURN
     ;; ENDIF
     ;; last_index  = LONG(tmp.index)
     ;; t1          = 0.0D
     ;; t2          = 0.0D
     ;; temp        = GET_FA_EES(t1,INDEX=0.0D)
     ;; temp        = GET_FA_EES(t2,INDEX=DOUBLE(last_index))

     ;;Did this up top
     ;; GET_2DT,'je_2d_fs','fa_ees_c',name='JEe',t1=t1,t2=t2,energy=energy_electrons
     ;; GET_DATA,'JEe',DATA=tmp
     tmp = {x:eMomStruct_allAngle.time,y:eMomStruct_allAngle.je}
     STORE_DATA,'JEe',DATA=tmp

     ll = 0
     name = names_2dt[ll]
     title = titls_2dt[ll]
     lims = lims_2dt[*,ll]

     CASE N_ELEMENTS(lims) OF
        2: BEGIN
           YLIM,name,lims[0],lims[1]
        END
        3: BEGIN
           YLIM,name,lims[0],lims[1],lims[2] ; set y limits
        END
        ELSE:
     ENDCASE

     OPTIONS,name,'ytitle',title ; set y title
     OPTIONS,name,'panel_size',3 ; set panel size
     OPTIONS,name,'x_no_interp',1
     OPTIONS,name,'y_no_interp',1

     ;; tFlux    = tmp.x
     ;; doDat    = tmp.y
     ;; IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmpJEe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
              tmp, $
              /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
              /NO_SEPARATE_DC_AC, $
              /USE_DOUBLE_STREAKER, $
              ONESEC_TS=tS_1s)
        ;; tmp.y = SMOOTH(tmp.y,5)
        ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
        ;; tFlux = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(tmpJEe.y) AND tmpJEe.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     ;; tmpJEe    = {x:tmpJEe.x[good_i],y:ALOG10(tmpJEe.y[good_i])}
     ;; tFlux    = tFlux[good_i]
     ;; doDat    = ALOG10(doDat[good_i])

     ;; STORE_DATA,'JEe_tmp',DATA={x:tFlux,y:doDat}
     STORE_DATA,'JEe_tmp',DATA={x:tmpJEe.x[good_i],y:ALOG10(tmpJEe.y[good_i])}
     YLIM,'JEe_tmp',-5,1,0                                                  ; set y limits
     OPTIONS,'JEe_tmp','ytitle','Downgoing Elec.!CEnergy Flux!CmW/(m!U2!N)' ; set y title
     OPTIONS,'JEe_tmp','panel_size',3                                       ; set panel size
     OPTIONS,'JEe_tmp','yticks',6                                           ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickv',[-5,-4,-3,-2,-1,0,1]                        ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickname',['10!U-5!N','10!U-4!N','10!U-3!N', $
                                '10!U-2!N','10!U-1!N','10!U0!N','10!U1!N'] ; set y-axis labels
     OPTIONS,'JEe_tmp','x_no_interp',1
     OPTIONS,'JEe_tmp','y_no_interp',1
     OPTIONS,'JEe_tmp','ystyle',1
     OPTIONS,'JEe_tmp','psym',psym_ptcl ;period symbol
     OPTIONS,'JEe_tmp','symsize',symsize_ptcl ;period symbol

; Step 5 - Ion flux

     ;; GET_DATA,'Ji',DATA=tmp
     tmp = ionupJ

     IF N_ELEMENTS(tmp) EQ 0 THEN STOP

     tmpJi = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
             tmp, $
             /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
             /NO_SEPARATE_DC_AC, $
             /USE_DOUBLE_STREAKER, $
             ONESEC_TS=tS_1s)

     IF SIZE(tmpJi,/TYPE) NE 8 THEN BEGIN
        PRINT,"Mislyktes fordi det er ingen identifisert ion-utstrømming her"
        PRINT,"Tilbake ..."
        RETURN
     ENDIF

     ;;Make all upgoing, pre-log
     good_i   = WHERE(FINITE(tmpJi.y) AND tmpJi.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     STORE_DATA,'Ji_tmp',DATA={x:tmpJi.x[good_i],y:ALOG10(tmpJi.y[good_i])}
     YLIM,'Ji_tmp',6,10,0                                               ; set y limits
     OPTIONS,'Ji_tmp','ytitle','Upward Ion!CNumber Flux!C(#/cm!U2!N-s)' ; set y title
     OPTIONS,'Ji_tmp','panel_size',3                                    ; set panel size
     OPTIONS,'Ji_tmp','yticks',4                                        ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickv',[6,7,8,9,10]                         ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickname',['10!U6!N', $
                                   '10!U7!N','10!U8!N','10!U9!N','10!U10!N'] ; set y-axis labels
     OPTIONS,'Ji_tmp','ynozero',1
     OPTIONS,'Ji_tmp','psym',psym_ptcl ;period symbol
     OPTIONS,'Ji_tmp','symsize',symsize_ptcl ;period symbol

; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south also change plot title

     GET_DATA,'LAT',data=data

     if (n_elements(data.y) le 0) then return

     bb = where (data.y gt 10,nn)
     if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

     bb = where (data.y lt -10,nn)
     if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

     hemisph = GETENV('FAST_ORBIT_HEMISPHERE')

     GET_DATA,'ORBIT',data=data
     nn = N_ELEMENTS(data.y)/2
     orbit = data.y(nn)
     orbit_lab = STRCOMPRESS(STRING(orbit,FORMAT="(i5.4)"),/REMOVE_ALL)
     tplot_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

     tPlt_vars=['dB_fac_v','pFluxHigh','pFluxLow','JEe_tmp','Je_tmp','Ji_tmp']

     IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotDirSuff
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+tmpPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+tmpPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO


           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE

        ;; CASE 1 OF
        ;;    KEYWORD_SET(plot_north): BEGIN
        ;;       tLims = tlimit_north
        ;;    END
        ;;    KEYWORD_SET(plot_south): BEGIN
        ;;       tLims = tlimit_south
        ;;    END
        ;;    ELSE: BEGIN
        ;;       tLims = je_tmp_tBounds
        ;;    END
        ;; ENDCASE

        LOADCT2,ctNum
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tmp_tBounds


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

     ENDIF

     IF KEYWORD_SET(Strangeway_2005_Fig3_plot) THEN BEGIN

        tmpPlotName2    = outPlotName + '-Sway2005Style' + '__itvl_' + itvlString

        ;;;;;;;;;;;;;;;;;;;;
        varName = eAV_variable + "tmp"

        finite = WHERE(FINITE(eAV.DC),nFinite)
        yRange = nFinite GT 0 ? [MIN(eAV.DC[finite]) > (-300),MAX(eAV.DC[finite]) < 300] : [-100,200]

        yTitle = eAV_variable EQ 'EFIT_ALONG_V' ? $
                 'EFIT ALONG V!Dsc!N!C!C[DC] (mV/m)' : $
                 'E ALONG V!Dsc!N!C!C[DC] (mV/m)'
        dlimit = {spec:0, ystyle:1, yrange:yRange, $
                  ytitle:yTitle, $
                  panel_size:3}
        STORE_DATA,varName,DATA={x: [[eAV.x],[eAV.x]], $
                                 y: [[MAKE_ARRAY(N_ELEMENTS(eAV.x),VALUE=0)], $
                                     [eAV.DC]]},DLIMIT=dlimit
        OPTIONS,varName,'ytitle','E along V!Dsc!N!C!C[DC] (mV/m)'
        OPTIONS,varName,'tplot_routine','mplot'
        OPTIONS,varName,'colors',[normColorI,normColorI]

        ;;;;;;;;;;;;;;;;;;;;
        varName = 'dB_fac_interp'

        finite = WHERE(FINITE(dBp.DC),nFinite)
        yRange = nFinite GT 0 ? [MIN(dBp.DC[finite]) > (-1500),MAX(dBp.DC[finite]) < 2000] : [-600,800]

        dLimit = {spec:0, ystyle:1, yrange:yRange, $
                  ytitle:'dB Perp.!C!C[DC] (nT)', $
                  panel_size:3}
        STORE_DATA,varName,DATA={x: [[dBp.x],[dBp.x]], $
                                 y: [[MAKE_ARRAY(N_ELEMENTS(dBp.x),VALUE=0)], $
                                     [dBp.DC]]},DLIMITS=dLimit
        OPTIONS,varName,'colors',[normColorI,normColorI]
        OPTIONS,varName,'tplot_routine','mplot'
        OPTIONS,varName,'colors',[normColorI,normColorI]

        ;;;;;;;;;;;;;;;;;;;;
        varName = 'pFlux'

        finite = WHERE(FINITE(pFB.DC),nFinite)
        yRange = nFinite GT 0 ? [MIN(pFB.DC[finite]) > (-100) ,MAX(pFB.DC[finite]) < 150] : [-20,80]

        STORE_DATA,varName, $
                   DATA={x:[[dBp.x],[dBp.x]], $
                         y:[ $
                   [MAKE_ARRAY(N_ELEMENTS(dBp.x),VALUE=0.)], $
                   [pFB.DC] $
                           ]}, $
                   DLIMIT={spec:0, ystyle:1, yrange:yRange, $
                           ytitle:'Poynting Flux!C!C[DC] (mW/m!U2!N)', $
                           panel_size:3}
        OPTIONS,varName,'colors',[normColorI,normColorI]

        ;;;;;;;;;;;;;;;;;;;;
        varName='tmpJe'

        finite = WHERE(FINITE(tmpJe.y),nFinite)
        yRange = nFinite GT 0 ? [MIN(tmpJe.y[finite]) > (-1.e10),MAX(tmpJe.y[finite]) < 3.e10] : [-5.e9,1.5e10]

        STORE_DATA,varName,DATA=tmpJe
        YLIM,varName,yRange[0],yRange[1],0                      ; set y limits
        OPTIONS,varName,'ytitle','Electron Flux!C#/(cm!U2!N-s)' ; set y title
        OPTIONS,varName,'panel_size',3                          ; set panel size

        ;;;;;;;;;;;;;;;;;;;;
        varName='tmpJEe'

        finite = WHERE(FINITE(tmpJEe.y),nFinite)
        yRange = nFinite GT 0 ? [MIN(tmpJEe.y[finite]) > (-50),MAX(tmpJEe.y[finite]) < 50] : [-1,6]

        STORE_DATA,varName,DATA=tmpJEe
        YLIM,varName,yRange[0],yRange[1],0                      ; set y limits
        OPTIONS,varName,'ytitle','Electron!CEnergy Flux!CmW/(m!U2!N)' ; set y title
        OPTIONS,varName,'panel_size',3                                ; set panel size

        ;;;;;;;;;;;;;;;;;;;;
        varName='tmpJi'

        finite = WHERE(FINITE(tmpJi.y),nFinite)
        yRange = nFinite GT 0 ? [MIN(tmpJi.y[finite]) > (-3.e9),MAX(tmpJi.y[finite]) < 3.e10] : [-1.e9,6.e9]

        STORE_DATA,varName,DATA=tmpJi
        YLIM,varName,yRange[0],yRange[1],0                      ; set y limits
        OPTIONS,varName,'ytitle','Ion Flux!C#/(cm!U2!N-s)' ; set y title
        OPTIONS,varName,'panel_size',3                     ; set panel size

        tPlt_vars=[eAV_variable+"tmp",'dB_fac_interp','pFlux','tmpJe','tmpJEe','DSP_integ','tmpJi']

        IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


           IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
              SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotDirSuff
           ENDIF

           IF KEYWORD_SET(save_png) THEN BEGIN
              CGPS_OPEN, plotDir+tmpPlotName2+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
           ENDIF ELSE BEGIN
              IF KEYWORD_SET(save_ps) THEN BEGIN

                 POPEN,plotDir+tmpPlotName2,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
                 DEVICE,/PALATINO


              ENDIF ELSE BEGIN
                 WINDOW,0,XSIZE=600,YSIZE=800
              ENDELSE
           ENDELSE

           ;; CASE 1 OF
           ;;    KEYWORD_SET(plot_north): BEGIN
           ;;       tLims = tlimit_north
           ;;    END
           ;;    KEYWORD_SET(plot_south): BEGIN
           ;;       tLims = tlimit_south
           ;;    END
           ;;    ELSE: BEGIN
           ;;       tLims = je_tmp_tBounds
           ;;    END
           ;; ENDCASE

           LOADCT2,ctNum
           TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tmp_tBounds


           IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
              PCLOSE
           ENDIF ELSE BEGIN

           ENDELSE

        ENDIF

     ENDIF

     ;; IF ~KEYWORD_SET(full_pFlux) THEN BEGIN
     ;;    pFVLow  = MAKE_ARRAY(N_ELEMENTS(pFBLow),/FLOAT)
     ;;    pFVHigh = MAKE_ARRAY(N_ELEMENTS(pFBHigh),/FLOAT)
     ;; ENDIF ELSE BEGIN
     ;;    pFLuxStruct = CREATE_STRUCT(pFluxStruct, $
     ;;                                'v',{x:eAV.x,DC:pFVLow,AC:pFVHigh}, $
     ;;                                'full_pFlux',KEYWORD_SET(full_pFlux))
     ;; ENDELSE

     IF ~KEYWORD_SET(include_E_near_B) THEN BEGIN
        eNB = eAV
        eNB.DC[*] = 0.
        eNB.AC[*] = 0.
     ENDIF

     ;;If the B structs have a common time series, only dBp keeps the x member of its struct
     ;; B_has_common_TS = ARRAY_EQUAL(dBp.x,dBv.x) AND ARRAY_EQUAL(dBp.x,dBB.x) AND ARRAY_EQUAL(dBv.x,dBB.x)
     B_has_common_TS = 1 ;Of COURSE B has a common TS!
     IF B_has_common_TS THEN BEGIN

        dBp  = {x:dBp.x, $
                DC:dBp.DC, $
                AC:dBp.AC, $
                common_ts:1B}

        dBv  = {DC:dBv.DC, $
                AC:dBv.AC}

        dBB  = {DC:dBB.DC, $
                AC:dBB.AC}

     ENDIF

     ;;If the E structs have a common time series, only dBp keeps the x member of its struct
     ;; E_has_common_TS = ARRAY_EQUAL(eAV.x,eNB.x) AND ARRAY_EQUAL(eAV.x,tmpDSP.x) AND ARRAY_EQUAL(eNB.x,tmpDSP.x)
     E_has_common_TS = ARRAY_EQUAL(eAV.x,eNB.x) AND ARRAY_EQUAL(eAV.x,tmpDSP.x) AND ARRAY_EQUAL(eNB.x,tmpDSP.x)
     IF E_has_common_TS THEN BEGIN

        eAV     = {x:eAV.x, $
                   DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

        eNB     = {DC:eNB.DC, $
                   AC:eNB.AC}

        ;; tmpDSP  = {DC:tmpDSP.DC, $
        ;;            AC:tmpDSP.AC}
        tmpDSP  = {y:tmpDSP.y}

     ENDIF ELSE BEGIN

        ;;See if DSP is messing things up
        IF ( N_ELEMENTS(tmpDSP.x) EQ ( N_ELEMENTS(eAV.x) + 1 ) ) AND $
           ARRAY_EQUAL(eAV.x,eNB.x) THEN BEGIN

           IF ARRAY_EQUAL(eAV.x,tmpDSP.x[0:-2]) THEN BEGIN
              E_has_common_TS     = 1
              ;; tmpDSP              = {DC:tmpDSP.DC[0:-2], $
              ;;                        AC:tmpDSP.AC[0:-2]}
              tmpDSP              = {y:tmpDSP.y[0:-2]}
           ENDIF ELSE BEGIN

              IF ARRAY_EQUAL(eAV.x,tmpDSP.x[1:-1]) THEN BEGIN
                 E_has_common_TS  = 1
                 ;; tmpDSP           = {DC:tmpDSP.DC[1:-1], $
                 ;;                     AC:tmpDSP.AC[1:-1]}
                 tmpDSP           = {y:tmpDSP.y[1:-1]}
              ENDIF

           ENDELSE

        ENDIF

        IF E_has_common_TS THEN BEGIN
           eAV  = {x:eAV.x, $
                   DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

           eNB  = {DC:eNB.DC, $
                   AC:eNB.AC}

        ENDIF

     ENDELSE

     ptcl_has_common_TS = ARRAY_EQUAL(tmpJEe.x,tmpJe.x) AND ARRAY_EQUAL(tmpJEe.x,tmpJi.x) AND ARRAY_EQUAL(tmpJe.x,tmpJi.x)

     IF ptcl_has_common_TS THEN BEGIN

        tmpJEe  = {x:tmpJEe.x, $
                   y:tmpJEe.y, $
                   ;; DC:tmpJEe.DC, $
                   ;; AC:tmpJEe.AC, $
                   common_ts:1B}

        tmpJe   = {y:tmpJe.y} ;; DC:tmpJe.DC, $
        ;; AC:tmpJe.AC}

        tmpJi   = {y:tmpJi.y} ;;DC:tmpDSP.DC, $
        ;; AC:tmpDSP.AC}

     ENDIF

     ;;...And if they ALL have the same time series, we're only keeping one
     IF B_has_common_TS AND E_has_common_TS AND ptcl_has_common_TS THEN BEGIN

        dBp     = {x:dBp.x, $
                   DC:dBp.DC, $
                   AC:dBp.AC, $
                   common_ts:1B, $
                   commonest_ts:1B}

        eAV     = {DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

        tmpJEe  = {y:tmpJEe.y, $
                   ;; DC:tmpJEe.DC, $
                   ;; AC:tmpJEe.AC, $
                   common_ts:1B}
     ENDIF

     tmpStruct = {dB:{p:TEMPORARY(dBp), $
                      v:TEMPORARY(dBv), $
                      B:TEMPORARY(dBB)}, $
                  e:{AlongV:TEMPORARY(eAV   ), $
                     NearB :TEMPORARY(eNB   ), $
                     dsp   :TEMPORARY(tmpDSP)}, $
                  pFlux : CREATE_STRUCT('p',pFP, $
                                        'v',(KEYWORD_SET(full_pFlux) ? pFV : 0B), $
                                        'b',pFB), $
                  ptcl:{jEe:TEMPORARY(tmpJEe), $
                        je :TEMPORARY(tmpJe), $
                        ji :TEMPORARY(tmpJi)}, $
                  ptclMoms:{allPA:eMomStruct_allAngle, $
                            lcPA:eMomStruct_lcAngle, $
                            ionMom:ionMomStruct}, $
                  info:{full_pFlux            : KEYWORD_SET(full_pFlux), $
                        decimate_eb_calc_pFlux : KEYWORD_SET(decimate_eb_calc_pFlux), $
                        interp_4Hz_to_1s       : KEYWORD_SET(interp_4Hz_to_1s      ), $
                        include_E_near_B       : BYTE(KEYWORD_SET(include_E_near_B)), $
                        eField_fit_variables   : BYTE(KEYWORD_SET(use_eField_fit_variables))}}
     ;; outflow_i:[[start_i],[stop_i]]}

     PRINT,"Adding struct for interval " + itvlString + " in orbit " + orbString + ' ...'
     structList.Add,tmpStruct

  ENDFOR

  CASE 1 OF
     KEYWORD_SET(save_individual_orbit): BEGIN

        PRINT,"Saving " + indiv_orbFile + ' ...'
        SAVE,structList,FILENAME=outDir+indiv_orbFile

     END
     ELSE: BEGIN

        ;; IF ~KEYWORD_SET(no_hash_update) THEN BEGIN

        ;;    IF FILE_TEST(outDir+hashFile) THEN BEGIN
        ;;       PRINT,"Restoring hash file " + hashFile + " ..."
        ;;       RESTORE,outDir+hashFile

        ;;       CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
        ;;          -1: BEGIN
        ;;             PRINT,'Adding stuff from orbit ' + orbString + ' ...'
        ;;             swHash  = swHash + ORDEREDHASH(orbit,structList)
        ;;          END
        ;;          ELSE: BEGIN
        ;;             PRINT,'Replacing hash entry for orbit ' + orbString + ' ...'
        ;;             swHash[orbit] = structList
        ;;          END
        ;;       ENDCASE

        ;;       PRINT,'Saving Strangeway statistics hash ...'

        ;;       SAVE,swHash,FILENAME=outDir+hashFile

        ;;    ENDIF ELSE BEGIN

        ;;       PRINT,'Creating Strangeway statistics hash for orbit ' + orbString + ' ...'

        ;;       swHash = ORDEREDHASH(orbit,structList)

        ;;       SAVE,swHash,FILENAME=outDir+hashFile

        ;;    ENDELSE

        ;; ENDIF

     END
  ENDCASE

  RETURN

END
