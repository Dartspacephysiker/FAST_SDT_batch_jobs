;;2017/02/21
PRO JOURNAL__20170221__ORBIT_1773__FIG2TIME

  COMPILE_OPT IDL2

  ;;get orbTimes here
  orbs           = 1773
  orbTimes       = ['97-02-01/09:25:50','97-02-01/09:27:30']
  orbBurstTimes  = ['97-02-01/09:26:05','97-02-01/09:27:27']
  bonusPrefs     = '--Elphic_et_al_1998--Fig2'

  kStats_startStops__ees = LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
                                                 ['09:27:05','09:27:15']])
  kStats_startStops__eeb = LIST('1997-02-01/' + [['09:26:12','09:26:23'], $
                                                 ['09:26:53','09:27:07.5']])
  just_diff_eFlux      = 1
  units                = 'eFlux'
  ;; units                = 'flux'
  ;; units                = 'dfStd'
  
  outDir               = '~/software/sdt/batch_jobs/saves_output_etc/'


  ;; electron_angleRange  = [-24,24]

  save_diff_eFlux_file = 0
  load_diff_eFlux_file = 1
  restore_fitFile      = 0

  burstItvl            = 0

  ;;Which classic event?
  ;; '0 :  Elphic_et_al_1998'
  evtNum               = 0

  ;;survey window
  eeb_or_eesArr        = ['ees','ies']
  ;; eeb_or_eesArr                = ['eeb','ieb']

  order                        = ['downgoing_e','upgoing_e','upgoing_i']
  energyArr                    = [[3e1,3.0e4],[3e1,3.0e4],[1e2,2.4e4]]

  ;; min_peak_energy              = KEYWORD_SET(upgoing) ? 100 : 500
  ;; max_peak_energy              = KEYWORD_SET(upgoing) ? 3e4 : !NULL
  min_peak_energyArr           = [300,100,100]
  max_peak_energyArr           = [3e4,3e4,2.4e4]

  ;;If doing upgoing electrons
  peak_energy__start_at_highE  = [0,1,1]
  upgoing                      = [0,1,1]

  ;;String setup
  orbit                = orbs      [evtNum]
  IF (STRUPCASE(eeb_or_ees) EQ 'EEB') OR (STRUPCASE(eeb_or_ees) EQ 'IEB') THEN BEGIN
     t1Str             = orbBurstTimes[0] ;,evtNum]
     t2Str             = orbBurstTimes[1] ;,evtNum]
  ENDIF ELSE BEGIN
     t1Str             = orbTimes[0] ;,evtNum]
     t2Str             = orbTimes[1] ;,evtNum]
  ENDELSE
  t1                   = STR_TO_TIME(t1Str)
  t2                   = STR_TO_TIME(t2Str)

  bonusPref            = bonusPrefs[evtNum]

  ;; downAngleRange       = [-24,24]

  nLoop = N_ELEMENTS(eeb_or_eesArr)
  dEF_list = LIST()
  dEF_1c_list = LIST()
  FOR k=0,nLoop-1 DO BEGIN


     eeb_or_ees = eeb_or_eesArr[k]

     GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI, $
        t1,t2, $
        lc_angleRange, $
        i_angle,i_angle_up, $
        north_south, $
        ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
        CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
        OUT_E_ANGLE=e_angle, $
        ANGLESTR=angleStr, $
        /JUST_ONE

     CASE upgoing[k] OF
        0: BEGIN
           angleRange = lc_angleRange
        END
        1: BEGIN
           ;; CASE STRMATCH(STRUPCASE(eeb_or_ees),'EE*') OF
           ;;    0: BEGIN
           angleRange = lc_angleRange - 180.
           ;;    END
           ;; ENDCASE
        END
     ENDCASE

     IF FILE_TEST(

     GET_DIFF_EFLUX,T1=t1,T2=t2, $
                    EEB_OR_EES=eeb_or_ees, $
                    NAME__DIFF_EFLUX=name__diff_eFlux, $
                    /CALC_GEOM_FACTORS, $
                    UNITS=eSpecUnits, $          
                    FIT_EACH_ANGLE=fit_each_angle, $
                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                    OUT_DIFF_EFLUX=diff_eflux, $
                    SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                    LOAD_DAT_FROM_FILE=loadFile, $
                    LOAD_DIR=loadDir

     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   ;; LOAD_DAT_FROM_FILE=loadFile, $ ;;handled through proto
                                   EEB_OR_EES=KF__SDTData_opt.EEB_or_EES, $
                                   SPECTRA_AVERAGE_INTERVAL=KF__SDTData_opt.spec_avg_intvl, $
                                   IN_PROTOSTRUCT=diff_eFlux, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ESPECUNITS=units, $
                                   ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   /FIT_EACH_ANGLE, $ ;Perma-set because we do all angles for 2D fitting
                                   OUT_ONEDAT=out_oneDat, $
                                   DEF_ONECOUNT=dEF_oneCount, $
                                   QUIET=quiet

     KAPPA_FITTER_BLACKBOX,orbit, $
                           ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                           ELECTRON_LOSSCONEANGLE=electron_lca, $
                           ENERGY_ELECTRONS=energy_electrons, $
                           JUST_DIFF_EFLUX=just_diff_eFlux, $
                           DIFF_EFLUX=diff_eFlux, $
                           DEF_ONECOUNT=dEF_oneCount, $
                           UPGOING=upgoing, $
                           MIN_PEAK_ENERGY=min_peak_energy, $
                           MAX_PEAK_ENERGY=max_peak_energy, $
                           PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
                           EEB_OR_EES=eeb_or_ees, $
                           SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                           CHI2_THRESHOLD=chi2_thresh, $
                           CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                           HIGHDENSITY_THRESHOLD=highDens_thresh, $
                           LOWDENSITY_THRESHOLD=lowDens_thresh, $
                           DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                           N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                           RESTORE_FITFILE=restore_fitFile, $
                           T1STR=t1Str, $
                           T2STR=t2Str, $
                           SHOW_POST_PLOTS=show_post_plots, $
                           ONLY_1D_FITS=only_1D_fits, $
                           FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                           FIT1D__NFLUX=fit1D__nFlux, $
                           FIT1D__WEIGHTING=fit1D__weighting, $
                           FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                           FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                           FIT2D__SAVE_ALL_CANDIDATE_PLOTS=fit2D__save_all_candidate_plots, $
                           FIT2D__WEIGHTING=fit2D__weighting, $
                           ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                           SAVE_KAPPA_PLOTS=save_kappa_plot, $
                           SAVEKAPPA_BONUSPREF=bonusPref, $
                           CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                           PLOTDIR=plotDir, $
                           SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                           SWAY__SAVE_PS=sway__save_ps, $
                           SWAY__SAVE_PNG=sway__save_png, $
                           SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                           SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                           SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                           SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                           SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                           KSUM__SAVE_PS=kSum__save_ps, $
                           KSUM__SAVE_PNG=kSum__save_png, $
                           KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                           KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                           OUT_FIT2DK=fit2DK, $
                           OUT_FIT2DGAUSS=fit2DG, $
                           OUT_KAPPA_FIT_STRUCTS=kappaFits, $
                           OUT_GAUSS_FIT_STRUCTS=gaussFits, $
                           FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                           FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                           SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                           LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                           KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                           KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__include_these_startstops,$
                           DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                           DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time

     dEF_list.Add,TEMPORARY(diff_eFlux)
     dEF_1c_list.Add,TEMPORARY(dEF_oneCount)
  ENDFOR


END


