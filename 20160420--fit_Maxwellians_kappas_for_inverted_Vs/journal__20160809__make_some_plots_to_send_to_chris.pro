PRO JOURNAL__20160809__MAKE_SOME_PLOTS_TO_SEND_TO_CHRIS

  COMPILE_OPT idl2

  use_mpFit1D        = 1

  outDir             = '~/software/sdt/batch_jobs/saves_output_etc/'

  fitFile1           = '20160809--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150--mpfitfun1d--saveme.sav'
  diff_eFlux_file1   = 'orb_1849--diff_eflux--eeb--output_from_get_losscone_and_eflux_data.sav'


  fitFile2           = '20160809--Elphic_et_al_1998--Kappa_fits_and_Gauss_fits--ees--fit2d--all_times--150to150--mpfitfun1d.sav'
  diff_eFlux_file2   = 'orb_1773--diff_eflux--ees--output_from_get_losscone_and_eflux_data.sav'


  RESTORE,outDir+fitFile1
  RESTORE,outDir+diff_eFlux_file1

  ;; kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DKappa_inf_list, $
  ;;                                                  HIGHDENSITY_THRESHOLD=highDens_thresh, $
  ;;                                                  KAPPA_LOWTHRESHOLD=lKappa_thresh, $
  ;;                                                  KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
  ;;                                                  /DESTROY_INFO_LIST, $
  ;;                                                  OUT_GOOD_I=includeK_i, $
  ;;                                                  OUT_GOOD_T=includeK_t, $
  ;;                                                  OUT_BAD_I=excludeK_i, $
  ;;                                                  OUT_BAD_T=excludeK_t)

  ;; gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DGauss_inf_list, $
  ;;                                                  HIGHDENSITY_THRESHOLD=highDens_thresh, $
  ;;                                                  KAPPA_LOWTHRESHOLD=lKappa_thresh, $
  ;;                                                  KAPPA_HIGHTHRESHOLD=100.1, $
  ;;                                                  DESTROY_INFO_LIST, $
  ;;                                                  OUT_GOOD_I=includeG_i, $
  ;;                                                  OUT_GOOD_T=includeG_t, $
  ;;                                                  OUT_BAD_I=excludeG_i, $
  ;;                                                  OUT_BAD_T=excludeG_t)

  ;; PARSE_KAPPA_FIT_STRUCTS,kappa2D.params1D, $
  ;;                         A=a, $
  ;;                         TIME=kappaTime, $
  ;;                         STRUCT_A=AStruct, $
  ;;                         NAMES_A=ANames, $
  ;;                         CHI2=chi2, $
  ;;                         PVAL=pVal, $
  ;;                         FITSTATUS=fitStatus, $
  ;;                         USE_MPFIT1D=use_mpFit1D

  ;; PARSE_KAPPA_FIT_STRUCTS,gauss2D.params1D, $
  ;;                         A=AGauss, $
  ;;                         STRUCT_A=AStructGauss, $
  ;;                         TIME=gaussTime, $
  ;;                         NAMES_A=AGaussNames, $
  ;;                         CHI2=chi2Gauss, $
  ;;                         PVAL=pValGauss, $
  ;;                         FITSTATUS=gaussfitStatus, $
  ;;                         USE_MPFIT1D=use_mpFit1D


  iTime              = 0
  ;; curDataStr         = 
  limits             = {zrange:[1e6,1e9]}

  POPEN,'Kappa--orbit_1893'
  PLOT_CONTOUR2D_MODEL_AND_DATA__SELECTED2DFIT,fit2DKappa_inf_list[iTime], $
     MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(diff_eFlux,iTime), $
     LIMITS=limits, $
     /ADD_FITPARAMS_TEXT, $
     KSDTDATA_OPT=kSDTData_opt, $
     FITSTRING='Kappa'

  STOP

END

