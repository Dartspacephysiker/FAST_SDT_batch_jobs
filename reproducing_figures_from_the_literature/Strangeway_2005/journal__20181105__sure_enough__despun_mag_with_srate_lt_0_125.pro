;2018/11/05
;; Poenget er at man kan sikkert få despun dB data som har et oppløsning på mindre enn 0.125 s. Med bane-8262 får jeg masse punkter med noe omtrent 0.0078125
PRO JOURNAL__20181105__SURE_ENOUGH__DESPUN_MAG_WITH_SRATE_LT_0_125

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; thresh_eFlux = 5e5
  ;; enforce_this_sample_rate = 1.25
  ;; energy_electrons_lb = 50

  ;; indivSuff = '-rawProds_med_dB_sc_and_spinplane_E.sav'


  ;; STRANGEWAY_2005__V3,/SAVE_PS, $
  ;;                     IONSPECS_UPDOWNMINRATIO=2, $
  ;;                     IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
  ;;                     IONSPECS_THRESH_EFLUX=thresh_eFlux, $
  ;;                     /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
  ;;                     /STRANGEWAY_2005_FIG3_PLOT, $
  ;;                     /REMAKE_DIFF_EFLUX, $
  ;;                     /SAVE_INDIVIDUAL_DATA_PRODUCTS_AND_QUIT, $
  ;;                     SAVE_INDIVIDUAL_DATA_PRODUCTS__FSUFF=indivSuff, $
  ;;                     ENERGY_ELECTRONS_LB=energy_electrons_lb

  ;; THEN, with a break at line 357 (  OPTIONS,'dB_fac_v','panel_size',2),
  ;; Do this:


  ;; diff = data.x[1:-1]-data.x[0:-2]
  ;; this = WHERE(diff LT 0.125)

  ;; OR, just do this:

  UCLA_MAG_DESPIN
  GET_DATA,'dB_fac',data=data

  diff = data.x[1:-1]-data.x[0:-2]
  this = WHERE(diff LT 0.125)

  PRINT,N_ELEMENTS(this)/FLOAT(N_ELEMENTS(data.x))
  ;; This ratio is around 0.57 for orbit 8262

END
