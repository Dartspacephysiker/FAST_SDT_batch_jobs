;2019/01/16
PRO JOURNAL__20190116__MAKE_JGR_FIG1

  COMPILE_OPT IDL2,STRICTARRSUBS

  thresh_eFlux = 5e5
  enforce_this_sample_rate = 1.25
  energy_electrons_lb = 50

  fracBelowThatMustBeUpward = 0.75D

  upDownMinRatio = 5
  uDMRStr = STRING(FORMAT='(I0)',upDownMinRatio)

  save_individual_data_products__only_db_and_ions = 1
  indivSuff = '-rawProds__dB_and_ions__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav'

  make_special_JGR_plot = 1

  STRANGEWAY_2005__V3,/SAVE_PS, $
                      IONSPECS_UPDOWNMINRATIO=upDownMinRatio, $
                      IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
                      IONSPECS_THRESH_EFLUX=thresh_eFlux, $
                      IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
                      /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
                      /STRANGEWAY_2005_FIG3_PLOT, $
                      ;; /REMAKE_DIFF_EFLUX, $
                      /SAVE_INDIVIDUAL_DATA_PRODUCTS_AND_QUIT, $
                      SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_IONS=save_individual_data_products__only_ions, $
                      SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_DB_AND_IONS=save_individual_data_products__only_db_and_ions, $
                      SAVE_INDIVIDUAL_DATA_PRODUCTS__FSUFF=indivSuff, $
                      MAKE_IONS_OXYGEN=make_ions_oxygen, $
                      MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
                      EFIELD_SHADOW_NOTCH=shadow_notch, $
                      EFIELD_SINTERP=sInterp, $
                      ENERGY_ELECTRONS_LB=energy_electrons_lb


END
