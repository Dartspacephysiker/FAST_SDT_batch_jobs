;;09/24/16
;;All this stuff should be according to Strangeway et al. [2005]

  minILAT           = 50 

  energy_ions       = [0,120.]
  energy_electrons  = [50,30000.]

  strWay_orbs       = INDGEN(33)+8260
  upper_ion_e       = HASH(strWay_orbs,MAKE_ARRAY(33,VALUE=120.,/FLOAT))
  upper_ion_e[8276] = 300 ;eV—so say Strangeway et al. [2005] in Appendix A for this orbit

  ;;Smooth windows
  DSP_smoothWindow_halfLength    = 2.0
  fields_smoothWindow_halfLength = 2.0


  energy_ions       = [0,500.]
  upper_ion_e       = HASH(strWay_orbs,MAKE_ARRAY(33,VALUE=500.,/FLOAT))
