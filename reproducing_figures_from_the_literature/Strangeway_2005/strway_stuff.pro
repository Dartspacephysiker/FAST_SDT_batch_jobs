;;09/24/16
;;All this stuff should be according to Strangeway et al. [2005]

  minILAT           = 50 

  energy_ions       = [4,120.]
  energy_electrons  = [50,30000.]

  strWay_orbs       = INDGEN(33)+8260
  upper_ion_e       = HASH(strWay_orbs,MAKE_ARRAY(33,VALUE=120.,/FLOAT))
  upper_ion_e[8260] = 40
  upper_ion_e[8261] = 40
  upper_ion_e[8262] = 30  
  upper_ion_e[8263] = 30  
  upper_ion_e[8264] = 30  
  upper_ion_e[8265] = 30  
  upper_ion_e[8266] = 50
  upper_ion_e[8267] = 40
  upper_ion_e[8268] = 50
  upper_ion_e[8269] = 100
  upper_ion_e[8270] = 80
  upper_ion_e[8271] = 40
  upper_ion_e[8272] = 50
  upper_ion_e[8273] = 90
  upper_ion_e[8274] = 50
  upper_ion_e[8275] = 100
  upper_ion_e[8276] = 300 ;eV—so say Strangeway et al. [2005] in Appendix A for this orbit
  upper_ion_e[8277] = 200
  upper_ion_e[8278] = 200
  upper_ion_e[8279] = 300
  upper_ion_e[8280] = 50
  upper_ion_e[8281] = 300
  upper_ion_e[8282] = 300
  upper_ion_e[8283] = 80
  upper_ion_e[8284] = 90
  upper_ion_e[8285] = 40
  upper_ion_e[8286] = 70
  upper_ion_e[8287] = 80
  upper_ion_e[8288] = 30
  upper_ion_e[8289] = 60
  upper_ion_e[8290] = 30
  upper_ion_e[8291] = 30
  upper_ion_e[8292] = 40

  ;;Smooth windows
  DSP_smoothWindow_halfLength    = 2.0
  fields_smoothWindow_halfLength = 2.0

  mu_0         = DOUBLE(4.0D*!PI*1e-7)

  ;;Allowable difference between t{1,2} and nearest fields data
  tBuf         = 10.

  ;; energy_ions       = [0,500.]
  ;; upper_ion_e       = HASH(strWay_orbs,MAKE_ARRAY(33,VALUE=500.,/FLOAT))
