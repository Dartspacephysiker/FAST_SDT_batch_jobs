;;09/02/16
PRO JOURNAL__20160902__TEST_FLUX_KAPPA2D__HORSESHOE_ROUTINE

  COMPILE_OPT IDL2

  eeb = LOAD_EXAMPLE_SDT_STRUCT(/EEB,ORBIT=1849)

  lca       = 150
  mu_0      = COS(lca/180.*!PI)

  IF N_ELEMENTS(eeb) EQ 0 THEN STOP

  IF ~eeb.valid THEN STOP

  a         = [1e3,600,3.0,0.5] 

  ex        = eeb

  that      = KAPPA_FLUX2D__HORSESHOE(ex.energy,ex.theta,a, $
                                      /BINGHAM_STYLE, $
                                      MU_0=mu_0) 
  ex.data = that 
  CONTOUR2D,ex,/POLAR

END
