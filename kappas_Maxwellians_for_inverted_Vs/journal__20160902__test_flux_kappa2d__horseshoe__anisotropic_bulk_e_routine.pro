;;09/02/16
PRO JOURNAL__20160902__TEST_FLUX_KAPPA2D__HORSESHOE__ANISOTROPIC_BULK_E_ROUTINE, $
   REDUCENEGFAC=reduceNegFac, $
   LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
   PLOT_FACTOR=plot_factor, $
   PLOT_COMPARISON=plot_comparison, $
   PLOT_CONTOUR_COMPARISON=plot_contour_comparison


  COMPILE_OPT IDL2

  dir  = '~/software/sdt/batch_jobs/saves_output_etc/'
  file = '20160902--checking_out_horseshoe_fit--lca_40.sav'

  minEn   = 800

  lca       = 150
  mu_0      = COS(lca/180.*!PI)

  RESTORE,dir+file

  angles  = curDataStr.theta[0,*]
  nAngles = N_ELEMENTS(angles)
  
  P       = fit2dstruct.bestfit1dparams
  bulk_e  = P[0]

  bestAngle_i   = FIX(fit2dstruct.bestAngle_i)

  factor =   KAPPA_EFLUX__ANISOTROPY_DIST(curDataStr.energy, $
                                          curDataStr.theta, $
                                          curDataStr.data, $
                                          bestAngle_i, $
                                          BULK_ENERGY=P[0], $
                                          MIN_ENERGY=minEn, $
                                          REDUCENEGFAC=reduceNegFac, $
                                          LOGSCALE_REDUCENEGFAC=logScale_reduceNegFac, $
                                          PLOT_FACTOR=plot_factor, $
                                          PLOT_COMPARISON=plot_comparison, $
                                          OUT_PEAK_ENERGIES=peakEn__en, $
                                          OUT_ANGLES=peakEn__angle)

  tmp       = curDataStr

  that      = KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY(tmp.energy,tmp.theta,P, $
                                                         /BINGHAM_STYLE, $
                                                         MU_0=mu_0, $
                                                         BULK_E_ANISOTROPY=factor) 

  tmp.data  = that 

  zRange    = [1e6,1e9]
  CASE 1 OF
     KEYWORD_SET(plot_contour_comparison): BEGIN
        CONTOUR2D,tmp,/POLAR, $
                  /FILL, $
                  LIMITS={zRange:zRange}

        CONTOUR2D,curDataStr,/POLAR, $
                  /OVERPLOT, $
                  LIMITS={zRange:zRange}
     END
     ELSE: BEGIN

        CONTOUR2D,tmp,/POLAR, $
                  /FILL, $
                  LIMITS={zRange:zRange}

     END
  ENDCASE

  

END
