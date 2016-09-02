;;09/02/16
PRO JOURNAL__20160902__FIGURE_OUT_ANISOTROPY_FACTOR_FOR_BULK_E

  COMPILE_OPT IDL2

  dir  = '~/software/sdt/batch_jobs/saves_output_etc/'
  file = '20160902--checking_out_horseshoe_fit--lca_40.sav'

  minEn   = 800

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
                                          OUT_PEAK_ENERGIES=peakEn__en, $
                                          OUT_ANGLES=peakEn__angle)


  window  = WINDOW(DIMENSIONS=[1200,800])

  that = PLOT(peakEn__angle,peakEn__en/peakEn__en[0], $
              NAME='Data', $
              TITLE='Variation in E$_{bulk}$ with pitch angle', $
              YRANGE=[0.19,1.02], $
              XTITLE='Pitch angle (deg)', $
              YTITLE='E$_{peak}$/E$_{peak,field-aligned}$', $
              SYMBOL='*', $
              SYM_SIZE=2.0, $
              FONT_SIZE=18, $
              LINESTYLE='', $
              CURRENT=window)

  those = PLOT(peakEn__angle,factor, $
               NAME='Model function', $
               SYMBOL='*', $
               SYM_SIZE=2.0, $
               FONT_SIZE=18, $
               LINESTYLE='', $
               COLOR='red', $
               /OVERPLOT, $
               CURRENT=window)

  legend = LEGEND( $
           ;; TARGET=[that,those], $
           TARGET=[that,those], $
           POSITION=[0.35,0.8], $
           /NORMAL)

  STOP
END
