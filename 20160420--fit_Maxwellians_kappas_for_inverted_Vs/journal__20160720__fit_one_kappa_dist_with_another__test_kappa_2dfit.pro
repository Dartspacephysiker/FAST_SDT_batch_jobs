;;07/20/16
PRO JOURNAL__20160720__FIT_ONE_KAPPA_DIST_WITH_ANOTHER__TEST_KAPPA_2DFIT

  COMPILE_OPT IDL2

  fit_tol               = 1e-2
  max_iter              = 1000

  inDir                 = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile               = '20160718--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--ees--fit_each_angle--solid_fits_from_three_times.sav'

  RESTORE,inDir+fitFile

  ;; dat1                  = synthpackage[0,0]
  ;; kappa1                = synthpackage[1,0]

  dataStrs              = synthPackage[0]
  kappaStrs             = synthPackage[1]
  gaussStrs             = synthPackage[2]
  
  nEnergies             = N_ELEMENTS(dataStrs[0].energy[*,0])
  nTotAngles            = N_ELEMENTS(dataStrs[0].energy[0,*])

  nStructs              = N_ELEMENTS(kappaStrs)

  tmpIndex              = 0

  FOR iTime=0,nStructs-1 DO BEGIN


     iTime1              = iTime
     iTime2              = iTime+2
     ;;Current struct
     curKappaStr1        = kappaStrs[iTime1]
     curDataStr1         = dataStrs[iTime1]
     curKappaStr2        = kappaStrs[iTime2]
     curDataStr2         = dataStrs[iTime2]

     KAPPA__GET_GOOD_INDS_FROM_FITSTRUCTS,kappaFits,iTime1,tmpIndex,curKappaStr1.theta[nEnergies/2,*], $
                                          nGoodFits1, $
                                          goodAngles1,good_angleBin_i1, $
                                          good_kappaFits_i1

     KAPPA__GET_GOOD_INDS_FROM_FITSTRUCTS,kappaFits,iTime2,tmpIndex,curKappaStr2.theta[nEnergies/2,*], $
                                          nGoodFits2, $
                                          goodAngles2,good_angleBin_i2, $
                                          good_kappaFits_i2

     ;;OK, now that we've got all the fits that succeeded, let's see how they do in the mosh pit
     chiArray              = !NULL
     fitparams2dArray      = !NULL


     FOR iWin=0,nGoodFits1-1 DO BEGIN
        SETUP_KAPPA_FIT2D_TEST,good_angleBin_i1,good_kappaFits_i1,iWin, $
                               nEnergies,nTotAngles, $
                               curKappaStr1,kappaFits,curDataStr1, $
                               iAngle1,iKappa1,testKappa1,testKappaFit1,testArray1, $
                               craptest1, $
                               wts1,X2D1,Y2D1,dataToFit1, $
                               fa1,dens_param1

        iWin2 = iWin+3
        SETUP_KAPPA_FIT2D_TEST,good_angleBin_i2,good_kappaFits_i2,iWin, $
                               nEnergies,nTotAngles, $
                               curKappaStr2,kappaFits,curDataStr2, $
                               iAngle2,iKappa2,testKappa2,testKappaFit2,testArray2, $
                               craptest2, $
                               wts2,X2D2,Y2D2,dataToFit2, $
                               fa2,dens_param2

        Fitparams2D        = MPFIT2DFUN('KAPPA_FLUX2D__SCALE_DENSITY',X2D1,Y2D1,craptest2.data, $
                                        err, $
                                        dens_param1, $
                                        WEIGHTS=wts1, $
                                        FUNCTARGS=fa1, $
                                        BESTNORM=bestNorm, $
                                        NFEV=nfev, $
                                        FTOL=fit_tol, $
                                        STATUS=status, $
                                        best_resid=best_resid, $
                                        pfree_index=ifree, $
                                        calc_fjac=calc_fjac, $
                                        best_fjac=best_fjac, $
                                        parinfo=parinfo, query=query, $
                                        npegged=npegged, nfree=nfree, dof=dof, $
                                        covar=covar, perror=perror, $
                                        MAXITER=max_iter, $
                                        niter=niter, $
                                        YFIT=yfit1to2, $
                                        quiet=quiet, $
                                        ERRMSG=errmsg, $
                                        _EXTRA=extra)

        chiArray           = [chiArray,bestNorm]
        fitparams2dArray   = [fitparams2dArray,fitparams2d]
     ENDFOR

  ENDFOR
END
