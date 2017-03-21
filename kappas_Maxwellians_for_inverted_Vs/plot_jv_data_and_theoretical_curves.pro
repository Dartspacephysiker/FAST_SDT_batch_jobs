;2017/03/18
PRO PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData, $
                                        CURPOTLIST=curPotList, $
                                        MINPOT=minPot, $
                                        MAXPOT=maxPot, $
                                        MINCUR=minCur, $
                                        MAXCUR=maxCur, $
                                        USEINDS=useInds, $
                                        PLOT_J_RATIOS=plot_j_ratios, $
                                        PLOT_ION_ELEC_RATIOS=plot_ion_elec_ratios, $
                                        ORIGINATING_ROUTINE=routName, $
                                        PLOTDIR=plotDir, $
                                        SAVEPLOT=savePlot, $
                                        SPNAME=spName, $
                                        AVGS_FOR_FITTING=avgs_JVfit, $
                                        FIT_TIME_SERIES=fit_time_series, $
                                        FIT_TSERIES__A_IN=A_in

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(plot_ion_elec_ratios) THEN BEGIN
     nListMem        = N_ELEMENTS(curPotList)         
     nHere           = N_ELEMENTS(curPotList[0].time)
     errorBarFac     = KEYWORD_SET(errorBarFac) ? errorBarFac : 1.

     looking         = nListMem
     ind             = 0

     WHILE (looking GT 0) DO BEGIN
        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*DOWN*E') THEN BEGIN
           looking--
           edind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*E') THEN BEGIN
           looking--
           euind = ind
        ENDIF

        IF STRMATCH(STRUPCASE(curPotList[ind].label),'*UP*I') THEN BEGIN
           looking--
           iuind = ind
        ENDIF
        ind++
     ENDWHILE

     ji_je_ratio           = curPotList[iuind].cur/curPotList[edind].cur

  ENDIF

  curDat                      = jvplotdata.cur*(-1D-6) / (KEYWORD_SET(plot_j_ratios) ? jvplotdata.cur*(-1D-6) : 1.D)
  divDat                      = jvplotdata.cur*(-1D-6)

  ;; SAVE,KnightRelat30,KnightRelat300,KnightRelat3000,jvplotdata,FILENAME=
  ;; RESTORE,'
  CASE 1 OF
     KEYWORD_SET(fit_time_series): BEGIN

        maxIter     = 150
        fit_tol     = 1D-15
        gTol        = 1D-15

        ;;            kappa,            Temp,            Dens,  R_B
        A           = KEYWORD_SET(A_in) ? A_in : [  10,avgs_JVfit.T.avg,avgs_JVfit.N.avg, 1D3]

        ;;Keep the original guesses
        Aorig       = A
        AGaussOrig  = A

        kappa_fixA  = [0,1,1,0]
        gauss_fixA  = [1,1,1,0]

        ;; PRINT,"Kappa startGuess: "
        ;; PRINT_JV_FIT_PARAMS,A
        ;; PRINT,"Gauss startGuess: "
        ;; PRINT_JV_FIT_PARAMS,AGaussOrig

        kappaParamStruct = INIT_JV_FITPARAM_INFO(           A,kappa_fixA)
        gaussParamStruct = INIT_JV_FITPARAM_INFO(TEMPORARY(A),gauss_fixA)

        fa_kappa    = {no_mult_by_charge : 1B, $
                       is_Maxwellian_fit : 0B, $
                       in_temperatures   : jvPlotData.TDown[avgs_JVfit.useInds], $
                       in_densities      : jvPlotData.NDown[avgs_JVfit.useInds]}

        fa_Gauss    = {no_mult_by_charge : 1B, $
                       is_Maxwellian_fit : 1B, $
                       in_temperatures   : jvPlotData.TDown[avgs_JVfit.useInds], $
                       in_densities      : jvPlotData.NDown[avgs_JVfit.useInds]}

        jvFitFunc   = 'JV_CURVE_FIT__MAXWELL_KAPPA'
        OKStatus    = [1,2,3,4] ;These are all the acceptable outcomes of fitting with MPFIT2DFUN

        X           = jvPlotData.pot[avgs_JVfit.useInds]
        Y           = jvPlotData.cur[avgs_JVfit.useInds]*(-1D)
        XError      = jvPlotData.potErr[avgs_JVfit.useInds]
        YError      = jvPlotData.curErr[avgs_JVfit.useInds]
        weights     = 1./ABS(jvPlotData.curErr[avgs_JVfit.useInds])^2

        A           = MPFITFUN(jvFitFunc, $
                               X,Y, $
                               /NAN, $
                               WEIGHTS=weights, $
                               FUNCTARGS=fa_kappa, $
                               BESTNORM=bestNorm, $
                               NFEV=nfev, $
                               FTOL=fit_tol, $
                               GTOL=gTol, $
                               STATUS=status, $
                               BEST_RESID=best_resid, $
                               PFREE_INDEX=ifree, $
                               CALC_FJAC=calc_fjac, $
                               BEST_FJAC=best_fjac, $
                               PARINFO=kappaParamStruct, $
                               QUERY=query, $
                               NPEGGED=npegged, $
                               NFREE=nfree, $
                               DOF=dof, $
                               COVAR=covar, $
                               PERROR=perror, $
                               MAXITER=maxIter, $
                               NITER=itNum, $
                               YFIT=yFit, $
                               /QUIET, $
                               ERRMSG=errMsg, $
                               _EXTRA=extra)

        AGauss      = MPFITFUN(jvFitFunc, $
                               X,Y, $
                               /NAN, $
                               WEIGHTS=weights, $
                               FUNCTARGS=fa_Gauss, $
                               BESTNORM=bestNorm, $
                               NFEV=nfev, $
                               FTOL=fit_tol, $
                               GTOL=gTol, $
                               STATUS=gaussStatus, $
                               BEST_RESID=best_resid, $
                               PFREE_INDEX=ifree, $
                               CALC_FJAC=calc_fjac, $
                               BEST_FJAC=best_fjac, $
                               PARINFO=gaussParamStruct, $
                               QUERY=query, $
                               NPEGGED=npegged, $
                               NFREE=nfree, $
                               DOF=dof, $
                               COVAR=covar, $
                               PERROR=perror, $
                               MAXITER=maxIter, $
                               NITER=itNum, $
                               YFIT=yGaussFit, $
                               /QUIET, $
                               ERRMSG=errMsg, $
                               _EXTRA=extra)

        PRINT,"TIME SERIES: Kappa fitparams : "
        PRINT_JV_FIT_PARAMS,A
        PRINT,""
        PRINT,"TIME SERIES: Gauss fitparams: "
        PRINT_JV_FIT_PARAMS,AGauss
        PRINT,""

        kappas             = A[0]

        R_Bs__K            = A[3]
        R_Bs__M            = AGauss[3]

        TmultFac__kappa    = [1]
        TmultFac__Maxwell  = [1]
     END
     ELSE: BEGIN

        ;; R_Bs__M         = [30,300,3000]
        R_Bs__M            = [100,10000,100,10000]
        ;; R_Bs__K         = [30,300,3000]
        ;; kappas          = [2.0,2.0,2.0,1.6]
        TmultFac__Maxwell  = [1,1,10,10]
        R_Bs__K            = [100,100,10000,10000]
        kappas             = [2.0,1.8,2.0,1.8]
        TmultFac__kappa    = [1,1,1,1]

     END
  ENDCASE

  nR_Bs__M                 = N_ELEMENTS(R_Bs__M)
  nR_Bs__K                 = N_ELEMENTS(R_Bs__K)
  nDer                     = N_ELEMENTS(useInds)

  maxwellJVs                  = MAKE_ARRAY(nR_Bs__M,nDer,/DOUBLE)
  kappaJVs                    = MAKE_ARRAY(nR_Bs__K,nDer,/DOUBLE)

  MaxwellTransp               = 30
  MaxwellSym                  = '*'
  MaxwellColors               = ['Red','Brown','Dark Green','Dark Orange']
  MaxwellLinestyle            = ['']
  MaxwellNames                = MAKE_ARRAY(nR_Bs__M,/STRING)

  kappaTransp                 = 30
  kappaSym                    = ['x','tu','+','td']
  kappaColors                 = ['Purple','Brown','Gray',"Violet"]
  kappaLinestyle              = ['']
  kappaNames                  = MAKE_ARRAY(nR_Bs__K,/STRING)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     maxwellJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
                                                        jvplotdata.ndown[useInds], $
                                                        jvplotdata.pot[useInds], $
                                                        R_Bs__M[k], $
                                                        /NO_MULT_BY_CHARGE)

     MaxwellNames[k] = 'R!DB!N = ' + STRING(FORMAT='(G0.4)',R_Bs__M[k])
     MaxwellNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," (T*=",I0,")")', $
                            R_Bs__M[k],TmultFac__Maxwell[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        maxwellJVs[k,*] /= divDat[useInds]
     ENDIF
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_11(kappas[k],jvplotdata.tdown[useInds], $
                                                       jvplotdata.ndown[useInds], $
                                                       jvplotdata.pot[useInds], $
                                                       R_Bs__K[k], $
                                                       /NO_MULT_BY_CHARGE)

     kappaNames[k] = STRING(FORMAT='("R!DB!N = ",G0.4," ($\kappa$=",F0.2,",T*=",I0,")")', $
                            R_Bs__K[k],kappas[k],TmultFac__kappa[k])

     IF KEYWORD_SET(plot_j_ratios) THEN BEGIN
        kappaJVs[k,*] /= divDat[useInds]
     ENDIF
  ENDFOR

  MaxwellPlots = MAKE_ARRAY(nR_Bs__M,/OBJ)
  kappaPlots   = MAKE_ARRAY(nR_Bs__K,/OBJ)
  window1      = WINDOW(DIMENSIONS=[1000,800], $
                        BUFFER=savePlot)
  yLog         = 0
  dataLStyle   = ''
  dataSym      = 'o'
  dataName     = 'Data'
  xTitle       = 'Potential (V)'
  yTitle       = KEYWORD_SET(plot_j_ratios) ? "J!D||,obs!N / J!D||,theor!N" : $
                 'Current density ($\mu$A/m!U2!N), mapped to 100km'

  ji_je_lStyle = ''
  ji_je_sym    = 's'
  ji_je_name   = 'J!Di!N over J!De!N'

  dataplot     = PLOT(jvplotdata.pot[useInds], $
                  curDat[useInds], $
                  LINESTYLE=dataLStyle, $
                  SYMBOL=dataSym, $
                  XTITLE=xTitle, $
                  YTITLE=yTitle, $
                  NAME=dataName, $
                  YLOG=yLog, $
                  /CURRENT)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     MaxwellPlots[k] = PLOT(jvplotdata.pot[useInds], $
                            MaxwellJVs[k,*], $
                            TRANSPARENCY=MaxwellTransp, $
                            LINESTYLE=MaxwellLinestyle, $
                            SYMBOL=MaxwellSym, $
                            COLOR=MaxwellColors[k], $
                            /OVERPLOT, $
                            NAME=MaxwellNames[k])
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaPlots[k] = PLOT(jvplotdata.pot[useInds], $
                        kappaJVs[k,*], $
                        TRANSPARENCY=kappaTransp, $
                        LINESTYLE='', $
                        SYMBOL=kappaSym[k], $
                        COLOR=kappaColors[k], $
                        /OVERPLOT, $
                        NAME=kappaNames[k])
  ENDFOR

  legArr = [dataplot, $
            MaxwellPlots, $
            kappaPlots]

  IF KEYWORD_SET(ji_je_ratio) THEN BEGIN

     ji_je_plot = PLOT(jvplotdata.pot[useInds], $
                       ji_je_ratio[useInds], $
                       LINESTYLE=ji_je_lStyle, $
                       SYMBOL=ji_je_sym, $
                       ;; XTITLE=xTitle, $
                       ;; YTITLE=yTitle, $
                       NAME=ji_je_name, $
                       YLOG=yLog, $
                       /CURRENT, $
                       /OVERPLOT)

     legArr = [legArr,ji_je_plot]

  ENDIF
  leg = LEGEND(TARGET=legArr)

  IF KEYWORD_SET(savePlot) THEN BEGIN

     IF ~KEYWORD_SET(sPName) THEN BEGIN
        sPName = routName + '-JV_Theor.png'
     ENDIF

     IF ~KEYWORD_SET(plotDir) THEN BEGIN
        pDirSuff      = '/cur_and_pot_analysis'
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=pDirSuff
     ENDIF

     PRINT,"Saving to " + sPName + ' ...'

     window1.Save,plotDir+sPName

  ENDIF

END
