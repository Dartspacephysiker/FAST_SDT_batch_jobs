;2017/03/18
PRO PLOT_JV_DATA_AND_THEORETICAL_CURVES,jvPlotData

  COMPILE_OPT IDL2,STRICTARRSUBS

  negcur_i      = WHERE(jvplotdata.cur LE 0)
  negcur_i      = negcur_i[SORT(jvplotdata.pot[negcur_i])]

  ;;The points that have a clear affinity for kappa = 2
  thesepointslovekappa_ii = WHERE((jvplotdata.pot[negcur_i] LE 4000) AND (jvplotdata.cur[negcur_i]*(-1D-6) GE 1D-6),nLovers)
  PRINT,"THESE POINTS LOVE KAPPA=2.0"
  loveKappa_i = negcur_i[thesepointslovekappa_ii]
  GET_STREAKS,loveKappa_i[SORT(loveKappa_i)],START_I=loveKappa_iStrt_ii,STOP_I=loveKappa_iStop_ii,OUT_STREAKLENS=streakLens
  times = TIME_TO_STR(jvplotdata.time[loveKappa_i[SORT(jvplotdata.time[loveKappa_i])]],/MS)
  FOR k=0,nLovers-1 DO BEGIN
     PRINT,TIME_TO_STR(jvplotdata.time[loveKappa_i[k]])
  ENDFOR

  useInds  = negcur_i
  useInds  = loveKappa_i

  ;; SAVE,KnightRelat30,KnightRelat300,KnightRelat3000,jvplotdata,FILENAME=
  ;; RESTORE,'
  R_Bs__M           = [30,300,3000]
  R_Bs__K           = [30,300,3000]
  kappas            = [2.0,2.0,2.0,1.6]
  TmultFac__kappa   = 1
  R_B1              = 3000

  nR_Bs__M          = N_ELEMENTS(R_Bs__M)
  nR_Bs__K          = N_ELEMENTS(R_Bs__K)
  nDer              = N_ELEMENTS(useInds)

  maxwellJVs        = MAKE_ARRAY(nR_Bs__M,nDer,/DOUBLE)
  kappaJVs          = MAKE_ARRAY(nR_Bs__K,nDer,/DOUBLE)

  MaxwellTransp     = 30
  MaxwellSym        = '*'
  MaxwellColors     = ['Red','Brown','Dark Green']
  MaxwellLinestyle  = ['']
  MaxwellNames      = MAKE_ARRAY(nR_Bs__M,/STRING)

  kappaTransp       = 30
  kappaSym          = ['x','tu']
  kappaColors       = ['Purple','Brown']
  kappaLinestyle    = ['']
  kappaNames        = MAKE_ARRAY(nR_Bs__K,/STRING)

  FOR k=0,nR_Bs__M-1 DO BEGIN
     maxwellJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
                                                        jvplotdata.ndown[useInds], $
                                                        jvplotdata.pot[useInds], $
                                                        R_Bs__M[k], $
                                                        /NO_MULT_BY_CHARGE)

     MaxwellNames    = 'R!DB!N = ' + STRING('(I0)',R_Bs__M[k])

  ENDFOR

  ;; KnightRelat300 = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
  ;;                                                   jvplotdata.ndown[useInds], $
  ;;                                                   jvplotdata.pot[useInds], $
  ;;                                                   300, $
  ;;                                                   /NO_MULT_BY_CHARGE)
  ;; KnightRelat3000 = KNIGHT_RELATION__DORS_KLETZING_4(jvplotdata.tdown[useInds], $
  ;;                                                    jvplotdata.ndown[useInds], $
  ;;                                                    jvplotdata.pot[useInds], $
  ;;                                                    3000, $
  ;;                                                    /NO_MULT_BY_CHARGE)

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaJVs[k,*] = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
                                                       jvplotdata.ndown[useInds], $
                                                       jvplotdata.pot[useInds], $
                                                       R_Bs__K[k], $
                                                       /NO_MULT_BY_CHARGE)

     kappaNames[k] = 'R!DB!N = ' + STRING('(I0)',R_Bs__K[k]) + STRING(' ("$\kappa$=",F0.2)',kappas[k]) + STRING(FORMAT='(",T*=",I0,")")',TmultFac__kappa[k])
  ENDFOR
  ;; kRelat300 = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
  ;;                                                   jvplotdata.ndown[useInds], $
  ;;                                                   jvplotdata.pot[useInds], $
  ;;                                                   300, $
  ;;                                                   /NO_MULT_BY_CHARGE)
  ;; kRelat3000 = KNIGHT_RELATION__DORS_KLETZING_11(kappa,jvplotdata.tdown[useInds], $
  ;;                                                jvplotdata.ndown[useInds], $
  ;;                                                jvplotdata.pot[useInds], $
  ;;                                                3000, $
  ;;                                                /NO_MULT_BY_CHARGE)
  ;; ;; kRelat3001 = KNIGHT_RELATION__DORS_KLETZING_11(kappa1,jvplotdata.tdown[useInds]*20., $
  ;; kRelat3001  = KNIGHT_RELATION__DORS_KLETZING_11(kappa1,jvplotdata.tdown[useInds]*TmultFac, $
  ;;                                                 jvplotdata.ndown[useInds], $
  ;;                                                 jvplotdata.pot[useInds], $
  ;;                                                 3000, $
  ;;                                                 /NO_MULT_BY_CHARGE)

  MaxwellPlots = MAKE_ARRAY(nR_Bs__M,/OBJ)
  kappaPlots   = MAKE_ARRAY(nR_Bs__K,/OBJ)
  wind         = WINDOW(DIMENSIONS=[1000,800])
  yLog         = 0
  dataLStyle   = ''
  dataSym      = 'o'
  dataName     = 'Data'
  xTitle       = 'Potential (V)'
  yTitle       = 'Current density ($\mu$A/m!U2!N), mapped to 100km'

  dataplot     = PLOT(jvplotdata.pot[useInds], $
                  jvplotdata.cur[useInds]*(-1D-6), $
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
                            LINESTYLE=MaxwellLinestyle[k], $
                            SYMBOL=MaxwellSym, $
                            COLOR=MaxwellColors[k], $
                            /OVERPLOT, $
                            NAME=MaxwellNames[k])
  ENDFOR

  FOR k=0,nR_Bs__K-1 DO BEGIN
     kappaPlots[k] = PLOT(jvplotdata.pot[useInds], $
                        kappaJVs[*,k], $
                        TRANSPARENCY=kappaTransp, $
                        LINESTYLE='', $
                        SYMBOL=kappaSym[k], $
                        COLOR=kappaColors[k], $
                        /OVERPLOT, $
                        NAME=kappaNames[k])
  ENDFOR
  leg = LEGEND(TARGET=[dataplot, $
                       MaxwellPlots, $
                       kappaPlots])

  STOP


END
