;2018/12/14
PRO JOURNAL__20181214__KAPPA2__SIZE_OF_ERRORS_FOR_PARAMS_DENSITIES__ORBS_1607_4682

  COMPILE_OPT IDL2,STRICTARRSUBS

  errDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
  momDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/cur_and_pot_analysis/'

  errF1 = errDir + '20180817-orb_1607-KandGfits-ees-2NDKAPPA-only_fit_peak_eRange-sRate0_63-01_04_20__500-01_05_54__000-2DPARMERRORS_TWOSIDED-5000Rolls.sav'

  ;; momF1 = momDir + 'Orbit_1607--2NDKAPPA-meal-01_04_20__500-01_05_54__000-sc_pot-sRate0_63.sav'
  ;; mamF1 = momDir + 'Orbit_1607--2NDKAPPA-comboMeal.sav'
  mamF1 = momDir + 'Orbit_1607--2NDKAPPA-comboMeal2.sav'

  ;; momF2 = momDir + 'Orbit_4682--2NDKAPPA-meal-09_05_40__000-09_06_55__000-sc_pot-sRate1_25.sav'
  ;; errF2 = errDir + '20180816-orb_4682-KandGfits-ees-2NDKAPPA-only_fit_peak_eRange-sRate1_25-09_05_40__000-09_06_55__000-2DPARMERRORS_TWOSIDED-5000Rolls.sav'
  mamF2 = 'Orbit_4682--2NDKAPPA-comboMeal2.sav'

  fArr = [mamF1,mamF2]
  orbArr = [1607,4682]

  ;; percentiles
  pctls  = (FINDGEN(50)+1.)*2./100.

  FOR k=0,N_ELEMENTS(fArr)-1 DO BEGIN

     orbit = orbArr[k]
     file = fArr[k]

     PRINT,orbit,file

     RESTORE,file

     ;; relErrDensK = MAX(ABS(dens2dk.dy),DIMENSION=2)/dens2dk.y*100
     ;; relErrDensG = MAX(ABS(dens2dg.dy),DIMENSION=2)/dens2dg.y*100
     ;; relErrDensD = dens2dd.dy/dens2dd.y*100

     ;; relErrK = MAX(ABS(kappa2derr),DIMENSION=2)/k2dvals*100

     relErrDensK = (MAX(ABS(dens2dk.dy),DIMENSION=2)+MIN(ABS(dens2dk.dy),DIMENSION=2))/dens2dk.y*100
     relErrDensG = (MAX(ABS(dens2dg.dy),DIMENSION=2)+MIN(ABS(dens2dg.dy),DIMENSION=2))/dens2dg.y*100
     relErrDensD = dens2dd.dy/dens2dd.y*100

     relErrK = (MAX(ABS(kappa2derr),DIMENSION=2)+MIN(ABS(kappa2derr),DIMENSION=2))/k2dvals*100

     ;; PRINT,FORMAT='(1000(F0.2,","))',relErrDensK

     densPK = CGPERCENTILES(relErrDensK,PERCENTILES=pctls)
     densPG = CGPERCENTILES(relErrDensG,PERCENTILES=pctls)
     densPD = CGPERCENTILES(relErrDensD,PERCENTILES=pctls)

     kappaP = CGPERCENTILES(relErrK,PERCENTILES=pctls)

     PRINT,""
     PRINT,"Kappa"
     PRINT,"----------"
     PRINT,FORMAT='("PCTL",TR4,A8)',"Kappa"
     FOR j=0,N_ELEMENTS(pctls)-1 DO PRINT,FORMAT='(I3,TR5,F8.2)',pctls[j]*100,kappaP[j]

     PRINT,""
     PRINT,"DENSITIES"
     PRINT,"----------"
     PRINT,FORMAT='("PCTL",TR4,A8,TR4,A8,TR4,A8)',"Kappa","Gauss","Data"
     FOR j=0,N_ELEMENTS(pctls)-1 DO PRINT,FORMAT='(I3,TR5,F8.2,TR5,F8.2,TR5,F8.2)',pctls[j]*100,densPK[j],densPG[j],densPD[j]
        
     IF KEYWORD_SET(show_plots) THEN BEGIN
        wind = WINDOW(DIMENSIONS=[800,800], $
                      TITLE=STRING(FORMAT='("Orbit ",I0, " MAX(Density err) / density %")',orbit))

        scat1  = SCATTERPLOT(pctls,densPK, $
                             YTITLE='Kappa', $
                             XTITLE='Percentile', $
                             YLOG=0, $
                             XRANGE=[0,0.9], $
                             YRANGE=[0,100], $
                             LAYOUT=[1,3,1], $
                             CURRENT=wind)
     ENDIF     

     STOP

  ENDFOR

  STOP

END
