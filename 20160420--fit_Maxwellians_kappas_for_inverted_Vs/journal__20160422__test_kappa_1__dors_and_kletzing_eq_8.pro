PRO JOURNAL__20160422__TEST_KAPPA_1__DORS_AND_KLETZING_EQ_8


  n                  = 1e-6              ;density in m^-3
  kappa              = [100,30,10,3,1.5] ;should be Maxwellian
  w                  = 1                 ;most likely speed in m/s


  A                  = [n,w,kappa]

  nPoints            = 1e3
  topV               = 10
  V                  = (INDGEN(nPoints)/DOUBLE(nPoints))*topV


  F_arr              = MAKE_ARRAY(N_ELEMENTS(V),N_ELEMENTS(kappa),/DOUBLE)
  color              = ['black','blue','red','green','orange']
  FOR i=0,N_ELEMENTS(kappa)-1 DO BEGIN
     A               = [n,w,kappa[i]]

     KAPPA_1__DORS_AND_KLETZING_EQ_8,V,A,F

     F_arr[*,i]      = F

  ENDFOR

  wind               = WINDOW(DIMENSIONS=[1000,750])

  plotArr            = MAKE_ARRAY(N_ELEMENTS(kappa),/OBJ)

  plotArr[0]         = PLOT(V/w,F_arr[*,0], $
                            NAME='$\kappa$ = ' + STRCOMPRESS(kappa[0],/REMOVE_ALL), $
                            TITLE='Kappa dists', $
                            XTITLE=" $ V/w $ ", $
                            YTITLE='Prob. density ($\vec{v}$)',$
                            /YLOG, $
                            /CURRENT)
  FOR i=1,N_ELEMENTS(kappa)-1 DO BEGIN
     plotArr[i]      = PLOT(V/w,F_arr[*,i], $
                            NAME='$\kappa$ = ' + STRCOMPRESS(kappa[i],/REMOVE_ALL), $
                            /YLOG, $
                            COLOR=color[i], $
                            /OVERPLOT, $
                            /CURRENT)
  ENDFOR

  plotLegend         = LEGEND(TARGET=plotArr,POSITION=[0.35,0.3],/NORMAL)


END