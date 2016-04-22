PRO JOURNAL__20160422__TEST_KAPPA_ENERGY__LIVADIOTIS_MCCOMAS_EQ_B12


  E_b                = 1.0D            ; bulk energy in eV
  T                  = 1.0D            ; temp in eV
  kappa              = [100,3,2,1.6,1.51,1.50001] ;a few kappas

  nPoints            = 1e4
  topEnergy          = 5*E_b
  energy             = DOUBLE((INDGEN(nPoints)/DOUBLE(nPoints))*topEnergy)


  F_arr              = MAKE_ARRAY(N_ELEMENTS(energy),N_ELEMENTS(kappa),/DOUBLE)
  color              = ['black','blue','red','green','orange','purple']
  FOR i=0,N_ELEMENTS(kappa)-1 DO BEGIN
     A               = [E_b,T,kappa[i]]

     KAPPA_ENERGY__LIVADIOTIS_MCCOMAS_EQ_B12,energy,A,F

     F_arr[*,i]      = F

  ENDFOR

  yRange             = [0,1]
  wind               = WINDOW(DIMENSIONS=[1000,750])

  plotArr            = MAKE_ARRAY(N_ELEMENTS(kappa),/OBJ)

  plotArr[0]         = PLOT(energy/T,F_arr[*,0], $
                            NAME='$\kappa$ = ' + STRCOMPRESS(kappa[0],/REMOVE_ALL), $
                            TITLE='Kappa dists', $
                            YRANGE=yRange, $
                            XTITLE=" $ E/E_b $ ", $
                            YTITLE='Prob. density ($E$)',$
                            ;; /YLOG, $
                            /CURRENT)
  FOR i=1,N_ELEMENTS(kappa)-1 DO BEGIN
     plotArr[i]      = PLOT(energy/T,F_arr[*,i], $
                            NAME='$\kappa$ = ' + STRCOMPRESS(kappa[i],/REMOVE_ALL), $
                            YRANGE=yRange, $
                            ;; /YLOG, $
                            COLOR=color[i], $
                            /OVERPLOT, $
                            /CURRENT)
  ENDFOR

  plotLegend         = LEGEND(TARGET=plotArr,POSITION=[0.7,0.7],/NORMAL)


END