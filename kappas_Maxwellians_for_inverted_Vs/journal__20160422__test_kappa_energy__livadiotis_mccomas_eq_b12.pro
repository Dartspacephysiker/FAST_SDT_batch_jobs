PRO JOURNAL__20160422__TEST_KAPPA_ENERGY__LIVADIOTIS_MCCOMAS_EQ_B12


  E_b                = 1.0D            ; bulk energy in eV
  T                  = 1.0D            ; temp in eV
  kappa              = [100,3,2,1.6,1.51,1.50001] ;a few kappas

  nPoints            = 1e4
  topEnergy          = 5*E_b
  energy             = DOUBLE((INDGEN(nPoints)/DOUBLE(nPoints))*topEnergy)


  F_arr              = MAKE_ARRAY(N_ELEMENTS(energy),N_ELEMENTS(kappa),/DOUBLE)
  color              = ['red','blue','green','purple','cyan','brown']
  linestyle          = ['-',':','--','-.','-','-.']
  thick              = 2.0
  fontSize           = 15
  format             = '(' + ['I0','I0','I0','F0.1','F0.2','F0.5'] + ')'
  FOR i=0,N_ELEMENTS(kappa)-1 DO BEGIN
     A               = [E_b,T,kappa[i]]

     KAPPA_ENERGY__LIVADIOTIS_MCCOMAS_EQ_B12,energy,A,F

     F_arr[*,i]      = F

  ENDFOR

  yRange             = [0,1]
  wind               = WINDOW(DIMENSIONS=[1000,500])

  plotArr            = MAKE_ARRAY(N_ELEMENTS(kappa),/OBJ)

  plotArr[0]         = PLOT(energy/T,F_arr[*,0], $
                            NAME='$\kappa$ = ' + STRING(FORMAT=format[0],kappa[0]), $
                            TITLE='Kappa distribution of energy', $
                            YRANGE=yRange, $
                            COLOR=color[0], $
                            LINESTYLE=linestyle[0], $
                            THICK=thick, $
                            FONT_SIZE=fontSize, $
                            XTITLE=" $ E/E_b $ ", $
                            YTITLE='Prob. density ($E$)',$
                            ;; /YLOG, $
                            /CURRENT)
  FOR i=1,N_ELEMENTS(kappa)-1 DO BEGIN
     plotArr[i]      = PLOT(energy/T,F_arr[*,i], $
                            NAME='$\kappa$ = ' + STRING(FORMAT=format[i],kappa[i]), $
                            YRANGE=yRange, $
                            ;; /YLOG, $
                            COLOR=color[i], $
                            LINESTYLE=linestyle[i], $
                            THICK=thick, $
                            FONT_SIZE=fontSize, $
                            /OVERPLOT, $
                            /CURRENT)
  ENDFOR

  plotLegend         = LEGEND(TARGET=plotArr,POSITION=[0.7,0.7],/NORMAL,FONT_SIZE=fontSize)


END