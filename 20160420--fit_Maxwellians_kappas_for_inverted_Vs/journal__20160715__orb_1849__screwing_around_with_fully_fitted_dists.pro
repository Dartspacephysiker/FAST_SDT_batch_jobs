;2016/07/15
;Here's what you're getting in this fitfile:
;; GAUSSFITS       LIST  <ID=5  NELEMENTS=285>
;; JE              STRUCT    = -> <Anonymous> Array[1]
;; JEE             STRUCT    = -> <Anonymous> Array[1]
;; KAPPAFITS       LIST  <ID=2  NELEMENTS=285>
;; SYNTHPACKAGE    LIST  <ID=1144  NELEMENTS=3>
PRO JOURNAL__20160715__ORB_1849__SCREWING_AROUND_WITH_FULLY_FITTED_DISTS

  COMPILE_OPT idl2

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/kappa_fits/Orbit_1849__McFadden_et_al_inverted_V'

  outDir   = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile  = '20160716--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--fit_each_angle.sav'

  savePlot = 0
  savePref = 'Orb_1849--Currents_obtained_from_J_2D_B'
  saveDir  = '~/Desktop/'

  RESTORE,outDir+fitFile

  energy = [1000,3.2e4]
  angle  = [-45,45] 

  saveSuff =  STRING(FORMAT='("--eRange_",I0,"-",I0,"--' + $
                     'aRange_",F0.1,"-",F0.1)', $
                     energy, $
                     angle)

  saveName = savePref+saveSuff+'.png'

  ind    = 0 

  nGFits = N_ELEMENTS(synthPackage[0])
  currents = MAKE_ARRAY(3,nGFits,/FLOAT)
  times    = MAKE_ARRAY(3,nGFits,/DOUBLE)

  FOR k=0,nGFits-1 DO BEGIN
     this   = [J_2D_B(synthPackage[0,k],ANGLE=angle,ENERGY=energy), $
               J_2D_B(synthPackage[1,k],ANGLE=angle,ENERGY=energy), $
               J_2D_B(synthPackage[2,k],ANGLE=angle,ENERGY=energy)]*1.6e-9

     currents[*,k] = this
     times[k]      = synthPackage[0,k].time
  ENDFOR

  plotArr = MAKE_ARRAY(3,/OBJ)

  col     = ['black','red','blue']
  names   = ['Obs','Kappa',"Gauss"]
  psym    = ['','*','*']
  ls      = [1,6,6]

  window  = WINDOW(DIMENSIONS=[900,600])

  fontSize = 18
  title   = STRING(FORMAT='("Energy range: [",G0.2," ,  ",G0.2,"] eV!C' + $
                 'Angle range: [",F0.1," ,  ",F0.1,"]")', $
                 energy, $
                 angle)

  xTitle = "Seconds since " + TIME_TO_STR(times[0],/MSEC)
  yTitle = "Current (microA/m!U2!N)"
  yRange = [0,3.2]

  FOR i=0,2 DO BEGIN
     plotArr[0] = PLOT(times-times[0],currents[i,*], $
                       NAME=names[i], $
                       TITLE=title, $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       YRANGE=yRange, $
                       SYMBOL=(i EQ 0) ? !NULL : psym[i], $
                       COLOR=col[i], $
                       FONT_SIZE=fontSize, $
                       LINESTYLE=(i EQ 0) ? !NULL : ls[i], $
                       OVERPLOT=i GT 0, $
                       CURRENT=window)

  ENDFOR

  legend      = LEGEND(TARGET=plotArr[*], $
                       /NORMAL, $
                       FONT_SIZE=fontSize, $
                       POSITION=[0.3,0.8])


  IF KEYWORD_SET(savePlot) THEN BEGIN
     window.Save,saveDir+saveName
     window.Close
     window = !NULL
  ENDIF

  STOP
END