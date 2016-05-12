;2016/05/11 Trying to pull the data out
;Other work to be done includes identifying monoenergetic, inverted-V structures

  ;;fire up
  @startup
  
  energy_electrons          = [5e1,3e4]

  ;; From the FAST ESA IDL demo
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Examples of 3D plots data distribution at peak
  
  ;;some good times for Orb 10000
  ;; timeStr = '99-3-2/17:58:39'
  timeStr = '99-3-2/18:08:42'
  
  ;;For energy spectrogram
  t1Str = '99-3-2/18:08:36'
  t2Str = '99-3-2/18:09:00'
  
  t1    = STR_TO_TIME(t1Str)
  t2    = STR_TO_TIME(t2Str)
  
  
  ;;get_orbit data
  get_fa_orbit,t1,t2 ;,/all
  
  ;;define loss cone angle
  get_data,'ALT',data=alt
  loss_cone_alt=alt.y[0]*1000.0
  lcw = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI

  get_data,'ILAT',data=ilat
  north_south = ABS(ilat.y[0])/ilat.y[0]
  
  get_data,'ORBIT',data=orbit
  orb        = orbit.y[0]

  ;;Loss cone stuff
  IF north_south EQ -1 THEN BEGIN
     e_angle=[180.-lcw,180+lcw] ; for Southern Hemis.
     ;;i_angle=[270.0,90.0]	
     ;;elimnate ram from data
     i_angle=[180.0,360.0]
     i_angle_up=[270.0,360.0]
     
  ENDIF ELSE BEGIN
     e_angle=[360.-lcw,lcw]     ;	for Northern Hemis.
     ;;i_angle=[90.,270.0]
     ;;eliminate ram from data
     i_angle=[0.0,180.0]
     i_angle_up=[90.0,180.0]
     
  ENDELSE

  ;;Make the spectrogram data struct
  GET_EN_SPEC,'fa_ees',units='eflux',name='el',retrace=1,t1=t1,t2=t2,ANGLE=e_angle
  ;;GET the spectrogram data struct
  GET_DATA,'el',data=eSpec
  
  bounds    = [10:25:3] 
  nPlots    = N_ELEMENTS(bounds)

  x         = eSpec.v[0,*]
  xRange    = [eSpec.v[0,-1],eSpec.v[0,0]]
  yRange    = [MIN(eSpec.y[WHERE(eSpec.y GT 0)]),1e9]

  title     = STRING(FORMAT='("Loss-cone e!U-!N spectra from Orbit ",I0,", ",A0)',orb,STRMID(TIME_TO_STR(eSpec.x[bounds[0]]),0,10))
  xTitle    = "Energy (eV)"
  yTitle    = "Energy flux (eV/(cm^2-s-sr-eV)??)"

  orbDate   = STRMID(TIME_TO_STR(eSpec.x[bounds[0]]),0,10)
  plotSN    = STRING(FORMAT='("losscone_e-_spectra--orb_",I0,"__",A0,".png")',orb,orbDate)

  window    = WINDOW(DIMENSION=[800,600])
  plotArr   = MAKE_ARRAY(nPlots,/OBJ) 
  colorList = GENERATE_LIST_OF_RANDOM_COLORS(nPlots) 
  FOR i=0,nPlots-1 DO BEGIN
     plotArr[i] = PLOT(x, $ ;x, $
                       eSpec.y[bounds[i],*], $
                       TITLE=title, $
                       NAME=STRMID(TIME_TO_STR(eSpec.x[bounds[i]]),11,9), $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       YLOG=1, $
                       XLOG=1, $
                       THICK=2.2, $
                       COLOR=colorList[i], $
                       OVERPLOT=i GT 0, $
                       CURRENT=window) 
  ENDFOR
  legend = LEGEND(TARGET=plotArr[*],POSITION=[0.45,0.45],/NORMAL)
  window.save,plotSN

  ;;Get all the dist. functions for the time range of interest
  FOR i=0,FIX(t2-t1)-1 DO BEGIN
     t_temp   = t1 + i
     dat      = get_fa_ees(t_temp)

     out_tStr = (STRMID(TIME_TO_STR(dat.time),11,9)).Replace(':', '_')
     outFN    = STRING(FORMAT='("Orb_",I0,"--",A0,"__",A0,"--particle_dist")',orb,orbDate,out_tStr)
     ;;Open postscript, plot,and close
     cgPS_Open, FILENAME=outFN+'.ps'
     RAINBOW_COLORS,N_COLORS=nLevels
     ;; loadct2,43
     ;; contour2d,dat,/label,ncont=20,/POLAR,/FILL ; plot contour plot 
     contour2d,dat,/label,ncont=20,/FILL ; plot contour plot 
     cgPS_Close

     ;; CGPS2RASTER,outFN+'.ps',outFN+'.gif',/DELETE_PS ;for the movie
     CGPS2RASTER,outFN+'.ps',outFN+'.png',/DELETE_PS
  ENDFOR

  ;;Spectrum stuff
  t=str_to_time(timeStr)
  dat = get_fa_ees(t) ; get electron esa survey
  spec2d,dat,/label ; plot spectra
  pitch2d,dat,/label,energy=[2000,10000] ; plot pitch angle
  contour2d,dat,/label,ncont=20 ; plot contour plot
  dat = get_fa_ies(t); get ion esa survey data
  contour2d,dat,/label,ncont=20 ; plot contour plot
  fu_spec2d,'n_2d_fs',dat,/integ_f,/integ_r ; plot partial density, partial integral densities
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Example functions
  t=str_to_time(timeStr)
  dat = get_fa_ees(t) ; get electron survey data
  print,n_2d_fs(dat,energy=[100,30000]) ; print density >100 eV, #/cm3
  print,j_2d_fs(dat,energy=[100,30000]) ; print flux >100 eV, #/cm2-s
  print,je_2d_fs(dat,energy=[100,30000]) ; print energy flux >100 eV, ergs/cm2-s
  print,v_2d_fs(dat,energy=[100,30000]) ; print Vx,Vy,Vz, km/s
  print,p_2d_fs(dat,energy=[100,30000]) ; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
  print,t_2d_fs(dat,energy=[100,30000]) ; print Tx,Ty,Tz,Tavg, eV
  print,vth_2d_fs(dat,energy=[100,30000]) ; print Vthx,Vthy,Vthz,Vthavg, km/s
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Fitting data to an accelerated Maxwellian
  t=str_to_time(timeStr)
  dat = get_fa_ees(t) ; get data at t
  
  ;;Sum over energies above some amount or other
  funct_fit2d,dat,angle=[-45,45] ; fit the data
  ;; click left button on the peak energy (6keV)
  ;;  click left button on the lower limit to the energy range fit ( 6 keV)
  ;;  click left button on the upper limit to the energy range fit (15 keV)
  ;;  click the right button to end the selection
  ;;  plot will show a maxwellian fit to data over the energy range
  ;;  text on the screen will show the source temperature and density 
