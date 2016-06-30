;2016/05/11 Trying to pull the data out
;Other work to be done includes identifying monoenergetic, inverted-V structures
PRO JOURNAL__20160511__ORB10000__18_08_42

  SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/20160420--fit_Maxwellians_kappas_for_inverted_Vs/Orbit_10000'

  eSpecUnits                = 'DF'

  ;;Contour plot options (units = 'DF' recommended if doing velocity
  do_velocity               = 1
  polarDist                 = 1
  do_postscript             = 1
  plotExt                   = '.gif'
  plotExt                   = '.png'
  
  ;;fire up
  @startup
  
  energy_electrons          = [3e1,3e4]

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
  ;; GET_EN_SPEC,'fa_ees',units='eflux',name='el',retrace=1,t1=t1,t2=t2,ANGLE=e_angle
  GET_EN_SPEC,'fa_ees',UNITS=eSpecUnits,NAME='el',RETRACE=1,T1=t1,T2=t2,ANGLE=e_angle
  ;;GET the spectrogram data struct
  GET_DATA,'el',data=eSpec
  
  bounds    = [10:25:3] 
  nPlots    = N_ELEMENTS(bounds)

  x         = eSpec.v[0,*]
  xRange    = [eSpec.v[0,-1],eSpec.v[0,0]]

  title     = STRING(FORMAT='("Loss-cone e!U-!N spectra from Orbit ",I0,", ",A0)',orb,STRMID(TIME_TO_STR(eSpec.x[bounds[0]]),0,10))
  xTitle    = "Energy (eV)"
  CASE STRUPCASE(eSpecUnits) OF
     'DF': BEGIN
        yTitle    = "Phase space density (s!U3!N/(km!U3!N cm^!U3!N))"
        yRange    = [MIN(eSpec.y[WHERE(eSpec.y GT 0)]),MAX(eSpec.y)]
        ZLIM,lim,1e-14,1e-9,1
        IF KEYWORD_SET(do_velocity) THEN BEGIN
           XLIM,lim,-3e4,3e4
           YLIM,lim,-3e4,3e4
        ENDIF
     END
     'DFSTD': BEGIN
        yTitle    = "Phase space density (s!U3!N m^!U-6!N)"
        yRange    = [MIN(eSpec.y[WHERE(eSpec.y GT 0)]),MAX(eSpec.y)]
        ZLIM,lim,1e-15,1e-5,1
     END
     'EFLUX': BEGIN
        yTitle    = "Energy flux (eV/(cm^2-s-sr-eV)??)"
        yRange    = [MIN(eSpec.y[WHERE(eSpec.y GT 0)]),1e9]
        ZLIM,lim,1e5,1e9,1
     END
  ENDCASE

  orbDate   = STRMID(TIME_TO_STR(eSpec.x[bounds[0]]),0,10)
  plotSN    = STRING(FORMAT='("losscone_e-_spectra--",A0,"--orb_",I0,"__",A0,".png")',eSpecUnits,orb,orbDate)

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
  PRINT,'Saving to ' + plotSN + '...'
  window.save,plotDir+plotSN

  ;;Get all the dist. functions for the time range of interest
  ;; FOR i=0,FIX(t2-t1)-1 DO BEGIN
  ;; FOR i=0,1 DO BEGIN
     ;; t_temp   = t1 + i
  t_temp      = t1
  dat         = get_fa_ees(t_temp)
  WHILE (t_temp LE t2) DO BEGIN
     out_tStr = (STRMID(TIME_TO_STR(dat.time,/MSEC),11,11)).Replace(':', '_')
     outFN    = STRING(FORMAT='("Orb_",I0,"--",A0,"__",A0,"--particle_dist",A0)',orb,orbDate,out_tStr,eSpecUnits)
     ;;Open postscript, plot,and close
     IF KEYWORD_SET(do_postscript) THEN cgPS_Open, FILENAME=plotDir+outFN+'.ps'
     RAINBOW_COLORS,N_COLORS=nLevels

     ;; loadct2,43
     ;; contour2d,dat,/label,ncont=20,/POLAR,/FILL ; plot contour plot 
     CONTOUR2D,dat, $
               VEL=do_velocity, $
               LIMITS=lim,$
               /LABEL, $
               NCONT=29, $
               /FILL, $
               POLAR=polarDist, $
               UNITS=eSpecUnits ; plot contour plot 

     IF KEYWORD_SET(do_postscript) THEN BEGIN
        cgPS_Close
        CGPS2RASTER,plotDir+outFN+'.ps',plotDir+outFN+plotExt,/DELETE_PS
     ENDIF 
     
     dat      = get_fa_ees(t_temp,/ADVANCE)

  ENDWHILE
  ;; ENDFOR

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


END