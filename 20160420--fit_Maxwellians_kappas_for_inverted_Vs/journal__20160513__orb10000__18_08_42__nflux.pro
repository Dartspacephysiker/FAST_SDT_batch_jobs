;2016/05/11 Trying to pull the data out
;Other work to be done includes identifying monoenergetic, inverted-V structures
PRO JOURNAL__20160513__ORB10000__18_08_42__NFLUX

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
  GET_EN_SPEC,'fa_ees',UNITS=eSpecUnits,NAME='el',RETRACE=1,T1=t1,T2=t2,ANGLE=e_angle
  ;;GET the spectrogram data struct
  GET_DATA,'el',data=eSpec
  
  ;;Make the nFlux data struct
  GET_2DT_TS,'j_2d_fs_en','fa_ees',t1=t1,t2=t2,name='Je_en',energy=energy_electrons,ANGLE=e_angle
  ;;GET the spectrogram data struct
  GET_DATA,'Je_en',data=je_en

  ;; bounds    = [10:25:3] 
  bounds    = [15:17] 
  nPlots    = N_ELEMENTS(bounds)

  x         = REVERSE(REFORM(eSpec.v[0,*]))
  xRange    = [eSpec.v[0,-1],eSpec.v[0,0]]

  title     = STRING(FORMAT='("Loss-cone e!U-!N spectra from Orbit ",I0,", ",A0)',orb,STRMID(TIME_TO_STR(je_en.x[bounds[0]]),0,10))
  xTitle    = "Energy (eV)"
  yTitle    = "Losscone Number flux (#/cm!U2!N-s)"
  yRange    = [MIN(je_en.y[WHERE(je_en.y GT 0)]),MAX(je_en.y)]

  orbDate   = STRMID(TIME_TO_STR(je_en.x[bounds[0]]),0,10)
  plotSN    = STRING(FORMAT='("losscone_numberflux--orb_",I0,"__",A0,".png")',orb,orbDate)

  window    = WINDOW(DIMENSION=[800,600])
  plotArr   = MAKE_ARRAY(nPlots,/OBJ) 
  colorList = GENERATE_LIST_OF_RANDOM_COLORS(nPlots) 
  FOR i=0,nPlots-1 DO BEGIN
     plotArr[i] = PLOT(x, $ ;x, $
                       je_en.y[bounds[i],*], $
                       TITLE=title, $
                       NAME=STRMID(TIME_TO_STR(je_en.x[bounds[i]]),11,9), $
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
  window.save,plotSN

;Bonus
fu_spec2d,'n_2d_fs',dat,OUT_PARTIAL=dn_2d,ANGLE=e_angle ;,/integ_f,/integ_r ; plot partial density, partial integral densities
fu_spec2d,'j_2d_fs',dat,OUT_PARTIAL=dj_2d,ANGLE=e_angle ;,/integ_f,/integ_r ; plot partial density, partial integral densities

  SAVE,je_en,x,dn_2d,dj_2d,eSpec,bounds,orb,orbDate,nPlots,FILENAME='/SPENCEdata/software/sdt/batch_jobs/20160420--fit_Maxwellians_kappas_for_inverted_Vs/nFlux_and_eSpec--orb_10000__18_08_36-18_09_00.sav'

END