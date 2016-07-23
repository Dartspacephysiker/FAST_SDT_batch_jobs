;2016/07/23 This one's for Chris
PRO JOURNAL__20160723__MAKE_PITCH2D_OVERLAID_WITH_DENSITY__ORB_1849,SAVE_PS=save_ps

  SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY
  outPSFile  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--orb1849--pitch2d--fit_overlaid_w_data--2'

  zRange     = [1e6,2e9]
  densStrPos = [0.18,0.18]

  inDir      = '~/software/sdt/batch_jobs/saves_output_etc/'
  ;; fitFile = '20160721--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times.sav'
  ;; fitFile = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--40to40.sav'
  fitFile    = '20160723--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150.sav'

  RESTORE,inDir+fitFile

  ;; fit0 = fit2dgauss_inf_list[0].bestfitstr 
  ;; dat0 = synthpackage[0,1]

  ;; fit0 = fit2dgauss_inf_list[1].bestfitstr 
  ;; dat0 = synthpackage[0,3]

  fit0 = fit2dgauss_inf_list[30].bestfitstr 
  dat0 = synthpackage[0,60]

  ;; fit0 = fit2dgauss_inf_list[300].bestfitstr 
  ;; dat0 = synthpackage[0,594]

  IF ( ABS(fit0.time-dat0.time) GT 0.01 ) THEN STOP

  aRange = [-150,150] 
  fitD = N_2D_FS(fit0,ANGLE=aRange,ENERGY=[900,4e4]) 
  datD = N_2D_FS(dat0,ANGLE=aRange,ENERGY=[900,4e4])

  PRINT,FORMAT='("Fit Density: ",F0.2)',fitD
  PRINT,FORMAT='("Dat Density: ",F0.2)',datD

  densString = STRING(FORMAT='("Fit Density  : ",F0.2,"!CData Density: ",F0.2)',fitD,datD)

  IF KEYWORD_SET(save_ps) THEN BEGIN
     POPEN,plotDir+outPSFile,/LAND,FONT=-1
     DEVICE,/PALATINO,FONT_SIZE=8
  ENDIF ELSE BEGIN
     WINDOW,0,XSIZE=700,YSIZE=700
  ENDELSE

  LOADCT,39

  CONTOUR2D,fit0,/POLAR,/FILL,LIMITS={ZRANGE:zRange},/LABEL
  CONTOUR2D,dat0,/POLAR,/OVERPLOT,LIMITS={ZRANGE:zRange},/LABEL

  XYOUTS,densStrPos[0],densStrPos[1],densString,/NORMAL,FONT=-1,CHARSIZE=1.6 ;,COLOR=250

  IF KEYWORD_SET(save_ps) THEN BEGIN
     PCLOSE
  ENDIF ELSE BEGIN

  ENDELSE

END