;;2016/05/19
;;Let's see how Newell's algorithm does for picking out the clearly visible inverted V from orbit 17773 treated by Elphic et al. [1998]
PRO JOURNAL__20160520__ORBIT_1773__TRY_PLOTTING_EVENT_TYPES_WITH_TPLOT,EVENTS=events

  energy_electrons        = [50.,30000.]
  t1Str                   = '97-2-1/09:25:30'
  t2Str                   = '97-2-1/09:28:00'
  ;; t1Str                   = '97-2-1/09:26:12'
  ;; t2Str                   = '97-2-1/09:28:00'

  t1                      = STR_TO_TIME(t1Str)
  t2                      = STR_TO_TIME(t2Str)

  use_sc_pot_for_min      = 1

  ;;Get orbit stuff
  GET_FA_ORBIT,t1,t2

  ;;Define loss cone angle
  GET_DATA,'ALT',DATA=alt
  loss_cone_alt           = alt.y[0]*1000.0
  lcw                     = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
  GET_DATA,'ILAT',DATA=ilat
  north_south             = ABS(ilat.y[0])/ilat.y[0]
  
  if north_south EQ -1 then begin
     e_angle              = [180.-lcw,180+lcw] ; for Southern Hemis.

     ;;Eliminate ram from data
     i_angle              = [180.0,360.0]
     i_angle_up=[270.0,360.0]
     
  endif else begin
     e_angle              = [360.-lcw,lcw] ;	for Northern Hemis.

     ;;Eliminate ram from data
     i_angle              = [0.0,180.0]
     i_angle_up           = [90.0,180.0]
     
  endelse

  ;;Calculate current from ESAs
  IF KEYWORD_SET(use_sc_pot_for_min) THEN BEGIN
     GET_SC_POTENTIAL,T1=t1,T2=t2,DATA=sc_pot

     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=t1,T2=t2, $
                          NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot, $
                    OUT_SC_TIME=out_sc_time, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     ;;A little check thing
     ;; FOR i=0,N_ELEMENTS(je.x)-1 DO PRINT,TIME_TO_STR([out_sc_time[i+1],je.x[i]],/MSEC),out_sc_time[i+1]-je.x[i]
  ENDIF ELSE BEGIN

     GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2, $
                NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle
  ENDELSE

  ;;Remove_crap
  GET_DATA,'Je_lc',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  je_lc_tmp_time          = tmp.x[keep2]
  je_lc_tmp_data          = tmp.y[keep2]
  STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:(-1.)*je_lc_tmp_data}
  YLIM,'Je_lc',-1.e9,2.e9
  OPTIONS,'Je_lc','yticks',4                           ; set y-axis labels
  OPTIONS,'Je_lc','ytickname',['-1e9','0','1e9','2e9'] ; set y-axis labels
  OPTIONS,'Je_lc','ytickv',[-1e9,0,1e9,2e9]            ; set y-axis labels
  OPTIONS,'Je_lc','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N'

  ;;Get electron energy flux in loss cone
  GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='JEe',ENERGY=energy_electrons,ANGLE=e_angle
  GET_DATA,'JEe',DATA=tmp
  ;;remove crap
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  jee_tmp_time            = tmp.x[keep2]
  jee_tmp_data            = tmp.y[keep2]
  STORE_DATA,'JEe',DATA={x:jee_tmp_time,y:jee_tmp_data}

  ;;All around the world
  ;; GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
  ;;            NAME='JEe_tot',ENERGY=energy_electrons
  ;; GET_DATA,'JEe_tot',DATA=tmp
  ;; ;;remove crap
  ;; keep1                   = WHERE(FINITE(tmp.y) NE 0)
  ;; tmp.x                   = tmp.x[keep1]
  ;; tmp.y                   = tmp.y[keep1]
  ;; keep2                   = WHERE(ABS(tmp.y) GT 0.0)

  ;; jee_tot_tmp_time        = tmp.x[keep2]
  ;; jee_tot_tmp_data        = tmp.y[keep2]
  ;; STORE_DATA,'JEe_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_tmp_data}

  ;; ;;Get ratios for mapping to 100 km as well as signs for ensuring downward is positive
  ;; GET_FA_ORBIT,jee_tmp_time,/TIME_ARRAY,/ALL
  ;; GET_DATA,'ILAT',DATA=tmp
  ;; sgn_flx                 = tmp.y/ABS(tmp.y)
  ;; GET_DATA,'B_model',DATA=tmp1
  ;; GET_DATA,'BFOOT',DATA=tmp2
  ;; mag1                    = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
  ;; mag2                    = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
  ;; ratio                   = (mag2/mag1)

  ;; ;;Scale electron energy flux to 100km, pos flux earthward
  ;; jee_ionos_tmp_data      = sgn_flx*jee_tmp_data*ratio
  ;; STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
  
  ;; jee_tot_ionos_tmp_data  = sgn_flx*jee_tot_tmp_data*ratio
  ;; STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_0',ANGLE=e_angle,RETRACE=1,T1=t1,T2=t2,/CALIB


  ;;Now get 'em all, see what we gots
  GET_DATA,'el_0', DATA=eSpec                                            ; get data structure
  GET_DATA,'JEe',DATA=Jee
  GET_DATA,'Je_lc',DATA=Je
  ;; GET_DATA,'JEe_tot',DATA=tmp

  ;;Because we need MLT
  GET_FA_ORBIT,eSpec.x,/TIME_ARRAY
  GET_DATA,'MLT',DATA=mlt
  mlt       = mlt.y

  GET_DATA,'ILAT',DATA=ilat
  ilat      = ilat.y

  GET_DATA,'ALT',DATA=alt
  alt      = alt.y

  GET_DATA,'ORBIT',DATA=orbit
  orbit    = orbit.y


  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpec,Jee,Je,mlt,ilat,alt,orbit,events,SC_POT=out_sc_pot
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now put the plots together!
  
  ;;First spectral types
  PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,events,TPLOT_NAME=st_tplot
  
  ;;Now the spectrum itself
  ;; GET_EN_SPEC,'fa_' + survOrBurst + '_c',UNITS='eflux',NAME='el_0',ANGLE=e_angle,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_EN_SPEC,'fa_ees_c',UNITS='eflux',NAME='el_0',ANGLE=e_angle,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_0', DATA=tmp                                            ; get data structure
  tmp.y                   = tmp.y>1.e1        ; Remove zeros 
  tmp.y                   = ALOG10(tmp.y)     ; Pre-log
  STORE_DATA,'el_0', DATA=tmp                                          ; store data structure
  OPTIONS,'el_0','spec',1                                              ; set for spectrogram
  ZLIM,'el_0',7,9,0                                                    ; set z limits
  YLIM,'el_0',4,30000,1                                                ; set y limits
  OPTIONS,'el_0','ytitle','e- 0!Uo!N-22!Uo!N!CEnergy (eV)'               ; y title
  OPTIONS,'el_0','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'el_0','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','yticks',3                                            ; set y-axis labels
  OPTIONS,'el_0','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'el_0','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  ;; OPTIONS,'el_0','panel_size',1.5                                        ; set panel size

  IF KEYWORD_SET(save_png) THEN BEGIN
     CGPS_OPEN, './plots/'+fileName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(save_ps) THEN BEGIN
        POPEN,'./plots/'+fileName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
        DEVICE,/PALATINO,FONT_SIZE=8
     ENDIF ELSE BEGIN
        WINDOW,0,XSIZE=600,YSIZE=800
     ENDELSE
  ENDELSE
  
  ;; LOADCT,74
  LOADCT,39

  TPLOT,['el_0',st_tplot],VAR_LABEL=['ALT','MLT','ILAT'],TRANGE=tplot_tRange
  ;;got more than we need so smoothing can be nice
  ;; TLIMIT,t1+10.,t2-10.


  IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
     ;; CGPS_CLOSE, PNG=KEYWORD_SET(save_png),DELETE_PS=KEYWORD_SET(save_png);, WIDTH=1000
     PCLOSE
  ENDIF ELSE BEGIN
     ;; IF KEYWORD_SET(save_ps) THEN BEGIN
     ;;    PCLOSE
     ;; ENDIF
  ENDELSE

END