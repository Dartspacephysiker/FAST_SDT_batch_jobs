;2016/05/16 Reproducing Fig. 1 from Elphic et al. [1998] as a step toward gaining confidence in our kappa fits
;FA_FIELDS_PHASE: estimates the angle of the vector pointing towards the Sun and along the ambient magnetic
;field in the spacecraft spin plane using on-board sun sensor and magnetometer data.
;
;FA_FIELDS_DESPIN: estimates despun spin plane electric field (original version, optimized for DC E-field
;estimation (few Hz and below)).
;
;FA_FIELDS_DESPIN_SVY_LONG: newer version of FA_FIELDS_DESPIN, again optimized for DC E-field
;estimation.
;
;FA_FIELDS_DESPIN_{4K,16K,HSBM}: newer versions of despin codes optimized for Burst and HSBM data;
;HSBM supports three-axis despin. Results are stored as TPLOT quantities.
;
;FA_FIELDS_DESPIN_HG: refilters HG-type data from the original high-pass frequency of 3.5 kHz to 300 Hz, then
;despins and stores as a TPLOT quantity.
;
;SIMPLE_DESPIN: a bare-bones despin routine that handles the transformation of contiguously sampled data from
;pairs of antennas. No gain or offset adjustment is performed. Suitable for careful use on AC electric fields. Produces
;FAST fields data structures.
;
;All the newer versions of DESPIN support spectral density estimation of the despun electric field data within the
;routines themselves, rather than through a separate call to the spectral density estimation routines (see Spectral
;Estimates below).

PRO JOURNAL__20160628__ORB_12111_BIG_DAWNWARD_IMF,SAVE_PNG=save_png,SAVE_PS=save_ps

  panelSize               = 2.5
  energy_electrons        = [50.,30000.]
  energy_ions             = [10.,30000.]
  survOrBurst             = 'ees'
  iSurvOrBurst            = 'ies'

  ucla_mag_despin         = 1

  t1Str                   = '1999-09-12/19:24:30'
  t2Str                   = '1999-09-12/19:34:00'

  t1                      = STR_TO_TIME(t1Str)
  t2                      = STR_TO_TIME(t2Str)

  outPlotName             = 'Orb_12111--Extreme_dawnward_IMF--Fig_1'

  ;;Get fields stuff, eFields and magFields
  FA_FIELDS_DESPIN,T1=t1,T2=t2,DAT=despun_E
  GET_DATA,'E_NEAR_B',DATA=eNearB
  GET_DATA,'E_ALONG_V',DATA=eAlongV

  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     UCLA_MAG_DESPIN

     GET_DATA,'dB_fac_v',DATA=db_fac
     mintime              = MIN(ABS(t1-db_fac.x),ind1)
     mintime              = MIN(ABS(t2-db_fac.x),ind2)
     
     magx                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
     magy                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}
     magz                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

  ENDIF

  ;;Get orbit stuff
  GET_FA_ORBIT,magz.x,/TIME_ARRAY ;,/all

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

  ;;Get speed and position for calculation of mag stuff
  GET_DATA,'fa_vel',DATA=vel
  speed                   = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  old_pos                 = 0.
  position                = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  speed_mag_point         = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(magz.x)-2 DO BEGIN
     speed_point_ind      = MIN(ABS(vel.x-magz.x[j]),ind)

     speed_mag_point[j]   = speed[ind]
     samplingperiod       = magz.x[j+1] - magz.x[j]

     position[j]          = old_pos + speed_mag_point[j]*samplingperiod
     old_pos              = position[j]
  ENDFOR

  ;;Calculate the current from mag
  ;; magz.y=smooth(magz.y,40)
  ;; store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
  deltaBX                 = DERIV(position,SMOOTH(magz.y,100))
  ;; jtemp                = ABS(1.0e-3*(deltaBx)/1.26e-6)
  ;; jtemp                = 1.0e-3*(deltaBx)/1.26e-6
  ;;in number flux units
  ;; jtemp                   = 1.0e-3*(deltaBx)/1.26e-6  * (DOUBLE(1. / 1.6e-9))

  ;; sign_jtemp              = ABS(deltaBx)/deltaBx
  ;; STORE_DATA,'jtemp',DATA={x:magz.x,y:jtemp}
  ;; ;; OPTIONS,'jtemp','psym','10'
  ;; ;; OPTIONS,'jtemp','fill',1
  ;; YLIM,'jtemp',-2.5e9,2.5e9
  ;; OPTIONS,'jtemp','yticks',4                           ; set y-axis labels
  ;; OPTIONS,'jtemp','ytickname',['-1e9','0','1e9','2e9'] ; set y-axis labels
  ;; OPTIONS,'jtemp','ytickv',[-2e9,-1e9,0,1e9,2e9]            ; set y-axis labels
  ;; OPTIONS,'jtemp','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N)'
  ;; OPTIONS,'jtemp,','tplot_routine','polyfill_tplot'
  ;; ;; OPTIONS,'jtemp','color','808080'x
  ;; OPTIONS,'jtemp','color',250

  ;;make fill for this guy
  ;; jtemp_fill = {x:[magz.x[0],magz.x,magz.x[-1]],y:[0.,jtemp,0.]}
  ;; STORE_DATA,'jtemp_fill',DATA={x:[magz.x[0],magz.x,magz.x[-1]],y:[0.,jtemp,0.]} ;jtemp_fill
  ;; STORE_DATA,'jtemp_fill',DATA={x:[magz.x[0],magz.x,magz.x[-1]],y:[0.,jtemp,0.]} ;jtemp_fill
  ;; OPTIONS,'jtemp_fill,','tplot_routine','polyfill_tplot'
  ;; OPTIONS,'jtemp_fill','color','808080'x
  ;; OPTIONS,'jtemp','line_fill',1
  ;; OPTIONS,'jtemp_fill,','tplot_routine','polyfill'
  ;; OPTIONS,'jtemp_fill','color','808080'x

  ;;calculate potential, since we're thinking about it
  ;; magz                    = {time:magz.x,comp1:magz.y,ncomp:1}
  ;; eAlongV                 = {time:eAlongV.x,comp1:eAlongV.y}
  ;; FA_FIELDS_COMBINE,magz,eAlongV,RESULT=eAlongV_interp,/INTERP,DELT_T=50.,/TALK
  ;; potential               = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  ;; potential[0]            = 0.0D
  ;; FOR i=1,N_ELEMENTS(potential)-1 DO BEGIN
  ;;    potential[i]         = TSUM(position[0:i],eAlongV_interp[0:i])
  ;; ENDFOR

  ;; ;;Now set them right again
  ;; magz                    = {x:magz.time,y:magz.comp1}
  ;; eAlongV                 = {x:eAlongV.time,y:eAlongV.comp1}
  ;; ;; potential            = {x:magz.x,y:potential}
  ;; STORE_DATA,'POTENTIAL',DATA={x:magz.x,y:potential/1000.-8000.}
  ;; OPTIONS,'POTENTIAL','ytitle','Potential!C(V)' ; y title
  ;; YLIM,'POTENTIAL',-8500,1900

  ;;Calculate current from ESAs
  GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle

  ;;Remove_crap
  GET_DATA,'Je_lc',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  je_lc_tmp_time          = tmp.x[keep2]
  je_lc_tmp_data          = tmp.y[keep2]
  STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:je_lc_tmp_data}
  YLIM,'Je_lc',-1.e9,4.e9
  OPTIONS,'Je_lc','yticks',5                           ; set y-axis labels
  OPTIONS,'Je_lc','ytickname',['-1e9','0','1e9','2e9','3e9'] ; set y-axis labels
  OPTIONS,'Je_lc','ytickv',[-1e9,0,1e9,2e9,3e9]            ; set y-axis labels
  OPTIONS,'Je_lc','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N)'
  OPTIONS,'Je_lc','panel_size',panelSize                         ; set panel size

  ;;Get electron energy flux in loss cone
  GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='JEe',ENERGY=energy_electrons,ANGLE=e_angle

  GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='JEe_tot',ENERGY=energy_electrons

  GET_DATA,'JEe',DATA=tmp
  ;;remove crap
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  jee_tmp_time            = tmp.x[keep2]
  jee_tmp_data            = tmp.y[keep2]
  OPTIONS,'JEe','ytitle','e!U-!N Energy Flux!C!C(mW/m!U2!N)' ; y title
  OPTIONS,'JEe','panel_size',panelSize                         ; set panel size

  STORE_DATA,'JEe',DATA={x:jee_tmp_time,y:jee_tmp_data}

  GET_DATA,'JEe_tot',DATA=tmp
  ;;remove crap
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)

  jee_tot_tmp_time        = tmp.x[keep2]
  jee_tot_tmp_data        = tmp.y[keep2]
  STORE_DATA,'JEe_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_tmp_data}

  ;;Ions
  GET_2DT,'j_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Ji',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  GET_2DT,'je_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Jei',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB

  GET_DATA,'Ji',DATA=tmp
  keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Jei',DATA=tmp
  keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Ji',DATA=tmp
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  ji_tmp_time             = tmp.x[keep2]
  ji_tmp_data             = tmp.y[keep2]
  STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
  GET_DATA,'Jei',DATA=tmp
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  jei_tmp_time             = tmp.x[keep2]
  jei_tmp_data             = tmp.y[keep2]
  STORE_DATA,'Jei',DATA={x:jei_tmp_time,y:jei_tmp_data}

  GET_DATA,'Je_lc',DATA=tmp_je
  GET_DATA,'JEe',DATA=tmp_jee
  GET_DATA,'Ji',DATA=tmp_ji
  GET_DATA,'Jei',DATA=tmp_jei
  ;; chare                   = tmp_jee.y/tmp_je.y*6.242*1.0e11
  ;; chari                   = tmp_jei.y/tmp_ji.y*6.242*1.0e11
  ;; chartot                 = chare+chari
  ;; STORE_DATA,'charepanel',DATA={x:[[tmp_jee.x],[tmp_jee.x],[tmp_jee.x]],y:[[chari],[chare],[chartot]]}

  ;; OPTIONS,'charepanel','tplot_routine','mplot'
  ;; OPTIONS,'charepanel','ytitle','E/q Volts'
  ;; OPTIONS,'charepanel','labels',['Ion','Electron','Total']
  ;; OPTIONS,'charepanel','colors',[red,green,20]
  ;; OPTIONS,'charepanel','labflag',1
  ;; OPTIONS,'charepanel','yticks',5                                     ; set y-axis labels
  ;; OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  ;; OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels



  ;;Get ratios for mapping to 100 km as well as signs for ensuring downward is positive
  GET_FA_ORBIT,jee_tmp_time,/TIME_ARRAY,/ALL
  GET_DATA,'ILAT',DATA=tmp
  sgn_flx                 = tmp.y/ABS(tmp.y)
  GET_DATA,'B_model',DATA=tmp1
  GET_DATA,'BFOOT',DATA=tmp2
  mag1                    = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
  mag2                    = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
  ratio                   = (mag2/mag1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;get B model on same time stuff as dB
  ;;Subtract B_model from despun B_east
  ;; magz                 = {time:magz.x,comp1:magz.y,ncomp:1}
  ;; bModel               = {time:tmp1.x,comp1:tmp1.y[*,1]}
  ;; FA_FIELDS_COMBINE,magz,bModel,RESULT=bModel_interp,/INTERP,DELT_T=50.,/TALK
  ;; dB_east              = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  ;; dB_east              = magz.comp1 - bModel_interp

  ;;Now set them right again, store
  ;; magz                 = {x:magz.time,y:magz.comp1}
  ;; STORE_DATA,'dB_East',DATA={x:magz.x,y:magz.y}
  ;; OPTIONS,'dB_East','ytitle',CGGREEK('Delta') + 'B!DEast!N (nT)' ; y title
  ;; OPTIONS,'dB_East','yticks',4                                   ; set y-axis labels
  ;; OPTIONS,'dB_East','ytickname',['-00','0','100','200']          ; set y-axis labels
  ;; OPTIONS,'dB_East','ytickv',[-100,0,100,200]                    ; set y-axis labels
  

  ;;Scale electron energy flux to 100km, pos flux earthward
  jee_ionos_tmp_data      = sgn_flx*jee_tmp_data*ratio
  STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
  
  jee_tot_ionos_tmp_data  = sgn_flx*jee_tot_tmp_data*ratio
  STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion spectrogram - survey data, remove retrace, upgoing ions
  GET_EN_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_180',ANGLE=[135,225],RETRACE=1,T1=t1,T2=t2
  GET_DATA,'ion_180',DATA=tmp                                           ; get data structure
  tmp.y = tmp.y > 1.                                                      ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                 ; Pre-log
  STORE_DATA,'ion_180',DATA=tmp                                         ; store data structure
  OPTIONS,'ion_180','spec',1                                            ; set for spectrogram
  ZLIM,'ion_180',5,8.0,0                                                  ; set z limits
  YLIM,'ion_180',3,30000,1                                              ; set y limits
  OPTIONS,'ion_180','ytitle','i+ 135!Uo!N-180!Uo!N!C!CEnergy (eV)'      ; y title
  OPTIONS,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'          ; z title
  OPTIONS,'ion_180','x_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','y_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','yticks',3                                          ; set y-axis labels
  OPTIONS,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'ion_180','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'ion_180','panel_size',panelSize                                        ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion pitch angle spectrogram - survey data, remove retrace, >10 ions
  GET_PA_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_pa',ENERGY=[10,30000],RETRACE=1,/SHIFT90,T1=t1,T2=t2
  GET_DATA,'ion_pa',DATA=tmp                              ; get data structure
  tmp.y = tmp.y > 1.                                        ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                   ; Pre-log
  STORE_DATA,'ion_pa',DATA=tmp                            ; store data structure
  OPTIONS,'ion_pa','spec',1                               ; set for spectrogram
  ZLIM,'ion_pa',5,8.0,0                                     ; set z limits
  YLIM,'ion_pa',-90,270,0                                ; set y limits
  OPTIONS,'ion_pa','ytitle','i+ >10 eV!C!C Pitch Angle'   ; y title
; OPTIONS,'ion_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'ion_pa','x_no_interp',1                        ; don't interpolate
  OPTIONS,'ion_pa','y_no_interp',1                        ; don't interpolate
  OPTIONS,'ion_pa','yticks',4                             ; set y-axis labels
  OPTIONS,'ion_pa','ytickname',['-90','0','90','180','270'] ; set y-axis labels
  OPTIONS,'ion_pa','ytickv',[-90,0,90,180,270]              ; set y-axis labels
  OPTIONS,'ion_pa','panel_size',panelSize                         ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_0',ANGLE=e_angle,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_0', DATA=tmp                                            ; get data structure
  tmp.y                   = tmp.y>1.e1        ; Remove zeros 
  tmp.y                   = ALOG10(tmp.y)     ; Pre-log
  STORE_DATA,'el_0', DATA=tmp                                          ; store data structure
  OPTIONS,'el_0','spec',1                                              ; set for spectrogram
  ZLIM,'el_0',6,10,0                                                    ; set z limits
  YLIM,'el_0',4,30000,1                                                ; set y limits
  OPTIONS,'el_0','ytitle','e- downgoing !C!CEnergy (eV)'               ; y title
  OPTIONS,'el_0','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'el_0','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','yticks',3                                            ; set y-axis labels
  OPTIONS,'el_0','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'el_0','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'el_0','panel_size',panelSize                                        ; set panel size

  ;; Electron pitch angle spectrogram - survey data, remove retrace, >100 electrons
  GET_PA_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_pa',ENERGY=energy_electrons,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_pa',DATA=tmp                                  ; get data structure
  tmp.y                   = tmp.y>1.e1                                         ; Remove zeros
  tmp.y                   = ALOG10(tmp.y)                                      ; Pre-log
  STORE_DATA,'el_pa',DATA=tmp                                ; store data structure
  OPTIONS,'el_pa','spec',1                                   ; set for spectrogram
  ZLIM,'el_pa',6,10,0                                        ; set z limits
  YLIM,'el_pa',0,360,0                                       ; set y limits
  OPTIONS,'el_pa','ytitle','e- >50 eV!C!C Pitch Angle'       ; y title
  OPTIONS,'el_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'el_pa','x_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','y_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','yticks',4                                 ; set y-axis labels
  OPTIONS,'el_pa','ytickname',['0','90','180','270','360']   ; set y-axis labels
  OPTIONS,'el_pa','ytickv',[0,90,180,270,360]                ; set y-axis labels
  OPTIONS,'el_pa','panel_size',panelSize                             ; set panel size 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOR PLOTTING WHEN THE TIME COMES

  IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Elphic_et_al_1998'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=1;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=1 ;,XSIZE=4,YSIZE=7
           ;; POPEN,'./plots/Elphic_et_al_1998--Fig_1',FONT=1,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE
     
     LOADCT,39
     !p.charsize=1.3
     TPLOT,['Je_lc','JEe','el_0','el_pa','ion_180','ion_pa'], $
           VAR_LABEL=['ALT','MLT','ILAT'], $
           TRANGE=[t1,t2]
     ;; TPLOT_PANEL,VARIABLE='jtemp',OPLOTVAR='jtemp_fill'

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        CGPS_CLOSE, PNG=KEYWORD_SET(save_png),DELETE_PS=KEYWORD_SET(save_png);, WIDTH=1000
     ENDIF ELSE BEGIN
        ;; IF KEYWORD_SET(save_ps) THEN BEGIN
        ;;    PCLOSE
        ;; ENDIF
     ENDELSE


  ENDIF

END
