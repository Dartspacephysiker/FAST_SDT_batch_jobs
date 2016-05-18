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

PRO JOURNAL__20160517__REPRODUCE_MCFADDEN_ET_AL_1998__FIG_1,SAVE_PNG=save_png,SAVE_PS=save_ps

  ;; @startup

  survOrBurst             = 'eeb'
  iSurvOrBurst            = 'ieb'

  energy_electrons        = [100.,30000.]
  energy_ions             = [10.,30000.]
  ucla_mag_despin         = 1
  do_losscone             = 0
  ;;Orbit 1894
  t1Str                   = '97-2-8/10:11:22'
  t2Str                   = '97-2-8/10:11:52'

  t1                      = STR_TO_TIME(t1Str)
  t2                      = STR_TO_TIME(t2Str)
  t1Adj                   = t1-10.
  t2Adj                   = t2+10.

  red                     = 250
  green                   = 130
  black                   = 10

  ;;Get fields stuff, eFields and magFields
  FA_FIELDS_DESPIN,T1=t1Adj,T2=t2Adj,DAT=despun_E
  GET_DATA,'E_NEAR_B',DATA=eNearB
  GET_DATA,'E_ALONG_V',DATA=eAlongV
  STORE_DATA,'E_ALONG_V',DATA={x:eAlongV.x,y:SMOOTH(eAlongV.y,160,/EDGE_TRUNCATE)}
  OPTIONS,'E_ALONG_V','ytitle','E Along V!C(mV/m)'
  YLIM,'E_ALONG_V',-1000,1000

  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     UCLA_MAG_DESPIN

     GET_DATA,'dB_fac_v',DATA=db_fac
     mintime              = MIN(ABS(t1-db_fac.x),ind1)
     mintime              = MIN(ABS(t2-db_fac.x),ind2)
     
     magx                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
     magy                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}
     magz                 = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

  ENDIF ELSE BEGIN
     dat = GET_FA_FIELDS('MagDC',t1,/START)
     field = GET_FA_FIELDS('MagDC',t1,t2,/STORE)

     GET_DATA,'MagDCcomp1',DATA=magx
     GET_DATA,'MagDCcomp2',DATA=magy
     GET_DATA,'MagDCcomp3',DATA=magz

  ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  IF KEYWORD_SET(do_losscone) THEN BEGIN
     ;;Define loss cone angle
     GET_DATA,'ALT',DATA=alt
     loss_cone_alt           = alt.y[0]*1000.0
     lcw                     = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
     GET_DATA,'ILAT',DATA=ilat
     north_south             = ABS(ilat.y[0])/ilat.y[0]
     
     if north_south EQ -1 then begin
        eAngle              = [180.-lcw,180+lcw] ; for Southern Hemis.

        ;;Eliminate ram from data
        iAngle              = [180.0,360.0]
        iAngle=[270.0,360.0]
        
     endif else begin
        eAngle              = [360.-lcw,lcw] ;	for Northern Hemis.

        ;;Eliminate ram from data
        iAngle              = [0.0,180.0]
        iAngle           = [90.0,180.0]
        
     endelse
  ENDIF ELSE BEGIN
     eAngle = [360.-25.,25.]
     iAngle = [135.,225.]
  ENDELSE

  ;; eAngleChare = [-180,180]
  ;; iAngleChari = [-180,180]
  eAngleChare = eAngle
  iAngleChari = iAngle
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  GET_EN_SPEC,'fa_' + survOrBurst + '_c',UNITS='eflux',NAME='el_0',ANGLE=eAngle,RETRACE=1,T1=t1,T2=t2,/CALIB
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Electron pitch angle spectrogram - survey data, remove retrace, >100 electrons
  GET_PA_SPEC,'fa_' + survOrBurst + '_c',UNITS='eflux',NAME='el_pa',ENERGY=energy_electrons,/SHIFT90,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_pa',DATA=tmp                                  ; get data structure
  tmp.y                   = tmp.y>1.e1                                         ; Remove zeros
  tmp.y                   = ALOG10(tmp.y)                                      ; Pre-log
  STORE_DATA,'el_pa',DATA=tmp                                ; store data structure
  OPTIONS,'el_pa','spec',1                                   ; set for spectrogram
  ZLIM,'el_pa',7,9,0                                        ; set z limits
  YLIM,'el_pa',-90,270,0                                       ; set y limits
  OPTIONS,'el_pa','ytitle','e- >100 eV!C!C Pitch Angle'       ; y title
  OPTIONS,'el_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'el_pa','x_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','y_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','yticks',4                                 ; set y-axis labels
  OPTIONS,'el_pa','ytickname',['-90','0','90','180','270'] ; set y-axis labels
  OPTIONS,'el_pa','ytickv',[-90,0,90,180,270]              ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion spectrogram - survey data, remove retrace, upgoing ions
  GET_EN_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_180',ANGLE=[135,225],RETRACE=1,T1=t1,T2=t2
  GET_DATA,'ion_180',DATA=tmp                                           ; get data structure
  tmp.y = tmp.y > 1.                                                      ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                 ; Pre-log
  STORE_DATA,'ion_180',DATA=tmp                                         ; store data structure
  OPTIONS,'ion_180','spec',1                                            ; set for spectrogram
  ZLIM,'ion_180',5,7,0                                                  ; set z limits
  YLIM,'ion_180',3,30000,1                                              ; set y limits
  OPTIONS,'ion_180','ytitle','i+ 135!Uo!N-180!Uo!N!C!CEnergy (eV)'      ; y title
  OPTIONS,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'          ; z title
  OPTIONS,'ion_180','x_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','y_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','yticks',3                                          ; set y-axis labels
  OPTIONS,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'ion_180','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'ion_180','panel_size',1                                        ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion pitch angle spectrogram - survey data, remove retrace, >10 ions
  GET_PA_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_pa',ENERGY=[10,30000],RETRACE=1,/SHIFT90,T1=t1,T2=t2
  GET_DATA,'ion_pa',DATA=tmp                              ; get data structure
  tmp.y = tmp.y > 1.                                        ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                   ; Pre-log
  STORE_DATA,'ion_pa',DATA=tmp                            ; store data structure
  OPTIONS,'ion_pa','spec',1                               ; set for spectrogram
  ZLIM,'ion_pa',5,7,0                                     ; set z limits
  YLIM,'ion_pa',-90,270,0                                ; set y limits
  OPTIONS,'ion_pa','ytitle','i+ >10 eV!C!C Pitch Angle'   ; y title
; OPTIONS,'ion_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'ion_pa','x_no_interp',1                        ; don't interpolate
  OPTIONS,'ion_pa','y_no_interp',1                        ; don't interpolate
  OPTIONS,'ion_pa','yticks',4                             ; set y-axis labels
  OPTIONS,'ion_pa','ytickname',['-90','0','90','180','270'] ; set y-axis labels
  OPTIONS,'ion_pa','ytickv',[-90,0,90,180,270]              ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  GET_2DT,'j_2d_fs','fa_' + survOrBurst + '_c',NAME='Je',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  GET_2DT,'je_2d_fs','fa_' + survOrBurst + '_c',NAME='Jee',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  GET_2DT,'j_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Ji',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  GET_2DT,'je_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Jei',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  ;;Remove_crap
  GET_DATA,'Je',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  GET_DATA,'Jee',DATA=tmp
  keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Ji',DATA=tmp
  keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Jei',DATA=tmp
  keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Je',DATA=tmp
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  je_tmp_time             = tmp.x[keep2]
  je_tmp_data             = tmp.y[keep2]
  STORE_DATA,'Je',DATA={x:je_tmp_time,y:(-1.)*je_tmp_data}
  GET_DATA,'Jee',DATA=tmp
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  jee_tmp_time             = tmp.x[keep2]
  jee_tmp_data             = tmp.y[keep2]
  STORE_DATA,'Jee',DATA={x:jee_tmp_time,y:(-1.)*jee_tmp_data}
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

  GET_DATA,'Je',DATA=tmp_je
  GET_DATA,'Jee',DATA=tmp_jee
  GET_DATA,'Ji',DATA=tmp_ji
  GET_DATA,'Jei',DATA=tmp_jei
  chare                   = tmp_jee.y/tmp_je.y*6.242*1.0e11
  chari                   = tmp_jei.y/tmp_ji.y*6.242*1.0e11
  chartot                 = chare+chari
  STORE_DATA,'charepanel',DATA={x:[[tmp_jee.x],[tmp_jee.x],[tmp_jee.x]],y:[[chari],[chare],[chartot]]}

  OPTIONS,'charepanel','tplot_routine','mplot'
  OPTIONS,'charepanel','ytitle','E/q Volts'
  OPTIONS,'charepanel','labels',['Ion','Electron','Total']
  OPTIONS,'charepanel','colors',[red,green,20]
  OPTIONS,'charepanel','labflag',1
  OPTIONS,'charepanel','yticks',5                                     ; set y-axis labels
  OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;dB panel

  ;;Get speed and position for calculation of mag stuff
  GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
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

  ;;calculate sum of Je, since we're thinking about it
  magz                    = {time:magz.x,comp1:magz.y,ncomp:1}
  je                      = {time:tmp_je.x,comp1:tmp_je.y}
  FA_FIELDS_COMBINE,magz,je,RESULT=je_interp,/INTERP,DELT_T=50.,/TALK
  je_integ_interp         = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  je_integ_interp[0]      = 0.0D
  FOR i=1,N_ELEMENTS(je_integ_interp)-1 DO BEGIN
     je_integ_interp[i]         = TSUM(position[0:i],je_interp[0:i])
  ENDFOR
  
  ;;Fix what we did to poor magz
  magz                    = {x:magz.time,y:magz.comp1}
  je                      = {x:je.time,y:(-1.)*je.comp1}
  STORE_DATA,'Je',DATA=je

  ;; Fix it up, get the numbers back to nanotesla
  ;; jtemp = 1.0e-3*(deltaBx)/1.26e-6  * (DOUBLE(1. / 1.6e-9))
  je_integ_interp = je_integ_interp*1.0e3*1.26e-6*1.6e-9
  je_integ_interp = je_integ_interp+(magz.y[0]-je_integ_interp[0])

  ;;Get orbit stuff
  ;; GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
  GET_DATA,'B_model',DATA=tmp1

  ;;Now make me smile, do the model subtraction
  ;; STORE_DATA,'dBpanel',DATA={x:TRANSPOSE([[magz.x],[magz.x]]),y:TRANSPOSE([[magz.y-tmp1.y[*,1]],[je_integ_interp]])}
  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[je_integ_interp],[magz.y]]}
  ENDIF ELSE BEGIN
     ;; STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[magz.y-tmp1.y[*,1]],[je_integ_interp]]}
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[je_integ_interp],[magz.y-tmp1.y[*,1]]]}
  ENDELSE
  ;;Now set options
  OPTIONS,'dBpanel','tplot_routine','mplot'
  OPTIONS,'dBpanel','ytitle','dB!CnT'
  OPTIONS,'dBpanel','labels',[CGGREEK('Sigma')+'Je*dx','B-B!Dmodel!N']
  OPTIONS,'dBpanel','colors',[green,red]
  OPTIONS,'dBpanel','labflag',1
  OPTIONS,'dBpanel','yticks',3                                     ; set y-axis labels
  ;; OPTIONS,'dBpanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'dBpanel','ytickv',[100,200,300]           ; set y-axis labels
  YLIM,'dBpanel',100,350

  ;; Electron flux
  GET_2DT,'j_2d_fs','fa_eeb_c',name='JeF',t1=t1,t2=t2,energy=[20,30000],ANGLE=eAngle
  GET_DATA,'JeF',DATA=tmp
  tmp.y                   = tmp.y>1.e1        ; Remove zeros 
  ;; tmp.y                   = ALOG10(tmp.y)     ; Pre-log
  STORE_DATA,'JeF',DATA={x:tmp.x,y:tmp.y}
  YLIM,'JeF',1.e8,2.e9,1                                                 ; set y limits
  OPTIONS,'JeF','ytitle','Electrons!C!C1/(cm!U2!N-s)'                     ; set y title 
  OPTIONS,'JeF','colors',green
  ;; OPTIONS,'JeF','tplot_routine','pmplot'                                ; set 2 color plot
  ;; OPTIONS,'JeF','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] ; set color label
  ;; OPTIONS,'JeF','labflag',3                                               ; set color label
  ;; OPTIONS,'JeF','labpos',[5.e7]                                     ; set color label
  ;; OPTIONS,'JeF','panel_size',1                                            ; set panel size 
  ;; OPTIONS,'JeF','bins',[1,0]

;; GET_2DT,'j_2d_b','fa_' + survOrBurst + '_c',NAME='JeF',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngle,/CALIB
  ;; YLIM,'JeF',1.e7,2.e9,1                                                ; set y limits
  ;; YLIM,'Je',1.e7,2.e9,1                                                ; set y limits
  ;; OPTIONS,'JeF','ytitle','1!C1/(cm!U2!N-s)'                           ; set y title
  ;; OPTIONS,'JeF','tplot_routine','pmplot'                                ; set 2 color plot
  ;; OPTIONS,'Je','labels','Downgoing!C Electrons' ; set color label
  ;; OPTIONS,'Je','labflag',1                      ; set color label
  ;; OPTIONS,'Je','panel_size',1                                            ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOR PLOTTING

  IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, './plots/McFadden_et_al_1998--Fig_1.ps',FONT=0;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           ;; CGPS_OPEN, './plots/McFadden_et_al_1998--Fig_1.ps',FONT=0,XSIZE=4,YSIZE=7
           POPEN,'./plots/McFadden_et_al_1998--Fig_1',/PORT,FONT=-1;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8
           ;; DEVICE,SET_FONT='Garamond*15'
           ;; !P.FONT = -1
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE
     
     ;; LOADCT,74
     LOADCT,39

     TPLOT,['el_0','el_pa','ion_180','ion_pa','E_ALONG_V','charepanel','dBpanel','JeF'],VAR_LABEL=['ALT','MLT','ILAT'],TRANGE=[t1,t2]
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


  ENDIF

END

