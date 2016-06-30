 
PRO JOURNAL__20160517__REPRODUCE_MCFADDEN_ET_AL_1998__FIG_2,SAVE_PNG=save_png,SAVE_PS=save_ps

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

  tplot_tRange            = STR_TO_TIME(['97-2-8/10:11:30','97-2-8/10:11:36'])

  red                     = 250
  green                   = 130
  black                   = 10

  outPlotName             = 'McFadden_et_al_1998--Fig_2'

  ;;Get fields stuff, eFields and magFields
  FA_FIELDS_DESPIN,T1=t1Adj,T2=t2Adj,DAT=despun_E,/SLOW
  ;; FA_FIELDS_DESPIN_4K,/SHADOW_NOTCH
  GET_DATA,'E_NEAR_B',DATA=eNearB
  GET_DATA,'E_ALONG_V',DATA=eAlongV
  ;; STORE_DATA,'E_ALONG_V_SMOOTH',DATA={x:eAlongV.x,y:SMOOTH(eAlongV.y,160,/EDGE_TRUNCATE)}
  ;; OPTIONS,'E_ALONG_V_SMOOTH','ytitle','E Along V!C(mV/m)'
  ;; YLIM,'E_ALONG_V_SMOOTH',-1000,1000
  n_eAlongV = N_ELEMENTS(eAlongV.x)
  reduceFac = 160
  decimateFactor = reduceFac/16
  ;; ealongv.x[164]-ealongv.x[0]
  ;; 0.080078125000000000  
  reduce = n_eAlongV MOD reduceFac
  eAlongV = {x:eAlongV.x[0:n_eAlongV-1-reduce],y:eAlongV.y[0:n_eAlongV-1-reduce]}
  STORE_DATA,'E_ALONG_V',DATA={x:REBIN(eAlongV.x,N_ELEMENTS(eAlongV.x)/decimateFactor),y:REBIN(SMOOTH(eAlongV.y,reduceFac,/EDGE_TRUNCATE),N_ELEMENTS(eAlongV.y)/decimateFactor)}
  GET_DATA,'E_ALONG_V',DATA=eAlongV
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
  ;; Ion spectrogram - survey data, remove retrace, upgoing ions
  GET_EN_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_180',ANGLE=[0,180],RETRACE=1,T1=t1,T2=t2
  GET_DATA,'ion_180',DATA=tmp                                           ; get data structure
  tmp.y = tmp.y > 1.                                                      ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                 ; Pre-log
  STORE_DATA,'ion_180',DATA=tmp                                         ; store data structure
  OPTIONS,'ion_180','spec',1                                            ; set for spectrogram
  ZLIM,'ion_180',5,7,0                                                  ; set z limits
  YLIM,'ion_180',3,30000,1                                              ; set y limits
  OPTIONS,'ion_180','ytitle','ion 0!Uo!N-180!Uo!N!C!CEnergy (eV)'      ; y title
  OPTIONS,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'          ; z title
  OPTIONS,'ion_180','x_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','y_no_interp',1                                     ; don't interpolate
  OPTIONS,'ion_180','yticks',3                                          ; set y-axis labels
  OPTIONS,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'ion_180','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'ion_180','panel_size',1                                        ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  ;; GET_2DT,'j_2d_fs','fa_' + survOrBurst + '_c',NAME='Je',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  ;; GET_2DT,'je_2d_fs','fa_' + survOrBurst + '_c',NAME='Jee',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  GET_2DT,'j_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Ji',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari
  GET_2DT,'je_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Jei',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari
  ;;Remove_crap
  GET_DATA,'Ji',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  ;; keep1                   = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  ;; keep2                   = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
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

  GET_DATA,'Ji',DATA=tmp_ji
  GET_DATA,'Jei',DATA=tmp_jei
  chari                   = tmp_jei.y/tmp_ji.y*6.242*1.0e11


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now potential
  INTEGRATE_LOWFREQ_E_ALONG_V_FOR_POTENTIAL,eAlongV,DATA=potential
  ;; GET_FA_ORBIT,ji_tmp_time,/TIME_ARRAY,/ALL
  ;; GET_DATA,'fa_vel',DATA=vel
  ;; speed                   = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  ;; old_pos                 = 0.
  ;; position                = MAKE_ARRAY(N_ELEMENTS(ji_tmp_time),/DOUBLE)
  ;; speed_ji_point         = MAKE_ARRAY(N_ELEMENTS(ji_tmp_time),/DOUBLE)
  ;; FOR j=0L,N_ELEMENTS(ji_tmp_time)-2 DO BEGIN
  ;;    speed_point_ind      = MIN(ABS(vel.x-ji_tmp_time[j]),ind)

  ;;    speed_ji_point[j]   = speed[ind]
  ;;    samplingperiod       = ji_tmp_time[j+1] - ji_tmp_time[j]

  ;;    position[j]          = old_pos + speed_ji_point[j]*samplingperiod
  ;;    old_pos              = position[j]
  ;; ENDFOR

  ;;Get nearest to plot edge, zero it
  nearest                 = MIN(ABS(potential.x-tplot_tRange[0]),ind)
  ;; STORE_DATA,'POTENTIAL',DATA={x:ji.x,y:(-1)*(potential.y-potential.y[ind])}
  STORE_DATA,'POTENTIAL',DATA={x:potential.x,y:(-1)*(potential.y-potential.y[ind])}
  ;; STORE_DATA,'POTENTIAL',DATA={x:ji.x,y:(-1)*(SMOOTH(potential.y,5,/EDGE_TRUNCATE))}
  OPTIONS,'POTENTIAL','ytitle','Potential!C(V)' ; y title
  YLIM,'POTENTIAL',-4000,1000

  ji                      = {time:ji_tmp_time,comp1:ji_tmp_data,ncomp:1}
  potential                 = {time:potential.x,comp1:potential.y}
  FA_FIELDS_COMBINE,ji,potential,RESULT=potential_interp,/INTERP,DELT_T=50.,/TALK
  ji                      = {x:ji.time,y:ji.comp1}
  potential                 = {x:ji.x,y:potential_interp}
  nearest                 = MIN(ABS(potential.x-tplot_tRange[0]),ind)
  nearest                 = MIN(ABS(ji.x-tplot_tRange[0]),indChar)


  STORE_DATA,'charepanel',DATA={x:[[tmp_ji.x],[tmp_ji.x]],y:[[chari-chari[indchar]],[potential.y-potential.y[ind]]]}
  OPTIONS,'charepanel','tplot_routine','mplot'
  OPTIONS,'charepanel','ytitle','Ion E/q!C(eV)'
  OPTIONS,'charepanel','labels',['Ion Beam',CGGREEK('Sigma')+'Je*dx']
  OPTIONS,'charepanel','colors',[red,black]
  OPTIONS,'charepanel','labflag',1
  OPTIONS,'charepanel','yticks',7                                     ; set y-axis labels
  OPTIONS,'charepanel','ytickname',['-1e3','0','1e3','2e3','3e3','4e3','5e3'] ; set y-axis labels
  OPTIONS,'charepanel','ytickv',[-1e3,0.,1.0e3,2.e3,3.e3,4.e3,5.e3]           ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOR PLOTTING

  IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/McFadden_et_al_1998'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           POPEN,plotDir+outPlotName,/PORT,FONT=-1;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE
     
     ;; LOADCT,74
     LOADCT,39

     TPLOT,['el_0','ion_180','E_ALONG_V','POTENTIAL','charepanel'],VAR_LABEL=['ALT','MLT','ILAT'],TRANGE=tplot_tRange
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

