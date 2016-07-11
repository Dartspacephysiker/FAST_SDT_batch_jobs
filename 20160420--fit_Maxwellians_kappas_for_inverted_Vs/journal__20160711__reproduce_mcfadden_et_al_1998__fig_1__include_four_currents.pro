;;07/11/16
PRO JOURNAL__20160711__REPRODUCE_MCFADDEN_ET_AL_1998__FIG_1__INCLUDE_FOUR_CURRENTS,SAVE_PNG=save_png,SAVE_PS=save_ps

  COMPILE_OPT IDL2

  ;; @startup
  fitDir  = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile = '20160710--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits.sav'

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
  blue                    = 80
  black                   = 10

  outPlotName             = 'McFadden_et_al_1998--Fig_1--with_kappa_and_four_currents'

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
  tmp.y                   = tmp.y>1.e1                                 ; Remove zeros 
  tmp.y                   = ALOG10(tmp.y)                              ; Pre-log
  STORE_DATA,'el_0', DATA=tmp                                          ; store data structure
  OPTIONS,'el_0','spec',1                                              ; set for spectrogram
  ZLIM,'el_0',7,9,0                                                    ; set z limits
  YLIM,'el_0',4,30000,1                                                ; set y limits
  OPTIONS,'el_0','ytitle','e- 0!Uo!N-22!Uo!N!CEnergy (eV)'             ; y title
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
  tmp.y                   = tmp.y>1.e1                       ; Remove zeros
  tmp.y                   = ALOG10(tmp.y)                    ; Pre-log
  STORE_DATA,'el_pa',DATA=tmp                                ; store data structure
  OPTIONS,'el_pa','spec',1                                   ; set for spectrogram
  ZLIM,'el_pa',7,9,0                                         ; set z limits
  YLIM,'el_pa',-90,270,0                                     ; set y limits
  OPTIONS,'el_pa','ytitle','e- >100 eV!C!C Pitch Angle'      ; y title
  OPTIONS,'el_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'el_pa','x_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','y_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','yticks',4                                 ; set y-axis labels
  OPTIONS,'el_pa','ytickname',['-90','0','90','180','270']   ; set y-axis labels
  OPTIONS,'el_pa','ytickv',[-90,0,90,180,270]                ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion spectrogram - survey data, remove retrace, upgoing ions
  ionAngle = [135,225]
  ionAngle = [0,180]
  ionAStr  = STRCOMPRESS(ionAngle,/REMOVE_ALL)
  GET_EN_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_180',ANGLE=ionAngle,RETRACE=1,T1=t1,T2=t2
  GET_DATA,'ion_180',DATA=tmp                                             ; get data structure
  tmp.y = tmp.y > 1.                                                      ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                                   ; Pre-log
  STORE_DATA,'ion_180',DATA=tmp                                           ; store data structure
  OPTIONS,'ion_180','spec',1                                              ; set for spectrogram
  ZLIM,'ion_180',5,7,0                                                    ; set z limits
  YLIM,'ion_180',3,30000,1                                                ; set y limits
  OPTIONS,'ion_180','ytitle','i+ '+ionAStr[0]+'!Uo!N-'+ionAStr[1]+'!Uo!N!C!CEnergy (eV)' ; y title
  OPTIONS,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'ion_180','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'ion_180','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'ion_180','yticks',3                                            ; set y-axis labels
  OPTIONS,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'ion_180','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  OPTIONS,'ion_180','panel_size',1                                        ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion pitch angle spectrogram - survey data, remove retrace, >10 ions
  GET_PA_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_pa',ENERGY=[10,30000],RETRACE=1,/SHIFT90,T1=t1,T2=t2
  GET_DATA,'ion_pa',DATA=tmp                                ; get data structure
  tmp.y = tmp.y > 1.                                        ; Remove zeros
  tmp.y = ALOG10(tmp.y)                                     ; Pre-log
  STORE_DATA,'ion_pa',DATA=tmp                              ; store data structure
  OPTIONS,'ion_pa','spec',1                                 ; set for spectrogram
  ZLIM,'ion_pa',5,7,0                                       ; set z limits
  YLIM,'ion_pa',-90,270,0                                   ; set y limits
  OPTIONS,'ion_pa','ytitle','i+ >10 eV!C!C Pitch Angle'     ; y title
; OPTIONS,'ion_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'ion_pa','x_no_interp',1                          ; don't interpolate
  OPTIONS,'ion_pa','y_no_interp',1                          ; don't interpolate
  OPTIONS,'ion_pa','yticks',4                               ; set y-axis labels
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

  GET_DATA,'Je',DATA=Je
  GET_DATA,'Jee',DATA=Jee
  GET_DATA,'Ji',DATA=Ji
  GET_DATA,'Jei',DATA=Jei
  chare                   = Jee.y/Je.y*6.242*1.0e11
  chari                   = Jei.y/Ji.y*6.242*1.0e11
  chartot                 = chare+chari
  STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari],[chare],[chartot]]}

  OPTIONS,'charepanel','tplot_routine','mplot'
  OPTIONS,'charepanel','ytitle','E/q Volts'
  OPTIONS,'charepanel','labels',['Ion','Electron','Total']
  OPTIONS,'charepanel','colors',[red,green,20]
  OPTIONS,'charepanel','labflag',1
  OPTIONS,'charepanel','yticks',5                                     ; set y-axis labels
  OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

  ;;Now restore kappa file, get kappa vals
  RESTORE,fitDir+fitFile
    fitStatus = !NULL
  ;; gaussFitStatus = !NULL
  FOR i=0,N_ELEMENTS(out_kappa_fit_structs)-1 DO BEGIN
     fitStatus = [fitStatus,out_kappa_fit_structs[i].fitStatus]
     ;; gaussFitStatus = [gaussFitStatus,out_gauss_fit_structs[i].fitStatus]
  ENDFOR
  badFits_i = WHERE(fitStatus GT 0,nBadFits,COMPLEMENT=goodFits_i)  
  ;; badGaussFits_i = WHERE(gaussFitStatus GT 0,nBadGaussFits)  

  PARSE_KAPPA_FIT_STRUCTS,out_kappa_fit_structs, $
                          A=a, $
                          TIME=kappaTime, $
                          STRUCT_A=Astruct, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus  

  PARSE_KAPPA_FIT_STRUCTS,out_gauss_fit_structs, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=gaussTime, $
                          NAMES_A=AGauss_names, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus  
  PRINT,"NbadFits      : ",nBadFits
  PRINT,"NbadGaussFits : ",nBadGaussFits
  PRINT,"NBothBad      : ",N_ELEMENTS(CGSETINTERSECTION(badFits_i,badGaussFits_i))

  Astruct.kappa[badFits_i] = 0.01
  ;; STORE_DATA,'kappa_fit',DATA={x:je.x[goodFits_i],y:Astruct.kappa[goodFits_i]}
  ;; STORE_DATA,'kappa_fit',DATA={x:je.x[0:-1:4],y:Astruct.kappa}
  STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
  YLIM,'kappa_fit',1.0,100,1
  OPTIONS,'kappa_fit','ytitle',"Kappa!CFit Val"
  OPTIONS,'kappa_fit','psym',2 ;Asterisk
  ;; OPTIONS,'kappa_fit','plotsym',1 ;Plus sign
  ;; OPTIONS,'kappa_fit','linestyle',6 ;Asterisk


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Four-current panel (it's like four-cheese pizza)

  ;;Get mag current
  jMag = GET_CURRENT_FROM_FLUXMAG(t1Adj,t2Adj, $
                                  db_fac,vel, $
                                  USE_DESPUN=ucla_mag_despin, $
                                  SDTNAME__JMAG=jMagName, $
                                  ;; INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                  ;; SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                  QUIET=quiet)

  ;;Get electron ESA current, ion ESA current
  Je_current = (-1.)*Je.y*1.6e-9 ;;in microA/m2
  Ji_current =       Ji.y*1.6e-9 ;;in microA/m2

  ;;Get Kappa-predicted current
  ;;use Astruct[goodFits_i]
  ;;Using chartot (chari+chare) for potential drop
  kappaStr                = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
  Je_interp               = {time:Je.x,comp1:Je_current,ncomp:1}
  Jtot_interp             = {time:Je.x,comp1:Je_current+Ji_current,ncomp:1}
  chartot_interp          = {time:Jee.x,comp1:chartot,ncomp:1}
  magz_interp             = {time:magz.x,comp1:jMag,ncomp:1}
  ;; FA_FIELDS_COMBINE,magz,chartot_interp,RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,Je_interp,RESULT=Je_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,Jtot_interp,RESULT=Jtot_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,chartot_interp,RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,magz_interp,RESULT=magz_interp,/INTERP,DELT_T=50.,/TALK
  
  ;;Fix what we did to poor magz
  Je_interp               = {x:Je_interp.time,y:Je_interp.comp1}
  Jtot_interp             = {x:Jtot_interp.time,y:Jtot_interp.comp1}
  chartot_interp          = {x:chartot_interp.time,y:chartot_interp.comp1}
  magz_interp             = {x:magz_interp.time,y:magz_interp.comp1}
  STORE_DATA,'Je',DATA=Je
  ;; kappa_current = KNIGHT_RELATION__DORS_KLETZING_11(Astruct.kappa[goodFits_i],Astruct.temp[goodFits_i],Astruct.N[goodFits_i], $
  kappa_current = KNIGHT_RELATION__DORS_KLETZING_11(Astruct.kappa,Astruct.temp,Astruct.N, $
                                                    chartot_interp.y,R_B) ;, $
                                                    ;; IN_POTBAR=in_potBar, $
                                                    ;; OUT_POTBAR=potBar)
  ;;Get Maxwellian-predicted current
  maxwell_current = KNIGHT_RELATION__DORS_KLETZING_4(AStructGauss.temp,AstructGauss.N,chartot_interp.y,R_B) ;, $
                                          ;; IN_POTBAR=in_potBar, $
                                          ;; OUT_POTBAR=potBar)

  ;;Get integrated-kappa-model current
  

  ;;Set up plot
  IF KEYWORD_SET(use_total_current) THEN BEGIN
     esa_current = Jtot_interp.y
     esa_name    = 'e- and i+ ESA'
  ENDIF ELSE BEGIN
     esa_current = Je_interp.y
     esa_name    = 'e- ESA'
  ENDELSE
  
  STORE_DATA,'fourcheese',DATA={x:[[kappaStr.time],[kappaStr.time],[kappaStr.time]],y:[[magz_interp.y],[esa_current],[kappa_current]]}

  OPTIONS,'fourcheese','tplot_routine','mplot'
  OPTIONS,'fourcheese','ytitle',''
  OPTIONS,'fourcheese','labels',['Fluxmag',esa_name,'Kappa model','Integrated Fit']
  OPTIONS,'fourcheese','colors',[red,green,blue,20]
  OPTIONS,'fourcheese','labflag',1
  OPTIONS,'fourcheese','yticks',5                                     ; set y-axis labels
  OPTIONS,'fourcheese','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'fourcheese','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels



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
  Je                      = {time:Je.x,comp1:Je.y}
  FA_FIELDS_COMBINE,magz,Je,RESULT=Je_interp,/INTERP,DELT_T=50.,/TALK
  Je_integ_interp         = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  Je_integ_interp[0]      = 0.0D
  FOR i=1,N_ELEMENTS(Je_integ_interp)-1 DO BEGIN
     Je_integ_interp[i]         = TSUM(position[0:i],Je_interp[0:i])
  ENDFOR
  
  ;;Fix what we did to poor magz
  magz                    = {x:magz.time,y:magz.comp1}
  Je                      = {x:Je.time,y:(-1.)*Je.comp1}
  STORE_DATA,'Je',DATA=Je

  ;; Fix it up, get the numbers back to nanotesla
  ;; jtemp = 1.0e-3*(deltaBx)/1.26e-6  * (DOUBLE(1. / 1.6e-9))
  Je_integ_interp = Je_integ_interp*1.0e3*1.26e-6*1.6e-9
  Je_integ_interp = Je_integ_interp+(magz.y[0]-Je_integ_interp[0])

  ;;Get orbit stuff
  ;; GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
  GET_DATA,'B_model',DATA=tmp1

  ;;Now make me smile, do the model subtraction
  ;; STORE_DATA,'dBpanel',DATA={x:TRANSPOSE([[magz.x],[magz.x]]),y:TRANSPOSE([[magz.y-tmp1.y[*,1]],[Je_integ_interp]])}
  JeDat           = Je_integ_interp > 1
  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     magDat       = magz.y > 1
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[JeDat],[magz.y]]}
  ENDIF ELSE BEGIN
     ;; STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[magz.y-tmp1.y[*,1]],[Je_integ_interp]]}
     magDat       = (magz.y-tmp1.y[*,1]) > 1
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[JeDat],[magDat]]}
  ENDELSE
  ;;Now set options
  OPTIONS,'dBpanel','tplot_routine','mplot'
  OPTIONS,'dBpanel','ytitle','dB!CnT'
  OPTIONS,'dBpanel','labels',[CGGREEK('Sigma')+'Je*dx','B-B!Dmodel!N']
  OPTIONS,'dBpanel','colors',[green,red]
  OPTIONS,'dBpanel','labflag',1
  ;; OPTIONS,'dBpanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  ;; OPTIONS,'dBpanel','yticks',3                                     ; set y-axis labels
  ;; OPTIONS,'dBpanel','ytickv',[100,200,300]           ; set y-axis labels
  ;; OPTIONS,'dBpanel','yticks',5                     ; set y-axis labels
  ;; OPTIONS,'dBpanel','ytickv',[100,150,200,250,300] ; set y-axis labels
  YLIM,'dBpanel',100,300,STYLE=1

  ;; Electron flux
  GET_2DT,'j_2d_fs','fa_eeb_c',name='JeF',t1=t1,t2=t2,energy=[20,30000],ANGLE=eAngle
  GET_DATA,'JeF',DATA=tmp
  tmp.y                   = tmp.y>1.e1 ; Remove zeros 
  ;; tmp.y                   = ALOG10(tmp.y)     ; Pre-log
  STORE_DATA,'JeF',DATA={x:tmp.x,y:tmp.y}
  YLIM,'JeF',1.e8,2.e9,1                              ; set y limits
  OPTIONS,'JeF','ytitle','Electrons!C!C1/(cm!U2!N-s)' ; set y title 
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

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/McFadden_et_al_1998'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           ;; CGPS_OPEN, './plots/McFadden_et_al_1998--Fig_1.ps',FONT=0,XSIZE=4,YSIZE=7
           POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8
           ;; DEVICE,SET_FONT='Garamond*15'
           ;; !P.FONT = -1
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE
     
     ;; LOADCT,74
     LOADCT,39

     TPLOT,['el_0','el_pa','ion_180','ion_pa','E_ALONG_V','charepanel','kappa_fit','dBpanel','JeF'],VAR_LABEL=['ALT','MLT','ILAT'],TRANGE=[t1,t2]
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
