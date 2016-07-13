;;2016/07/12
PRO JOURNAL__20160712__REPRODUCE_ELPHIC_FIG_1__INCLUDE_FOUR_CURRENTS,SAVE_PNG=save_png,SAVE_PS=save_ps

  R_B     = 20

  fitDir  = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile = '20160712--Elphic_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'

  kappaTxtFile                 = '20160712--Elphic_et_al_1998--Kappa_fits.txt'
  gaussTxtFile                 = '20160712--Elphic_et_al_1998--Gauss_fits.txt'

  ;;Restore up front so it doesn't corrupt future variables
  RESTORE,fitDir+fitFile

  energy_electrons        = [50.,32000.]
  ucla_mag_despin         = 1

  ;; t1Str                     = '97-02-01/09:25:30'
  ;; t2Str                     = '97-02-01/09:27:59'
  
  t1Str                     = '97-02-01/09:26:15'
  t2Str                     = '97-02-01/09:27:10'
  
  t1                      = STR_TO_TIME(t1Str)
  t2                      = STR_TO_TIME(t2Str)
  t1Adj                   = t1-10.
  t2Adj                   = t2+10.

  outPlotName             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--Elphic_et_al_1998--Fig_1--with_kappa_and_four_currents'

  fitDir  = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile = '20160712--Elphic_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'

  survOrBurst             = 'ees'
  iSurvOrBurst            = 'ies'

  energy_electrons        = [100.,36000.]
  energy_ions             = [10.,36000.]
  ucla_mag_despin         = 1
  do_losscone             = 1

  red                     = 250
  green                   = 130
  blue                    = 80
  black                   = 10

  ;;Get fields stuff, eFields and magFields
  FA_FIELDS_DESPIN,T1=t1Adj,T2=t2Adj,DAT=despun_E
  GET_DATA,'E_NEAR_B',DATA=eNearB
  GET_DATA,'E_ALONG_V',DATA=eAlongV
  STORE_DATA,'E_ALONG_V',DATA={x:eAlongV.x,y:SMOOTH(eAlongV.y,160,/EDGE_TRUNCATE)}
  OPTIONS,'E_ALONG_V','ytitle','E Along V!C(mV/m)'
  YLIM,'E_ALONG_V',-1000,1000

  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     GET_DATA,'dB_fac_v',DATA=db_fac
     IF SIZE(db_fac,/TYPE) NE 8 THEN BEGIN
        UCLA_MAG_DESPIN

        GET_DATA,'dB_fac_v',DATA=db_fac
     ENDIF

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

  GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
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
     eAngle = [360.-30.,30.]
     iAngle = [135.,225.]
  ENDELSE

  ;; eAngleChare = [-180,180]
  ;; iAngleChari = [-180,180]
  eAngleChare = [360.-30.,30.]
  iAngleChari = [135.,225.]
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
  deltaBX                 = DERIV(position,SMOOTH(magz.y,100))

  ;;in number flux units
  jtemp                   = 1.0e-3*(deltaBx)/1.26e-6  * (DOUBLE(1. / 1.6e-9))

  sign_jtemp              = ABS(deltaBx)/deltaBx
  STORE_DATA,'jtemp',DATA={x:magz.x,y:jtemp}
  ;; OPTIONS,'jtemp','psym','10'
  ;; OPTIONS,'jtemp','fill',1
  YLIM,'jtemp',-1.e9,2.e9
  OPTIONS,'jtemp','yticks',4                           ; set y-axis labels
  OPTIONS,'jtemp','ytickname',['-1e9','0','1e9','2e9'] ; set y-axis labels
  OPTIONS,'jtemp','ytickv',[-1e9,0,1e9,2e9]            ; set y-axis labels
  OPTIONS,'jtemp','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N)'
  OPTIONS,'jtemp','tplot_routine','polyfill_tplot'
  ;; OPTIONS,'jtemp','color','808080'x
  OPTIONS,'jtemp','fill_color',250

  ;;calculate potential, since we're thinking about it
  magz                    = {time:magz.x,comp1:magz.y,ncomp:1}
  eAlongV                 = {time:eAlongV.x,comp1:eAlongV.y}
  FA_FIELDS_COMBINE,magz,eAlongV,RESULT=eAlongV_interp,/INTERP,DELT_T=50.,/TALK
  potential               = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  potential[0]            = 0.0D
  FOR i=1,N_ELEMENTS(potential)-1 DO BEGIN
     potential[i]         = TSUM(position[0:i],eAlongV_interp[0:i])
  ENDFOR

  ;;Now set them right again
  magz                    = {x:magz.time,y:magz.comp1}
  eAlongV                 = {x:eAlongV.time,y:eAlongV.comp1}
  ;; potential            = {x:magz.x,y:potential}
  STORE_DATA,'POTENTIAL',DATA={x:magz.x,y:potential/1000.-8000.}
  OPTIONS,'POTENTIAL','ytitle','Potential!C(V)' ; y title
  OPTIONS,'POTENTIAL','yticks',5                           ; set y-axis labels
  OPTIONS,'POTENTIAL','ytickname',['-8000','','-4000','','0'] ; set y-axis labels
  OPTIONS,'POTENTIAL','ytickv',[-8000,-6000,-4000,-2000,0]            ; set y-axis labels
  YLIM,'POTENTIAL',-8500,1900

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Calculate current from ESAs
  GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='Je',ENERGY=energy_electrons ;,ANGLE=eAngle

  GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='Je_lc',ENERGY=energy_electrons,ANGLE=eAngle

  ;;Remove_crap
  GET_DATA,'Je',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  je_tmp_time          = tmp.x[keep2]
  je_tmp_data          = tmp.y[keep2]
  STORE_DATA,'Je',DATA={x:je_tmp_time,y:(-1.)*je_tmp_data}
  YLIM,'Je',-1.e9,2.e9
  OPTIONS,'Je','yticks',4                           ; set y-axis labels
  OPTIONS,'Je','ytickname',['-1e9','0','1e9','2e9'] ; set y-axis labels
  OPTIONS,'Je','ytickv',[-1e9,0,1e9,2e9]            ; set y-axis labels
  OPTIONS,'Je','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N)'
  OPTIONS,'Je','tplot_routine','polyfill_tplot'
  OPTIONS,'Je','fill_color',250

  GET_DATA,'Je_lc',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  je_lc_tmp_time          = tmp.x[keep2]
  je_lc_tmp_data          = tmp.y[keep2]


  ;;Get electron energy flux in loss cone
  GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='JEe',ENERGY=energy_electrons,ANGLE=eAngle

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

  ;;Now set them right again, store
  STORE_DATA,'dB_East',DATA={x:magz.x,y:magz.y-magz.y[0]+40}
  OPTIONS,'dB_East','ytitle',CGGREEK('Delta') + 'B!DEast!N (nT)' ; y title
  YLIM,'dB_East',-100,200
  OPTIONS,'dB_East','yticks',4                                   ; set y-axis labels
  OPTIONS,'dB_East','ytickname',['-100','0','100','200']          ; set y-axis labels
  OPTIONS,'dB_East','ytickv',[-100,0,100,200]                    ; set y-axis labels
  

  ;;Scale electron energy flux to 100km, pos flux earthward
  jee_ionos_tmp_data      = sgn_flx*jee_tmp_data*ratio
  OPTIONS,'JEei','ytitle','Electron!CEnergy!CFlux!C(erg!U2!Ns!U-1!N)'
  STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
  
  jee_tot_ionos_tmp_data  = sgn_flx*jee_tot_tmp_data*ratio
  STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}

  ;;Pedersen conductivity
  charE                   = (jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11/1000. ;in keV
  ;; charE                   = (jee_tot_tmp_data/je_tmp_data)*6.242*1.0e11/1000. ;in keV

  ped                     = 40. * charE * SQRT(jee_ionos_tmp_data)/(16.+charE^2.) > 0.1
  ;; ped                     = 40. * charE * SQRT(jee_tot_tmp_data)/(16.+charE^2.) > 1
  YLIM,'Ped',0.1,40,1                                                ; set y limits
  OPTIONS,'Ped','ytitle',CGGREEK('Sigma')+ '!Dp!N (mho)'               ; y title
  STORE_DATA,'Ped', DATA={x:jee_tmp_time,y:ped}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_0',ANGLE=eAngle,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_0', DATA=tmp                                            ; get data structure
  tmp.y                   = tmp.y>1.e1        ; Remove zeros 
  tmp.y                   = ALOG10(tmp.y)     ; Pre-log
  STORE_DATA,'el_0', DATA=tmp                                          ; store data structure
  OPTIONS,'el_0','spec',1                                              ; set for spectrogram
  ZLIM,'el_0',6,9,0                                                    ; set z limits
  YLIM,'el_0',5,30000,1                                                ; set y limits
  OPTIONS,'el_0','ytitle','e- downgoing !C!CEnergy (eV)'               ; y title
  ;; OPTIONS,'el_0','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'            ; z title
  OPTIONS,'el_0','ztitle','log eV!C!C/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'el_0','x_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','y_no_interp',1                                       ; don't interpolate
  OPTIONS,'el_0','yticks',3                                            ; set y-axis labels
  OPTIONS,'el_0','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  OPTIONS,'el_0','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  ;; OPTIONS,'el_0','panel_size',1.5                                        ; set panel size

  ;; Electron pitch angle spectrogram - survey data, remove retrace, >100 electrons
  GET_PA_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_pa', $
              ;; ENERGY=energy_electrons, $
              RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_pa',DATA=tmp                                  ; get data structure
  tmp.y                   = tmp.y>1.e1                                         ; Remove zeros
  tmp.y                   = ALOG10(tmp.y)                                      ; Pre-log
  STORE_DATA,'el_pa',DATA=tmp                                ; store data structure
  OPTIONS,'el_pa','spec',1                                   ; set for spectrogram
  ZLIM,'el_pa',6,10,0                                        ; set z limits
  YLIM,'el_pa',0,360,0                                       ; set y limits
  OPTIONS,'el_pa','ytitle','Electron Pitch!CAngle (deg)'       ; y title
  OPTIONS,'el_pa','ztitle','log eV!C!C/cm!U2!N-s-sr-eV' ; z title
  OPTIONS,'el_pa','x_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','y_no_interp',1                            ; don't interpolate
  OPTIONS,'el_pa','yticks',4                                 ; set y-axis labels
  OPTIONS,'el_pa','ytickname',['0','90','180','270','360']   ; set y-axis labels
  OPTIONS,'el_pa','ytickv',[0,90,180,270,360]                ; set y-axis labels

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
  STORE_DATA,'Je',DATA={x:je_tmp_time,y:je_tmp_data}
  GET_DATA,'Jee',DATA=tmp
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  jee_tmp_time             = tmp.x[keep2]
  jee_tmp_data             = tmp.y[keep2]
  STORE_DATA,'Jee',DATA={x:jee_tmp_time,y:jee_tmp_data}
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
  FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1},{time:Jei.x,comp1:chari,ncomp:1},RESULT=chari_interp,/INTERP,DELT_T=50.,/TALK
  ;; chari_interp            = {x:Jee.x,y:chari_interp}
  chartot                 = chare+chari_interp
  STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari_interp],[chare],[chartot]]}

  OPTIONS,'charepanel','tplot_routine','mplot'
  OPTIONS,'charepanel','ytitle','E/q Volts'
  OPTIONS,'charepanel','labels',['Ion','Electron','Total']
  OPTIONS,'charepanel','colors',[red,green,20]
  OPTIONS,'charepanel','labflag',1
  OPTIONS,'charepanel','yticks',5                                     ; set y-axis labels
  OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Kappa fit panel
  fitStatus = !NULL
  gaussFitStatus = !NULL
  FOR i=0,N_ELEMENTS(out_kappa_fit_structs)-1 DO BEGIN
     fitStatus = [fitStatus,out_kappa_fit_structs[i].fitStatus]
     gaussFitStatus = [gaussFitStatus,out_gauss_fit_structs[i].fitStatus]
  ENDFOR
  badFits_i = WHERE(fitStatus GT 0,nBadFits,COMPLEMENT=goodFits_i)
  badGaussFits_i = WHERE(gaussFitStatus GT 0,nBadGaussFits)

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

  nFits                    = N_ELEMENTS(Astruct.kappa)
  PRINT,""
  PRINT,"****************************************"
  PRINT,'NTotalFits    : ',N_ELEMENTS(nFits)
  PRINT,''
  PRINT,"NbadFits      : ",nBadFits
  PRINT,"NbadGaussFits : ",nBadGaussFits
  PRINT,"NBothBad      : ",N_ELEMENTS(CGSETINTERSECTION(badFits_i,badGaussFits_i))

  STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
  YLIM,'kappa_fit',1.0,100,1
  OPTIONS,'kappa_fit','ytitle',"Kappa!CFit Val"
  OPTIONS,'kappa_fit','psym',2 ;Asterisk

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
  ;;use Astruct[goodFits_i]; using chartot (chari+chare) for potential drop
  ;;
  kappaStr                = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
  ;; Je_interp               = {time:Je.x,comp1:Je_current,ncomp:1}
  ;; Ji_interp               = {time:Ji.x,comp1:Ji_current,ncomp:1}
  ;; Jtot_interp             = {time:Je.x,comp1:Je_current+Ji_current,ncomp:1}
  ;; chartot_interp          = {time:Jee.x,comp1:chartot,ncomp:1}
  ;; magz_interp             = {time:magz.x,comp1:jMag,ncomp:1}
  ;; FA_FIELDS_COMBINE,magz,chartot_interp,RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK

  ;;Align to kappa fits
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1},RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK


  ;;Align to Je
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:Ji.x,comp1:Ji_current,ncomp:1},RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:jMag.x,comp1:jMag.y,ncomp:1},RESULT=jMag_interp,/INTERP,DELT_T=50.,/TALK

  Jtot_interp             = {x:Je.x,y:Je_current+Ji_interp}

  STORE_DATA,'Je',DATA=Je

  kappa_current = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_11(Astruct.kappa,Astruct.temp,Astruct.N, $
                                                    chartot_interp/DOUBLE(1.6e-19),R_B) ;, $

  ;; badDens = WHERE(Astruct.N GT 1,nBD)
  ;; IF nBD GT 0 THEN BEGIN
  ;;    kappa_current[badDens] = !VALUES.D_NAN
  ;; ENDIF
  badFit                       = WHERE(fitStatus GT 0,nBF)
  IF nBF GT 0 THEN BEGIN
     kappa_current[badFit]     = !VALUES.D_NAN
  ENDIF

  ;;Get Maxwellian-predicted current
  maxwell_current = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_4(AStructGauss.temp,AstructGauss.N,chartot_interp/DOUBLE(1.6e-19),R_B) ;, $

  ;; badDens = WHERE(AstructGauss.N GT 1,nBD)
  ;; IF nBD GT 0 THEN BEGIN
  ;;    maxwell_current[badDens] = !VALUES.D_NAN
  ;; ENDIF
  badFit                       = WHERE(gaussFitStatus GT 0,nBF)
  IF nBF GT 0 THEN BEGIN
     maxwell_current[badFit]   = !VALUES.D_NAN
  ENDIF

  ;;Set up plot
  IF KEYWORD_SET(use_Je_current) THEN BEGIN
     esa_current = Je_current
     esa_name    = 'e- ESA'
  ENDIF ELSE BEGIN
     esa_current = Jtot_interp.y
     esa_name    = 'e- and i+ ESA'
  ENDELSE

  ;;Store all data for this chocolatier
  STORE_DATA,'onecheese',DATA={x:[Je.x], $
                                ;; y:[[jMag_interp],[esa_current],[kappa_current],[maxwell_current]]}
                                y:esa_current}
  STORE_DATA,'fourcheese',DATA={x:jMag.x, $
                                ;; y:[[jMag_interp],[esa_current],[kappa_current],[maxwell_current]]}
                                y:jMag.y}
  STORE_DATA,'toppings',DATA={x:[[kappaStr.time],[kappaStr.time]], $
                              ;; y:[[jMag_interp],[esa_current],[kappa_current],[maxwell_current]]}
                              y:[[maxwell_current],[kappa_current]]}

  OPTIONS,'onecheese','colors',green
  OPTIONS,'onecheese','tplot_routine','mplot'
  OPTIONS,'onecheese','ytitle','Current!C('+CGGREEK('mu')+'A/m!U2!Ns)'
  YLIM,   'onecheese',-2,0.1
  OPTIONS,'onecheese','labels',esa_name
  OPTIONS,'onecheese','labflag',3
  OPTIONS,'onecheese','labpos',-0.5
  ;; OPTIONS,'onecheese','yticks',5                                    ; set y-axis labels
  ;; OPTIONS,'onecheese','ytickname',['-4.0','-3.0','-2.0','-1.0','0'] ; set y-axis labels
  ;; OPTIONS,'onecheese','ytickv',[-4.0,-3.0,-2.0,-1.0,0.0]            ; set y-axis labels

  OPTIONS,'fourcheese','colors',red
  OPTIONS,'fourcheese','labels','Fluxgate mag'
  OPTIONS,'fourcheese','labflag',3
  OPTIONS,'fourcheese','labpos',-1.5

  OPTIONS,'toppings','labels' ,['Maxwellian Model','Kappa model']
  OPTIONS,'toppings','psym'   ,1
  OPTIONS,'toppings','colors' ,[20,blue]
  OPTIONS,'toppings','labflag',3
  OPTIONS,'toppings','labpos',[-3.5,-2.5]


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Some text output?
  ;; PRINT_DIAGNOSTICS__BIG_KAPPA_AND_GAUSS_CURRENT,jMag.y[50:-50],chartot_interp, $
  ;;    kappa_current,kappaTime,Astruct, $
  ;;    maxwell_current,gaussTime,AstructGauss

  IF KEYWORD_SET(output_fit_textFile) THEN BEGIN
     PRINT_KAPPA_FITS,chartot_interp, $
                      kappa_current, $
                      out_kappa_fit_structs, $
                      OUTFILE=kappaTxtFile, $
                      OUTDIR=outDir

     PRINT_KAPPA_FITS,chartot_interp, $
                      maxwell_current, $
                      out_gauss_fit_structs, $
                      OUTFILE=gaussTxtFile, $
                      OUTDIR=outDir
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOR PLOTTING
  IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN

     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Elphic_et_al_1998'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN
           POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8
        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=700,YSIZE=900
        ENDELSE
     ENDELSE

     LOADCT,39

     !p.charsize=1.3
     TPLOT,['dB_East','jtemp','Je','JEei','Ped','POTENTIAL','el_0','el_pa','onecheese','kappa_fit'], $
           VAR_LABEL=['ALT','MLT','ILAT'],TITLE='ORBIT 1773',TRANGE=[t1,t2]
     ;; TPLOT_PANEL,VARIABLE='jtemp',OPLOTVAR='jtemp_fill'

     ;;From McFadden et al. reproduction
     ;; TPLOT,['el_0','el_pa','ion_180','ion_pa','E_ALONG_V', $
     ;;        'charepanel','dBpanel','JeF','onecheese','kappa_fit'], $
     ;;       VAR_LABEL=['ALT','MLT','ILAT'], $
     ;;       TRANGE=[t1,t2]
     ;;got more than we need so smoothing can be nice
     ;; TLIMIT,t1+10.,t2-10.


     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings' ;,PSYM=1
     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        PCLOSE
     ENDIF

  ENDIF
END
