;2016/09//08
;And now it's time to apply the latest wisdoms to orb 1849
PRO JOURNAL__20160908__REPRODUCE_MCFADDEN_ET_AL_1998__FIG_1__OUTRIGHT_2D_FIT
   SAVE_DATA=save_data, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   FIT2D__USE_DATA_DENS=use_data_dens, $
   FIT2D__CALC_FITDENS_OVER_ARANGE=calc_fitDens__aRange, $
   FIT2D__BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkEnergy, $
   FIT2D__BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkEnergy, $
   FIT2D__NO_CHARI_FOR_POT=no_charI_for_pot, $
   HIGHDENSITY_THRESHOLD=highDens_thresh, $
   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
   USE_JE_CURRENT=use_je_current, $
   SDT_CALC__NO_MODEL=SDT_calc__no_model

  COMPILE_OPT IDL2

  IF N_ELEMENTS(use_je_current) EQ 0 THEN BEGIN
     use_je_current  = 1
  ENDIF

  use_mpFit1D        = 1

  ;; GET_DATA,'ion_pa',    DATA=ion_pa_originalsk                                
  ;; GET_DATA,'E_ALONG_V', DATA=eAlongV_originalsk
  ;; GET_DATA,'dB_fac_v',  DATA=db_fac_originalsk
  ;; GET_DATA,'ALT',       DATA=alt_originalsk
  ;; GET_DATA,'ILAT',      DATA=ilat_originalsk
  ;; GET_DATA,'el_0',      DATA=el_0_originalsk
  ;; GET_DATA,'el_pa',     DATA=el_pa_originalsk
  ;; GET_DATA,'ion_180',   DATA=ion_180_originalsk
  ;; GET_DATA,'ion_pa',    DATA=ion_pa_originalsk                                
  ;; GET_DATA,'Je',        DATA=Je_originalsk
  ;; GET_DATA,'Jee',       DATA=Jee_originalsk
  ;; GET_DATA,'Ji',        DATA=Ji_originalsk
  ;; GET_DATA,'Jei',       DATA=Jei_originalsk
  ;; GET_DATA,'fa_vel',    DATA=vel_originalsk
  ;; GET_DATA,'B_model',   DATA=B_model_originalsk
  ;; GET_DATA,'JeF',       DATA=JeF_originalsk

  R_B                = 40.0      ;For calculating Maxwellian and Kappa current

  fitDir             = '~/software/sdt/batch_jobs/saves_output_etc/'

  ;; fitFile            = '20160808--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150--mpfitfun1d.sav'

  fitFile            = '20160809--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--eeb--fit2d--all_times--150to150--mpfitfun1d--saveme.sav'

  kappaTxtFile                 = '20160809--McFadden_et_al_1998--Kappa_fits.txt'
  gaussTxtFile                 = '20160809--McFadden_et_al_1998--Gauss_fits.txt'

  offlineFile                    = 'orb_1849--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--burst_offline.sav'

  KAPPA_FITFILE_STRING,outSuff, $
                       R_B=R_B, $
                       USE_DATA_DENS=use_data_dens, $
                       BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkEnergy, $
                       BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkEnergy, $
                       CALC_FITDENS_OVER_ARANGE=calc_fitDens__aRange, $
                       NO_CHARI_FOR_POT=no_charI_for_pot, $
                       SDT_CALC__NO_MODEL=SDT_calc__no_model, $
                       LKAPPA_THRESH=lKappa_thresh, $
                       HKAPPA_THRESH=hKappa_thresh, $
                       HIGHDENS_THRESH=highDens_thresh, $
                       USE_MPFIT1D=use_mpFit1D, $
                       OUT_PARED_SUFF=paredSuff


  outSaveFile                    = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--McFadden_et_al_1998_Fig_1--four_currents--2dfits' + $
                                   paredSuff + '.sav'

  ;;Restore up front so it doesn't corrupt future variables
  RESTORE,fitDir+fitFile
  
  ;;No clobber
  je_fitFile         = je
  jee_fitFile        = jee
  je                 = !NULL
  jee                = !NULL

  survOrBurst                    = 'eeb'
  iSurvOrBurst                   = 'ieb'

  energy_electrons               = [3000.,36000.]
  energy_ions                    = [50.,36000.]
  ucla_mag_despin                = 1
  do_losscone                    = 0
  ;;Orbit 1849
  t1Str                          = '97-2-8/10:11:22'
  t2Str                          = '97-2-8/10:11:52'

  t1                             = STR_TO_TIME(t1Str)
  t2                             = STR_TO_TIME(t2Str)
  t1Adj                          = t1-10.
  t2Adj                          = t2+10.

  red                            = 250
  green                          = 130
  blue                           = 80
  black                          = 10

  outPlotName                    = STRING(FORMAT='(A0,"--McFadden_et_al_1998--Fig_1--with_kappa_and_four_currents",A0)', $
                                          GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                          outSuff)

  saveStr                        = 'SAVE,'

  ;;Get fields stuff, eFields and magFields
  FA_FIELDS_DESPIN,T1=t1Adj,T2=t2Adj,DAT=despun_E
  GET_DATA,'E_NEAR_B',DATA=eNearB
  GET_DATA,'E_ALONG_V',DATA=eAlongV
  GET_DATA,'E_ALONG_V',DATA=eAlongV_originalsk
  saveStr+='eAlongV_originalsk,'
  STORE_DATA,'E_ALONG_V',DATA={x:eAlongV.x,y:SMOOTH(eAlongV.y,160,/EDGE_TRUNCATE)} 
  OPTIONS,'E_ALONG_V','ytitle','E Along V!C(mV/m)'
  YLIM,'E_ALONG_V',-1000,1000

  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     GET_DATA,'dB_fac_v',DATA=db_fac
     IF SIZE(db_fac,/TYPE) NE 8 THEN BEGIN
        UCLA_MAG_DESPIN

        GET_DATA,'dB_fac_v',DATA=db_fac
     ENDIF

     GET_DATA,'dB_fac_v',DATA=db_fac_originalsk
     saveStr+='db_fac_originalsk,'

     mintime                     = MIN(ABS(t1-db_fac.x),ind1)
     mintime                     = MIN(ABS(t2-db_fac.x),ind2)

     magx                        = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
     magy                        = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}
     magz                        = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

  ENDIF ELSE BEGIN
     dat                         = GET_FA_FIELDS('MagDC',t1,/START)
     field                       = GET_FA_FIELDS('MagDC',t1,t2,/STORE)

     GET_DATA,'MagDCcomp1',DATA=magx
     GET_DATA,'MagDCcomp2',DATA=magy
     GET_DATA,'MagDCcomp3',DATA=magz

  ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  IF KEYWORD_SET(do_losscone) THEN BEGIN
     ;;Define loss cone angle
     GET_DATA,'ALT',DATA=alt
     GET_DATA,'ALT',DATA=alt_originalsk
     saveStr+='alt_originalsk,'
     loss_cone_alt               = alt.y[0]*1000.0
     lcw                         = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
     GET_DATA,'ILAT',DATA=ilat
     GET_DATA,'ILAT',DATA=ilat_originalsk
     saveStr+='ilat_originalsk,'
     north_south                 = ABS(ilat.y[0])/ilat.y[0]

     if north_south EQ -1 then begin
        eAngle                   = [180.-lcw,180+lcw] ; for Southern Hemis.

        ;;Eliminate ram from data
        iAngle                   = [180.0,360.0]
        iAngle=[270.0,360.0]

     endif else begin
        eAngle                   = [360.-lcw,lcw] ;	for Northern Hemis.

        ;;Eliminate ram from data
        iAngle                   = [0.0,180.0]
        iAngle                   = [90.0,180.0]

     endelse
  ENDIF ELSE BEGIN
     ;; eAngle                      = [360.-30.,30.]
     eAngle                      = [360.-40.,40.]
     iAngle                      = [135.,225.]
  ENDELSE

  ;; eAngleChare                 = [-180,180]
  ;; iAngleChari                 = [-180,180]
  eAngleChare                    = eAngle
  iAngleChari                    = iAngle
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  GET_EN_SPEC,'fa_' + survOrBurst + '_c',UNITS='eflux',NAME='el_0',ANGLE=eAngle,RETRACE=1,T1=t1,T2=t2,/CALIB
  GET_DATA,'el_0', DATA=tmp     ; get data structure
  GET_DATA,'el_0', DATA=el_0_originalsk
  saveStr+='el_0_originalsk,'
  tmp.y                          = tmp.y>1.e1                          ; Remove zeros
  tmp.y                          = ALOG10(tmp.y)                       ; Pre-log
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
  GET_DATA,'el_pa',DATA=tmp     ; get data structure
  GET_DATA,'el_pa',DATA=el_pa_originalsk
  saveStr+='el_pa_originalsk,'                               ; get data structure
  tmp.y                          = tmp.y>1.e1                ; Remove zeros
  tmp.y                          = ALOG10(tmp.y)             ; Pre-log
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
  ionAngle                       = [135,225]
  ionAngle                       = [0,180]
  ionAStr                        = STRCOMPRESS(ionAngle,/REMOVE_ALL)
  GET_EN_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_180',ANGLE=ionAngle,RETRACE=1,T1=t1,T2=t2
  GET_DATA,'ion_180',DATA=tmp   ; get data structure
  GET_DATA,'ion_180',DATA=ion_180_originalsk
  saveStr+='ion_180_originalsk,'
  tmp.y                          = ALOG10(tmp.y)                                         ; Pre-log
  STORE_DATA,'ion_180',DATA=tmp                                                          ; store data structure
  OPTIONS,'ion_180','spec',1                                                             ; set for spectrogram
  ZLIM,'ion_180',5,7,0                                                                   ; set z limits
  YLIM,'ion_180',3,30000,1                                                               ; set y limits
  OPTIONS,'ion_180','ytitle','i+ '+ionAStr[0]+'!Uo!N-'+ionAStr[1]+'!Uo!N!C!CEnergy (eV)' ; y title
  OPTIONS,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'                           ; z title
  OPTIONS,'ion_180','x_no_interp',1                                                      ; don't interpolate
  OPTIONS,'ion_180','y_no_interp',1                                                      ; don't interpolate
  OPTIONS,'ion_180','yticks',3                                                           ; set y-axis labels
  OPTIONS,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N']                ; set y-axis labels
  OPTIONS,'ion_180','ytickv',[10,100,1000,10000]                                         ; set y-axis labels
  OPTIONS,'ion_180','panel_size',1                                                       ; set panel size

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ion pitch angle spectrogram - survey data, remove retrace, >10 ions
  GET_PA_SPEC,'fa_' + iSurvOrBurst + '_c',UNITS='eflux',NAME='ion_pa',ENERGY=[10,30000],RETRACE=1,/SHIFT90,T1=t1,T2=t2
  GET_DATA,'ion_pa',DATA=tmp    ; get data structure
  GET_DATA,'ion_pa',DATA=ion_pa_originalsk
  saveStr+='ion_pa_originalsk,'                                
  tmp.y                          = tmp.y > 1.               ; Remove zeros
  tmp.y                          = ALOG10(tmp.y)            ; Pre-log
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
  GET_DATA,'Je',DATA=Je_originalsk
  saveStr+='Je_originalsk,'
  keep1                          = WHERE(FINITE(tmp.y) NE 0)
  keep2                          = WHERE(ABS(tmp.y) GT 0.0)
  GET_DATA,'Jee',DATA=tmp
  GET_DATA,'Jee',DATA=Jee_originalsk
  saveStr+='Jee_originalsk,'
  keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Ji',DATA=tmp
  GET_DATA,'Ji',DATA=Ji_originalsk
  saveStr+='Ji_originalsk,'
  keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Jei',DATA=tmp
  GET_DATA,'Jei',DATA=Jei_originalsk
  saveStr+='Jei_originalsk,'
  keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Je',DATA=tmp
  tmp.x                          = tmp.x[keep1]
  tmp.y                          = tmp.y[keep1]
  je_tmp_time                    = tmp.x[keep2]
  je_tmp_data                    = tmp.y[keep2]
  STORE_DATA,'Je',DATA={x:je_tmp_time,y:je_tmp_data}
  GET_DATA,'Jee',DATA=tmp
  tmp.x                          = tmp.x[keep1]
  tmp.y                          = tmp.y[keep1]
  jee_tmp_time                   = tmp.x[keep2]
  jee_tmp_data                   = tmp.y[keep2]
  STORE_DATA,'Jee',DATA={x:jee_tmp_time,y:jee_tmp_data}
  GET_DATA,'Ji',DATA=tmp
  tmp.x                          = tmp.x[keep1]
  tmp.y                          = tmp.y[keep1]
  ji_tmp_time                    = tmp.x[keep2]
  ji_tmp_data                    = tmp.y[keep2]
  STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
  GET_DATA,'Jei',DATA=tmp
  tmp.x                          = tmp.x[keep1]
  tmp.y                          = tmp.y[keep1]
  jei_tmp_time                   = tmp.x[keep2]
  jei_tmp_data                   = tmp.y[keep2]
  STORE_DATA,'Jei',DATA={x:jei_tmp_time,y:jei_tmp_data}

  GET_DATA,'Je',DATA=Je
  GET_DATA,'Jee',DATA=Jee
  GET_DATA,'Ji',DATA=Ji
  GET_DATA,'Jei',DATA=Jei
  chare                          = Jee.y/Je.y*6.242*1.0e11
  chari                          = Jei.y/Ji.y*6.242*1.0e11
  FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1},{time:Jei.x,comp1:chari,ncomp:1},RESULT=chari_interp,/INTERP,DELT_T=50.,/TALK
  ;; chari_interp                = {x:Jee.x,y:chari_interp}
  chartot                        = chare+chari_interp
  STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari_interp],[chare],[chartot]]}

  OPTIONS,'charepanel','tplot_routine','mplot'
  OPTIONS,'charepanel','ytitle','E/q Volts'
  OPTIONS,'charepanel','labels',['Ion','Electron','Total']
  OPTIONS,'charepanel','colors',[red,green,20]
  OPTIONS,'charepanel','labflag',1
  OPTIONS,'charepanel','yticks',5                                     ; set y-axis labels
  OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
  OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

  ;;Now get kappa vals from file restored at beginning of routine
  IF N_ELEMENTS(fit2DKappa_inf_list) NE N_ELEMENTS(fit2DGauss_inf_list) THEN BEGIN
     PRINT,"These lists are out of order! You're about to enter a world of confusion and pain, and I beg you reconsider."
     STOP
  ENDIF

  kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DKappa_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                                   /DESTROY_INFO_LIST, $
                                                   OUT_GOOD_I=includeK_i, $
                                                   OUT_GOOD_T=includeK_t, $
                                                   OUT_BAD_I=excludeK_i, $
                                                   OUT_BAD_T=excludeK_t)

  gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST(fit2DGauss_inf_list, $
                                                   HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                   KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                   KAPPA_HIGHTHRESHOLD=100.1, $
                                                   /DESTROY_INFO_LIST, $
                                                   OUT_GOOD_I=includeG_i, $
                                                   OUT_GOOD_T=includeG_t, $
                                                   OUT_BAD_I=excludeG_i, $
                                                   OUT_BAD_T=excludeG_t)

  PARSE_KAPPA_FIT_STRUCTS,kappa2D.params1D, $
                          A=a, $
                          TIME=kappaTime, $
                          STRUCT_A=AStruct, $
                          NAMES_A=ANames, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus, $
                            USE_MPFIT1D=use_mpFit1D

  PARSE_KAPPA_FIT_STRUCTS,gauss2D.params1D, $
                          A=AGauss, $
                          STRUCT_A=AStructGauss, $
                          TIME=gaussTime, $
                          NAMES_A=AGaussNames, $
                          CHI2=chi2Gauss, $
                          PVAL=pValGauss, $
                          FITSTATUS=gaussfitStatus, $
                          USE_MPFIT1D=use_mpFit1D

  nFits                        = N_ELEMENTS(Astruct.kappa)
  badFits_i                    = WHERE(fitStatus NE 0,nBadFits)
  badGaussFits_i               = WHERE(gaussFitStatus NE 0,nBadGaussFits)
  PRINT,""
  PRINT,"****************************************"
  PRINT,'NTotalFits    : ',nFits
  PRINT,''
  PRINT,"NbadFits      : ",nBadFits
  PRINT,"NbadGaussFits : ",nBadGaussFits
  PRINT,"NBothBad      : ",N_ELEMENTS(CGSETINTERSECTION(badFits_i,badGaussFits_i))

  STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
  YLIM,'kappa_fit',1.0,100,1
  OPTIONS,'kappa_fit','ytitle',"Kappa!CFit Val"
  OPTIONS,'kappa_fit','psym',2  ;Asterisk


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Four-current panel (it's like four-cheese pizza)

  ;;Get mag current
  jMag                         = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                                          db_fac,vel, $
                                                          USE_DESPUN=ucla_mag_despin, $
                                                          SDTNAME__JMAG=jMagName, $
                                                          ;; INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                                                          ;; SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                                                          QUIET=quiet)

  ;;Get electron ESA current, ion ESA current
  Je_current                   = (-1.)*Je.y*1.6e-9 ;;in microA/m2
  Ji_current                   =       Ji.y*1.6e-9 ;;in microA/m2

  ;;Get Kappa-predicted current
  kappaStr                       = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
  
  ;;Align to kappa fits
  FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current,ncomp:1}, $
                    RESULT=Je_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Ji.x,comp1:Ji_current,ncomp:1}, $
                    RESULT=Ji_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current+Ji_current,ncomp:1}, $
                    RESULT=Jtot_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chare,ncomp:1}, $
                    RESULT=chare_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chari_interp,ncomp:1}, $
                    RESULT=chari_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1}, $
                    RESULT=chartot_kappa_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:jMag.x,comp1:jMag.y,ncomp:1}, $
                    RESULT=jMag_kappa_interp,/INTERP,DELT_T=50.,/TALK
  
  ;;Align to Je
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:Ji.x,comp1:Ji_current,ncomp:1}, $
                    RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:jMag.x,comp1:jMag.y,ncomp:1}, $
                    RESULT=jMag_interp,/INTERP,DELT_T=50.,/TALK
  
  Jtot_interp                    = {x:Je.x,y:Je_current+Ji_interp}

  STORE_DATA,'Je',DATA=Je


  setup = {kappaS:Astruct, $
           gaussS:AStructGauss, $
           charE:charE_kappa_interp, $
           charI:charI_kappa_interp, $
           charTot:charTot_kappa_interp, $
           Jtot:Jtot_kappa_interp, $
           JMag:jMag_kappa_interp, $
           Je:Je_kappa_interp, $
           Ji:Ji_kappa_interp}

  SETUP_POTENTIAL_AND_CURRENT,setup, $ 
                              obs_current,obsName,obsSuff, $
                              kappaPot,gaussPot, $
                              potName,potTitleStr, $
                              USE_JE_CURRENT=use_je_current, $
                              USE_JMAG_CURRENT=use_jMag_current, $
                              ;; /BOTH_USE_KAPPA_BULKENERGY, $
                              ;; /BOTH_USE_MAXWELL_BULKENERGY, $
                              BOTH_USE_KAPPA_BULKENERGY=both_use_kappa_bulkEnergy, $
                              BOTH_USE_MAXWELL_BULKENERGY=both_use_maxwell_bulkEnergy, $
                              NO_CHARI_FOR_POT=no_charI_for_pot

  kappaDens = KAPPA__SELECT_2DFIT_DENS(kappa2D, $
                                       USE_DATA_DENS=use_data_dens, $
                                       CALC_FITDENS_OVER_ELECTRON_ARANGE=calc_fitDens__aRange, $
                                       ELECTRON_ANGLERANGE=(N_ELEMENTS(calc_fitDens__aRange) EQ 2 ? calc_fitDens__aRange : eAngleChare), $
                                       ;; ELECTRON_ANGLERANGE=eAngleChare, $
                                       FITTYPE__STRING='Kappa')

  gaussDens = KAPPA__SELECT_2DFIT_DENS(gauss2D, $
                                       USE_DATA_DENS=use_data_dens, $
                                       CALC_FITDENS_OVER_ELECTRON_ARANGE=calc_fitDens__aRange, $
                                       ELECTRON_ANGLERANGE=(N_ELEMENTS(calc_fitDens__aRange) EQ 2 ? calc_fitDens__aRange : eAngleChare), $
                                       ;; ELECTRON_ANGLERANGE=eAngleChare, $
                                       FITTYPE__STRING='Maxwell')

  CASE 1 OF
     KEYWORD_SET(SDT_calc__no_model): BEGIN
        GET_2DFIT_KAPPA_AND_MAXWELLIAN_CURRENT,kappa2D,gauss2D, $
                                               kappa_current,gauss_current, $
                                               ENERGY_ELECTRONS=energy_electrons, $
                                               ANGLE=eAngleCharE
     END
     ELSE: BEGIN
        GET_KAPPA_AND_MAXWELLIAN_CURRENT,AStruct,AStructGauss, $
                                         kappaPot,gaussPot,R_B, $
                                         kappa_current,gauss_current,obs_current, $
                                         DENSITY_KAPPA2D=kappaDens, $
                                         DENSITY_GAUSS2D=gaussDens ;, $
     END
  ENDCASE

  
  STORE_DATA,'onecheese',DATA={x:kappaTime, $

                               y:obs_current}
  STORE_DATA,'fourcheese',DATA={x:jMag.x, $

                                y:jMag.y}
  STORE_DATA,'toppings',DATA={x:[[kappaStr.time],[kappaStr.time]], $

                              y:[[gauss_current],[kappa_current]]}

  OPTIONS,'onecheese','colors',green
  OPTIONS,'onecheese','tplot_routine','mplot'
  OPTIONS,'onecheese','ytitle','Current!C('+CGGREEK('mu')+'A/m!U2!Ns)'
  YLIM,   'onecheese',-3,0

  OPTIONS,'onecheese','labels',obsName
  OPTIONS,'onecheese','labflag',3
  OPTIONS,'onecheese','labpos',-0.5
  OPTIONS,'onecheese','yticks',5                                    ; set y-axis labels
  OPTIONS,'onecheese','ytickname',['-4.0','-3.0','-2.0','-1.0','0'] ; set y-axis labels
  OPTIONS,'onecheese','ytickv',[-4.0,-3.0,-2.0,-1.0,0.0]            ; set y-axis labels

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
  ;;dB panel

  ;;Get speed and position for calculation of mag stuff
  GET_FA_ORBIT,magz.x,/TIME_ARRAY,/ALL
  GET_DATA,'fa_vel',DATA=vel
  GET_DATA,'fa_vel',DATA=vel_originalsk
  saveStr+='vel_originalsk,'
  speed                          = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  old_pos                        = 0.
  position                       = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  speed_mag_point                = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(magz.x)-2 DO BEGIN
     speed_point_ind             = MIN(ABS(vel.x-magz.x[j]),ind)

     speed_mag_point[j]          = speed[ind]
     samplingperiod              = magz.x[j+1] - magz.x[j]

     position[j]                 = old_pos + speed_mag_point[j]*samplingperiod
     old_pos                     = position[j]
  ENDFOR

  ;;calculate sum of Je, since we're thinking about it
  magz                           = {time:magz.x,comp1:magz.y,ncomp:1}
  Je                             = {time:Je.x,comp1:Je.y}
  FA_FIELDS_COMBINE,magz,Je, $
                    RESULT=Je_interp,/INTERP,DELT_T=50.,/TALK
  Je_integ_interp                = MAKE_ARRAY(N_ELEMENTS(magz.time),/DOUBLE)
  Je_integ_interp[0]             = 0.0D
  FOR i=1,N_ELEMENTS(Je_integ_interp)-1 DO BEGIN
     Je_integ_interp[i]          = TSUM(position[0:i],Je_interp[0:i])
  ENDFOR

  ;;Fix what we did to poor magz
  magz                           = {x:magz.time,y:magz.comp1}
  Je                             = {x:Je.time,y:(-1.)*Je.comp1}
  STORE_DATA,'Je',DATA=Je

  ;; Fix it up, get the numbers back to nanotesla
  Je_integ_interp                = Je_integ_interp*(-1.0e3)*1.26e-6*1.6e-9
  Je_integ_interp                = Je_integ_interp+(magz.y[0]-Je_integ_interp[0])

  ;;Get orbit stuff
  GET_DATA,'B_model',DATA=tmp1
  GET_DATA,'B_model',DATA=B_model_originalsk
  saveStr+='B_model_originalsk,'
  ;;Now make me smile, do the model subtraction
  JeDat                          = Je_integ_interp > 1
  IF KEYWORD_SET(ucla_mag_despin) THEN BEGIN
     magDat                      = magz.y > 1
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[JeDat],[magz.y]]}
  ENDIF ELSE BEGIN
     magDat                      = (magz.y-tmp1.y[*,1]) > 1
     STORE_DATA,'dBpanel',DATA={x:[[magz.x],[magz.x]],y:[[JeDat],[magDat]]}
  ENDELSE
  ;;Now set options
  OPTIONS,'dBpanel','tplot_routine','mplot'
  OPTIONS,'dBpanel','ytitle','dB!CnT'
  OPTIONS,'dBpanel','labels',[CGGREEK('Sigma')+'Je*dx','B-B!Dmodel!N']
  OPTIONS,'dBpanel','colors',[green,red]
  OPTIONS,'dBpanel','labflag',1
  YLIM,'dBpanel',100,300,STYLE=1

  ;; Electron flux
  GET_2DT,'j_2d_fs','fa_eeb_c',name='JeF',t1=t1,t2=t2,energy=[100,30000],ANGLE=eAngle
  GET_DATA,'JeF',DATA=tmp
  GET_DATA,'JeF',DATA=JeF_originalsk
  saveStr+='JeF_originalsk,'
  tmp.y                          = tmp.y>1.e1 ; Remove zeros
  STORE_DATA,'JeF',DATA={x:tmp.x,y:tmp.y}
  YLIM,'JeF',1.e8,2.e9,1                              ; set y limits
  OPTIONS,'JeF','ytitle','Electrons!C!C1/(cm!U2!N-s)' ; set y title
  OPTIONS,'JeF','colors',green
  OPTIONS,'JeF','labels','Downgoing!C Electrons' ; set color label



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Some text output?
  ;; PRINT_DIAGNOSTICS__BIG_KAPPA_AND_GAUSS_CURRENT,jMag.y[50:-50],chartot_kappa_interp, $
  ;;    kappa_current,kappaTime,Astruct, $
  ;;    gauss_current,gaussTime,AstructGauss

  IF KEYWORD_SET(output_fit_textFile) OR KEYWORD_SET(print_current_info) THEN BEGIN

     IF KEYWORD_SET(output_fit_textFile) THEN BEGIN
        kappaTxt                 = kappaTxtFile
        gaussTxt                 = gaussTxtFile
     ENDIF

     PRINT_KAPPA_FITS,chartot_kappa_interp, $
                      kappa_current, $
                      kappaFits, $
                      OUTFILE=kappaTxt, $
                      OUTDIR=outDir

     PRINT_KAPPA_FITS,chartot_kappa_interp, $
                      gauss_current, $
                      gaussFits, $
                      OUTFILE=gaussTxt, $
                      OUTDIR=outDir

  ENDIF

  saveStr += 'FILENAME="'+fitDir+offlineFile + '"'

  PRINT,"Saving to " + offlineFile + " ..."
  this     = EXECUTE(saveStr)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Save data?
  IF KEYWORD_SET(save_data) THEN BEGIN
     PRINT,'Saving data to ' + outSaveFile + ' ...'

     GET_DATA,'onecheese',DATA=jESA
     GET_DATA,'fourcheese',DATA=jMag

     chari                       = chari_interp ;;This is just charI interped to charE resolution—a difference of ~one data point if memory serves
     SAVE,R_B, $
          chartot,chartot_kappa_interp, $
          chari,chari_kappa_interp, $
          chare,chare_kappa_interp, $
          kappaStr,kappa_current,gauss_current, $
          jESA,obsName, $
          jMag, $
          Je_kappa_interp,Ji_kappa_interp,Jtot_kappa_interp,jMag_kappa_interp, $
          FILENAME=fitDir+outSaveFile
     
  ENDIF
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOR PLOTTING
  IF NOT KEYWORD_SET(dontShowPlots) THEN BEGIN
     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/McFadden_et_al_1998'
     ENDIF

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN

           POPEN,plotDir+outPlotName,/PORT,FONT=-1 
           DEVICE,/PALATINO,FONT_SIZE=8

        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=700,YSIZE=900
        ENDELSE
     ENDELSE

     LOADCT,39

     TPLOT,['E_ALONG_V','charepanel','onecheese','kappa_fit'], $
           VAR_LABEL=['ALT','MLT','ILAT'], $
           TRANGE=[t1,t2]



     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
     TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN

        PCLOSE
     ENDIF ELSE BEGIN

     ENDELSE
  ENDIF

END


