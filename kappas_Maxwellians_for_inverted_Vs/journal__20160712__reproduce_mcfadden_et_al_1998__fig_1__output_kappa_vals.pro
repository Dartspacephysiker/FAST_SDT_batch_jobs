;;07/12/16
PRO JOURNAL__20160712__REPRODUCE_MCFADDEN_ET_AL_1998__FIG_1__OUTPUT_KAPPA_VALS

  COMPILE_OPT IDL2

  fitDir                       = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile                      = '20160712--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--synthetic_SDT_structs.sav'

  kappaTxtFile                 = '20160712--McFadden_et_al_1998--Kappa_fits.txt'
  gaussTxtFile                 = '20160712--McFadden_et_al_1998--Gauss_fits.txt'

  ;;Restore up front so it doesn't corrupt future variables
  RESTORE,fitDir+fitFile

  survOrBurst                  = 'eeb'
  iSurvOrBurst                 = 'ieb'

  energy_electrons             = [100.,36000.]
  energy_ions                  = [10.,36000.]
  ucla_mag_despin              = 1
  do_losscone                  = 0
  ;;Orbit 1894
  t1Str                        = '97-2-8/10:11:22'
  t2Str                        = '97-2-8/10:11:52'

  t1                           = STR_TO_TIME(t1Str)
  t2                           = STR_TO_TIME(t2Str)
  t1Adj                        = t1-10.
  t2Adj                        = t2+10.

  red                          = 250
  green                        = 130
  blue                         = 80
  black                        = 10

  outPlotName                  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--McFadden_et_al_1998--Fig_1--with_kappa_and_four_currents'

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

     mintime                   = MIN(ABS(t1-db_fac.x),ind1)
     mintime                   = MIN(ABS(t2-db_fac.x),ind2)
     
     magx                      = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
     magy                      = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}
     magz                      = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}

  ENDIF ELSE BEGIN
     dat                       = GET_FA_FIELDS('MagDC',t1,/START)
     field                     = GET_FA_FIELDS('MagDC',t1,t2,/STORE)

     GET_DATA,'MagDCcomp1',DATA=magx
     GET_DATA,'MagDCcomp2',DATA=magy
     GET_DATA,'MagDCcomp3',DATA=magz

  ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  IF KEYWORD_SET(do_losscone) THEN BEGIN
     ;;Define loss cone angle
     GET_DATA,'ALT',DATA=alt
     loss_cone_alt             = alt.y[0]*1000.0
     lcw                       = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
     GET_DATA,'ILAT',DATA=ilat
     north_south               = ABS(ilat.y[0])/ilat.y[0]
     
     if north_south EQ -1 then begin
        eAngle                 = [180.-lcw,180+lcw] ; for Southern Hemis.

        ;;Eliminate ram from data
        iAngle                 = [180.0,360.0]
        iAngle=[270.0,360.0]
        
     endif else begin
        eAngle                 = [360.-lcw,lcw] ;	for Northern Hemis.

        ;;Eliminate ram from data
        iAngle                 = [0.0,180.0]
        iAngle                 = [90.0,180.0]
        
     endelse
  ENDIF ELSE BEGIN
     eAngle                    = [360.-30.,30.]
     iAngle                    = [135.,225.]
  ENDELSE

  ;; eAngleChare               = [-180,180]
  ;; iAngleChari               = [-180,180]
  eAngleChare                  = eAngle
  iAngleChari                  = iAngle

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  GET_2DT,'j_2d_fs','fa_' + survOrBurst + '_c',NAME='Je',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  GET_2DT,'je_2d_fs','fa_' + survOrBurst + '_c',NAME='Jee',T1=t1,T2=t2,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
  GET_2DT,'j_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Ji',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  GET_2DT,'je_2d_fs','fa_' + iSurvOrBurst + '_c',NAME='Jei',T1=t1,T2=t2,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
  ;;Remove_crap
  GET_DATA,'Je',DATA=tmp
  keep1                        = WHERE(FINITE(tmp.y) NE 0)
  keep2                        = WHERE(ABS(tmp.y) GT 0.0)
  GET_DATA,'Jee',DATA=tmp
  keep1                        = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                        = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Ji',DATA=tmp
  keep1                        = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                        = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Jei',DATA=tmp
  keep1                        = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
  keep2                        = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
  GET_DATA,'Je',DATA=tmp
  tmp.x                        = tmp.x[keep1]
  tmp.y                        = tmp.y[keep1]
  je_tmp_time                  = tmp.x[keep2]
  je_tmp_data                  = tmp.y[keep2]
  STORE_DATA,'Je',DATA={x:je_tmp_time,y:je_tmp_data}
  GET_DATA,'Jee',DATA=tmp
  tmp.x                        = tmp.x[keep1]
  tmp.y                        = tmp.y[keep1]
  jee_tmp_time                 = tmp.x[keep2]
  jee_tmp_data                 = tmp.y[keep2]
  STORE_DATA,'Jee',DATA={x:jee_tmp_time,y:jee_tmp_data}
  GET_DATA,'Ji',DATA=tmp
  tmp.x                        = tmp.x[keep1]
  tmp.y                        = tmp.y[keep1]
  ji_tmp_time                  = tmp.x[keep2]
  ji_tmp_data                  = tmp.y[keep2]
  STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
  GET_DATA,'Jei',DATA=tmp
  tmp.x                        = tmp.x[keep1]
  tmp.y                        = tmp.y[keep1]
  jei_tmp_time                 = tmp.x[keep2]
  jei_tmp_data                 = tmp.y[keep2]
  STORE_DATA,'Jei',DATA={x:jei_tmp_time,y:jei_tmp_data}

  GET_DATA,'Je',DATA=Je
  GET_DATA,'Jee',DATA=Jee
  GET_DATA,'Ji',DATA=Ji
  GET_DATA,'Jei',DATA=Jei
  chare                        = Jee.y/Je.y*6.242*1.0e11
  chari                        = Jei.y/Ji.y*6.242*1.0e11
  FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1},{time:Jei.x,comp1:chari,ncomp:1},RESULT=chari_interp,/INTERP,DELT_T=50.,/TALK
  ;; chari_interp              = {x:Jee.x,y:chari_interp}
  chartot                      = chare+chari_interp
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
  fitStatus                    = !NULL
  gaussFitStatus               = !NULL
  FOR i=0,N_ELEMENTS(out_kappa_fit_structs)-1 DO BEGIN
     fitStatus                 = [fitStatus,out_kappa_fit_structs[i].fitStatus]
     gaussFitStatus            = [gaussFitStatus,out_gauss_fit_structs[i].fitStatus]
  ENDFOR
  badFits_i                    = WHERE(fitStatus GT 0,nBadFits,COMPLEMENT=goodFits_i)  
  badGaussFits_i               = WHERE(gaussFitStatus GT 0,nBadGaussFits)  

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

  nFits                        = N_ELEMENTS(Astruct.kappa)
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
  ;;use Astruct[goodFits_i]; using chartot (chari+chare) for potential drop
  ;;
  kappaStr                     = {time:kappaTime,comp1:aStruct.kappa,ncomp:1}
  ;; Je_interp                 = {time:Je.x,comp1:Je_current,ncomp:1}
  ;; Ji_interp                 = {time:Ji.x,comp1:Ji_current,ncomp:1}
  ;; Jtot_interp               = {time:Je.x,comp1:Je_current+Ji_current,ncomp:1}
  ;; chartot_interp            = {time:Jee.x,comp1:chartot,ncomp:1}
  ;; magz_interp               = {time:magz.x,comp1:jMag,ncomp:1}
  ;; FA_FIELDS_COMBINE,magz,chartot_interp,RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK

  ;;Align to Je
  FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current,ncomp:1},RESULT=Je_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Ji.x,comp1:Ji_current,ncomp:1},RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
  ;; FA_FIELDS_COMBINE,kappaStr,Jtot_interp,RESULT=Jtot_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1},RESULT=chartot_interp,/INTERP,DELT_T=50.,/TALK
  FA_FIELDS_COMBINE,kappaStr,{time:jMag.x,comp1:jMag.y,ncomp:1},RESULT=magz_interp,/INTERP,DELT_T=50.,/TALK
  
  ;;Fix what we did to poor magz
  ;; Je_interp                 = {x:Je_interp.time,y:Je_interp.comp1}
  ;; Ji_interp                 = {x:Ji_interp.time,y:Ji_interp.comp1}
  ;; Jtot_interp               = {x:Je_interp.time,y:Je_interp.y+Ji_interp.y}
  Jtot_interp                  = {x:kappaStr.time,y:Je_interp+Ji_interp}
  ;; chartot_interp            = {x:chartot_interp.time,y:chartot_interp.comp1}
  ;; magz_interp               = {x:magz_interp.time,y:magz_interp.comp1}
  STORE_DATA,'Je',DATA=Je

  ;; kappa_current             = KNIGHT_RELATION__DORS_KLETZING_11(Astruct.kappa[goodFits_i],Astruct.temp[goodFits_i],Astruct.N[goodFits_i], $
  kappa_current                = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_11(Astruct.kappa,Astruct.temp,Astruct.N, $
                                                                                   chartot_interp/DOUBLE(1.6e-19),R_B) ;, $
  ;; IN_POTBAR=in_potBar, $
  ;; OUT_POTBAR=potBar)

  badDens                      = WHERE(Astruct.N GT 1,nBD)
  IF nBD GT 0 THEN BEGIN
     kappa_current[badDens]    = !VALUES.D_NAN
  ENDIF
  badFit                       = WHERE(fitStatus GT 0,nBF)
  IF nBF GT 0 THEN BEGIN
     kappa_current[badFit]     = !VALUES.D_NAN
  ENDIF
  ;; badK                      = WHERE(~FINITE(kappa_current))
  ;; IF badK[0] NE -1 THEN BEGIN
  ;;    kappa_current[badK]    = 0.0D
  ;; ENDIF

  ;;Get Maxwellian-predicted current
  maxwell_current              = DOUBLE(1.0e6) * KNIGHT_RELATION__DORS_KLETZING_4(AStructGauss.temp,AstructGauss.N,chartot_interp/DOUBLE(1.6e-19),R_B) ;, $
  ;; IN_POTBAR=in_potBar, $
  ;; OUT_POTBAR=potBar)
  badDens                      = WHERE(AstructGauss.N GT 1,nBD)
  IF nBD GT 0 THEN BEGIN
     maxwell_current[badDens]  = !VALUES.D_NAN
  ENDIF
  badFit                       = WHERE(gaussFitStatus GT 0,nBF)
  IF nBF GT 0 THEN BEGIN
     maxwell_current[badFit]   = !VALUES.D_NAN
  ENDIF
  ;; badM                      = WHERE(~FINITE(maxwell_current))
  ;; IF badM[0] NE -1 THEN BEGIN
  ;;    maxwell_current[badM]  = 0.0D
  ;; ENDIF



  ;;Make text output
  SET_TXTOUTPUT_DIR,outDir,/FOR_SDT,ADD_SUFF='/McFadden_et_al_1998'
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

  ;;Set up plot
  IF KEYWORD_SET(use_Je_current) THEN BEGIN
     esa_current               = Je_interp
     esa_name                  = 'e- ESA'
  ENDIF ELSE BEGIN
     esa_current               = Jtot_interp.y
     esa_name                  = 'e- and i+ ESA'
  ENDELSE
  
  STORE_DATA,'fourcheese',DATA={x:[[kappaStr.time],[kappaStr.time]], $
                                ;; y:[[magz_interp],[esa_current],[kappa_current],[maxwell_current]]}
                                y:[[esa_current],[magz_interp]]}
  OPTIONS,'fourcheese','colors',[green,red]
  OPTIONS,'fourcheese','tplot_routine','mplot'
  OPTIONS,'fourcheese','ytitle','Current!C('+CGGREEK('mu')+'A/m!U2!Ns)'
  YLIM,'fourcheese',-4,0
  ;; OPTIONS,'fourcheese','yticks',7                                                  ; set y-axis labels
  ;; OPTIONS,'fourcheese','ytickname',['-3.0','-2.5','-2.0','-1.5','-1.0','-0.5','0'] ; set y-axis labels
  ;; OPTIONS,'fourcheese','ytickv',[-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0]                ; set y-axis labels
  ;; OPTIONS,'fourcheese','labels',['Maxwellian Model','Kappa model',esa_name,'Fluxmag']
  OPTIONS,'fourcheese','labels',[esa_name,'Fluxmag']
  OPTIONS,'fourcheese','yticks',5                                    ; set y-axis labels
  OPTIONS,'fourcheese','ytickname',['-4.0','-3.0','-2.0','-1.0','0'] ; set y-axis labels
  OPTIONS,'fourcheese','ytickv',[-4.0,-3.0,-2.0,-1.0,0.0]            ; set y-axis labels

END
