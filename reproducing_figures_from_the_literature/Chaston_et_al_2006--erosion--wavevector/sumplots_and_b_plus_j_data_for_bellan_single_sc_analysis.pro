;;2017/02/23
PRO SUMPLOTS_AND_B_PLUS_J_DATA_FOR_BELLAN_SINGLE_SC_ANALYSIS, $
   T1=t1, $
   T2=t2, $
   ORBIT=orbit, $
   TIMEBAR_TIMES=timeBar_times, $
   EEB_OR_EES=eeb_or_ees, $
   IEB_OR_IES=ieb_or_ies, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   ION_ANGLE=ion_angle, $
   TPLT_VARS=tPlt_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   USE_DB_FAC=use_db_fac, $
   SKIP_DESPIN=skip_despin, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   PLOTPREF=plotPref, $
   TPLOT_RIGHTNOW=tPlot_rightNow, $
   SAVE_B_AND_J_DATA=save_B_and_J_data, $
   SAVESUFF=saveSuff, $
   SAVEDIR=saveDir, $
   ANCILLARY_PLOTS=ancillary_plots, $
   BONUSSUFF=bonusSuff, $
   PLOTDIRSUFF=plotDirSuff, $
   ADD_TIMEBAR=add_timebar

  IF ~KEYWORD_SET(eeb_or_ees) THEN BEGIN
     eeb_or_ees     = 'eeb'
  ENDIF
  IF ~KEYWORD_SET(ieb_or_ies) THEN BEGIN
     ieb_or_ies     = 'ieb'
  ENDIF

  despunStr         = KEYWORD_SET(skip_despin) ? '-no_B_despin' : ''

  IF ~KEYWORD_SET(plotPref) THEN plotPref = ''
  IF ~KEYWORD_SET(saveSuff) THEN saveSuff = ''

  orbStr            = STRCOMPRESS(orbit,/REMOVE_ALL)
  ee_ie_string      = '-' + eeb_or_ees + '-' + ieb_or_ies

  outPlotName       = 'Orb_' + orbStr + plotPref + ee_ie_string + despunStr
  saveFile          = 'Orbit_' + orbStr + '-B_and_J-' + $
                      GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
                      ee_ie_string + despunStr + $
                      saveSuff + '.sav'


  IF N_ELEMENTS(ancillary_plots) EQ 0 THEN ancillary_plots = 1
  IF KEYWORD_SET(ancillary_plots) THEN BEGIN
     outPlotName   += '-with_ancillaries'
  ENDIF

  dlist = GET_DQDS(START_TIMES=start_times,END_TIMES=end_times)
  this  = WHERE(STRMATCH(STRUPCASE(dlist),STRUPCASE('*ees*')) OR $
                STRMATCH(STRUPCASE(dlist),STRUPCASE('*eeb*')) OR $
                STRMATCH(STRUPCASE(dlist),STRUPCASE('*ies*')) OR $
                STRMATCH(STRUPCASE(dlist),STRUPCASE('*ieb*'))    $
                ,nMatch)
  IF nMatch GT 0 THEN BEGIN
     t1BKUP = start_times[this[0]]
     t2BKUP = end_times[this[0]]
  ENDIF

  IF KEYWORD_SET(t1) THEN BEGIN
     CASE SIZE(t1,/TYPE) OF
        7: BEGIN
           t1ZoomStr   = t1
        END
        5: BEGIN
           t1ZoomStr   = TIME_TO_STR(t1,/MS)
        END
        ELSE: BEGIN
           PRINT,"Bogusness?"
           STOP
        END
     ENDCASE
  ENDIF ELSE BEGIN
     t1ZoomStr         = t1BKUP
  ENDELSE

  IF KEYWORD_SET(t2) THEN BEGIN
     CASE SIZE(t2,/TYPE) OF
        7: BEGIN
           t2ZoomStr   = t2
        END
        5: BEGIN
           t2ZoomStr   = TIME_TO_STR(t2,/MS)
        END
        ELSE: BEGIN
           PRINT,"Bogusness?"
           STOP
        END
     ENDCASE
  ENDIF ELSE BEGIN
     t2ZoomStr         = t2BKUP
  ENDELSE

  t1Zoom            = STR_TO_TIME(t1ZoomStr)
  t2Zoom            = STR_TO_TIME(t2ZoomStr)

  IF GET_SDT_TIMESPAN(t1SDT,t2SDT) THEN BEGIN
     PRINT,'SDT timespan is from ',TIME_TO_STR(t1SDT),' to ',TIME_TO_STR(t2SDT)
     IF (t1SDT GT t1Zoom) OR (t2SDT LT t2Zoom) THEN BEGIN
        PRINT,"Hosed it! You've got insufficient data."
        RETURN
     ENDIF
  ENDIF ELSE BEGIN
     PRINT,' Could not get timespan! No SDT available?'
     RETURN
  ENDELSE

  IF KEYWORD_SET(timeBar_times) THEN BEGIN
     CASE SIZE(timeBar_times,/TYPE) OF
        7: BEGIN
           timesBarStr = timeBar_times
        END
        5: BEGIN
           timesBarStr = TIME_TO_STR(timeBar_times,/MS)
        END
        ELSE: BEGIN
           PRINT,"Bogusness?"
           STOP
        END
     ENDCASE

     IF N_ELEMENTS(add_timeBar) EQ 0 THEN BEGIN
        add_timeBar    = 1B
     ENDIF
  ENDIF

  ;;Alternative
  IF KEYWORD_SET(bonusSuff) THEN BEGIN
     outPlotName      += bonusSuff
     saveFile         += bonusSuff
  ENDIF

  timesBar          = STR_TO_TIME(timesBarStr)

  IF ~KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons  = [0.,30000.]
  ENDIF
  IF ~KEYWORD_SET(energy_ions) THEN BEGIN
     energy_ions       = [0.,30000.]
  ENDIF
  ;; IF ~KEYWORD_SET(ion_angle) THEN BEGIN
  ;;    ion_angle         = [180,360]
  ;; ENDIF

  EFieldVar         = 'EFIT_ALONG_VSC'
  EFieldVar         = 'E_ALONG_V'
  EFieldSpecVar     = 'E_ALONG_V_16k'
  sc_potName        = 'SC_POT'

  ;; plotDirSuff       = '/PREVIII_proceedings'
  IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotDirSuff
  ENDIF

  IF (KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) $
     AND KEYWORD_SET(tPlot_rightNow) $
  THEN BEGIN

     needed         = ["Eesa_Energy" , $
                       "Eesa_Angle"  , $
                       "Iesa_Energy" , $
                       "Iesa_Angle"  , $
                       "Je_plot"     , $
                       "jtemp"       , $
                       "dB_East"     , $
                       "E_ALONG_V", $
                       sc_potName]
     nNeeded        = N_ELEMENTS(needed)

     TPLOT_NAMES,NAMES=names
     haveIt         = 0
     iGuy           = 0
     WHILE iGuy LT nNeeded DO BEGIN
        IF (N_ELEMENTS(WHERE((STRUPCASE(needed))[iGuy] EQ STRUPCASE(names),/NULL)) GT 0) THEN BEGIN
           PRINT,"Got it: ",needed[iGuy]
           haveIt++
        ENDIF

        iGuy++
     ENDWHILE

     IF haveIt EQ nNeeded THEN BEGIN

        GET_DATA,sc_potName,DATA=sc_pot
        ;; GET_DATA,'dB_fac_v',DATA=db_fac
        GET_DATA,'jtemp',DATA=jtemp

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO,FONT_SIZE=8


           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE

        CASE 1 OF
           ;; (N_ELEMENTS(tlimit_north) gt 0): BEGIN
           ;;    tlimit,tlimit_north
           ;; END
           ;; (N_ELEMENTS(tlimit_south) gt 0): BEGIN
           ;;    tlimit,tlimit_south
           ;; END
           KEYWORD_SET(plot_north): BEGIN
              tLims = tlimit_north
           END
           KEYWORD_SET(plot_south): BEGIN
              tLims = tlimit_south
           END
           ELSE: BEGIN
              tLims = [t1Zoom,t2Zoom]
           END
        ENDCASE

        LOADCT2,40
        TPLOT,/LASTVAR,VAR=['ALT','ILAT','MLT'],TRANGE=tLims
        ;; TPLOT_PANEL,VARIABLE=langVar,OPLOTVAR='ESACur'
        TPLOT_PANEL,sc_pot.x,(-1.)*sc_pot.y,VARIABLE='Iesa_Energy'                 ;,OPLOTVAR='SC_POT'
        TPLOT_PANEL,jtemp.x,MAKE_ARRAY(N_ELEMENTS(jtemp.x),VALUE=0),VARIABLE='jtemp' ;,OPLOTVAR='SC_POT'

        IF KEYWORD_SET(add_timebar) THEN BEGIN
           TIMEBAR,timesBar,COLOR=!D.N_COLORS-4
        ENDIF


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

        RETURN

     ENDIF

  ENDIF

  !Y.STYLE = (!Y.STYLE) OR 16

  IF N_ELEMENTS(use_db_fac) EQ 0 AND N_ELEMENTS(use_db_fac_v) EQ 0 THEN use_db_fac  = 1

  ;; Step 0 - safety measure - delete all tplot quantities if found

  @tplot_com

  nn = N_ELEMENTS(data_quants)

  IF (nn GT 1) THEN FOR n = nn-1L,1L,-1L DO STORE_DATA,data_quants[n].name,/DELETE

  field  = GET_FA_FIELDS('MagDC',t1Zoom-10,t2Zoom+10,/STORE)
  magAC = GET_FA_FIELDS('Mag3ac_S',t1Zoom-10,t2Zoom+10,/STORE)

  magVar = 'MAGDATA'

  GET_FA_ORBIT,t1Zoom-10,t2Zoom+10,/DEFINITIVE
  GET_DATA,'ORBIT',DATA=orbit
  orbit = orbit.y[0]

  GET_DATA,'MagDCcomp3',DATA=magz
  GET_DATA,'Mag3ac_S',DATA=magz_AC

  ;;Get model field for subtraction
  GET_FA_ORBIT,magz_AC.x,/DEFINITIVE,/ALL,/TIME_ARRAY
  GET_DATA,'B_model',DATA=bMod

  ;;Which? Those.
  ;; this  = plot(bmod.x-bmod.x[0],bmod.y[*,0]-bmod.y[0,0])
  ;; that  = plot(bmod.x-bmod.x[0],bmod.y[*,1]-bMod.y[0,1],/OVERPLOT,COLOR='RED')
  ;; those = plot(bmod.x-bmod.x[0],bmod.y[*,2]-bMod.y[0,2],/OVERPLOT,COLOR='GREEN')
  min  = MIN(bmod.x-t1Zoom,ind)
  bOff = bmod.y[*,2]-bMod.y[ind,2]

  FA_FIELDS_COMBINE,{time:magz_AC.x,comp1:magz_AC.y,ncomp:1}, $
                    {time:magz.x,comp1:magz.y}, $
                    RESULT=magzInterp, $
                    /SPLINE

  magz = {x:magz_AC.x, $
          y:magzInterp}

  bro = WHERE(FINITE(magz.y))
  firstB = magz.y[bro[0]]

  ;; bogus = (magz_AC.x-magz_AC.x[0])/(magz_AC.x[-1]-magz_AC.x[0])*(-900)

  magVarData = {x:[[magz_AC.x],[magz_AC.x]], $
                y:[[magz.y-firstB+bOff],[magz.y+magz_AC.y-firstB+bOff]]}
                ;; y:[[magz.y-firstB+bogus],[magz.y+magz_AC.y-firstB+bogus]]}

  STORE_DATA,magVar,DATA=magVarData
  OPTIONS,magVar,'yrange',[-1200,500]
  OPTIONS,magVar,'ytitle','B!Dy!N!C!C(nT))'
  OPTIONS,magVar,'panel_size',2
  OPTIONS,magVar,'colors',[6,4]
  OPTIONS,magVar,'labels',['FG','FG+SC']

  t1 = magz.x[0]
  t2 = magz.x[N_ELEMENTS(magz.x)-1L]
  tlimit_all = [t1,t2]

  ;; tPlt_vars = magVar

  ;; IF KEYWORD_SET(screen_plot) THEN BEGIN
  ;;    TPLOT,tPlt_vars,TRANGE=tlimit_all
  ;; ENDIF


; Step 1 - DC Mag data


; step 2 - E field

; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
  sdt_idx = get_sdt_run_idx()

  prog = getenv('FASTBIN') + '/showDQIs'
  IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
     IF (sdt_idx GE 10) THEN BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I2)')
     ENDIF ELSE BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I1)')
     ENDELSE
     SPAWN, [prog, sidstr], result, /NOSHELL
  ENDIF ELSE BEGIN
     SPAWN, prog, result, /NOSHELL
  ENDELSE


  b = WHERE(STRPOS(result,'V1-V4_S') GE 0,nb4)
  IF (nb4 GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nb4 = 0
  b = WHERE(STRPOS(result,'V1-V2_S') GE 0,nb2)
  IF (nb2 GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nb2 = 0
  IF (nb4 GT 0) THEN v12 = GET_FA_FIELDS('V1-V4_S',/all) $
  ELSE IF (nb2 GT 0) THEN v12 = GET_FA_FIELDS('V1-V2_S',/all)

  b = WHERE(STRPOS(result,'V5-V8_S') GE 0,nb5)
  IF (nb5 GT 0) THEN v58 = GET_FA_FIELDS('V5-V8_S',/all)

  got_efield = (nb4 GT 0 or nb2 GT 0) and nb5 GT 0

  IF (got_efield) THEN BEGIN

; despin e field data

     FA_FIELDS_DESPIN,v58,v12 ;,/shadow_notch,/sinterp

     FA_FIELDS_DESPIN_16K
     ;; OPTIONS,'EFIT_ALONG_V','yrange',0
     ;; OPTIONS,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
     ;; OPTIONS,'EFIT_ALONG_V','panel_size',2

; reset time limits IF needed

     ;; get_data,'EFIT_ALONG_V',DATA=data
     ;; t1 = data.x[0]
     ;; t2 = data.x[N_ELEMENTS(data.x)-1L]

     ;; IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
     ;;    IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
     ;;    IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
     ;;    get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
     ;;    get_new_igrf,/no_store_old
     ;; ENDIF

     ;; check for southern hemisphere and fix
     ;; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

     GET_DATA,'B_model',DATA=bm
     GET_DATA,'fa_vel',DATA=vel
     GET_DATA,'fa_pos',DATA=pos
     n        = N_ELEMENTS(REFORM(pos.y[*,0]))
     rxv      = DBLARR(n,3)
     rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
     rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
     rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
     vxb      = DBLARR(n,3)
     vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
     vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
     vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
     tst      = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

     GET_DATA,'EFIT_ALONG_V',DATA=data,DLIMIT=dlimit
     y2     = SPL_INIT(pos.x-tlimit_all[0],tst, $
                       /DOUBLE)
     tst_   = SPL_INTERP(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0], $
                         /DOUBLE)
     data.y = data.y*tst_/ABS(tst_)
     STORE_DATA,'EFIT_ALONG_VSC',DATA=data,DLIMIT=dlimit

     ;;Stuff for EField plot
     GET_DATA,EFieldVar,DATA=dat
     junk   = MIN(ABS(dat.x-t1Zoom),minEInd)
     junk   = MIN(ABS(dat.x-t2Zoom),maxEInd)
     EInds  = [minEInd:maxEInd]
     ;; OPTIONS,EFieldVar,'yrange',[-750,250]
     YLIM,EFieldVar,MIN(dat.y[EInds]),MAX(dat.y[EInds]),0;MINMAX((TEMPORARY(dat.y))[EInds])
     OPTIONS,EFieldVar,'ytitle','E along V!Dsc!N!C!C(mV/m)'
     OPTIONS,EFieldVar,'panel_size',2
     ;; OPTIONS,EFieldVar,'yticks',4
     ;; OPTIONS,EFieldVar,'ytickv',[-750,-500,-250,0,250]

     ;; store_data,'E_NEAR_B',/delete
     ;; store_data,'E_ALONG_V',/delete
     ;; store_data,'EFIT_NEAR_B',/delete
     ;; store_data,'EFIT_ALONG_V',/delete

     t = 0.
     dat=get_fa_fields('V5-V8_S',t,/start)
     IF dat.valid EQ 0 THEN BEGIN
        print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
        data_valid=0.0
     ENDIF ELSE BEGIN

        efieldV58=get_fa_fields('V5-V8_S',t1Zoom,t2Zoom)
        efieldV1214=get_fa_fields('V1-V2_S',t1Zoom,t2Zoom)
        IF efieldV1214.valid EQ 0 THEN BEGIN
           print,'No V1-V2_S data - trying V1-V4_S'
           efieldV1214=get_fa_fields('V1-V4_S',t1Zoom,t2Zoom)
           IF efieldV1214.valid EQ 0 AND KEYWORD_SET(burst) THEN BEGIN
              print,'No V1-V4_S data - trying V1-V2_4k (burst)'
              efieldV1214=get_fa_fields('V1-V2_4k',t1Zoom,t2Zoom)
              IF efieldV1214.valid EQ 0 THEN BEGIN
                 print,'No V1-V2_4k data - trying V1-V4_4k (burst)'
                 efieldV1214=get_fa_fields('V1-V4_4k',t1Zoom,t2Zoom)
                 IF efieldV1214.valid EQ 0 THEN BEGIN
                    print,'No FAST fields data-get_fa_fields returned invalid data'
                    data_valid=0.0
                 ENDIF
              ENDIF
           ENDIF ELSE BEGIN
              print,'No FAST fields data-get_fa_fields returned invalid data'
              data_valid=0.0
           ENDELSE
        ENDIF
     ENDELSE

     FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk

     efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}

     ;; STORE_DATA,EFieldVar,DATA=efield

     ;; magz={time:magz.x,comp1:magz.y,ncomp:1}
     ;; efield={time:efield.x,comp1:efield.y}

     ;; FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
     ;; fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

     ;; Langmuir_2=get_fa_fields('NE2_S',t1Zoom,t2Zoom)
     ;; Langmuir_6=get_fa_fields('NE6_S',t1Zoom,t2Zoom)
     ;; Langmuir_9=get_fa_fields('NE9_S',t1Zoom,t2Zoom)
     ;; Langmuir_data=[0]
     ;; Langmuir_time=[0]
     ;; Langmuir_prob=[0]
     ;; IF Langmuir_2.valid NE 0 THEN BEGIN
     ;;    langmuir_data=[Langmuir_data,Langmuir_2.comp1]
     ;;    langmuir_time=[Langmuir_time,Langmuir_2.time]
     ;;    langmuir_prob=[Langmuir_prob,replicate(2,N_ELEMENTS(Langmuir_2.time))]
     ;; ENDIF
     ;; IF Langmuir_6.valid NE 0 THEN BEGIN
     ;;    langmuir_data=[Langmuir_data,Langmuir_6.comp1]
     ;;    langmuir_time=[Langmuir_time,Langmuir_6.time]
     ;;    langmuir_prob=[Langmuir_prob,replicate(6,N_ELEMENTS(Langmuir_6.time))]
     ;; ENDIF
     ;; IF Langmuir_9.valid NE 0 THEN BEGIN
     ;;    langmuir_data=[Langmuir_data,Langmuir_9.comp1]
     ;;    langmuir_time=[Langmuir_time,Langmuir_9.time]
     ;;    langmuir_prob=[Langmuir_prob,replicate(9,N_ELEMENTS(Langmuir_9.time))]
     ;; ENDIF
     ;; IF N_ELEMENTS(langmuir_data) GT 1 THEN BEGIN
     ;;    langmuir_time=langmuir_time(1:N_ELEMENTS(Langmuir_time)-1)
     ;;    langmuir_data=langmuir_data(1:N_ELEMENTS(Langmuir_time)-1)
     ;;    langmuir_prob=langmuir_prob(1:N_ELEMENTS(Langmuir_time)-1)
     ;;    time_order_langmuir=sort(langmuir_time)
     ;;    langmuir={x:langmuir_time(time_order_langmuir),y:langmuir_data(time_order_langmuir)}
     ;;    dens_probe={x:langmuir_time(time_order_langmuir),y:langmuir_prob(time_order_langmuir)}
     ;; ENDIF ELSE data_valid=0.0

     ;; langVar = 'LANGMUIR'
     ;; STORE_DATA,langVar,DATA=langmuir
     ;; YLIM,langVar,2e1,2e4,1
     ;; OPTIONS,langVar,'ytitle','Probe Current!C(nA)'
     ;; OPTIONS,langVar,'panel_size',3

     ;; tPlt_vars = [langVar,tPlt_vars]

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[EFieldVar] ELSE tPlt_vars=[EFieldVar,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF ELSE IF (N_ELEMENTS(tPlt_vars) ne 0) THEN BEGIN

     tPlt_vars = 'dB_fac_v'

     IF (KEYWORD_SET(use_db_fac)) THEN tPlt_vars = 'dB_fac'
     IF ~KEYWORD_SET(no_blank_panels) THEN tPlt_vars = 'dB_fac_v'

  ENDIF


  ;; GET_DATA,EFieldSpecVar,DATA=eAlongV16K
  ;; GET_DATA,'E_NEAR_B_16k',DATA=eNearB16K

  ;; yVar = eAlongV16K.y
  ;; yVar = SQRT(eAlongV16K.y^2 + eNearB16K.y^2)


  ;; eAlongV16KTmp   = {TIME         :  eAlongV16K.x , $
  ;;                    COMP1        :  yVar         , $
  ;;                    NCOMP        : 1             , $
  ;;                    VALID        : 1             , $
  ;;                    DATA_NAME    :'E Along V'    , $
  ;;                    PROJECT_NAME : 'FAST'        , $
  ;;                    UNITS_NAME   : 'mV/m'        , $
  ;;                    CALIBRATED   : 1}

  ;; n_ave = 2
  ;; nPts  = 1024
  ;; slide = 1.0
  ;; ESpecVar = 'EAVSpec'
  ;; ESpecThresh = 1e-6          ;in (mV/m)^2/Hz

  ;; spec = FA_FIELDS_SPEC(eAlongV16KTmp, $
  ;;                       /STORE, $
  ;;                       T_NAME=ESpecVar, $
  ;;                       STRUCTURE=eAVSpec, $
  ;;                       NPTS=nPts, $
  ;;                       N_AVE=n_ave, $
  ;;                       SLIDE=slide)

  ;; eAVSpecLims      = [1.e-5,1.e4]
  ;; ZLIM,ESpecVar,eAVSpecLims[0],eAVSpecLims[1],1 ; set z limits
  ;; YLIM,ESpecVar,1.e-2,1.e1,1
  ;; OPTIONS,ESpecVar,'ytitle','Frequency!C(kHz)'
  ;; OPTIONS,ESpecVar,'ztitle','Log ' + eAVSpec.units_name ; z title
  ;; OPTIONS,ESpecVar,'panel_size',2.0

  ;; GET_DATA,ESpecVar,DATA=tmp
  ;; tmp.y[WHERE(~FINITE(tmp.y) OR (tmp.y LT ESpecThresh) )] = 0.0
  ;; STORE_DATA,ESpecVar,DATA=tmp

     ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[ESpecVar] ELSE tPlt_vars=[ESpecVar,tPlt_vars]

  IF KEYWORD_SET(skip_despin) THEN BEGIN
     field    = GET_FA_FIELDS('MagDC',t1Zoom,t2Zoom,/STORE)

     GET_DATA,'MagDCcomp1',data=magz
     GET_DATA,'MagDCcomp2',data=magx
     GET_DATA,'MagDCcomp3',data=magy ;Need magy to be cross-track

     mintime  = MIN(ABS(t1-magx.x),ind1)
     mintime  = MIN(ABS(t2-magx.x),ind2)

     magx     = {x : magx.x[ind1:ind2],y : magx.y[ind1:ind2]}
     magy     = {x : magy.x[ind1:ind2],y : magy.y[ind1:ind2]}
     magz     = {x : magz.x[ind1:ind2],y : magz.y[ind1:ind2]}

     ;;This is such that the spin axis–aligned component (magz) shows up as index 1, as in db_fac.y[*,1] == magz.y.
     db_fac   = {x : magx.x, $
                 y : [[magx.y],[magz.y],[magy.y]]}
     STORE_DATA,'dB_fac_v',DATA=db_fac
     STORE_DATA,'dB_fac',DATA=db_fac

     is_despun = 0B

  ENDIF ELSE BEGIN

     UCLA_MAG_DESPIN

     ;;   From UCLA_MAG_DESPIN:
     ;;   "Field-aligned velocity-based coordinates defined as:    "
     ;;   "z (ind 2)-along B, y (ind 1)-cross track (BxV), x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)

     ;;OR (if not using dB_fac_v):
     ;;z (or 2)-along B, y (or 1)-east (BxR), x (or 0)-nominally out

     GET_DATA,'dB_fac_v',DATA=db_fac
     mintime  = MIN(ABS(t1-db_fac.x),ind1)
     mintime  = MIN(ABS(t2-db_fac.x),ind2)

     magx     = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
     magy     = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}
     magz     = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}

     is_despun = 1B

  ENDELSE

  ;;Get orbit stuff
  GET_FA_ORBIT,magy.x,/TIME_ARRAY ;,/all

  ;;Define loss cone angle
  GET_DATA,'ALT',DATA=alt
  loss_cone_aLT           = alt.y[0]*1000.0
  lcw                     = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
  GET_DATA,'ILAT',DATA=ilat
  north_south             = ABS(ilat.y[0])/ilat.y[0]

  IF north_south EQ -1 THEN BEGIN
     e_angle              = [180.-lcw,180+lcw] ; for Southern Hemis.

     ;;Eliminate ram from data
     i_angle              = [180.0,360.0]
     i_angle_up           = [270.0,360.0]

  ENDIF ELSE BEGIN
     e_angle              = [360.-lcw,lcw] ;	for Northern Hemis.

     ;;Eliminate ram from data
     i_angle              = [0.0,180.0]
     i_angle_up           = [90.0,180.0]

  ENDELSE

  IF ~KEYWORD_SET(ion_angle) THEN BEGIN
     ion_angle            = i_angle
  ENDIF

  ;;Get speed and position for calculation of mag stuff
  GET_DATA,'fa_vel',DATA=vel
  speed                   = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  old_pos                 = 0.
  position                = MAKE_ARRAY(N_ELEMENTS(magy.x),/DOUBLE)
  speed_mag_point         = MAKE_ARRAY(N_ELEMENTS(magy.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(magy.x)-2 DO BEGIN
     speed_point_ind      = MIN(ABS(vel.x-magy.x[j]),ind)

     speed_mag_point[j]   = speed[ind]
     samplingperiod       = magy.x[j+1] - magy.x[j]

     position[j]          = old_pos + speed_mag_point[j]*samplingperiod
     old_pos              = position[j]
  ENDFOR

  ;;Pick up inds from mag
  junk                    = MIN(ABS(magy.x-t1Zoom),minMagInd)
  junk                    = MIN(ABS(magy.x-t2Zoom),maxMagInd)
  magInds                 = [minMagInd:maxMagInd]
  ;;Calculate the current from mag
  deltaBY                 = DERIV(position,SMOOTH(magy.y,5))
  ;; deltaBY                 = DERIV(position,magy.y)
  ;; deltaBY                 = DERIV(position,SMOOTH(magy.y,5))
  ;; jtemp                = ABS(1.0e-3*(deltaBx)/1.26e-6)
  ;; jtemp                = 1.0e-3*(deltaBx)/1.26e-6
  ;;in number flux units
  jtemp                   = 1.0e-3*(deltaBY)/1.26e-6
  muLetter = '!4l!X'
  ;; IF KEYWORD_SET(show_currents_not_fluxes) THEN BEGIN
  ;; YLIM,'jtemp',-50,100
  YLIM,'jtemp',MIN(jtemp[magInds]),MAX(jtemp[magInds])
  ;; OPTIONS,'jtemp','yticks',4                              ; set y-axis labels
  ;; OPTIONS,'jtemp','ytickname',['-2e1','0','2e1','4e1']    ; set y-axis labels
  ;; OPTIONS,'jtemp','ytickv',[-2e1,0,2e1,4e1]               ; set y-axis labels
  OPTIONS,'jtemp','ytitle','Mag current!C(' + muLetter + 'A/m!U2!N)'
  OPTIONS,'jtemp','panel_size',2
  ;; ENDIF ELSE BEGIN
  ;;    jtemp               *= (DOUBLE(1. / 1.6e-9))
  ;;    YLIM,'jtemp',-1.e10,2.e10
  ;;    OPTIONS,'jtemp','yticks',4                           ; set y-axis labels
  ;;    OPTIONS,'jtemp','ytickname',['-1e10','0','1e10','2e10'] ; set y-axis labels
  ;;    OPTIONS,'jtemp','ytickv',[-1e10,0,1e10,2e10]            ; set y-axis labels
  ;;    OPTIONS,'jtemp','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N)'
  ;; ENDELSE
  sign_jtemp              = ABS(deltaBY)/deltaBY
  STORE_DATA,'jtemp',DATA={x:magy.x,y:jtemp}
  ;; OPTIONS,'jtemp','psym','10'
  ;; OPTIONS,'jtemp','fill',1
  ;; OPTIONS,'jtemp','tplot_routine','polyfill_tplot'
  ;; ;; OPTIONS,'jtemp','color','808080'x
  ;; OPTIONS,'jtemp','fill_color',250
  STORE_DATA,'dB_East',DATA={x:magy.x,y:magy.y-magy.y[minMagInd]+100}
  OPTIONS,'dB_East','ytitle',CGGREEK('Delta') + 'B!DEast!N (nT)' ; y title
  YLIM,'dB_East',MIN(magy.y[magInds]-magy.y[minMagInd]+100),MAX(magy.y[magInds]-magy.y[minMagInd]+100)
  OPTIONS,'dB_East','yticks',2                                   ; set y-axis labels
  OPTIONS,'dB_East','ytickname',['200','400']          ; set y-axis labels
  OPTIONS,'dB_East','ytickv',[200,400]                    ; set y-axis labels
  OPTIONS,'dB_East','panel_size',2


; Step 3 - Iesa data

  prog = getenv('FASTBIN') + '/showDQIs'
  IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
     IF (sdt_idx GE 10) THEN BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I2)')
     ENDIF ELSE BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I1)')
     ENDELSE
     SPAWN, [prog, sidstr], result, /NOSHELL
  ENDIF ELSE BEGIN
     SPAWN, prog, result, /NOSHELL
  ENDELSE
  b = where (STRPOS(result,'Iesa Survey') GE 0,nesa)
  IF (nesa GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nesa = 0

  IF (nesa GT 0) THEN BEGIN

; ION ENERGY

     var_name = 'Iesa_Energy'
     GET_EN_SPEC,T1=t1Zoom,T2=t2Zoom, $
                 'fa_'+ieb_or_ies+'_c',NAME=var_name,UNITS='eflux',ANGLE=ion_angle
     ;; data.y = alog10(data.y)
     ;; store_data,var_name, data=data
     OPTIONS,var_name,'spec',1
     ZLIM,var_name,1e5,1e8,0
     YLIM,var_name,4,30000,1
     OPTIONS,var_name,'ytitle','Ions!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','eV!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF


  ;; ION PITCH ANGLE
  var_NAME='Iesa_Angle'
  GET_PA_SPEC,"fa_"+ieb_or_ies+"_c",UNITS='eflux',NAME=var_name,ENERGY=energy_ions
  GET_DATA,var_name,DATA=data
  ;; this = WHERE(data.v[0,*] GT 170)
  ;; data = {x:data.x, $
  ;;         y:data.y[*,this], $
  ;;         v:data.v[*,this]}


  ;; STORE_DATA,var_name,DATA=data
  OPTIONS,var_name,'spec',1
  ZLIM,var_name,1e5,5e7,0
  YLIM,var_name,ion_angle[0],ion_angle[1],0
  OPTIONS,var_name,'ytitle','Ions!C!CAngLE (Deg.)'
  OPTIONS,var_name,'ztitle','eV!C/cm!U2!N-s-sr-eV'
  OPTIONS,var_name,'x_no_interp',1
  OPTIONS,var_name,'y_no_interp',1
  OPTIONS,var_name,'panel_size',2

  ;;For shifting things 90°
  ;; GET_DATA,var_name,DATA=data
  ;; bb = WHERE(data.v GT 270.,nb)
  ;; IF (nb GT 0) THEN data.v[bb] = data.v[bb]-360.
  ;; nn = N_ELEMENTS(data.x)
  ;; FOR n=0,nn-1L do BEGIN
  ;;    bs          = SORT(data.v[n,*])
  ;;    data.v[n,*] = data.v[n,bs]
  ;;    data.y[n,*] = data.y[n,bs]
  ;; ENDFOR
  ;; store_data,var_name, data=data
  ;; OPTIONS,var_name,'yminor',9
  ;; OPTIONS,var_name,'yticks',1
  ;; OPTIONS,var_name,'ytickv',[180,270,360]
  OPTIONS,var_name,'ynozero',1
  OPTIONS,var_name,'ystyle',16
  ;; YLIM,var_name,-90,270,0

  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

  ;; reset time limits if needed
  t1 = data.x[0]
  t2 = data.x[N_ELEMENTS(data.x)-1L]

  IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
     IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
     IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
     GET_FA_ORBIT,tlimit_all[0],tlimit_all[1], $
                  /ALL,STATUS=no_model, $
                  DELTA=1., $
                  /DEFINITIVE, $
                  /DRAG_PROP
     GET_NEW_IGRF,/NO_STORE_OLD
  ENDIF

  IF (KEYWORD_SET(screen_plot)) THEN BEGIN
     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
  ENDIF

  ;;Get potential
  sc_pot  = GET_FA_POTENTIAL(t1Zoom,t2Zoom, $
                                 ;; /SPIN, $
                                 /REPAIR)

  sc_pot  = {x:sc_pot.time, $
             y:(-1.)*sc_pot.comp1, $ ;;Reverse sign of pot for use with GET_2DT_TS_POT
             valid:sc_pot.valid}

  STORE_DATA,sc_potName,DATA={x:sc_pot.x,y:(-1.)*sc_pot.y}


  GET_2DT_TS_POT,'j_2d_fs','fa_'+ieb_or_ies,NAME='Ji_up',T1=t1,T2=t2, $
                 ENERGY=energy_ions,ANGLE=ion_angle, $
                 sc_pot=sc_pot
  ;; YLIM,'Ji_up',1.e5,1.e8,1   ; set y limits
  ;; OPTIONS,'Ji_up','tplot_routine','pmplot'   ; set 2 color plot
  ;; OPTIONS,'Ji_up','labels',['Downgoing!C Ions','Upgoing!C Ions ']    ; set color label
  ;; OPTIONS,'Ji_up','labflag',3        ; set color label
  ;; OPTIONS,'Ji_up','labpos',[2.e7,1.e6]       ; set color label
  GET_DATA,'Ji_up',DATA=tmp
  ;; tmp.y = SMOOTH((-1.)*tmp.y,5)
  ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
  ;; STORE_DATA,'Ji_up',DATA={x:tS_1s,y:doDat}
  YLIM,'Ji_up',1.e7,1.e10,1                             ; set y limits
  OPTIONS,'Ji_up','ytitle','Ion Flux!C#/(cm!U2!N-s)'    ; set y title
  OPTIONS,'Ji_up','panel_size',2                        ; set panel size

; Step 4 - Eesa data

  prog = getenv('FASTBIN') + '/showDQIs'
  IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
     IF (sdt_idx GE 10) THEN BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I2)')
     ENDIF ELSE BEGIN
        sidstr = STRING(sdt_idx, FORMAT='(I1)')
     ENDELSE
     SPAWN, [prog, sidstr], result, /NOSHELL
  ENDIF ELSE BEGIN
     SPAWN, prog, result, /NOSHELL
  ENDELSE
  b = where (STRPOS(result,'Eesa Survey') GE 0,nesa)
  IF (nesa GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN nesa = 0

  IF (nesa GT 0) THEN BEGIN

; ELECTRON ENERGY

     var_NAME='Eesa_Energy'
     GET_EN_SPEC,T1=t1Zoom,T2=t2Zoom, $
                 'fa_'+eeb_or_ees+'_c',NAME=var_name,UNITS='eflux',RETRACE=1
     GET_DATA,var_name, data=data
     data.y = alog10(data.y)
     store_data,var_name, data=data
     OPTIONS,var_name,'spec',1
     ZLIM,var_name,6,10,0
     YLIM,var_name,5,30000,1
     OPTIONS,var_name,'ytitle','Electrons!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle','Log eV!C/cm!U2!N-s-sr-eV'
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF


; ELECTRON PITCH ANGLE

  var_NAME='Eesa_Angle'
  get_pa_spec,"fa_"+eeb_or_ees+"_c",UNITS='eflux',NAME=var_name,ENERGY=energy_electrons
  ;; GET_DATA,var_name, data=data
  ;; data.y = alog10(data.y)
  ;; store_data,var_name, data=data
  OPTIONS,var_name,'spec',1
  ZLIM,var_name,1e6,5e9,0
  YLIM,var_name,0,360,0
  ;; OPTIONS,var_name,'ytitle','Electrons > 10 eV!C!CAngLE (Deg.)'
  OPTIONS,var_name,'ytitle','Electrons!C!CAngLE (Deg.)'
  OPTIONS,var_name,'ztitle','eV!C/cm!U2!N-s-sr-eV'
  OPTIONS,var_name,'x_no_interp',1
  OPTIONS,var_name,'y_no_interp',1
  OPTIONS,var_name,'panel_size',2

  GET_DATA,var_name, data=data
  bb = where (data.v GT 270.,nb)
  IF (nb GT 0) THEN data.v[bb]=data.v[bb]-360.
  nn = N_ELEMENTS(data.x)
  for n = 0,nn-1L do BEGIN & $
     bs = sort (data.v[n,*]) & $
     data.v[n,*]=data.v[n,bs] & $
     data.y[n,*]=data.y[n,bs] & $
     endfor
     store_data,var_name, data=data
     OPTIONS,var_name,'yminor',9
     OPTIONS,var_name,'yticks',4
     OPTIONS,var_name,'ytickv',[-90,0,90,180,270]
     YLIM,var_name,-90,270,0

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

; reset time limits IF needed

     t1 = data.x[0]
     t2 = data.x[N_ELEMENTS(data.x)-1L]

     IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
        IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
        IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
        get_fa_orbit,tlimit_all[0],tlimit_all[1],/all,status=no_model,delta=1.,/definitive,/drag_prop
        get_new_igrf,/no_store_old
     ENDIF

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

     sphRadius = 4e-2 ;in meters, from Ergun et al. [2001]
     sphCrossSec = !PI*sphRadius^2

     ;;Get EESA current
     GET_2DT_TS_POT,'j_2d_b','fa_eeb',T1=t1Zoom,T2=t2Zoom, $
                    NAME='Je_tot', $
                    ENERGY=energy_electrons, $
                    SC_POT=sc_pot

     GET_DATA,'Je_tot',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     ;;NOTE: we here decide to make currents field-aligned.
     ;;That is, positive currents are along B; in SH, that means spaceward
     jeTotTmp_time  = tmp.x
     jeTotTmp       = tmp.y*1.6e-9*(-1.) ;;in microA/cm2, and flip sign


     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IeTotTmp_time  = tmp.x[keep2]
     IeTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IeTotTmp      *= sphCrossSec

     Je_z           = {x:jeTotTmp_time,y:jeTotTmp}
     IeTot_z        = {x:IeTotTmp_time,y:IeTotTmp}
     STORE_DATA,'ESACur',DATA=IeTot_z
     OPTIONS,'ESACur','colors',250

     STORE_DATA,'Je_tot',DATA=Je_z

  IF KEYWORD_SET(save_B_and_J_data) OR KEYWORD_SET(ancillary_plots) THEN BEGIN
     ;; UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

     PRINT,"Getting Ji current density fo' yeh'"
     GET_2DT_TS_POT,'j_2d_b','fa_ieb',T1=t1Zoom,T2=t2Zoom, $
                    NAME='Ji_tot', $
                    ENERGY=[0,energy_ions[1]], $
                    SC_POT=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ies',T1=t1Zoom,T2=t2Zoom, $
                    NAME='Ji_tot_S', $
                    ENERGY=[0,energy_ions[1]], $
                    ANGLE=[180,360], $
                    SC_POT=sc_pot
     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=t1Zoom,T2=t2Zoom, $
                    NAME='Je_tot_S', $
                    ENERGY=energy_electrons, $
                    SC_POT=sc_pot

     ;;First, burst ion data
     GET_DATA,'Ji_tot',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jiTotTmp_time  = tmp.x
     jiTotTmp       = tmp.y*1.6e-9*2. ;;in microA/m2, times 2 since half angle range

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IiTotTmp_time  = tmp.x[keep2]
     IiTotTmp       = tmp.y[keep2]*1.6e-6*2. ;;in nanoA/m2
     IiTotTmp      *= sphCrossSec

     Ji_z           = {x:jiTotTmp_time,y:jiTotTmp}
     IiTot_z        = {x:IiTotTmp_time,y:IiTotTmp}

     STORE_DATA,'Ji_tot',DATA=Ji_z

     ;;Now survey ESA ion data for patching the burst holes
     GET_DATA,'Ji_tot_S',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jiTotTmp_time  = tmp.x
     jiTotTmp       = tmp.y*1.6e-9*2. ;;in microA/m2, times 2 since half angle range

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IiTotTmp_time  = tmp.x[keep2]
     IiTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IiTotTmp      *= sphCrossSec

     Ji_z_S         = {x:jiTotTmp_time,y:jiTotTmp}
     IiTot_z_S      = {x:IiTotTmp_time,y:IiTotTmp}

     ;;Now electron ESA survey
     GET_DATA,'Je_tot_S',DATA=tmp
     keep1          = WHERE(FINITE(tmp.y))
     tmp.x          = tmp.x[keep1]
     tmp.y          = tmp.y[keep1]

     ;;For output
     jeTotTmp_time  = tmp.x
     jeTotTmp       = tmp.y*1.6e-9 ;;in microA/m2

     ;;For nice plots
     tmp.y         *= -1. ;;Since we're in Southern Hemi
     keep2          = WHERE(tmp.y GT 0.0)
     IeTotTmp_time  = tmp.x[keep2]
     IeTotTmp       = tmp.y[keep2]*1.6e-6 ;;in nanoA/m2
     IeTotTmp      *= sphCrossSec

     Je_z_S         = {x:jeTotTmp_time,y:jeTotTmp}
     IeTot_z        = {x:IeTotTmp_time,y:IeTotTmp}


  ENDIF

  IF KEYWORD_SET(save_B_AND_J_data) THEN BEGIN
     IF ~KEYWORD_SET(saveDir) THEN BEGIN
        saveDir    = '/SPENCEdata/Research/Satellites/FAST/single_sc_wavevector/saves_output_etc/'
     ENDIF
     ;; B_J_fiLE = 'Chaston_et_al_2006-B_and_J.dat'

     GET_DATA,'dB_fac_v',DATA=dB_fac_v
     GET_DATA,'dB_fac',DATA=dB_fac

     PRINT,'Saving ' + saveFile + ' ...'
     SAVE,Je_z,Ji_z, $
          Je_z_S,Ji_z_S, $
          dB_fac_v,dB_fac, $
          orbit, $
          is_despun, $
          timeBar_times,t1Zoom,t2Zoom, $
          FILENAME=saveDir+saveFile

  ENDIF


; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south also chanGE plot title

  GET_DATA,'LAT',DATA=data

  IF (N_ELEMENTS(data.y) LE 0) THEN return

  bb = where (data.y GT 10,nn)
  IF (nn GT 0) THEN tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

  bb = where (data.y LT -10,nn)
  IF (nn GT 0) THEN tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

  hemisph = getenv('FAST_ORBIT_HEMISPHERE')

  GET_DATA,'ORBIT',DATA=data
  nn = N_ELEMENTS(data.y)/2
  orbit = data.y(nn)
  orbit_lab = strcompress(STRING(orbit,format="(i5.4)"),/remove_all)
  tplot_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

; force tPlt_vars to be all the panels unless no_blank_panels is set

  IF ~KEYWORD_SET(no_blank_panels) THEN BEGIN


; Eesa_Energy

     bdat = where(tPlt_vars EQ 'Eesa_Energy',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [34119.7,26091.5,50.9600,5.88000]
        store_data,'Eesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Eesa_Energy','spec',1
        ZLIM,'Eesa_Energy',4,9,0
        YLIM,'Eesa_Energy',5,30000,1
        OPTIONS,'Eesa_Energy','ytitle','Electrons!C!CEnergy (eV)'
        OPTIONS,'Eesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Eesa_Energy','x_no_interp',1
        OPTIONS,'Eesa_Energy','y_no_interp',1
        OPTIONS,'Eesa_Energy','panel_size',2
     ENDIF

; Eesa_Angle

     bdat = where(tPlt_vars EQ 'Eesa_Angle',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        store_data,'Eesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Eesa_Angle','spec',1
        ZLIM,'Eesa_Angle',4,9,0
        YLIM,'Eesa_Angle',-90,270,0
        OPTIONS,'Eesa_Angle','ytitle','Electrons > 10 eV!C!CAngLE (Deg.)'
        OPTIONS,'Eesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Eesa_Angle','x_no_interp',1
        OPTIONS,'Eesa_Angle','y_no_interp',1
        OPTIONS,'Eesa_Angle','panel_size',2
        OPTIONS,'Eesa_Angle','yminor',9
        OPTIONS,'Eesa_Angle','yticks',4
        OPTIONS,'Eesa_Angle','ytickv',[-90,0,90,180,270]
     ENDIF

; Iesa_Energy

     bdat = where(tPlt_vars EQ 'Iesa_Energy',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [26808.3,11827.2,27.7200,4.62000]
        store_data,'Iesa_Energy', data={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Iesa_Energy','spec',1
        ZLIM,'Iesa_Energy',4,9,0
        YLIM,'Iesa_Energy',4,30000,1
        OPTIONS,'Iesa_Energy','ytitle','Ions!C!CEnergy (eV)'
        OPTIONS,'Iesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Iesa_Energy','x_no_interp',1
        OPTIONS,'Iesa_Energy','y_no_interp',1
        OPTIONS,'Iesa_Energy','panel_size',2
     ENDIF

; Iesa_Angle

     bdat = where(tPlt_vars EQ 'Iesa_Angle',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        store_data,'Iesa_Angle', data={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Iesa_Angle','spec',1
        ZLIM,'Iesa_Angle',4,9,0
        YLIM,'Iesa_Angle',-90,270,0
        OPTIONS,'Iesa_Angle','ytitle','Ions!C!CAngLE (Deg.)'
        OPTIONS,'Iesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Iesa_Angle','x_no_interp',1
        OPTIONS,'Iesa_Angle','y_no_interp',1
        OPTIONS,'Iesa_Angle','panel_size',2
        OPTIONS,'Iesa_Angle','yminor',9
        OPTIONS,'Iesa_Angle','yticks',4
        OPTIONS,'Iesa_Angle','ytickv',[-90,0,90,180,270]
     ENDIF

     ;; tPlt_vars=['Iesa_Energy','Iesa_Angle','Ji_up','Eesa_Energy','Eesa_Angle', $
     ;;             EFieldVar,ESpecVar,magVar,langVar]
     ;; tPlt_vars=['Iesa_Energy','Iesa_Angle','Ji_up','Eesa_Energy','Eesa_Angle', $
     ;;             EFieldVar,magVar,langVar]
     tPlt_vars=['Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','Je_plot', $
                 'jtemp','dB_East',EFieldVar]
  ENDIF

  IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN

     IF KEYWORD_SET(save_png) THEN BEGIN
        CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(save_ps) THEN BEGIN

           POPEN,plotDir+outPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
           DEVICE,/PALATINO,FONT_SIZE=8


        ENDIF ELSE BEGIN
           WINDOW,0,XSIZE=600,YSIZE=800
        ENDELSE
     ENDELSE

     CASE 1 OF
        ;; (N_ELEMENTS(tlimit_north) GT 0): BEGIN
        ;;    tlimit,tlimit_north
        ;; END
        ;; (N_ELEMENTS(tlimit_south) GT 0): BEGIN
        ;;    tlimit,tlimit_south
        ;; END
        KEYWORD_SET(plot_north): BEGIN
           tLims = tlimit_north
        END
        KEYWORD_SET(plot_south): BEGIN
           tLims = tlimit_south
        END
        ELSE: BEGIN
           tLims = [t1Zoom,t2Zoom]
        END
     ENDCASE

     ;; GET_DATA,'Ji_tot',DATA=tmpi
     GET_DATA,'Je_tot',DATA=tmpe

     jiDat = DATA_CUT('Ji_tot',tmpe.x)

     ;; OPTIONS,'Ji_tot','ytitle','Ion Current!C(!4l!XA m!U2!N)' ; set y title
     ;; OPTIONS,'Ji_tot','panel_size',2                          ; set panel size

     junk    = MIN(ABS(tmpe.x-t1Zoom),minCurInd)
     junk    = MIN(ABS(tmpe.x-t2Zoom),maxCurInd)
     curInds = [minCurInd:maxCurInd]

     OPTIONS,'Je_plot','ytitle','Current density!C(!4l!XA/m!U2!N)' ; set y title
     OPTIONS,'Je_plot','panel_size',2                             ; set panel size
     ;; YLIM,'Je_plot',-50,100,0
     YLIM,'Je_plot',MIN([jiDat[curInds],tmpe.y[curInds]]),MAX([jiDat[curInds],tmpe.y[curInds]]),0

     OPTIONS,'Je_plot','labels',['i!U+!N ESA','e!U-!N ESA']
     OPTIONS,'Je_plot','labflag',-1
     OPTIONS,'Je_plot','colors',[140,250,0]


     STORE_DATA,'Je_plot',DATA={x:[[tmpe.x],[tmpe.x],[tmpe.x]],y:[[jiDat],[tmpe.y],[MAKE_ARRAY(N_ELEMENTS(tmpe.y),VALUE=0.)]]}

     LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=tLims
     ;; TPLOT_PANEL,VARIABLE=langVar,OPLOTVAR='ESACur'
     TPLOT_PANEL,sc_pot.x,(-1.)*sc_pot.y,VARIABLE='Iesa_Energy' ;,OPLOTVAR='SC_POT'
     TPLOT_PANEL,magy.x,MAKE_ARRAY(N_ELEMENTS(magy.x),VALUE=0),VARIABLE='jtemp' ;,OPLOTVAR='SC_POT'

     IF KEYWORD_SET(add_timebar) THEN BEGIN
        TIMEBAR,timesBar,COLOR=!D.N_COLORS-4
     ENDIF


     IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
        PCLOSE
     ENDIF ELSE BEGIN

     ENDELSE

  ENDIF


  RETURN

END
