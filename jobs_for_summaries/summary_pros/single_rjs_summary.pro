PRO SINGLE_RJS_SUMMARY,time1,time2, $
                       TPLT_VARS=tPlt_vars, $
                       EEB_OR_EES=eeb_OR_ees, $
                       ENERGY_ELECTRONS=energy_electrons, $
                       ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
                       TLIMIT_NORTH=tlimit_north, $
                       TLIMIT_SOUTH=tlimit_south, $
                       TLIMIT_ALL=tlimit_all, $
                       SCREEN_PLOT=screen_plot, $
                       USE_FAC_V=use_fac_v, $
                       USE_FAC_NOT_V=use_fac, $
                       NO_BLANK_PANELS=no_blank_panels, $
                       ADD_KAPPA_PANEL=add_kappa_panel, $
                       ADD_CHARE_PANEL=add_chare_panel, $
                       ADD_NEWELL_PANEL=add_Newell_panel, $
                       NEWELL_2009_INTERP=Newell_2009_interp, $
                       ADD_IU_POT=add_iu_pot, $
                       SPECTROGRAM_UNITS=spectrogram_units, $
                       LOG_KAPPAPLOT=log_kappaPlot, $
                       FIT2DKAPPA_INF_LIST=fit2DKappa_inf_list, $
                       FIT2DGAUSS_INF_LIST=fit2DGauss_inf_list, $
                       KAPPAFIT1DS=kappaFit1Ds, $
                       GAUSSFIT1DS=gaussFit1Ds, $
                       CHI2_THRESHOLD=chi2_thresh, $
                       CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                       HIGHDENSITY_THRESHOLD=highDens_thresh, $
                       LOWDENSITY_THRESHOLD=lowDens_thresh, $
                       DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                       N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                       ION_ANGLERANGE=ion_angleRange, $
                       ION_ENERGYRANGE=ion_energyRange, $
                       CURPOTLIST=curPotList, $
                       CAP_STRUCT=cAP_struct, $
                       SAVE_PS=save_ps, $
                       SAVE_PNG=save_png, $
                       EPS=eps, $
                       OUTPLOT_BONUSPREF=bonusPref, $
                       SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                       GRL=GRL, $
                       PLOTDIR=plotDir


; create a summary plot of:
; SFA (AKR)
; DSP (VLF)
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; E fit along V (Southern hemisphere corrected)
; dB_fac_v (dB_fac and dB_SM also stored)

; Returns:
; tPlt_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data

; procedure for making summary plots
; batch_summary,tPlt_vars=tPlt_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all
; loadct2,40  ; load color table
; if (n_elements(tPlt_vars) gt 0) then TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
; IF (N_ELEMENTS(tlimit_north) GT 0) THEN tlimit,tlimit_north  ; northern hemisphere
; IF (N_ELEMENTS(tlimit_south) GT 0) THEN tlimit,tlimit_south  ; southern hemisphere

; IF running interactively
; batch_summary,tPlt_vars=tPlt_vars,/screen_plot,/no_blank_panels

; Input needed on:
; (a) Northern/southern hemisphere limits
; (b) ESA data limits
; (c) DSP calibration


; Under development - R. J. Strangeway 4/4/08

; Program will use fac_v if E field data are available, other use fac_v
; over-ride with use_fac_v and use_fac keywords

; if no_blank_panels is not set procedure will generate tplot data for all the parameters,
; including missing data, for a uniform plot product

; Step 0 - safety measure - delete all tplot quantities if found

@tplot_com

  ctNum = 39
  ctNum = 43                    ;Better; not oceans of green
  IF KEYWORD_SET(screen_plot) AND ~(KEYWORD_SET(save_ps) OR KEYWORD_SET(save_png)) THEN BEGIN
     DEVICE,PSEUDO_COLOR=8      ;fixes color table problem FOR machines with 24-bit color
     LOADCT2,ctNum
  ENDIF ELSE LOADCT2,ctNum         ; rainbow color map

  red              = (ctNum EQ 43) ? 235 : 250
  darkRed          = 250
  green            = 130
  blue             = 90
  maxwell          = 50
  black            = 10
  ;; gray             = 20
  poiple           = 40
  violet           = 60
  fuschia          = (ctNum EQ 43) ? 1 : 5

                                ;Determine units for electron energy and pitch-angle spectrograms
  defSpecUnits = 'eflux' 
  specUnits = KEYWORD_SET(spectrogram_units) ? spectrogram_units : defSpecUnits

  CASE STRUPCASE(specUnits) OF
     'FLUX': BEGIN

        specUnits = 'flux'
        ionSpecLogLims  = [1.,7.]
        specLogUnitsString = 'Log #!C/cm!U2!N-s-sr-eV'
        eSpecLogLims = [2.,8.]
     END
     'EFLUX': BEGIN
        specUnits = 'eflux'
        ionSpecLogLims  = [5.,9.]
        eSpecLogLims = [6.,9.]
        specLogUnitsString = 'Log eV!C/cm!U2!N-s-sr-eV'
     END
  ENDCASE

  IF NOT KEYWORD_SET(energy_ions) THEN energy_ions=[4,1.e4]

  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN ieb_or_ies = 'ieb' ELSE ieb_or_ies = 'ies'
  burst = (WHERE(STRMATCH(STRUPCASE(eeb_or_ees),'*B')))[0] NE -1

  nn = N_ELEMENTS(data_quants)

  IF (nn GT 1) THEN FOR n = nn-1L,1L,-1L DO STORE_DATA,data_quants(n).name,/DELETE

; Step 1 - DC Mag data

  GET_DATA,'dB_fac_v',DATA=data
  IF SIZE(data,/TYPE) NE 8 THEN $
        UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi,/QUIET

  IF (N_ELEMENTS(orbit) GT 0) THEN BEGIN

;  IF orbit > 9936 return (temporary fix)

     IF (orbit GT 9936) THEN BEGIN

        PRINT,""
        PRINT,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        PRINT,""
        ;; return

     ENDIF

     orbString           = STRING(FORMAT='(I0)',orbit)

;;Handle PNGness or PSness before kicking things off

     IF KEYWORD_SET(save_ps) THEN BEGIN

        outPlotName  = 'Strangeway_summary'
        outPlotName += '-' + orbString + (KEYWORD_SET(bonusPref) ? bonusPref : '' )

        IF N_ELEMENTS(Newell_2009_interp) GT 0 THEN BEGIN
           IF Newell_2009_interp EQ 0 THEN BEGIN
              outPlotName += '-not_Newell_interpreted'
           ENDIF
        ENDIF

        t1S = STRMID(TIME_TO_STR(time1,/MSEC),11,11)
        t2S = STRMID(TIME_TO_STR(time2,/MSEC),11,11)

        t1S = t1S.REPLACE(':', '_')
        t1S = t1S.REPLACE('.', '__')
        
        t2S = t2S.REPLACE(':', '_')
        t2S = t2S.REPLACE('.', '__')
        
        specAvgSuff = KEYWORD_SET(spectra_average_interval)                     ? $
                      STRING(FORMAT='("-avgItvl",I0)',spectra_average_interval) : $
                      ''
        outPlotName += '-' + t1S + '_-_' + t2S + specAvgSuff

        IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/Strangeway_et_al_2005'
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN

           CGPS_OPEN, plotDir+outPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7

        ENDIF ELSE BEGIN

           IF KEYWORD_SET(save_ps) THEN BEGIN

              filNavn = KEYWORD_SET(outPlotName) ? outPlotName : (orbString + '-single_rjs_summary')
              filTmp  = STRSPLIT(outPlotName,'.',/EXTRACT)
              filPref = (filTmp)[0] + '_' + specUnits
              filSuff = N_ELEMENTS(filTmp) GT 1 ? '.' + filTmp[1] : ''

              ;; Update filNavn so that it has specUnits on the end
              filNavn = filPref

              count = 0
              WHILE FILE_TEST(plotDir+filNavn+(KEYWORD_SET(eps) ? '.eps' : '.ps')) DO BEGIN
                 count++
                 filNavn = STRING(FORMAT='(A0,"-",I02,A0)', $
                                  filPref, $
                                  count, $
                                  filSuff)
              ENDWHILE

              POPEN,plotDir+filNavn,/PORT,FONT=-1, $
                    ENCAPSULATED=eps ;,XSIZE=4,YSIZE=7

              DEVICE,/PALATINO,FONT_SIZE=8

           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE

        ENDELSE

     ENDIF

; got mag data, set time limits, DELETE unused tplot variables, set tPlt_vars

     GET_DATA,'dB_fac_v',DATA=data
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0] ELSE t1 = time1
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[N_ELEMENTS(data.x)-1L] ELSE t2 = time2
     tlimit_all = [t1,t2]
     tPlt_vars = 'dB_fac_v'
     OPTIONS,'dB_fac_v','panel_size',2
     OPTIONS,'dB_fac','panel_size',2
     OPTIONS,'dB_sm','panel_size',2

     IF (KEYWORD_SET(use_fac)) THEN tPlt_vars = 'dB_fac'

     IF (not KEYWORD_SET(no_blank_panels)) THEN tPlt_vars = 'dB_fac_v'

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        wInd = 0
        WINDOW,wInd,XSIZE=700,YSIZE=900
        ;; tplot_OPTIONS,'region',[0.,0.5,1.0,1.0]
        ;; loadct2,39
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'], $
              WINDOW=wInd, $
              TRANGE=[t1,t2]
     ENDIF

  ENDIF

; step 2 - E field


; JBV, 2011/05/22.   IF we are running Multi-User SDT, we need
; to get the SDT index FOR this run.  Otherwise "showDQIs" won't
; return.  IF this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
  sdt_idx = GET_SDT_RUN_IDX()

  prog = GETENV('FASTBIN') + '/showDQIs'
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


  CASE 1 OF
     burst: BEGIN
        v12Str = 'V1-V2_S'
        v14Str = 'V1-V4_S'
        v58Str = 'V5-V8_S'
     END
     ELSE: BEGIN
        v12Str = 'V1-V2_S'
        v14Str = 'V1-V4_S'
        v58Str = 'V5-V8_S'
     END
  ENDCASE


  b = WHERE (STRPOS(result,v14Str) ge 0,nb4)
  IF (nb4 GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN nb4 = 0
  b = WHERE (STRPOS(result,v12Str) ge 0,nb2)
  IF (nb2 GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN nb2 = 0
  IF (nb4 GT 0) THEN v12=GET_FA_FIELDS(v14Str,/ALL) $
  ELSE IF (nb2 GT 0) THEN v12=GET_FA_FIELDS(v12Str,/ALL)

  b = WHERE (STRPOS(result,v58Str) ge 0,nb5)
  IF (nb5 GT 0) THEN v58=GET_FA_FIELDS(v58Str,/ALL)

  got_efield = (nb4 GT 0 or nb2 GT 0) and nb5 GT 0

  IF (got_efield) THEN BEGIN

; despin e field data

     FA_FIELDS_DESPIN,v58,v12,/SHADOW_NOTCH,/SINTERP

     OPTIONS,'EFIT_ALONG_V','yrange',0
     OPTIONS,'EFIT_ALONG_V','ytitle','E along V!C!C(mV/m)'
     OPTIONS,'EFIT_ALONG_V','panel_size',2

; reset time limits IF needed

     GET_DATA,'EFIT_ALONG_V',DATA=data
     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[N_ELEMENTS(data.x)-1L]

     IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
        IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
        IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
        GET_FA_ORBIT,tlimit_all[0],tlimit_all[1],/ALL,STATUS=no_model,DELTA=1.,/DEFINITIVE,/DRAG_PROP
        GET_NEW_IGRF,/NO_STORE_OLD
     ENDIF

; check FOR southern hemisphere and fix 
; NOTE IT IS ASSUMED THAT FA_FIELDS_DESPIN DOES NOT CORRECT PHASE

     GET_DATA,'B_model',DATA=bm
     GET_DATA,'fa_vel',DATA=vel
     GET_DATA,'fa_pos',DATA=pos
     n=N_ELEMENTS(reform(pos.y[*,0]))
     rxv = DBLARR(n,3)
     rxv[*,0] = pos.y[*,1]*vel.y[*,2] - pos.y[*,2]*vel.y[*,1]
     rxv[*,1] = pos.y[*,2]*vel.y[*,0] - pos.y[*,0]*vel.y[*,2]
     rxv[*,2] = pos.y[*,0]*vel.y[*,1] - pos.y[*,1]*vel.y[*,0]
     vxb = DBLARR(n,3)
     vxb[*,0] = vel.y[*,1]*bm.y[*,2] - vel.y[*,2]*bm.y[*,1]
     vxb[*,1] = vel.y[*,2]*bm.y[*,0] - vel.y[*,0]*bm.y[*,2]
     vxb[*,2] = vel.y[*,0]*bm.y[*,1] - vel.y[*,1]*bm.y[*,0]
     tst = rxv[*,0]*vxb[*,0] + rxv[*,1]*vxb[*,1] + rxv[*,2]*vxb[*,2]

     GET_DATA,'EFIT_ALONG_V',DATA=data,DLIMIT=dlimit
     ;; y2=spl_init(pos.x-tlimit_all[0],tst,/double)
     ;; tst_ = spl_interp(pos.x-tlimit_all[0],tst,y2,data.x-tlimit_all[0],/double)
     ;; data.y = data.y*tst_/abs(tst_)
     STORE_DATA,'EFIT_ALONG_VSC',DATA=data,DLIMIT=dlimit
     OPTIONS,'EFIT_ALONG_VSC','yrange',0
     OPTIONS,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
     OPTIONS,'EFIT_ALONG_VSC','panel_size',2

     STORE_DATA,'E_NEAR_B',/DELETE
     STORE_DATA,'E_ALONG_V',/DELETE
     STORE_DATA,'EFIT_NEAR_B',/DELETE
     STORE_DATA,'EFIT_ALONG_V',/DELETE

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['EFIT_ALONG_VSC'] ELSE tPlt_vars=['EFIT_ALONG_VSC',tPlt_vars]

     YLIM,'EFIT_ALONG_VSC',MIN(data.y),MAX(data.y),0
     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF ELSE IF (N_ELEMENTS(tPlt_vars) NE 0) THEN BEGIN

     tPlt_vars = 'dB_fac'
     IF (KEYWORD_SET(use_fac_v)) THEN tPlt_vars = 'dB_fac_v'
     IF (not KEYWORD_SET(no_blank_panels)) THEN tPlt_vars = 'dB_fac_v'

  ENDIF


; Step 3 - Iesa data

  prog = GETENV('FASTBIN') + '/showDQIs'
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
  b = WHERE (STRPOS(result,'Iesa Survey') ge 0,nesa)
  IF (nesa GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN nesa = 0

  IF (nesa GT 0) THEN BEGIN

; ION PITCH ANGLE

     var_NAME='Iesa_Angle'
     ion_ER = KEYWORD_SET(ion_energyRange) ? ion_energyRange : [4.,30000.]
     GET_PA_SPEC,'fa_' + ieb_or_ies + '_c', $
                 UNITS=specUnits, $
                 NAME=var_name, $
                 ENERGY=ion_ER, $
                 /RETRACE, $
                 /CALIB
     GET_DATA,var_name, DATA=data
     data.y = ALOG10(data.y)
     STORE_DATA,var_name, DATA=data
     OPTIONS,var_name,'spec',1	
     ;; ZLIM,var_name,4,9,0
        ZLIM,var_name, $
             (MIN(data.y[WHERE(FINITE(data.y))]) > ionSpecLogLims[0]), $
             (MAX(data.y[WHERE(FINITE(data.y))]) < ionSpecLogLims[1]),0
     ;; ZLIM,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     YLIM,var_name,0,360,0
     OPTIONS,var_name,'ytitle','Ions > ' + STRING(FORMAT='(I0)',ion_ER[0]) + ' eV!C!CAngle (Deg.)'
     OPTIONS,var_name,'ztitle',specLogUnitsString
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     GET_DATA,var_name, DATA=data
     bb = WHERE (data.v GT 270.,nb)
     IF (nb GT 0) THEN data.v(bb)=data.v(bb)-360.
     nn = N_ELEMENTS(data.x)
     FOR n = 0,nn-1L DO BEGIN
        bs = SORT (data.v(n,*))
        data.v(n,*)=data.v(n,bs)
        data.y(n,*)=data.y(n,bs)
     endfor
     STORE_DATA,var_name, DATA=data	
     OPTIONS,var_name,'yminor',9
     OPTIONS,var_name,'yticks',4
     OPTIONS,var_name,'ytickv',[-90,0,90,180,270]
     YLIM,var_name,-90,270,0

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

; reset time limits IF needed

     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[N_ELEMENTS(data.x)-1L]

     IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
        IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
        IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
        GET_FA_ORBIT,tlimit_all[0],tlimit_all[1],/ALL,STATUS=no_model,DELTA=1.,/DEFINITIVE,/DRAG_PROP
        GET_NEW_IGRF,/NO_STORE_OLD
     ENDIF

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

; ION ENERGY 

     var_NAME='Iesa_Energy'
     GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
                 NAME=var_name, $
                 UNITS=specUnits, $
                 ANGLE=ion_angleRange, $
                 /CALIB, $
                 RETRACE=1
     GET_DATA,var_name,DATA=data
     data.y = ALOG10(data.y)
     STORE_DATA,var_name,DATA=data
     OPTIONS,var_name,'spec',1	
     ;; ZLIM,var_name,4,9,0
     ;; ZLIM,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     ZLIM,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > ionSpecLogLims[0]), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < ionSpecLogLims[1]),0
     YLIM,var_name,4,24000,1
     OPTIONS,var_name,'ytitle','Loss-coNE Ions!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle',specLogUnitsString
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

     IF KEYWORD_SET(add_iu_pot) THEN BEGIN

        have_iu_pot = 0
        IF N_ELEMENTS(curPotList) GT 0 THEN BEGIN

           ;; potLStyle = 1 ;dotted
           ;; potLStyle = 2           ;dashed
           potLStyle = 0        ;solid
           potColor  = fuschia
           ;; potLStyle = 3 ;dash dot
           ;; potLStyle = 4 ;dash dot dot
           STORE_DATA,'iu_pot',DATA={x:curPotList[2].time,y:curPotList[2].peakE}
           OPTIONS,'iu_pot','LINESTYLE',potLStyle
           OPTIONS,'iu_pot','colors',potColor
           OPTIONS,'iu_pot','thick',2.0

           have_iu_pot = 1

        ENDIF

     ENDIF

  ENDIF


; Step 4 - Eesa data

  prog = GETENV('FASTBIN') + '/showDQIs'
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
  b = WHERE (STRPOS(result,'Eesa Survey') ge 0,nesa)
  IF (nesa GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN nesa = 0

  IF (nesa GT 0) THEN BEGIN

; ELECTRON PITCH ANGLE

     var_NAME='Eesa_Angle'
     GET_PA_SPEC,'fa_' + eeb_or_ees + '_c', $
                 UNITS=specUnits, $
                 NAME=var_name, $
                 ENERGY=[10.,30000.]
     GET_DATA,var_name,DATA=data 
     data.y = ALOG10(data.y)
     STORE_DATA,var_name,DATA=data
     OPTIONS,var_name,'spec',1
     ;; ZLIM,var_name,4,9,0
     ;; ZLIM,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     ZLIM,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > eSpecLogLims[0]), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < eSpecLogLims[1]),0
     YLIM,var_name,0,360,0
     OPTIONS,var_name,'ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
     OPTIONS,var_name,'ztitle',specLogUnitsString
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     GET_DATA,var_name, DATA=data
     bb = WHERE (data.v GT 270.,nb)
     IF (nb GT 0) THEN data.v(bb)=data.v(bb)-360.
     nn = N_ELEMENTS(data.x)
     FOR n = 0,nn-1L DO BEGIN
        bs = SORT (data.v(n,*))
        data.v(n,*)=data.v(n,bs)
        data.y(n,*)=data.y(n,bs)
     endfor
     STORE_DATA,var_name, DATA=data
     OPTIONS,var_name,'yminor',9
     OPTIONS,var_name,'yticks',4
     OPTIONS,var_name,'ytickv',[-90,0,90,180,270]
     YLIM,var_name,-90,270,0

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

; reset time limits IF needed

     IF N_ELEMENTS(time1) EQ 0 THEN t1 = data.x[0]
     IF N_ELEMENTS(time2) EQ 0 THEN t2 = data.x[N_ELEMENTS(data.x)-1L]

     IF ((t1 LT tlimit_all[0]) or (t2 GT tlimit_all[1])) THEN BEGIN
        IF (t1 LT tlimit_all[0]) THEN tlimit_all[0] = t1
        IF (t2 GT tlimit_all[1]) THEN tlimit_all[1] = t2
        GET_FA_ORBIT,tlimit_all[0],tlimit_all[1],/ALL,STATUS=no_model,DELTA=1.,/DEFINITIVE,/DRAG_PROP
        GET_NEW_IGRF,/NO_STORE_OLD
     ENDIF

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

; ELECTRON ENERGY

     var_NAME='Eesa_Energy'
     GET_EN_SPEC,'fa_' + eeb_or_ees + '_c', $
                 NAME=var_name, $
                 UNITS=specUnits, $
                 /CALIB, $
                 RETRACE=1
     GET_DATA,var_name,DATA=data
     data.y = ALOG10(data.y)
     STORE_DATA,var_name,DATA=data
     OPTIONS,var_name,'spec',1	
     ;; ZLIM,var_name,4,9,0
     ;; ZLIM,var_name,MIN(data.y[WHERE(FINITE(data.y))]),MAX(data.y[WHERE(FINITE(data.y))]),0
     ZLIM,var_name, $
          (MIN(data.y[WHERE(FINITE(data.y))]) > eSpecLogLims[0]), $
          (MAX(data.y[WHERE(FINITE(data.y))]) < eSpecLogLims[1]),0
     YLIM,var_name,5,30110,1
     OPTIONS,var_name,'ytitle','Electrons!C!CEnergy (eV)'
     OPTIONS,var_name,'ztitle',specLogUnitsString
     OPTIONS,var_name,'x_no_interp',1
     OPTIONS,var_name,'y_no_interp',1
     OPTIONS,var_name,'panel_size',2

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN 
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF


; Step 5 - VLF data


; DSP_V5-V8HG or DSP_V5-V8

  prog = GETENV('FASTBIN') + '/showDQIs'
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
  b = WHERE (STRPOS(result,'DspADC_V5-V8HG') ge 0,ndsphg)
  IF (ndsphg GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN ndsphg = 0
  b = WHERE ((STRPOS(result,'DspADC_V5-V8') ge 0) and (STRPOS(result,'DspADC_V5-V8HG') LT 0),ndsp)
  IF (ndsp GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN ndsp = 0

  IF (ndsphg GT 0) THEN dat=GET_FA_FIELDS('DspADC_V5-V8HG',/ALL) ELSE IF (ndsp GT 0) THEN dat=GET_FA_FIELDS('DspADC_V5-V8',/ALL)
  ndsp = (ndsp GT 0) or (ndsphg GT 0)

  IF (ndsp) THEN BEGIN
     data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
     STORE_DATA,'DSP_V5-V8', DATA=data
     ;; dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $

;  look FOR big jumps in time - blank these

     GET_DATA,'DSP_V5-V8',DATA=data
     dt = data.x[1:*]-data.x[0:*]
     ntimes=N_ELEMENTS(data.x)
     bg = WHERE (dt GT 300, ng)
     IF (ng GT 0) THEN BEGIN
        bbb = bg-1
        IF (bbb[0] LT 0) THEN bbb[0] = 0
        add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
        flag_dat = fltarr(ng*2)+!values.f_nan
        new_tag = [data.x,add_tag]
        tSORT = SORT(new_tag-new_tag[0])
        nvec=N_ELEMENTS(data.y)/ntimes
        new_dat = fltarr(N_ELEMENTS(new_tag),nvec)
        FOR nv = 0,nvec-1 DO BEGIN
           new_dat[*,nv] = [data.y[*,nv],flag_dat]
           new_dat[*,nv] = new_dat[tsort,nv]
        endfor
        DATA={x:new_tag[tsort],y:new_dat,v:data.v}
        STORE_DATA,'DSP_V5-V8',DATA=data
     ENDIF
     
     ;;Plot opts
     VLF_zRange = [MIN(data.y[WHERE(FINITE(data.y))] > (-13)), $
                   MAX(data.y[WHERE(FINITE(data.y))]) < (-5)]
     dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], $
               zrange:VLF_zRange, $
               ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
               ztitle: '(V/m)!U2!N/Hz', panel_size:2}

     STORE_DATA,'DSP_V5-V8', DLIMIT=dlimit

     OPTIONS,'DSP_V5-V8','x_no_interp',1
     OPTIONS,'DSP_V5-V8','y_no_interp',1


     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['DSP_V5-V8'] ELSE tPlt_vars=['DSP_V5-V8',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF ELSE BEGIN

  ENDELSE

; Step 5 - AKR data

  IF ~KEYWORD_SET(GRL) THEN BEGIN
     prog = GETENV('FASTBIN') + '/showDQIs'
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
     b = WHERE (STRPOS(result,'SfaAve_V5-V8') ge 0,nakr)
     IF (nakr GT 0) THEN IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') ge 0 THEN nakr = 0

     IF (nakr GT 0) THEN BEGIN

        dat = GET_FA_FIELDS('SfaAve_V5-V8', /ALL)
        data   = {x:dat.time, y:alog10(dat.comp1), v:dat.yaxis}
        STORE_DATA,'SFA_V5-V8', DATA=data
        ;; dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $

;  look FOR big jumps in time - blank these

        GET_DATA,'SFA_V5-V8',DATA=data
        dt = data.x[1:*]-data.x[0:*]
        ntimes=N_ELEMENTS(data.x)
        bg = WHERE (dt GT 300, ng)
        IF (ng GT 0) THEN BEGIN
           bbb = bg-1
           IF (bbb[0] LT 0) THEN bbb[0] = 0
           add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
           flag_dat = fltarr(ng*2)+!values.f_nan
           new_tag = [data.x,add_tag]
           tSORT = SORT(new_tag-new_tag[0])
           nvec=N_ELEMENTS(data.y)/ntimes
           new_dat = fltarr(N_ELEMENTS(new_tag),nvec)
           FOR nv = 0,nvec-1 DO BEGIN
              new_dat[*,nv] = [data.y[*,nv],flag_dat]
              new_dat[*,nv] = new_dat[tsort,nv]
           endfor
           DATA={x:new_tag[tsort],y:new_dat,v:data.v}
           STORE_DATA,'SFA_V5-V8',DATA=data
        ENDIF

        ;;Plot opts
        GET_DATA,'DSP_V5-V8',DLIMIT=VLF_dlimit

        ;;Put AKR and VLF data on same scale in z direction
        ;; AKR_zRange = [MIN([data.y[WHERE(FINITE(data.y))],VLF_dLimit.zRange[0]]) > (-15), $
        ;;               MAX([data.y[WHERE(FINITE(data.y))],VLF_dLimit.zRange[1]]) < (-6)]
        AKR_zRange = [MIN(data.y[WHERE(FINITE(data.y))]) > (-14), $
                      MAX(data.y[WHERE(FINITE(data.y))]) < (-6)]
        ;; VLF_dlimit.zRange = AKR_zRange
        ;; STORE_DATA,'DSP_V5-V8',DLIMIT=VLF_dlimit

        dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], $
                  zrange:AKR_zRange, $
                  ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        STORE_DATA,'SFA_V5-V8', DLIMIT=dlimit
        OPTIONS,'SFA_V5-V8','x_no_interp',1
        OPTIONS,'SFA_V5-V8','y_no_interp',1




        IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['SFA_V5-V8'] ELSE tPlt_vars=['SFA_V5-V8',tPlt_vars]

        IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
           ;; loadct2,40
           TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
        ENDIF

     ENDIF

  ENDIF
; STEP 6 - Clean up and return

; determiNE tlimit_north and tlimit_south also change plot title

  GET_DATA,'LAT',DATA=data

  IF (N_ELEMENTS(data.y) le 0) THEN return

  bb = WHERE (data.y GT 10,nn)
  IF (nn GT 0) THEN tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

  bb = WHERE (data.y LT -10,nn)
  IF (nn GT 0) THEN tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

  hemisph = GETENV('FAST_ORBIT_HEMISPHERE')

  GET_DATA,'ORBIT',DATA=data
  nn = N_ELEMENTS(data.y)/2
  orbit = data.y(nn)
  orbit_lab = STRCOMPRESS(STRING(orbit,FORMAT="(i5.4)"),/REMOVE_ALL)
  TPLOT_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

;;Include chare panel?

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  IF KEYWORD_SET(add_chare_panel) OR KEYWORD_SET(add_kappa_panel) OR KEYWORD_SET(add_Newell_panel) THEN BEGIN
     eAngle       = [360.-30.,30.]
     iAngle       = [135.,225.]
     eAngleChare  = eAngle
     iAngleChari  = iAngle
     t1eeb = 0.D 
     t2eeb = 0.D
     bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
     bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
     t1eeb = time1 > t1eeb
     t2eeb = time2 < t2eeb

     GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Je',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Jee',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     GET_2DT,'j_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Ji',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     GET_2DT,'je_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Jei',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     ;;Remove_crap
     GET_DATA,'Je',DATA=tmp
     ;; GET_DATA,'Je',DATA=Je_originalsk
     ;; saveStr+='Je_originalsk,'
     keep1                          = WHERE(FINITE(tmp.y) NE 0)
     keep2                          = WHERE(ABS(tmp.y) GT 0.0)
     GET_DATA,'Jee',DATA=tmp
     ;; GET_DATA,'Jee',DATA=Jee_originalsk
     ;; saveStr+='Jee_originalsk,'
     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
     GET_DATA,'Ji',DATA=tmp2
     badIons                        = 0
     IF N_ELEMENTS(tmp2.x) LE 2 THEN BEGIN
        badIons = 1
        tmp2 = tmp
        tmp2.y *= 0.
     ENDIF ELSE BEGIN
        ;; GET_DATA,'Ji',DATA=Ji_originalsk
        ;; saveStr+='Ji_originalsk,'
        keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp2.y) NE 0))
        keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp2.y) GT 0.0))
     ENDELSE
     GET_DATA,'Jei',DATA=tmp2
     IF N_ELEMENTS(tmp2.x) LE 2 THEN BEGIN
        badIons = badIons + 1
        tmp2 = tmp
        tmp2.y *= 0.
     ENDIF ELSE BEGIN
        ;; GET_DATA,'Jei',DATA=Jei_originalsk
        ;; saveStr+='Jei_originalsk,'
        keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp2.y) NE 0))
        keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp2.y) GT 0.0))
     ENDELSE
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
     IF badIons GT 0 THEN BEGIN
        ji_tmp_time                 = tmp.x[keep2]
        ji_tmp_data                 = tmp.y[keep2]
        jei_tmp_time                = tmp.x[keep2]
        jei_tmp_data                = tmp.y[keep2]
        ji_tmp_data                *= 0.
        jei_tmp_data               *= 0.
     ENDIF ELSE BEGIN
        GET_DATA,'Ji',DATA=tmp
        tmp.x                       = tmp.x[keep1]
        tmp.y                       = tmp.y[keep1]
        ji_tmp_time                 = tmp.x[keep2]
        ji_tmp_data                 = tmp.y[keep2]
        GET_DATA,'Jei',DATA=tmp
        tmp.x                       = tmp.x[keep1]
        tmp.y                       = tmp.y[keep1]
        jei_tmp_time                = tmp.x[keep2]
        jei_tmp_data                = tmp.y[keep2]
     ENDELSE
     STORE_DATA,'Ji',DATA={x:ji_tmp_time,y:ji_tmp_data}
     STORE_DATA,'Jei',DATA={x:jei_tmp_time,y:jei_tmp_data}

     GET_DATA,'Je',DATA=Je
     GET_DATA,'Jee',DATA=Jee
     GET_DATA,'Ji',DATA=Ji
     GET_DATA,'Jei',DATA=Jei
  ENDIF

  IF KEYWORD_SET(add_chare_panel) THEN BEGIN
     chare            = Jee.y/Je.y*6.242*1.0e11
     chari            = Jei.y/Ji.y*6.242*1.0e11
     charEBounds      = [MIN(chare[WHERE(chare GT 0)]) + MIN(chari[WHERE(chari GT 0)]), $
                         MAX(chare[WHERE(chare GT 0)]) + MAX(chari[WHERE(chari GT 0)])]
     ;; showLog_charE    = (ALOG10(MAX(chare[WHERE(chare GT 0)]))-ALOG10(MIN(chare[WHERE(chare GT 0)]))) GT 2
     ;; showLog_charE    = (ALOG10(charEBounds[1])-ALOG10(charEBounds[0])) GT 2
     showLog_charE    = 1B
     IF showLog_charE THEN BEGIN
        charEBounds[0] -= (charEBounds[0]*0.1)
        charEBounds[1] += (charEBounds[1]*0.1)
     ENDIF ELSE BEGIN
        charEBounds[0] /= 1.1
        charEBounds[1] *= 1.1
     ENDELSE
     chari_interp = DATA_CUT({x:Jei.x,y:chari},Jee.x,/IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,{time:Jee.x,comp1:Jee.y,ncomp:1}, $
     ;;                   {time:Jei.x,comp1:chari,ncomp:1}, $
     ;;                   RESULT=chari_interp, $
     ;;                   /INTERP, $
     ;;                   DELT_T=50., $
     ;;                   /TALK
     ;; chari_interp  = {x:Jee.x,y:chari_interp}
     chartot          = chare+chari_interp
     STORE_DATA,'charepanel',DATA={x:[[Jee.x],[Jee.x],[Jee.x]],y:[[chari_interp],[chare],[chartot]]}

     YLIM,'charepanel',charEBounds[0],charEBounds[1],showLog_charE
     OPTIONS,'charepanel','tplot_routine','mplot'
     OPTIONS,'charepanel','ytitle','E/q Volts'
     OPTIONS,'charepanel','labels',['Ion','Electron','Total']
     OPTIONS,'charepanel','colors',[red,green,20]
     OPTIONS,'charepanel','labflag',-1
     ;; OPTIONS,'charepanel','yticks',5                                   ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickname',['0','5e3','1.0e4','1.5e4','2.e4'] ; set y-axis labels
     ;; OPTIONS,'charepanel','ytickv',[0.,5.e3,1.0e4,1.5e4,2.0e4]           ; set y-axis labels

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['charepanel'] ELSE tPlt_vars=['charepanel',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF

;;Include kappa panel?
  IF KEYWORD_SET(add_kappa_panel) THEN BEGIN

     IF N_ELEMENTS(kappaFit1Ds) NE N_ELEMENTS(gaussFit1Ds) THEN STOP

     kappa2D            = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DKappa_inf_list, $
                                                         CHI2_THRESHOLD=chi2_thresh, $
                                                         CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                                         HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                         LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                                         KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                         KAPPA_HIGHTHRESHOLD=hKappa_thresh, $
                                                         DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                                         N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                                         /DESTROY_INFO_LIST, $
                                                         OUT_GOOD_I=includeK_i, $
                                                         OUT_GOOD_T=includeK_t, $
                                                         OUT_BAD_I=excludeK_i, $
                                                         OUT_BAD_T=excludeK_t)

     gauss2D            = PARSE_KAPPA_FIT2D_INFO_LIST_V2(fit2DGauss_inf_list, $
                                                         CHI2_THRESHOLD=chi2_thresh, $
                                                         CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                                                         HIGHDENSITY_THRESHOLD=highDens_thresh, $
                                                         LOWDENSITY_THRESHOLD=lowDens_thresh, $
                                                         KAPPA_LOWTHRESHOLD=lKappa_thresh, $
                                                         KAPPA_HIGHTHRESHOLD=100.1, $
                                                         DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                                                         N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                                                         /DESTROY_INFO_LIST, $
                                                         OUT_GOOD_I=includeG_i, $
                                                         OUT_GOOD_T=includeG_t, $
                                                         OUT_BAD_I=excludeG_i, $
                                                         OUT_BAD_T=excludeG_t)

     PRINT,"This has not been update to use 2D fit parameters (e.g., search FOR Astruct and see what turns up in this pro)!"
     PRINT,"Gotta fix it"
     STOP
     ;; PARSE_KAPPA_FIT_STRUCTS,kappaFit1Ds, $
     ;;                         A=a, $
     ;;                         STRUCT_A=Astruct, $
     ;;                         TIME=kappaTime, $
     ;;                         MATCH_TIMES=kappa2D.SDT[*].time, $
     ;;                         NAMES_A=A_names, $
     ;;                         CHI2=chi2, $
     ;;                         PVAL=pVal, $
     ;;                         FITSTATUS=kappaFitStatus, $
     ;;                         /USE_MPFIT1D

     ;; PARSE_KAPPA_FIT_STRUCTS,gaussFit1Ds, $
     ;;                         A=AGauss, $
     ;;                         STRUCT_A=AStructGauss, $
     ;;                         TIME=GaussTime, $
     ;;                         MATCH_TIMES=kappa2D.SDT[*].time, $
     ;;                         NAMES_A=AGauss_names, $
     ;;                         CHI2=chi2Gauss, $
     ;;                         PVAL=pValGauss, $
     ;;                         FITSTATUS=gaussFit1DStatus, $
     ;;                         /USE_MPFIT1D

     ;; IF ~ARRAY_EQUAL(kappaTime,GaussTime) THEN STOP
     ;; nFits           = N_ELEMENTS(kappa2D.fitMoms.scDens)
     ;; badFits_i       = WHERE(kappaFitStatus NE 0,nBadFits)
     ;; badGaussFit1Ds_i  = WHERE(gaussFit1DStatus NE 0,nBadGaussFit1Ds)
     ;; bothBad_i       = ( (badFits_i[0] EQ -1) AND (badGaussFit1Ds_i[0] EQ -1 ) ) ? !NULL : $
     ;;                   CGSETINTERSECTION(badFits_i,badGaussFit1Ds_i)
     ;; PRINT,""
     ;; PRINT,"****************************************"
     ;; PRINT,'NTotalFits    : ',nFits
     ;; PRINT,''
     ;; PRINT,"NbadFits      : ",nBadFits
     ;; PRINT,"NbadGaussFit1Ds : ",nBadGaussFit1Ds
     ;; PRINT,"NBothBad      : ",N_ELEMENTS(bothBad_i)

     ;; STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:Astruct.kappa}
     STORE_DATA,'kappa_fit',DATA={x:kappaTime,y:REFORM(kappa2D.fitParams[2,*])}
     CASE 1 OF
        KEYWORD_SET(log_kappaPlot): BEGIN
           YLIM,'kappa_fit',1.0,100,1
        END
        ELSE: BEGIN
           YLIM,'kappa_fit',1.0,11,0
        END
     ENDCASE
     OPTIONS,'kappa_fit','ytitle',"Kappa!CFit Val"
     OPTIONS,'kappa_fit','psym',2 ;Asterisk


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Four-current panel (it's like four-cheese pizza)

     ;;Get mag current
     GET_DATA,'dB_fac_v',DATA=db_fac

     jMag                         = GET_CURRENT_FROM_FLUXMAG(t1,t2, $
                                                             db_fac,vel, $
                                                             /USE_DESPUN, $
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
     Je_kappa_interp       = DATA_CUT({x:Je.x,y:Je_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     Ji_kappa_interp       = DATA_CUT({x:Ji.x,y:Ji_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     Jtot_kappa_interp     = DATA_CUT({x:Je.x,y:Je_current+Ji_current}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chare_kappa_interp    = DATA_CUT({x:Jee.x,y:chare}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chari_kappa_interp    = DATA_CUT({x:Jee.x,y:chari_interp}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     chartot_kappa_interp  = DATA_CUT({x:Jee.x,y:chartot}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     jMag_kappa_interp     = DATA_CUT({x:JMag.x,y:jMag.y}, $
                                      kappaStr.time, $
                                      /IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current,ncomp:1}, $
     ;;                   RESULT=Je_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Ji.x,comp1:Ji_current,ncomp:1}, $
     ;;                   RESULT=Ji_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Je.x,comp1:Je_current+Ji_current,ncomp:1}, $
     ;;                   RESULT=Jtot_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chare,ncomp:1}, $
     ;;                   RESULT=chare_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chari_interp,ncomp:1}, $
     ;;                   RESULT=chari_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:Jee.x,comp1:chartot,ncomp:1}, $
     ;;                   RESULT=chartot_kappa_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,kappaStr,{time:jMag.x,comp1:jMag.y,ncomp:1}, $
     ;;                   RESULT=jMag_kappa_interp,/INTERP,DELT_T=50.,/TALK
     
     ;;Align to Je
     Ji_interp   = DATA_CUT({x:Ji.x,y:Ji_current},Je.x, $
                            /IGNORE_NAN,GAP_DIST=3)
     Jmag_interp = DATA_CUT({x:JMag.x,y:jMag.y},Je.x, $
                            /IGNORE_NAN,GAP_DIST=3)
     ;; FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1}, $
     ;;                   {time:Ji.x,comp1:Ji_current,ncomp:1}, $
     ;;                   RESULT=Ji_interp,/INTERP,DELT_T=50.,/TALK
     ;; FA_FIELDS_COMBINE,{time:Je.x,comp1:Je_current,ncomp:1},{time:jMag.x,comp1:jMag.y,ncomp:1}, $
     ;;                   RESULT=jMag_interp,/INTERP,DELT_T=50.,/TALK
     
     ;; Jtot_interp = {x:Je.x,y:Je_current+Ji_interp}

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
     ;; STORE_DATA,'toppings',DATA={x:[[kappaStr.time],[kappaStr.time]], $
     ;;                             y:[[gauss_current],[kappa_current]]}
     STORE_DATA,'toppings',DATA={x:kappaStr.time, $
                                 y:kappa_current}
     STORE_DATA,'feta',DATA={x:kappaStr.time, $
                             y:gauss_current}

     
     oneCheeseBounds   = [MIN(obs_current) < MIN(gauss_current) < MIN(kappa_current), $
                          MAX(obs_current) > MAX(gauss_current) > MAX(kappa_current)]
     IF oneCheeseBounds[0] LT 0 THEN BEGIN
        showLog_oneCheese   = 0 
        oneCheeseBounds[0] /= 1.1
        oneCheeseBounds[1] *= 1.1
     ENDIF ELSE BEGIN
        showLog_oneCheese   = (ALOG10(oneCheeseBounds[1])-ALOG10(oneCheeseBounds[0])) GT 2
        oneCheeseBounds[0] -= (oneCheeseBounds[0]*0.1)
        oneCheeseBounds[1] += (oneCheeseBounds[1]*0.1)
     ENDELSE
     OPTIONS,'onecheese','colors',green
     OPTIONS,'onecheese','tplot_routine','mplot'
     OPTIONS,'onecheese','ytitle','Current!C('+CGGREEK('mu')+'A/m!U2!Ns)'
     YLIM,   'onecheese',oneCheeseBounds[0],oneCheeseBounds[1],showLog_oneCheese
     oneCheesePos = (INDGEN(4)+1)/5.

     OPTIONS,'onecheese','labels',obsName
     ;; OPTIONS,'onecheese','labflag',-1
     OPTIONS,'onecheese','labflag',3
     OPTIONS,'onecheese','labpos',oneCheesePos[0]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'onecheese','labpos',-0.25
     ;; OPTIONS,'onecheese','yticks',2                    ; set y-axis labels
     ;; OPTIONS,'onecheese','ytickname',['-2.0','-1.0','0'] ; set y-axis labels
     ;; OPTIONS,'onecheese','ytickv',[-2.0,-1.0,0.0]        ; set y-axis labels

     OPTIONS,'fourcheese','colors',red
     OPTIONS,'fourcheese','labels','Fluxgate mag'
     ;; OPTIONS,'fourcheese','labflag',-1
     OPTIONS,'fourcheese','labflag',3
     OPTIONS,'fourcheese','labpos',oneCheesePos[1]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'fourcheese','labpos',-0.75

     ;; OPTIONS,'toppings','labels' ,['Maxwellian Model','Kappa model']
     ;; OPTIONS,'toppings','psym'   ,1
     ;; OPTIONS,'toppings','colors' ,[20,blue]
     ;; ;; OPTIONS,'toppings','labflag',-1
     ;; OPTIONS,'toppings','labflag',3
     ;; OPTIONS,'toppings','labpos',[oneCheesePos[2],oneCheesePos[3]]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'toppings','labpos',[-1.75,-1.25]

     OPTIONS,'toppings','labels' ,'Kappa model'
     OPTIONS,'toppings','psym'   ,1
     OPTIONS,'toppings','colors' ,blue
     ;; OPTIONS,'toppings','labflag',-1
     OPTIONS,'toppings','labflag',3
     OPTIONS,'toppings','labpos',oneCheesePos[3]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'toppings','labpos',[-1.75,-1.25]

     OPTIONS,'feta','labels' ,'Maxwellian Model'
     OPTIONS,'feta','psym'   ,1
     OPTIONS,'feta','colors' ,maxwell
     ;; OPTIONS,'feta','labflag',-1
     OPTIONS,'feta','labflag',3
     OPTIONS,'feta','labpos',oneCheesePos[2]*(oneCheeseBounds[1]-oneCheeseBounds[0])+oneCheeseBounds[0]
     ;; OPTIONS,'feta','labpos',[-1.75,-1.25]

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['onecheese','kappa_fit'] ELSE tPlt_vars=['onecheese','kappa_fit',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'       ;,PSYM=1
     ENDIF

  ENDIF

  IF KEYWORD_SET(add_Newell_panel) THEN BEGIN

     var_NAME='Eesa_LC_Energy'
     ;;This already gets called above, but we need to call it again to handle angle restrictions
     GET_EN_SPEC,'fa_' + eeb_or_ees + '_c', $
                 T1=t1, $
                 T2=t2, $
                 NAME=var_name, $
                 UNITS='eflux', $
                 /CALIB, $
                 RETRACE=1, $
                 ANGLE=eAngle
     GET_DATA,var_name,DATA=data

     GET_FA_ORBIT,data.x,/TIME_ARRAY
     GET_DATA,'MLT',DATA=mlt
     mLT       = mlt.y

     GET_DATA,'ILAT',DATA=ilat
     ilat      = ilat.y

     GET_DATA,'ALT',DATA=alt
     aLT      = alt.y

     GET_DATA,'ORBIT',DATA=orbit
     orbit          = orbit.y
     sc_pot         = GET_FA_POTENTIAL(t1,t2, $
                                       ;; /SPIN, $
                                       /REPAIR)
     sc_pot_interp  = DATA_CUT({x:sc_pot.time,y:sc_pot.comp1},data.x) 
     this           = VALUE_CLOSEST2(data.x,jee.x) 
     data           = {x:data.x[this],y:data.y[this,*],v:data.v[this,*]}
     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,data,Jee,Je,mlt,ilat,alt,orbit,events,SC_POT=sc_pot_interp, $
                                             /QUIET

     var_name = 'newellPanel'
     PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,events,TPLOT_NAME=var_name, $
                                               /NO_STRICT_TYPES, $
                                               CONVERT_TO_NEWELL_INTERP=Newell_2009_interp

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[var_name] ELSE tPlt_vars=[var_name,tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
        ;; loadct2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ENDIF

  ENDIF

; force tPlt_vars to be all the panels unless no_blank_panels is set

  IF (NOT KEYWORD_SET(no_blank_panels)) THEN BEGIN


; SFA

     bdat = WHERE(tPlt_vars EQ 'SFA_V5-V8',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = [-112.700,12.5031,997.434,2015.75]
        STORE_DATA,'SFA_V5-V8', DATA={x:t_arr, y:y_arr, v:v_arr}
        dlimit = {spec:1, ystyle:1, yrange:[10., 1000.0], zrange:[-16,-10], $
                  ytitle:'AKR E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        STORE_DATA,'SFA_V5-V8', DLIMIT=dlimit
        OPTIONS,'SFA_V5-V8','x_no_interp',1
        OPTIONS,'SFA_V5-V8','y_no_interp',1
     ENDIF

; DSP

     bdat = WHERE(tPlt_vars EQ 'DSP_V5-V8',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = [0.0,0.128,9.984,16.352]
        STORE_DATA,'DSP_V5-V8', DATA={x:t_arr, y:y_arr, v:v_arr}
        dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-16,-6], $
                  ytitle:'VLF E 55m!C!C(kHz)', ylog:1, $
                  ztitle: '(V/m)!U2!N/Hz', panel_size:2}
        STORE_DATA,'DSP_V5-V8', DLIMIT=dlimit
        OPTIONS,'DSP_V5-V8','x_no_interp',1
        OPTIONS,'DSP_V5-V8','y_no_interp',1
     ENDIF

; Eesa_Energy

     bdat = WHERE(tPlt_vars EQ 'Eesa_Energy',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [34119.7,26091.5,50.9600,5.88000]
        STORE_DATA,'Eesa_Energy', DATA={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Eesa_Energy','spec',1	
        ZLIM,'Eesa_Energy',eSpecLogLims[0],eSpecLogLims[1],0
        YLIM,'Eesa_Energy',5,30000,1
        OPTIONS,'Eesa_Energy','ytitle','Electrons!C!CEnergy (eV)'
        OPTIONS,'Eesa_Energy','ztitle',specLogUnitsString
        OPTIONS,'Eesa_Energy','x_no_interp',1
        OPTIONS,'Eesa_Energy','y_no_interp',1
        OPTIONS,'Eesa_Energy','panel_size',2
     ENDIF

; Eesa_Angle

     bdat = WHERE(tPlt_vars EQ 'Eesa_Angle',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        STORE_DATA,'Eesa_Angle', DATA={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Eesa_Angle','spec',1	
        zlim,'Eesa_Angle',4,9,0
        YLIM,'Eesa_Angle',-90,270,0
        OPTIONS,'Eesa_Angle','ytitle','Electrons > 10 eV!C!CAngle (Deg.)'
        OPTIONS,'Eesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Eesa_Angle','x_no_interp',1
        OPTIONS,'Eesa_Angle','y_no_interp',1
        OPTIONS,'Eesa_Angle','panel_size',2
        OPTIONS,'Eesa_Angle','yminor',9
        OPTIONS,'Eesa_Angle','yticks',4
        OPTIONS,'Eesa_Angle','ytickv',[-90,0,90,180,270]
     ENDIF

; Iesa_Energy

     bdat = WHERE(tPlt_vars EQ 'Iesa_Energy',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [26808.3,11827.2,27.7200,4.62000]
        STORE_DATA,'Iesa_Energy', DATA={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Iesa_Energy','spec',1	
        zlim,'Iesa_Energy',4,9,0
        YLIM,'Iesa_Energy',4,24000,1
        OPTIONS,'Iesa_Energy','ytitle','Ions!C!CEnergy (eV)'
        OPTIONS,'Iesa_Energy','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Iesa_Energy','x_no_interp',1
        OPTIONS,'Iesa_Energy','y_no_interp',1
        OPTIONS,'Iesa_Energy','panel_size',2
     ENDIF

; Iesa_Angle

     bdat = WHERE(tPlt_vars EQ 'Iesa_Angle',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = fltarr(2,4)
        y_arr[*,*] = !values.f_nan
        v_arr = fltarr(2,4)
        v_arr[0,*] = [-87.7792,2.22077,114.721,267.206]
        STORE_DATA,'Iesa_Angle', DATA={x:t_arr, y:y_arr, v:v_arr}
        OPTIONS,'Iesa_Angle','spec',1	
        zlim,'Iesa_Angle',4,9,0
        YLIM,'Iesa_Angle',-90,270,0
        OPTIONS,'Iesa_Angle','ytitle','Ions!C!CAngle (Deg.)'
        OPTIONS,'Iesa_Angle','ztitle','Log eV!C!C/cm!U2!N-s-sr-eV'
        OPTIONS,'Iesa_Angle','x_no_interp',1
        OPTIONS,'Iesa_Angle','y_no_interp',1
        OPTIONS,'Iesa_Angle','panel_size',2
        OPTIONS,'Iesa_Angle','yminor',9
        OPTIONS,'Iesa_Angle','yticks',4
        OPTIONS,'Iesa_Angle','ytickv',[-90,0,90,180,270]
     ENDIF

; EFIT_ALONG_VSC

     bdat = WHERE(tPlt_vars EQ 'EFIT_ALONG_VSC',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = [!values.f_nan,!values.f_nan]
        STORE_DATA,'EFIT_ALONG_VSC', DATA={x:t_arr, y:y_arr}
        dlimit = {spec:0, ystyle:1, yrange:[-1000., 1000.], $
                  ytitle:'EFIT ALONG V!C!C55m (mV/m)', $
                  panel_size:3}
        STORE_DATA,'EFIT_ALONG_V',DLIMIT=dlimit
        OPTIONS,'EFIT_ALONG_VSC','yrange',[-100.,100.]
        OPTIONS,'EFIT_ALONG_VSC','ytitle','E along V!Dsc!N!C!C(mV/m)'
        OPTIONS,'EFIT_ALONG_VSC','panel_size',2
     ENDIF

; dB_fac_v

     bdat = WHERE(tPlt_vars EQ 'dB_fac_v',ndat)
     IF (ndat EQ 0) THEN BEGIN
        t_arr = tlimit_all
        y_arr = DBLARR(2,3)
        y_arr[*,*] = !values.d_nan
        STORE_DATA,'dB_fac_v', DATA={x:t_arr, y:y_arr}
        OPTIONS,'dB_fac_v','yrange',[-100,100]
        OPTIONS,'dB_fac_v','ytitle','dB_fac_v!C!C(nT))'
        OPTIONS,'dB_fac_v','panel_size',2
        OPTIONS,'dB_fac_v','colors',[6,4,2]
        OPTIONS,'dB_fac_v','labels',['v ((BxV)xB)','p (BxV)','b']
     ENDIF

     tPlt_vars=['Eesa_Energy','Eesa_Angle','Iesa_Energy','Iesa_Angle','SFA_V5-V8','DSP_V5-V8','EFIT_ALONG_VSC','dB_fac_v']

     IF KEYWORD_SET(add_chare_panel)  THEN tPlt_vars = [tPlt_vars[0:3],'charepanel',tPlt_vars[4:7]]

     IF KEYWORD_SET(add_kappa_panel)  THEN tPlt_vars = ['onecheese','kappa_fit',tPlt_vars]

     IF KEYWORD_SET(add_Newell_panel) THEN tPlt_vars = ['newellPanel',tPlt_vars]

  ENDIF

  ;;Help db_fac_v
  GET_DATA,'dB_fac_v',DATA=dat
  dat.y[*,0] = dat.y[*,0] - (dat.y[0,0]-dat.y[0,2])
  dat.y[*,1] = dat.y[*,1] - (dat.y[0,1]-dat.y[0,2])
  STORE_DATA,'dB_fac_v',DATA=dat

  IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
     ;; loadct2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'       ;,PSYM=1
     ENDIF
  ENDIF

  IF KEYWORD_SET(save_ps) THEN BEGIN

     CASE 1 OF
        KEYWORD_SET(plot_north): BEGIN
           tLims = tlimit_north

        END
        KEYWORD_SET(plot_south): BEGIN
           tLims = tlimit_south
        END
        ELSE: BEGIN
           ;; tLims = [t1ZoomStr,t2ZoomStr]
        END
     ENDCASE

     ;; LOADCT2,40
     TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=[t1,t2]

     IF KEYWORD_SET(add_kappa_panel) THEN BEGIN
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='fourcheese' ;,PSYM='*'
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='toppings'   ;,PSYM=1
        TPLOT_PANEL,VARIABLE='onecheese',OPLOTVAR='feta'       ;,PSYM=1
     ENDIF

     IF KEYWORD_SET(have_iu_pot) THEN BEGIN
        TPLOT_PANEL,VARIABLE='Iesa_Energy',OPLOTVAR='iu_pot'

        sjekke = 0
        STR_ELEMENT,cAP_struct,'iu_pot_tids',sjekke
        IF SIZE(sjekke,/TYPE) EQ 7 THEN BEGIN

           CASE NDIMEN(cAP_struct.iu_pot_tids) OF
              1: BEGIN

                 FOR k=0,N_ELEMENTS(cAP_struct.iu_pot_tids)-1 DO BEGIN
                    TIMEBAR,cAP_struct.iu_pot_tids[k],THICK=3.0,COLOR=red
                 ENDFOR

              END
              2: BEGIN

                 nHjar = N_ELEMENTS(cAP_struct.iu_pot_tids[0,*])
                 ;; colours = GENERATE_LIST_OF_RANDOM_COLORS(nHjar)
                 colours = [poiple,darkRed,green,blue,fuschia,LIST_TO_1DARRAY(GENERATE_LIST_OF_RANDOM_COLORS(5))]

                 FOR k=0,nHjar-1 DO BEGIN
                    TIMEBAR,S2T(cAP_struct.iu_pot_tids[0,k]),THICK=2.5,COLOR=colours[k]
                    TIMEBAR,S2T(cAP_struct.iu_pot_tids[1,k]),THICK=2.5,COLOR=colours[k]
                 ENDFOR
                 
              END
           ENDCASE

        ENDIF

     ENDIF

     CASE 1 OF
        KEYWORD_SET(save_png): BEGIN
           CGPS_CLOSE
        END
        KEYWORD_SET(save_ps): BEGIN
           PCLOSE
        END
        ELSE:
     ENDCASE
  ENDIF

  RETURN
END

