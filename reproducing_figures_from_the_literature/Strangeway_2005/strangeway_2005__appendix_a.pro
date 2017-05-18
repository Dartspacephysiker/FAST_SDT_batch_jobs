;09/28/16
;Trying to reproduce Strangeway's AC Poynting flux
;"But really, Spence—what's going on in this routine?"
;You want to know? Like … for real what's going on? 'Cause I'm serious.
;"No, I know. So am I."
;Don't mess with me, now.
;"I'm serious! Just a summary or a few words, that's all. It's not like a Trump/Russia sort of thing and I'm Trump and you're
;Comey, all right? Cool down, and tell me what's up!"
;All right, check it.
;
;SUMM'R DAT ROUTEEN
;==================
;The idea is to use ESA intervals to pick up
PRO STRANGEWAY_2005__APPENDIX_A, $
   TPLT_VARS=tPlt_vars, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   SCREEN_PLOT=screen_plot, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps

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
; if (n_elements(tPlt_vars) gt 0) then tplot,tPlt_vars,var=['ALT','ILAT','MLT']
; if (n_elements(tlimit_north) gt 0) then tlimit,tlimit_north  ; northern hemisphere
; if (n_elements(tlimit_south) gt 0) then tlimit,tlimit_south  ; southern hemisphere

; if running interactively
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

  ;; outflowMinLog10 = 6
  ;; ptsMinOutflow   = 60
  ;; allowableGap    = 3 ;seconds

  CASE 1 OF
     KEYWORD_SET(use_eField_fit_variables): BEGIN
        eAV_variable = 'EFIT_ALONG_V'
        eNB_variable = 'EFIT_NEAR_B'
     END
     ELSE: BEGIN
        eAV_variable = 'E_ALONG_V'
        eNB_variable = 'E_NEAR_B'
     END
  ENDCASE

  @tplot_com ;provides data_quants variable

  @startup

  @strway_stuff

  ON_ERROR,0

  ;;From UCLA_MAG_DESPIN:
  ;;"   Field-aligned coordinates defined as: 
  ;;"   z-along B, y-east (BxR), x-nominally out"
  ;;    (ind 2)    (ind 1)       (ind 0)

  magInd = 1

  normColorI   = (KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) ? 0 : 255

; Step 1 - DC Mag data

  @strangeway_2005__defaults__appendix_a.pro
  
  nn           = N_ELEMENTS(data_quants)

  if (nn GT 1) THEN for n = nn-1L,1L,-1L do STORE_DATA,data_quants(n).name,/delete

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 1 - DC Mag data

  UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi

  IF (N_ELEMENTS(orbit) GT 0) THEN BEGIN

     orbString           = STRING(FORMAT='(I0)',orbit)
     outPlotName        += '--' + orbString

     ;;Get time and Je info
     check =  LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit, $
                                           RETURN_STRUCT=return_struct, $
                                           /USE_DUPELESS_FILES, $
                                           JE_OUT=je_pristine, $
                                           TIME_RANGES_OUT=time_ranges, $
                                           TIME_RANGE_INDICES_OUT=time_range_indices, $
                                           NINTERVALS_OUT=number_of_intervals, $
                                           ;; OUT_JEFILENAME=jeFileName, $
                                           ;; CLEAN_DUPES=clean_dupes, $
                                           /QUIET)

     ;;Checkups
     IF check[0] EQ -1 THEN BEGIN
        PRINT,"Couldn't get Je time info for orbit " + orbString + '!!!'
        PRINT,"Out ..."
        RETURN
     ENDIF

     IF SIZE(je_pristine,/TYPE) NE 8 THEN BEGIN
        PRINT,"Apparently no ESA interval information for this orbit. Returning ..."
        RETURN
     ENDIF

     IF N_ELEMENTS(je_pristine.x) LE 1 THEN BEGIN
        PRINT,'Insufficient data to do anything awesome! Returning ...'
        RETURN
     ENDIF

     ;;Clean up based on ILAT
     GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL
     GET_DATA,'ILAT',DATA=ilat
     IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
        PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
        RETURN
     ENDIF
     IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
        PRINT,'Invalid ephemeris data for orb ' + orbString + '. Returning ...'
        RETURN
     ENDIF

     ;;Make sure we have data where we want it.
     keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        PRINT,'No data above min ILAT. Out!'
        RETURN
     ENDIF


     je_tBounds = [je_pristine.x[0],je_pristine.x[-1]]
     PRINT,FORMAT='(A0,T35,A0,", ",A0)',"Je_pristine beginning/end : ", $
           TIME_TO_STR(je_tBounds[0],/MSEC), $
           TIME_TO_STR(je_tBounds[1],/MSEC)


;  if orbit > 9936 return (temporary fix)

     if (orbit gt 9936) then begin

        PRINT,""
        PRINT,"BATCH_SUMMARY DISABLED FOR ORBITS > 9936, SORRY"
        PRINT,""
        return

     endif

; got mag data, set time limits, delete unused tplot variables, set tPlt_vars

     STORE_DATA,'BDATA',/delete
     STORE_DATA,'BFIT',/delete 
     STORE_DATA,'Bx_sp',/delete
     STORE_DATA,'By_sp',/delete
     STORE_DATA,'Bz_sp',/delete
     STORE_DATA,'Bx_sc',/delete
     STORE_DATA,'By_sc',/delete
     STORE_DATA,'Bz_sc',/delete
     STORE_DATA,'Bx_sp_sm',/delete
     STORE_DATA,'By_sp_sm',/delete
     STORE_DATA,'Bz_sp_sm',/delete
     STORE_DATA,'B_gei',/delete
     STORE_DATA,'B_sm',/delete
     STORE_DATA,'dB_sc',/delete
     STORE_DATA,'dB_gei',/delete
     STORE_DATA,'spin_freq',/delete
     STORE_DATA,'spin_phase',/delete
     STORE_DATA,'TORQ_X',/delete
     STORE_DATA,'TORQ_Y',/delete
     STORE_DATA,'TORQ_Z',/delete
     STORE_DATA,'BX_DEL',/delete
     STORE_DATA,'BY_DEL',/delete
     STORE_DATA,'BZ_DEL',/delete
     STORE_DATA,'BFIX',/delete
     STORE_DATA,'TW_ZX',/delete
     STORE_DATA,'TW_ZY',/delete
     STORE_DATA,'TW_YY',/delete
     STORE_DATA,'TW_YX',/delete
     STORE_DATA,'O_X',/delete
     STORE_DATA,'O_Y',/delete
     STORE_DATA,'B_model_old',/delete
     STORE_DATA,'Delta_B_model',/delete
     STORE_DATA,'despun_to_gei',/delete
     STORE_DATA,'gei_to_sm',/delete
     STORE_DATA,'gei_to_fac',/delete
     STORE_DATA,'gei_to_fac_v',/delete

     GET_DATA,'dB_fac_v',data=data
     t1 = data.x[0]
     t2 = data.x[n_elements(data.x)-1L]
     magz_tBounds = [t1,t2]
     tPlt_vars = 'dB_fac_v'
     OPTIONS,'dB_fac_v','panel_size',2
     OPTIONS,'dB_fac','panel_size',2
     OPTIONS,'dB_sm','panel_size',2

     PRINT,FORMAT='(A0,T35,A0,", ",A0)',"MAG beginning/end : ",TIME_TO_STR(t1,/MSEC),TIME_TO_STR(t2,/MSEC)


     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))

     tPlt_vars = 'dB_fac_v'

     if (keyword_set(screen_plot)) then begin
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     endif


  ENDIF ELSE BEGIN

     PRINT,"Couldn't pick up orb info from UCLA_MAG_DESPIN. OUT!"
     RETURN
  ENDELSE


; step 2 - E field

; JBV, 2011/05/22.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
  sdt_idx = get_sdt_run_idx()

  prog = GETENV('FASTBIN') + '/showDQIs'
  IF ((sdt_idx GE 0) AND (sdt_idx LT 100)) THEN BEGIN
     IF (sdt_idx GE 10) THEN BEGIN
        sidstr = STRING(sdt_idx, format='(I2)')
     endif else begin
        sidstr = STRING(sdt_idx, format='(I1)')
     endelse
     SPAWN, [prog, sidstr], result, /noshell
  endif else begin
     SPAWN, prog, result, /noshell
  endelse


  b = WHERE (STRPOS(result,'V1-V4_S') GE 0,nb4)
  IF (nb4 GT 0) THEN IF STRPOS(RESULT(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb4 = 0
  b = where (strpos(result,'V1-V2_S') ge 0,nb2)
  IF (nb2 GT 0) THEN IF STRPOS(RESULT(b(0)+1),'Points (cur/aloc): 0       /') ge 0 then nb2 = 0
  IF (nb4 GT 0) THEN v12=GET_FA_FIELDS('V1-V4_S',/ALL) $
  ELSE IF (nb2 GT 0) THEN v12=GET_FA_FIELDS('V1-V2_S',/ALL)

  b = where (strpos(result,'V5-V8_S') ge 0,nb5)
  if (nb5 gt 0) then v58=get_fa_fields('V5-V8_S',/all)

  got_efield = (nb4 gt 0 or nb2 gt 0) and nb5 gt 0

  if (got_efield) then begin

                                ; despin e field data

     FA_FIELDS_DESPIN,v58,v12,/SHADOW_NOTCH,/SINTERP

  ENDIF ELSE BEGIN
     PRINT,"Couldn't get E-field data! Out ..."
     RETURN
  ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 4 - Electron junk, AND
  ;; Step 5 - Ion flux

  ;;Handle ion adjustment
  IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]

  ;;Just get them all
  ;; t           = 0.0D
  ;; tmp         = GET_FA_EES_C(t,/EN)
  ;; IF tmp.valid EQ 0 THEN BEGIN
  ;;    PRINT,'Junk electron data'
  ;;    RETURN
  ;; ENDIF
  ;; last_index  = LONG(tmp.index)
  ;; t1Ptcl      = 0.0D
  ;; t2Ptcl      = 0.0D
  ;; temp        = GET_FA_EES(t1Ptcl,INDEX=0.0D)
  ;; temp        = GET_FA_EES(t2Ptcl,INDEX=DOUBLE(last_index))

  ;; PRINT,FORMAT='(A0,T35,A0,", ",A0)',"Particle beginning/end : ",TIME_TO_STR(,/MSEC),TIME_TO_STR(t2ptcl,/MSEC)


     

  types_2dt = ['je_2d_fs','j_2d_fs','j_2d_fs']
  routs_2dt = ['fa_ees_c','fa_ees_c','fa_ies_c']
  names_2dt = ['JEe','Je','Ji']

  ngsgn_2dt = [0,0,1]
  enrgy_2dt = [[energy_electrons],[energy_electrons],[energy_ions]]
  titls_2dt = ['Electron!CEnergy Flux!CmW/(m!U2!N)', $
               'Electron Flux!C!C#/(cm!U2!N-s)', $
               'Ion Flux!C#/(cm!U2!N-s)']
  lims_2dt  = [[-1.,6.,0],[-5.e9,1.5e10,0],[-1.e9,6.e9,0]]
  nFlux_2dt = MAKE_ARRAY(N_ELEMENTS(types_2dt),/LONG)
  of_pos    = [0,0,1]
  pr_pos    = [1,1,0]

  FOR ll=0,N_ELEMENTS(types_2dt)-1 DO BEGIN

     tmpType = types_2dt[ll]
     tmpRout = routs_2dt[ll]
     tmpName = names_2dt[ll]
     tmpNrg  = enrgy_2dt[*,ll]
     tmpTitl = titls_2dt[ll]
     tmpLims = lims_2dt[*,ll]

     GET_FA_PARTICLE_2DT,tmpType,tmpRout, $
        T1=je_tBounds[0], $
        T2=je_tBounds[1], $
        NAME=tmpName, $
        ENERGY=tmpNrg, $
        ;; ERANGE=er, $
        ;; EBINS=ebins, $
        ;; ANGLE=an, $
        ;; ARANGE=ar, $
        ;; BINS=bins, $
        ;; GAP_TIME=gap_time, $ 
        ;; NO_DATA=no_data, $
        ;; BKG=bkg, $
        ;; MISSING=missing, $
        ;; FLOOR=floor, $
        /CALIB, $
        TITLE=tmpTitl, $
        LIMS=tmpLims, $
        OUTFLOW_POSITIVE=of_pos[ll], $
        PRECIPITATION_POSITIVE=pr_pos[ll]
        
     ;; tmpDatStruct = CREATE_STRUCT(tmpDatStruct,tmpName+'_time',tmp.x,tmpName,tmp.y)
     ;; tmp1sStruct  = CREATE_STRUCT(tmp1sStruct,tmpName,doDat)

  ENDFOR

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Step 6 - VLF data

  ;;DSP_V5-V8HG or DSP_V5-V8

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
  b = WHERE(STRPOS(result,'DspADC_V5-V8HG') GE 0,ndsphg)
  IF (ndsphg GT 0) THEN BEGIN
     IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN ndsphg = 0
  ENDIF
  b = WHERE((STRPOS(result,'DspADC_V5-V8') GE 0) AND $
            (STRPOS(result,'DspADC_V5-V8HG') LT 0),ndsp)
  IF (ndsp GT 0) THEN BEGIN
     IF STRPOS(result(b(0)+1),'Points (cur/aloc): 0       /') GE 0 THEN ndsp = 0
  ENDIF

  if (ndsphg GT 0) THEN BEGIN
     data = GET_FA_FIELDS('DspADC_V5-V8HG',/all) 
  ENDIF else BEGIN
     IF (ndsp GT 0) THEN BEGIN
        data = GET_FA_FIELDS('DspADC_V5-V8',/all)
     ENDIF
  ENDELSE
  ndsp = (ndsp GT 0) or (ndsphg GT 0)

  IF nDSP EQ 0 THEN BEGIN
     PRINT,'Junk DSP data'
     RETURN
  ENDIF

  tmp_i = WHERE((data.time GE (je_tBounds[0]-tBuf)) AND $
                (data.time LE (je_tBounds[1]+tBuf)),nTmp)
  IF nTmp GT 1 THEN BEGIN

     nDSP = nTmp

     data   = {x:data.time[tmp_i], y:ALOG10(data.comp1[tmp_i,*]), v:data.yaxis}
     STORE_DATA,'DSP_V5-V8', DATA=data
     dlimit = {spec:1, ystyle:1, yrange:[0.1, 16.0], zrange:[-14,-4], $
               ytitle:'AC E 55m!C!C(kHz)', ylog:1, $
               ztitle: '(V/m)!U2!N/Hz', panel_size:2}
     STORE_DATA,'DSP_V5-V8', dlimit=dlimit
     OPTIONS,'DSP_V5-V8','x_no_interp',1
     OPTIONS,'DSP_V5-V8','y_no_interp',1

     ;;  look for big jumps in time - blank these

     GET_DATA,'DSP_V5-V8',DATA=data
     dt = data.x[1:*]-data.x[0:*]
     ntimes=N_ELEMENTS(data.x)
     bg = where (dt GT 300, ng)
     if (ng GT 0) THEN BEGIN
        bbb = bg-1
        if (bbb[0] lt 0) THEN bbb[0] = 0
        add_tag=[data.x[bg]+dt[bbb],data.x[bg+1]-dt[bbb]]
        flag_dat = fltarr(ng*2)+!values.f_nan
        new_tag = [data.x,add_tag]
        tsort = sort(new_tag-new_tag[0])
        nvec=N_ELEMENTS(data.y)/ntimes
        new_dat = fltarr(N_ELEMENTS(new_tag),nvec)
        for nv = 0,nvec-1 do BEGIN
           new_dat[*,nv] = [data.y[*,nv],flag_dat]
           new_dat[*,nv] = new_dat[tsort,nv]
        endfor
        DATA={x:new_tag[tsort],y:new_dat,v:data.v}
        STORE_DATA,'DSP_V5-V8',DATA=data
     endif

     if (N_ELEMENTS(tPlt_vars) eq 0) THEN tPlt_vars=['DSP_V5-V8'] else tPlt_vars=['DSP_V5-V8',tPlt_vars]

     if (keyword_set(screen_plot)) THEN BEGIN
        loadct2,40
        tplot,tPlt_vars,var=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     endif

     ;;Now integrate
     data.y    = 10.^data.y
     data.v   *= 1000.

     integData = MAKE_ARRAY(N_ELEMENTS(data.x),VALUE=0.)

     tmpF_i = LINDGEN(N_ELEMENTS(data.v)-1)+1
     FOR m=0,N_ELEMENTS(data.x)-1 DO BEGIN

        ;;"Intergrate," as some have it, and apply test
        integData[m] = INT_TABULATED(data.v[tmpF_i],data.y[m,tmpF_i])
     ENDFOR

     ;;Get rid of the square so that units are V/m
     integData = SQRT(integData)

     data      = {x:data.x, $
                  y:integData}

     PRINT,'SMOOTHDSP'
     DSP = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           data, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           /USE_DOUBLE_STREAKER, $
           ONESEC_TS=tS_1s)


     ;; STORE_DATA,'DSP_integ',DATA={x:data.x,y:data.y}
     STORE_DATA,'DSP_integ',DATA={x:DSP.x,y:dsp.DC+dsp.AC}
     dlimit = {ystyle:1, yrange:[0.0,0.05], $
               ytitle:'ELF Amplitude (V/m)', $
               panel_size:3}
     STORE_DATA,'DSP_integ', dlimit=dlimit
     OPTIONS,'DSP_integ','x_no_interp',1
     OPTIONS,'DSP_integ','y_no_interp',1


  ENDIF ELSE BEGIN

  ENDELSE


  ;;Now loop over stuff

  structList             = LIST()
  FOR jj=0,number_of_intervals-1 DO BEGIN


     itvlString     = STRCOMPRESS(jj,/REMOVE_ALL)

     tmpPlotName    = outPlotName + '__itvl_' + itvlString

     t1 = time_ranges[jj,0]
     t2 = time_ranges[jj,1]

     tmp_je_indices = time_range_indices[jj,*]

     ;;Clean up based on ILAT
     ;; GET_FA_ORBIT,je_pristine.x,/TIME_ARRAY,/DEFINITIVE,/ALL
     GET_DATA,'ILAT',DATA=ilat
     IF SIZE(ilat,/TYPE) NE 8 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF
     IF N_ELEMENTS(ilat.y) LE 1 THEN BEGIN
        PRINT,'Invalid ephemeris data for interval ' + itvlString + '. Skipping ...'
        CONTINUE
     ENDIF

     ;;Make sure we have data where we want it.
     keep  = WHERE(ABS(ilat.y) GE minILAT,nKeep)
     IF nKeep LE 1 THEN BEGIN
        ;; PRINT,'No data above min ILAT. Out!'
        ;; RETURN
        PRINT,'No data above min ILAT. Skipping this interval ...'
        CONTINUE
     ENDIF
     
     ;;Trim time series, if necessary
     IF nKeep LT N_ELEMENTS(je_pristine.x) THEN BEGIN

        closest1 = MIN(ABS(t1-je_pristine.x[keep]),tmpII_t1)
        closest2 = MIN(ABS(t2-je_pristine.x[keep]),tmpII_t2)

        ;;If more than 30 s from previous mark, we're in doubt
        IF (closest1 GT 600) OR (closest2 GT 600) THEN BEGIN
           PRINT,'Either t1 or t2 is more than 10 minutes from the previous mark ...'
           PRINT,'Questionable, indeed. Skipping this interval ...'
           CONTINUE
        ENDIF

        IF tmpII_t1 EQ tmpII_t2 THEN BEGIN 
           PRINT,'t1 and t2 are the same!' 
           PRINT,'Questionable, indeed. Skipping this interval ...' 
           CONTINUE 
        ENDIF

        t1             = je_pristine.x[keep[tmpII_t1]]
        t2             = je_pristine.x[keep[tmpII_t2]]
        tmp_je_indices = [keep[tmpII_t1],keep[tmpII_t2]]

     ENDIF

     je_tmp_tBounds    = [t1,t2]

     ;;Interp time series
     tS_1s = DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1))


     ;; Step 3 - Poynting flux
     GET_DATA,'dB_fac_v',data=magData
     GET_DATA,eAV_variable,data=eAlongV

     mintime = MIN(ABS(je_tmp_tBounds[0]-magData.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-magData.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable mag data here. Skipping interval ...'
        CONTINUE
     ENDIF

     ;;   From UCLA_MAG_DESPIN:
     ;;   "Field-aligned velocity-based coordinates defined as: "
     ;;   "z (ind 2)-along B, 
     ;;    y (ind 1)-cross track (BxV), 
     ;;    x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)
     magx = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,0]} 
     magy = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,2]} 
     magz = {x:magData.x[ind1:ind2], $
             y:magData.y[ind1:ind2,1]}

     ;;E-field trim
     mintime = MIN(ABS(je_tmp_tBounds[0]-eAlongV.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-eAlongV.x),ind2)

     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable eAlongV data here. Skipping interval ...'
        CONTINUE
     ENDIF

     eAlongV ={x:eAlongV.x[ind1:ind2], $
               y:eAlongV.y[ind1:ind2]}


     ;;DSP trim
     ;;And DSP
     mintime = MIN(ABS(je_tmp_tBounds[0]-dsp.x),ind1)
     mintime = MIN(ABS(je_tmp_tBounds[1]-dsp.x),ind2)


     IF ind1 EQ ind2 THEN BEGIN
        PRINT,'No usable DSP data here. Skipping interval ...'
        CONTINUE
     ENDIF

     tmpDSP ={x:DSP.x[ind1:ind2], $
              DC:DSP.DC[ind1:ind2], $
              AC:DSP.AC[ind1:ind2]}

     ;; magx = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,0])}

     ;; magy = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,2])}

     ;; magz = {x:magData.x, $
     ;;         y:REFORM(magData.y[*,magInd])}


     dBv = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magx, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)
     dBB = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magy, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)
     dBp = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           magz, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)

     eAV = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
           eAlongV, $
           INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
           ONESEC_TS=tS_1s)

     IF KEYWORD_SET(include_E_near_B) THEN BEGIN

        GET_DATA,eNB_variable,DATA=eNearB

        mintime = MIN(ABS(je_tmp_tBounds[0]-eNearB.x),ind1)
        mintime = MIN(ABS(je_tmp_tBounds[1]-eANearB.x),ind2)

        IF ind1 EQ ind2 THEN BEGIN
           PRINT,'No usable eNearB data here. Excluding eNearB ...'
           include_E_near_B = 0
           full_pFlux       = 0
        ENDIF ELSE BEGIN

           eNearB ={x:eNearB.x[ind1:ind2], $
                    y:eNearB.y[ind1:ind2]}

           eNB = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                 eNearB, $
                 INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
                 ONESEC_TS=tS_1s)

        ENDELSE

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Poynting fluxes!

     IF KEYWORD_SET(full_pFlux) THEN BEGIN


        pFBHigh = dBp.AC*eAV.AC/mu_0 ;Poynting flux along B
        pFPHigh = (eNB.AC*dBv.AC - $
                   1.*dBB.AC*eAV.AC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
        pFVHigh = (-1.)*eNB.AC*dBp.AC/mu_0

        pFBLow = dBp.DC*eAV.DC/mu_0 ;Poynting flux along B
        pFPLow = (eNB.DC*dBv.DC - $
                  1.*dBB.DC*eAV.DC)/mu_0 ;Poynting flux perp to B and to (Bxv)xB

        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
        pFVLow = (-1.)*eNB.DC*dBp.DC/mu_0

        pFVHigh *= 1e-9
        pFVLow  *= 1e-9

     ENDIF ELSE BEGIN

        pFBHigh =       dBp.AC *eAV.AC/mu_0 ;Poynting flux along B
        pFPHigh = (-1.)*dBB.AC*eAV.AC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

        pFBLow =       dBp.DC *eAV.DC/mu_0 ;Poynting flux along B
        pFPLow = (-1.)*dBB.DC*eAV.DC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
        ;;Negative sign comes out of S = 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

     ENDELSE

     pFBHigh *= 1e-9            ;Junk that nano prefix in nT
     pFPHigh *= 1e-9

     pFBLow *= 1e-9             ;Junk that nano prefix in nT
     pFPLow *= 1e-9


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;AC Poynting flux

     tField = dBp.x
     doDat  = pFBHigh
     ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
     ;;    tField = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.0,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     tField   = tField[good_i]
     doDat    = ALOG10(ABS(doDat[good_i]))


     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxHigh',DATA=tmp
     dLimit = {spec:0, $
               ystyle:1, $
               ytitle:'SFlux Wave!C[0.125-0.5 Hz]!C(mW/m!U2!N)', $
               yticks:6, $      
               ylog:0, $
               yrange:[-9,2], $
               ytickv:[-8,-6,-4,-2,0,2], $
               ytickname:['10!U-8!N','10!U-6!N','10!U-4!N', $
                          '10!U-2!N','10!U0!N','10!U2!N'], $
               ;; ylog:1, $
               ;; yrange:[1e-4,1e2], $
               ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $          
               ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
               ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               colors:normColorI ,$
               panel_size:3}
     STORE_DATA,'pFluxHigh',DLIMITS=dLimit
     OPTIONS,'pFluxHigh','x_no_interp',1
     OPTIONS,'pFluxHigh','y_no_interp',1

     IF (n_elements(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxHigh'] $
     ELSE tPlt_vars=['pFluxHigh',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;DC Poynting flux

     tField = dBp.x
     doDat  = pFBLow
     ;; IF KEYWORD_SET(smooth_fields) THEN BEGIN
     ;;    tField = tS_1s
     ;; ENDIF

     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(doDat) AND ABS(doDat) GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     tField   = tField[good_i]
     ;; doDat  = ABS(doDat[good_i])
     doDat  = ALOG10(ABS(doDat[good_i]))

     tmp    = {x:tField, $
               y:doDat}

     STORE_DATA,'pFluxLow',DATA=tmp

     dLimit = {spec:0, $
               ystyle:1, $
               ytitle:'SFlux Wave!C[< 0.125 Hz]!C(mW/m!U2!N)', $
               yticks:7, $      
               ylog:0, $
               yrange:[-4,2], $
               ytickv:[-4,-3,-2,-1,0,1,2], $          
               ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
                          '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               ;; ylog:1, $
               ;; yrange:[1e-4,1e2], $
               ;; ytickv:[1e-4,1e-3,1e-2,1e-1,1e0,1e1,1e2], $          
               ;; ytickname:['10!U-4!N','10!U-3!N','10!U-2!N', $
               ;;            '10!U-1!N','10!U0!N','10!U1!N','10!U2!N'], $
               colors:normColorI,$
               panel_size:3}
     STORE_DATA,'pFluxLow',DLIMITS=dLimit
     OPTIONS,'pFluxLow','x_no_interp',1
     OPTIONS,'pFluxLow','y_no_interp',1

     IF (n_elements(tPlt_vars) EQ 0) THEN tPlt_vars=['pFluxLow'] $
     ELSE tPlt_vars=['pFluxLow',tPlt_vars]

     IF (KEYWORD_SET(screen_plot)) THEN BEGIN
        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tBounds
     ENDIF


; Step 4 - Electron junk

     GET_DATA,'Je',DATA=tmp

     ;; tFlux    = tmp.x
     ;; doDat    = tmp.y
     ;; IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmpJe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
              tmp, $
              INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
              /NO_SEPARATE_DC_AC, $
              /USE_DOUBLE_STREAKER, $
              ONESEC_TS=tS_1s)
        ;; tmp.y = SMOOTH(tmp.y,5)
        ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
        ;; tFlux = tS_1s
     ;; ENDIF
     
     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(tmpJe.y) AND tmpJe.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     ;; tmpJe    = {x:tmpJe.x[good_i],y:ALOG10(tmpJe.y[good_i])}
     ;; tFlux    = tFlux[good_i]
     ;; doDat    = ALOG10(doDat[good_i])

     ;; STORE_DATA,'Je_tmp',DATA={x:tFlux,y:doDat}
     STORE_DATA,'Je_tmp',DATA={x:tmpJe.x[good_i],y:ALOG10(tmpJe.y[good_i])}
     YLIM,'Je_tmp',6,12,0                                                  ; set y limits
     OPTIONS,'Je_tmp','ytitle','Downgoing Elec.!CFlux!C#/cm!U2!N-s)' ; set y title
     OPTIONS,'Je_tmp','panel_size',3                                       ; set panel size
     OPTIONS,'Je_tmp','yticks',7                                           ; set y-axis labels
     OPTIONS,'Je_tmp','ytickv',[6,7,8,9,10,11,12]                         ; set y-axis labels
     OPTIONS,'Je_tmp','ytickname',['10!U6!N','10!U7!N','10!U8!N','10!U9!N','10!U10!N', $
                                   '10!U11!N','10!U12!N'] ; set y-axis labels
     OPTIONS,'Je_tmp','ynozero',1

; Electron energy flux

     ;; t           = 0.0D
     ;; tmp         = GET_FA_EES_C(t,/EN)
     ;; IF tmp.valid EQ 0 THEN BEGIN
     ;;    PRINT,'Junk'
     ;;    RETURN
     ;; ENDIF
     ;; last_index  = LONG(tmp.index)
     ;; t1          = 0.0D
     ;; t2          = 0.0D
     ;; temp        = GET_FA_EES(t1,INDEX=0.0D)
     ;; temp        = GET_FA_EES(t2,INDEX=DOUBLE(last_index))

     ;;Did this up top
     ;; GET_2DT,'je_2d_fs','fa_ees_c',name='JEe',t1=t1,t2=t2,energy=energy_electrons
     GET_DATA,'JEe',DATA=tmp

     ;; tFlux    = tmp.x
     ;; doDat    = tmp.y
     ;; IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     tmpJEe = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
              tmp, $
              INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
              /NO_SEPARATE_DC_AC, $
              /USE_DOUBLE_STREAKER, $
              ONESEC_TS=tS_1s)
        ;; tmp.y = SMOOTH(tmp.y,5)
        ;; doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
        ;; tFlux = tS_1s
     ;; ENDIF
     
     ;;Make all downgoing, pre-log
     good_i   = WHERE(FINITE(tmpJEe.y) AND tmpJEe.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     ;; tmpJEe    = {x:tmpJEe.x[good_i],y:ALOG10(tmpJEe.y[good_i])}
     ;; tFlux    = tFlux[good_i]
     ;; doDat    = ALOG10(doDat[good_i])

     ;; STORE_DATA,'JEe_tmp',DATA={x:tFlux,y:doDat}
     STORE_DATA,'JEe_tmp',DATA={x:tmpJEe.x[good_i],y:ALOG10(tmpJEe.y[good_i])}
     YLIM,'JEe_tmp',-5,1,0                                                  ; set y limits
     OPTIONS,'JEe_tmp','ytitle','Downgoing Elec.!CEnergy Flux!CmW/(m!U2!N)' ; set y title
     OPTIONS,'JEe_tmp','panel_size',3                                       ; set panel size
     OPTIONS,'JEe_tmp','yticks',7                                           ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickv',[-5,-4,-3,-2,-1,0,1]                        ; set y-axis labels
     OPTIONS,'JEe_tmp','ytickname',['10!U-5!N','10!U-4!N','10!U-3!N', $
                                '10!U-2!N','10!U-1!N','10!U0!N','10!U1!N'] ; set y-axis labels
     OPTIONS,'JEe_tmp','x_no_interp',1
     OPTIONS,'JEe_tmp','y_no_interp',1

; Step 5 - Ion flux

     ;; IF (WHERE(orbit EQ strWay_orbs))[0] NE -1 THEN energy_ions[1] = upper_ion_e[orbit]
     ;; GET_2DT,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=energy_ions

     GET_DATA,'Ji',DATA=tmp

     ;; tFlux    = tmp.x
     ;; doDat    = (-1.)*tmp.y
     ;; IF KEYWORD_SET(smooth_fluxes) THEN BEGIN
     ;;    tmp.y = SMOOTH(tmp.y,5)
     ;;    doDat = INTERPOL(tmp.y,tmp.x,tS_1s)
     ;;    tFlux = tS_1s
     ;; ENDIF

     tmpJi = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
             tmp, $
             INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
             /NO_SEPARATE_DC_AC, $
             /USE_DOUBLE_STREAKER, $
             ONESEC_TS=tS_1s)

     ;;Make all upgoing, pre-log
     ;; good_i   = WHERE(FINITE(doDat) AND doDat GT 0.00,nGood, $
     ;;                  COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     good_i   = WHERE(FINITE(tmpJi.y) AND tmpJi.y GT 0.00,nGood, $
                      COMPLEMENT=bad_i,NCOMPLEMENT=nBad)
     ;; tmpJi    = {x:tmpJi.x[good_i],y:ALOG10(tmpJi.y[good_i])}
     ;; tFlux    = tFlux[good_i]
     ;; doDat    = ALOG10(doDat[good_i])

     ;; STORE_DATA,'Ji_tmp',DATA={x:tFlux,y:doDat}
     STORE_DATA,'Ji_tmp',DATA={x:tmpJi.x[good_i],y:ALOG10(tmpJi.y[good_i])}
     YLIM,'Ji_tmp',4,10,0                                               ; set y limits
     OPTIONS,'Ji_tmp','ytitle','Upward Ion!CNumber Flux!C(#/cm!U2!N-s)' ; set y title
     OPTIONS,'Ji_tmp','panel_size',3                                    ; set panel size
     OPTIONS,'Ji_tmp','yticks',7                                        ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickv',[4,5,6,7,8,9,10]                         ; set y-axis labels
     OPTIONS,'Ji_tmp','ytickname',['10!U4!N','10!U5!N','10!U6!N', $
                                   '10!U7!N','10!U8!N','10!U9!N','10!U10!N'] ; set y-axis labels
     OPTIONS,'Ji_tmp','ynozero',1

; STEP 6 - Clean up and return

; determine tlimit_north and tlimit_south also change plot title

     GET_DATA,'LAT',data=data

     if (n_elements(data.y) le 0) then return

     bb = where (data.y gt 10,nn)
     if (nn gt 0) then tlimit_north=[data.x[bb[0]],data.x[bb[nn-1L]]]

     bb = where (data.y lt -10,nn)
     if (nn gt 0) then tlimit_south=[data.x[bb[0]],data.x[bb[nn-1L]]]

     hemisph = GETENV('FAST_ORBIT_HEMISPHERE')

     GET_DATA,'ORBIT',data=data
     nn = N_ELEMENTS(data.y)/2
     orbit = data.y(nn)
     orbit_lab = STRCOMPRESS(STRING(orbit,FORMAT="(i5.4)"),/REMOVE_ALL)
     tplot_OPTIONS,'title','FAST Orbit ' + orbit_lab + ' ' + hemisph

     tPlt_vars=['dB_fac_v','pFluxHigh','pFluxLow','JEe_tmp','Je_tmp','Ji_tmp']

     IF KEYWORD_SET(screen_plot) OR KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF=plotDirSuff
        ENDIF

        IF KEYWORD_SET(save_png) THEN BEGIN
           CGPS_OPEN, plotDir+tmpPlotName+'.ps',FONT=0 ;,XSIZE=4,YSIZE=7
        ENDIF ELSE BEGIN
           IF KEYWORD_SET(save_ps) THEN BEGIN

              POPEN,plotDir+tmpPlotName,/PORT,FONT=-1 ;,XSIZE=4,YSIZE=7
              DEVICE,/PALATINO,FONT_SIZE=8


           ENDIF ELSE BEGIN
              WINDOW,0,XSIZE=600,YSIZE=800
           ENDELSE
        ENDELSE
        
        ;; CASE 1 OF
        ;;    KEYWORD_SET(plot_north): BEGIN
        ;;       tLims = tlimit_north
        ;;    END
        ;;    KEYWORD_SET(plot_south): BEGIN
        ;;       tLims = tlimit_south
        ;;    END
        ;;    ELSE: BEGIN
        ;;       tLims = je_tmp_tBounds
        ;;    END        
        ;; ENDCASE

        LOADCT2,40
        TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'],TRANGE=je_tmp_tBounds


        IF KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps) THEN BEGIN
           PCLOSE
        ENDIF ELSE BEGIN

        ENDELSE

     ENDIF

     ;; IF ~KEYWORD_SET(full_pFlux) THEN BEGIN
     ;;    pFVLow  = MAKE_ARRAY(N_ELEMENTS(pFBLow),/FLOAT)
     ;;    pFVHigh = MAKE_ARRAY(N_ELEMENTS(pFBHigh),/FLOAT)
     ;; ENDIF ELSE BEGIN
     ;;    pFLuxStruct = CREATE_STRUCT(pFluxStruct, $
     ;;                                'v',{x:eAV.x,DC:pFVLow,AC:pFVHigh}, $
     ;;                                'full_pFlux',KEYWORD_SET(full_pFlux))
     ;; ENDELSE

     IF ~KEYWORD_SET(include_E_near_B) THEN BEGIN
        eNB = eAV
        eNB.DC[*] = 0.
        eNB.AC[*] = 0.
     ENDIF

     ;;If the B structs have a common time series, only dBp keeps the x member of its struct
     B_has_common_TS = ARRAY_EQUAL(dBp.x,dBv.x) AND ARRAY_EQUAL(dBp.x,dBB.x) AND ARRAY_EQUAL(dBv.x,dBB.x)

     IF B_has_common_TS THEN BEGIN

        dBp  = {x:dBp.x, $
                DC:dBp.DC, $
                AC:dBp.AC, $
                common_ts:1B}

        dBv  = {DC:dBv.DC, $
                AC:dBv.AC}

        dBB  = {DC:dBB.DC, $
                AC:dBB.AC}

     ENDIF

     ;;If the E structs have a common time series, only dBp keeps the x member of its struct
     E_has_common_TS = ARRAY_EQUAL(eAV.x,eNB.x) AND ARRAY_EQUAL(eAV.x,tmpDSP.x) AND ARRAY_EQUAL(eNB.x,tmpDSP.x)

     IF E_has_common_TS THEN BEGIN

        eAV     = {x:eAV.x, $
                   DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

        eNB     = {DC:eNB.DC, $
                   AC:eNB.AC}

        tmpDSP  = {DC:tmpDSP.DC, $
                   AC:tmpDSP.AC}

     ENDIF ELSE BEGIN

        ;;See if DSP is messing things up
        IF ( N_ELEMENTS(tmpDSP.x) EQ ( N_ELEMENTS(eAV.x) + 1 ) ) AND $
           ARRAY_EQUAL(eAV.x,eNB.x) THEN BEGIN
           
           IF ARRAY_EQUAL(eAV.x,tmpDSP.x[0:-2]) THEN BEGIN
              E_has_common_TS     = 1
              tmpDSP              = {DC:tmpDSP.DC[0:-2], $
                                     AC:tmpDSP.AC[0:-2]}
           ENDIF ELSE BEGIN

              IF ARRAY_EQUAL(eAV.x,tmpDSP.x[1:-1]) THEN BEGIN
                 E_has_common_TS  = 1
                 tmpDSP           = {DC:tmpDSP.DC[1:-1], $
                                     AC:tmpDSP.AC[1:-1]}
              ENDIF

           ENDELSE

        ENDIF
        
        IF E_has_common_TS THEN BEGIN
           eAV  = {x:eAV.x, $
                   DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

           eNB  = {DC:eNB.DC, $
                   AC:eNB.AC}

        ENDIF

     ENDELSE

     ptcl_has_common_TS = ARRAY_EQUAL(tmpJEe.x,tmpJe.x) AND ARRAY_EQUAL(tmpJEe.x,tmpJi.x) AND ARRAY_EQUAL(tmpJe.x,tmpJi.x)

     IF ptcl_has_common_TS THEN BEGIN

        tmpJEe  = {x:tmpJEe.x, $
                   y:tmpJEe.y, $
                   ;; DC:tmpJEe.DC, $
                   ;; AC:tmpJEe.AC, $
                   common_ts:1B}

        tmpJe   = {y:tmpJe.y} ;; DC:tmpJe.DC, $
        ;; AC:tmpJe.AC}

        tmpJi   = {y:tmpJi.y} ;;DC:tmpDSP.DC, $
        ;; AC:tmpDSP.AC}

     ENDIF

     ;;...And if they ALL have the same time series, we're only keeping one
     IF B_has_common_TS AND E_has_common_TS AND ptcl_has_common_TS THEN BEGIN

        dBp     = {x:dBp.x, $
                   DC:dBp.DC, $
                   AC:dBp.AC, $
                   common_ts:1B, $
                   commonest_ts:1B}

        eAV     = {DC:eAV.DC, $
                   AC:eAV.AC, $
                   common_ts:1B}

        tmpJEe  = {y:tmpJEe.y, $
                   ;; DC:tmpJEe.DC, $
                   ;; AC:tmpJEe.AC, $
                   common_ts:1B}
     ENDIF

     tmpStruct = {dB:{p:dBp, $
                      v:dBv, $
                      B:dBB}, $
                  e:{AlongV:eAV, $
                     NearB:eNB, $
                     dsp:tmpDSP, $
                     include_E_near_B:BYTE(KEYWORD_SET(include_E_near_B)), $
                     eField_fit_variables:BYTE(KEYWORD_SET(use_eField_fit_variables))}, $
                  ;; pFlux:{b:{x:eAV.x,DC:pFBLow,AC:pFBHigh}, $
                  ;;        p:{x:eAV.x,DC:pFPLow,AC:pFPHigh}, $
                  ;;        v:{x:eAV.x,DC:pFVLow,AC:pFVHigh}, $
                  ;;        full_pflux:KEYWORD_SET(full_pflux)}, $
                  ptcl:{jEe:tmpJEe, $
                        je:tmpJe, $
                        ji:tmpJi}};, $
                  ;; outflow_i:[[start_i],[stop_i]]}
     
     PRINT,"Adding struct for interval " + itvlString + " in orbit " + orbString + ' ...'
     structList.Add,tmpStruct

  ENDFOR

  IF ~KEYWORD_SET(no_hash_update) THEN BEGIN
     IF FILE_TEST(outDir+hashFile) THEN BEGIN
        PRINT,"Restoring hash file " + hashFile + " ..."
        RESTORE,outDir+hashFile

        CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
           -1: BEGIN
              PRINT,'Adding stuff from orbit ' + orbString + ' ...'
              swHash  = swHash + ORDEREDHASH(orbit,structList)
           END
           ELSE: BEGIN
              PRINT,'Replacing hash entry for orbit ' + orbString + ' ...'
              swHash[orbit] = structList
           END
        ENDCASE

        PRINT,'Saving Strangeway statistics hash ...'
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDIF ELSE BEGIN
        PRINT,'Creating Strangeway statistics hash for orbit ' + orbString + ' ...'
        swHash = ORDEREDHASH(orbit,structList)
        SAVE,swHash,FILENAME=outDir+hashFile
     ENDELSE
  ENDIF

  RETURN


END

