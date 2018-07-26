;2018/07/23
PRO JOURNAL__20180723__TRY_DIFFERENT_WAY, $
   SAVE_PS=save_ps

  COMPILE_OPT IDL2,STRICTARRSUBS

  remake_file = 0

  t=0
  dat = get_fa_ies(t,/st)
  if dat.valid eq 0 then begin
     print,' ERROR: No FAST ion survey data -- get_fa_ies(t,/st) returned invalid data'
     return
  endif

  makeZeroThreshEFlux = 1e6
  makeZeroVal = 0.001

  up_aRangeN = [90,270]
  down_aRangeN = [270,90]
  calib = 1
  get_en_spec_pro = 'fa_ies'

  varName = 'eSpec'
  GET_EN_SPEC,get_en_spec_pro,UNITS='eflux', $
              NAME=varName,RETRACE=1,CALIB=calib

  GET_DATA,varName,DATA=eSpec

  nHere = N_ELEMENTS(eSpec.x)
  GET_FA_ORBIT,eSpec.x,/TIME_ARRAY

  GET_DATA,"ORBIT",DATA=orbit
  orbit = orbit.y[nHere/2]

  ;; t1Str = '1998-09-25/00:00:00'
  ;; t2Str = '1998-09-25/00:18:00'

  ;; t1 = S2T(t1Str)
  ;; t2 = S2T(t2Str)

  loadDir = "/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/"

  savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-conic_vs_flux_ratios-"
  ;; saveTStr = STRMID(t1Str,0,10)                      + "__" $
  ;;            + (STRMID(t1Str,11,8)).Replace(":","_") + "-"  $
  ;;            + (STRMID(t2Str,11,8)).Replace(":","_")
  saveSuff = ".sav"

  ;; fName = savePref+saveTStr+saveSuff

  get_data,'ILAT',data=data

  if (n_elements(data.y) le 0) then return

  bb = where(data.y gt 10,nn)
  if (nn gt 0) then BEGIN
     tlimitN=[data.x[bb[0]],data.x[bb[nn-1L]]]
     tlimitNStr=T2S(tlimitN)
     saveTStrN = STRMID(tlimitNStr[0],0,10)                      + "__" $
                 + (STRMID(tlimitNStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tlimitNStr[1],11,8)).Replace(":","_")
     fNameN = savePref+saveTStrN+saveSuff
     PRINT,fNameN
  ENDIF

  bb = where(data.y lt -10,nn)
  if (nn gt 0) then BEGIN
     tlimitSouth=[data.x[bb[0]],data.x[bb[nn-1L]]]
     tlimitSouth=T2S(tlimitSouth)
     saveTStrS = STRMID(tlimitSouthStr[0],0,10)                      + "__" $
                 + (STRMID(tlimitSouthStr[0],11,8)).Replace(":","_") + "-"  $
                 + (STRMID(tlimitSouthStr[1],11,8)).Replace(":","_")
     fNameS = savePref+saveTStrS+saveSuff

     PRINT,fNameS
  ENDIF
  
  upVarNameN = 'eSpecUpN'
  downVarNameN = 'eSpecDownN'
  allAngleVarNameN = 'eSpecN'

  IF FILE_TEST(loadDir+fNameN) AND ~KEYWORD_SET(remake_file) THEN BEGIN
     PRINT,"Restoring " + fNameN + "..."
     RESTORE,loadDir+fNameN

     varName = upVarNameN
     STORE_DATA,varName,DATA=eSpecUpN

     varName = downVarNameN
     STORE_DATA,varName,DATA=eSpecDownN

     varName = allAngleVarNameN
     STORE_DATA,varName,DATA=eSpecN

  ENDIF ELSE BEGIN

     ;; Collect data into tplot structures

     ;; Ion differential energy flux - energy spectrograms

     GET_FA_RATIO_OF_ION_SPECTROGRAMS, $
        T1=tlimitN[0], $
        T2=tlimitN[1], $
        DOWNVARNAME=downVarNameN, $
        UPVARNAME=upVarNameN, $
        ALLANGLEVARNAME=allAngleVarNameN, $
        UP_ARANGE=up_aRangeN, $
        DOWN_ARANGE=down_aRangeN, $
        UNITS=units, $
        GET_EN_SPEC_PRO=get_en_spec_pro, $
        CALIB=calib, $
        MAKEZEROTHRESHEFLUX=makeZeroThreshEFlux, $
        MAKEZEROVAL=makeZeroVal, $, $
        OUT_DOWNESPEC=eSpecDownN, $
        OUT_UPESPEC=eSpecUpN, $
        OUT_ALLANGLEESPEC=eSpecN, $
        OUT_UPDOWNRATIOSPEC=upDownRatioSpecN, $
        OUT_UPALLRATIOSPEC=upAllRatioSpecN

     PRINT,"Saving " + fNameN + " ..."
     SAVE,eSpecN,eSpecUpN,eSpecDownN,upDownRatioSpecN,upAllRatioSpecN, $
          eSpecS,eSpecUpS,eSpecDownS,upDownRatioSpecS,upAllRatioSpecS, $
          up_aRangeS,down_aRangeS, $
          up_aRangeN,down_aRangeN, $
          FILENAME=loadDir+fNameN

  ENDELSE

  IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
     DOWNESPEC=eSpecDownN, $
     UPESPEC=eSpecUpN, $
     ALLANGLEESPEC=eSpecN, $
     UPDOWNRATIOSPEC=upDownRatioSpecN, $
     OUT_EBOUND=eBound

  varName = upVarNameN
  OPTIONS,varName,'spec',1	
  ZLIM,varName,1e4,1e8,1
  ylim,varName,3,40000,1
  options,varName,'ytitle','Upward ions!C!CEnergy (eV)'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',2
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  varName = downVarNameN
  options,varName,'spec',1	
  zlim,varName,1e4,1e8,1
  ylim,varName,3,40000,1
  options,varName,'ytitle','Downward ions !C!CEnergy (eV)'
  options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  options,varName,'x_no_interp',1
  options,varName,'y_no_interp',1
  options,varName,'panel_size',2
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  ;; varName = allAngleVarNameN
  ;; options,varName,'spec',1	
  ;; zlim,varName,1e4,1e8,1
  ;; ylim,varName,3,40000,1
  ;; options,varName,'ytitle','ions (all angles)!C!CEnergy (eV)'
  ;; options,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
  ;; options,varName,'x_no_interp',1
  ;; options,varName,'y_no_interp',1
  ;; options,varName,'panel_size',2
  ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  varName = "ratioN"
  data = upAllRatioSpecN
  STORE_DATA,varName,DATA=data  
  OPTIONS,varName,'spec',1
  YLIM,varName,4,24000,1
  ZLIM,varName,0.1,100,1
  OPTIONS,varName,'ytitle',"Ion energy (eV)"
  OPTIONS,varName,'ztitle','Up/all-angle ion eFlux'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1

  ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  upDownRatioSpecVarName = "ratioNUpDown"
  varName = upDownRatioSpecVarName
  data = upDownRatioSpecN
  STORE_DATA,varName,DATA=data  
  OPTIONS,varName,'spec',1
  YLIM,varName,4,24000,1
  ZLIM,varName,0.1,1000,1
  OPTIONS,varName,'ytitle',"Ion energy (eV)"
  OPTIONS,varName,'ztitle','Up/Down ion eFlux'
  OPTIONS,varName,'x_no_interp',1
  OPTIONS,varName,'y_no_interp',1
  options,varName,'panel_size',2

  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

  hvit             = 255
  eBoundVarName = "eBound"
  ;; potLStyle = 1 ;dotted
  ;; potLStyle = 2           ;dashed
  potLStyle = 0                 ;solid
  potColor  = hvit
  ;; potLStyle = 3 ;dash dot
  ;; potLStyle = 4 ;dash dot dot
  STORE_DATA,eBoundVarName,DATA={x:eBound.x,y:eBound.y}
  OPTIONS,eBoundVarName,'LINESTYLE',potLStyle
  OPTIONS,eBoundVarName,'colors',potColor
  OPTIONS,eBoundVarName,'thick',3.0

  IF ~KEYWORD_SET(save_ps) THEN BEGIN

     wInd = 0
     WINDOW,wInd,XSIZE=1200,YSIZE=600

  ENDIF ELSE BEGIN

     oldSize = !P.CHARSIZE
     oldSymSize = !P.SYMSIZE

     !P.CHARSIZE = 3.4
     !P.SYMSIZE  = 2.0

     psNavnN = fNameN.Replace(".sav","")

     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/outflow_summaries'
     ENDIF

     count = 0
     WHILE FILE_TEST(plotDir+psNavnN+(KEYWORD_SET(eps) ? '.eps' : '.ps')) DO BEGIN
        count++
        psNavnN = STRING(FORMAT='(A0,A0,"-",I02)', $
                         savePref, $
                         saveTStrN, $
                         count)
     ENDWHILE
     
     POPEN,plotDir+psNavnN, $
           ;; /LAND, $
           /PORT, $
           ;; ASPECT=0.625, $
           FONT=-1, $
           ENCAPSULATED=eps     ;,XSIZE=8,YSIZE=7
     DEVICE,/PALATINO,FONT_SIZE=3

  ENDELSE

  ctNum            = 43
  LOADCT2,ctNum

  ;; tplot_OPTIONS,'region',[0.,0.5,1.0,1.0]
  ;; loadct2,39
  TPLOT,REVERSE(tPlt_vars),VAR=['ALT','ILAT','MLT'], $
        WINDOW=wInd ;; , $
  ;; TRANGE=[t1,t2]


  TPLOT_PANEL,VARIABLE=upDownRatioSpecVarName,OPLOTVAR=eBoundVarName

  CASE 1 OF
     KEYWORD_SET(save_png): BEGIN
        CGPS_CLOSE
     END
     KEYWORD_SET(save_ps): BEGIN
        !P.CHARSIZE = TEMPORARY(oldSize)
        !P.SYMSIZE  = TEMPORARY(oldSymSize)

        PCLOSE
     END
     ELSE:
  ENDCASE

; Ion differential energy flux - angle spectrograms

  ;; get_pa_spec,get_en_spec_pro,units=uniter, $
;; NAMe='ion_low',energy=[50,1000],retrace=1,/shift90,CALIB=calib
  ;; options,'ion_low','spec',1	
  ;; zlim,'ion_low',1e4,1e8,1
  ;; ylim,'ion_low',-100,280,0
  ;; options,'ion_low','ytitle','ions .05-1 keV!C!C Pitch Angle'
  ;; options,'ion_low','ztitle','eV/cm!U2!N-s-sr-eV'
  ;; options,'ion_low','x_no_interp',1
  ;; options,'ion_low','y_no_interp',1
  ;; options,'ion_low','panel_size',2

  ;; get_pa_spec,get_en_spec_pro,units=uniter, $
;; NAMe='ion_high',energy=[1000,40000],retrace=1,/shift90,CALIB=calib
  ;; options,'ion_high','spec',1	
  ;; zlim,'ion_high',1e4,1e8,1
  ;; ylim,'ion_high',-100,280,0
  ;; options,'ion_high','ytitle','ions >1 keV!C!C Pitch Angle'
  ;; options,'ion_high','ztitle','eV/cm!U2!N-s-sr-eV'
  ;; options,'ion_high','x_no_interp',1
  ;; options,'ion_high','y_no_interp',1
  ;; options,'ion_high','panel_size',2

END
