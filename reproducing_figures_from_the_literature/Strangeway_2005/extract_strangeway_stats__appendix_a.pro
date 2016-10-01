;;09/24/16
FUNCTION EXTRACT_STRANGEWAY_STATS__APPENDIX_A, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   FOLD_INTERVALS=fold_intervals, $
   SAVE_PLOTS=save_plots, $
   PLOTDIR=plotDir, $
   PLOTS_PREFIX=plots_prefix, $
   SQUARE_WINDOW=square_window, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2

  @strway_stuff

  ;;Some outflow defaults
  outflowMinLog10 = 6
  nMinOutflow     = 60
  allowableGap    = 3 ;seconds

  ;;Outputs
  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_2005/'
  hashFile     = 'Strangeway_et_al_2005__real_thing--outflow_intervals.sav'

  IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
     hashFile    +='--eFieldFits'
  ENDIF

  IF ( ABS(energy_ions[0] - 4.)   LT 0.01 ) THEN bonusSuff += '--4eV_lower'
  IF ( ABS(energy_ions[1] - 500.) LT 0.01 ) THEN bonusSuff += '--500eV_upper'

  hashFile    += bonusSuff
  outPlotName += bonusSuff

  ;;Update hashfile name and outPlotName
  plotPref = SETUP_STRANGEWAY_BRAMBLES_PLOTPREF($
             USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
             ONLY_FASTSRVY_DATA=only_128Ss_data, $
             INCLUDE_E_NEAR_B=include_E_near_B, $
             FULL_PFLUX_CALC=full_pFlux, $
             FIELDS_INTERP=do_fields_interp, $
             FIELDS_SPLINE=do_fields_spline)
  
  hashFile    += plotPref
  outPlotName += plotPref

  defs = SETUP_STRANGEWAY_STATS__DEFAULTS($
         AVERAGES=averages, $
         INTEGRALS=integrals, $
         NORTH=north, $
         SOUTH=south, $
         DAY=day, $
         NIGHT=night)

  IF FILE_TEST(outDir+hashFile) THEN BEGIN
     PRINT,"Restoring hash file ..."
     RESTORE,outDir+hashFile

  ENDIF ELSE BEGIN
     PRINT,'No swHash here! Returning ...'
     RETURN,-1
  ENDELSE

  maxNElems    = 1e6
  maxNItvls    = 30S

  tmpKey       = (swHash.Keys())[0]
  tmpStruct    = (swHash[tmpKey])[0]

  Btags        = TAG_NAMES(tmpStruct.dB)
  Etags        = TAG_NAMES(tmpStruct.E)
  Htags        = TAG_NAMES(tmpStruct.ptcl)
  ;; Ptags        = TAG_NAMES(tmpStruct.pFlux)
  Ptags        = ['b','p']
  IF KEYWORD_SET(full_pFlux) THEN Ptags = [Ptags,'v']
  
  nBTags       = N_ELEMENTS(Btags)
  nETags       = N_ELEMENTS(Etags)
  nHTags       = N_ELEMENTS(Htags)
  nPTags       = N_ELEMENTS(Ptags)

  orbArr       = MAKE_ARRAY(maxNElems          ,/LONG ,VALUE=0) 
  itvlArr      = MAKE_ARRAY(maxNElems          ,/INTEG,VALUE=0) 
  noENBArr     = MAKE_ARRAY(2,maxNElems        ,/INTEG,VALUE=0) 

  BArr         = MAKE_ARRAY(maxNElems,nBTags   ,/FLOAT,VALUE=0.) 
  EArr         = MAKE_ARRAY(maxNElems,nETags   ,/FLOAT,VALUE=0.) 
  PArr         = MAKE_ARRAY(maxNElems,nPTags   ,/FLOAT,VALUE=0.) 
  HArr         = MAKE_ARRAY(maxNElems,nHTags   ,/FLOAT,VALUE=0.) 

  nCount       = 0
  orbCnt       = 0
  FOREACH value, swHash, key DO BEGIN

     ;;Anything here?
     IF N_ELEMENTS(value[0]) EQ 0 THEN CONTINUE

     ;;How many?
     nItvls    = N_ELEMENTS(value)

     IF nItvls EQ 0 THEN CONTINUE

     FOR k=0,nItvls-1 DO nItvl[k] = N_ELEMENTS(value[k].ptcl.ji.x)

     nThisOrb  = FIX(TOTAL(nItvl))

     ;;Just initialize
     tmpBArr   = 0.
     tmpEArr   = 0.
     tmpHArr   = 0.
     tmpPArr   = 0.

     IF nThisOrb NE 0 THEN BEGIN

        ;;Now loop over intervals within this orbit
        tmptmp        = 0
        FOR k=0,nItvls-1 DO BEGIN

           have_univ_TS  = TAG_EXIST(value.[k].(0).(0),'COMMONEST_TS')

           ;;Pick up fields
           dBp           = value[k].dB.p
           dBv           = value[k].dB.v
           dBB           = value[k].dB.B

           eAV           = value[k].E.AlongV
           eNB           = value[k].E.NearB

           IF ~KEYWORD_SET(full_pFlux) THEN BEGIN

              eNB.DC[*]  = 0.
              eNB.AC[*]  = 0.

           ENDIF ELSE BEGIN
              ;;Keep some stats if we're going to attempt the full pFlux sitiation
              IF ~value[k].E.include_E_Near_B THEN noENBArr[*,orbCnt+k] = [key,k]
           ENDELSE

           ;;Calc Poynting flux

           ;; IF KEYWORD_SET(full_pFlux) THEN BEGIN

           ;;Poynting flux along B
           pFBHigh  = dBp.AC*eAV.AC/mu_0 
           pFBLow   = dBp.DC*eAV.DC/mu_0 

           ;;Poynting flux perp to B and to (Bxv)xB
           pFPHigh  = (eNB.AC*dBv.AC - $
                       1.*dBB.AC*eAV.AC)/mu_0 
           pFPLow   = (eNB.DC*dBv.DC - $
                       1.*dBB.DC*eAV.DC)/mu_0 

           ;;Negative sign comes out of S EQ 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
           pFVHigh  = (-1.)*eNB.AC*dBp.AC/mu_0
           pFVLow   = (-1.)*eNB.DC*dBp.DC/mu_0

           ;; ENDIF ELSE BEGIN

           ;;    pFBHigh  =       dBp.AC *eAV.AC/mu_0 ;Poynting flux along B
           ;;    pFPHigh  = (-1.)*dBB.AC*eAV.AC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;    ;;Negative sign comes out of S EQ 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

           ;;    pFBLow   =       dBp.DC *eAV.DC/mu_0 ;Poynting flux along B
           ;;    pFPLow   = (-1.)*dBB.DC*eAV.DC/mu_0  ;Poynting flux perp to B and to (Bxv)xB
           ;;    ;;Negative sign comes out of S EQ 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system

           ;; ENDELSE

           ;;Junk that nano prefix in nT
           pFBHigh *= 1e-9
           pFPHigh *= 1e-9

           pFBLow  *= 1e-9
           pFPLow  *= 1e-9

           pFVHigh *= 1e-9
           pFVLow  *= 1e-9

           ;;Get outflow intervals
           GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE, $
              value[k].ptcl.ji.x[WHERE(ALOG10(value[k].ptcl.ji.y) GE outflowMinLog10 AND FINITE(value[k].ptcl.ji.y))],0, $
              N=nMinOutflow, $
              GAP_TIME=allowableGap, $
              START_I=start_i, $
              STOP_I=stop_i, $
              STREAKLENS=lens

           tmpBArr   = MAKE_ARRAY(nThisOrb,nBTags   ,/FLOAT,VALUE=0.) 
           tmpEArr   = MAKE_ARRAY(nThisOrb,nETags   ,/FLOAT,VALUE=0.) 
           tmpPArr   = MAKE_ARRAY(nThisOrb,nPTags   ,/FLOAT,VALUE=0.) 
           tmpHArr   = MAKE_ARRAY(nThisOrb,nHTags   ,/FLOAT,VALUE=0.) 

           ;;Loop over B-field array stuff
           FOR l=0,nBTags DO BEGIN

              here_i  = (.eAlongV)

           ENDFOR

           nHere      = N_ELEMENTS(value[k].E.AlongV)

           IF nHere GT 0 THEN BEGIN

              curInds    = [tmptmp:tmptmp+nItvl[k]-1]

              tmp_eAlongV  [curInds] = tmpThing.eAlongV 
              tmp_dB_perp  [curInds] = tmpThing.dB_perp 
              tmp_pFAlongB [curInds] = tmpThing.pFAlongB
              tmp_je       [curInds] = tmpThing.je      
              tmp_jee      [curInds] = tmpThing.jee     
              tmp_ji       [curInds] = tmpThing.ji      
              tmp_dsp      [curInds] = tmpThing.dsp     
              
              tmptmp += nItvl[k]
           ENDIF

        ENDFOR



     ENDIF


     orbCnt++
  ENDFOREACH

  finStruct = {orbit     : orbArr      [0:nCount-1] , $  
               interval  : itvlArr     [0:nCount-1] , $  
               eAlongV   : eAlongVArr  [0:nCount-1] , $  
               dB_perp   : dB_perpArr  [0:nCount-1] , $  
               pFAlongB  : pFAlongBArr [0:nCount-1] , $ 
               je        : jeArr       [0:nCount-1] , $       
               jee       : jeeArr      [0:nCount-1] , $      
               ji        : jiArr       [0:nCount-1] , $
               dsp       : dspArr      [0:nCount-1] }

  sw_i = SORT(finStruct.orbit)
  
  finStruct   = {orbit     : finStruct.orbit    [sw_i], $
                 interval  : finStruct.interval [sw_i], $   
                 eAlongV   : finStruct.eAlongV  [sw_i], $   
                 dB_perp   : finStruct.dB_perp  [sw_i], $   
                 pFAlongB  : finStruct.pFAlongB [sw_i], $
                 je        : finStruct.je       [sw_i], $
                 jee       : finStruct.jee      [sw_i], $
                 ji        : (-1.)*finStruct.ji [sw_i], $
                 dsp       : finStruct.dsp      [sw_i]}

  IF ~KEYWORD_SET(no_plots) THEN BEGIN

     IF N_ELEMENTS(xQuants) EQ 0 THEN BEGIN
        xQuants = [4,5,6,8]
     ENDIF

     plotInfo  = {xQuants       : xQuants, $
                  xTitle        : ["", $
                                   "", $
                                   "$\Delta$B [DC] (nT)", $
                                   "E along V$_{sc}$ [DC] (mV/m)", $
                                   "Poynting Flux [DC] (mW/m^2)", $
                                   "Average Electron Flux (#/cm$^2$/s)", $
                                   "Average Electron Energy Flux (mW/m$^2$)", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "Average ELF amplitude (V/m)"], $
                  xRange        : [[0.,0.], $
                                   [0.,0.], $
                                   [0.,0.], $
                                   [0.,0.], $
                                   [1e-1,1e2], $
                                   [1e7,1e10], $
                                   [1e-2,1e0], $
                                   [1e6,1e10], $
                                   [1e-3,1e-1]], $
                  yTitle        : "Ion Flux (#/cm!U2!N/s)", $
                  yData         : finStruct.ji, $
                  yRange        : [1e6,1e10], $
                  plotNames     : ["", $
                                   "", $
                                   "$dB__vs__ionNumFlux", $
                                   "E_along_V__vs__ionNumFlux", $
                                   "DC_Poynting_flux__vs__ionNumFlux", $
                                   "eNumFlux__vs__ionNumFlux", $
                                   "eFlux__vs__ionNumFlux", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "ELF_amplitude__vs__ionNumFlux"], $
                  canonPref     : 'Strangeway_2005_Appendix_A--', $
                  plotDirSuff   : '/Strangeway_et_al_2005--Appendix_A', $
                  plots_prefix  : (KEYWORD_SET(bonusSuff) ? bonusSuff : '') + $ 
                                  defs.statStr+'--'+defs.sideStr+'--'+defs.hemStr+'--', $
                  verboten      : [0,1,2,3], $
                  navn_verboten : ["Orbit    (ind 0)", $
                                   "Interval (ind 1)", $
                                   "EalongV  (ind 2)", $                               
                                   "dB       (ind 3)"]}


     PLOT_STRANGEWAY_STATS, $
        finStruct, $
        PLOTINFO=plotInfo, $
        OUT_PLOTARR=plotArr, $
        SQUARE_WINDOW=square_window, $
        SAVE_PLOTS=save_plots, $
        PLOTDIR=plotDir

  ENDIF

  RETURN,finStruct

END

