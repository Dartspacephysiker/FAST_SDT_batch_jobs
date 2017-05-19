;;09/24/16
FUNCTION SETUP_TAGNAMES_AND_INDICES,tmpStruct, $
                                    Bind,Eind,Hind,Pind, $
                                    Btags,Etags,Htags,Ptags, $
                                    nBTags,nETags,nHTags,nPTags, $
                                    BPind,BVind,BBind, $
                                    EAVind,ENBind,EDSPind,EIENBind, $
                                    HJEeInd,HJeInd,HJiInd, $
                                    PPind,PVind,PBind,have_included_pFlux

  strucTags    = TAG_NAMES(tmpStruct)

  Bind         = WHERE(STRUPCASE(strucTags) EQ 'DB')
  Eind         = WHERE(STRUPCASE(strucTags) EQ 'E')
  Hind         = WHERE(STRUPCASE(strucTags) EQ 'PTCL')
  Pind         = WHERE(STRUPCASE(strucTags) EQ 'PFLUX')

  IF (Bind[0] EQ -1) THEN BEGIN
     PRINT,"Couldn't find B-field member in this struct! Gotta return ..."
     RETURN,-1
  ENDIF

  IF (Eind[0] EQ -1) THEN BEGIN
     PRINT,"Couldn't find E-field member in this struct! Gotta return ..."
     RETURN,-1
  ENDIF

  IF (Hind[0] EQ -1) THEN BEGIN
     PRINT,"Couldn't find particle member in this struct! Gotta return ..."
     RETURN,-1
  ENDIF

  have_included_pFlux = (Pind[0] NE -1)
  IF ~have_included_pFlux THEN BEGIN
     PRINT,"Couldn't find pFlux in this struct! Telling mom ..."
  ENDIF

  Btags        = TAG_NAMES(tmpStruct.(Bind))
  Etags        = TAG_NAMES(tmpStruct.(Eind))
  Htags        = TAG_NAMES(tmpStruct.(Hind))
  ;; Ptags        = TAG_NAMES(tmpStruct.pFlux)
  Ptags        = ['p','v','b']
  ;; IF KEYWORD_SET(full_pFlux) THEN Ptags = [Ptags,'v']
  
  ;;Specifics for each type
  BPind        = (WHERE(STRUPCASE(Btags) EQ 'P'                ))[0]
  BVind        = (WHERE(STRUPCASE(Btags) EQ 'V'                ))[0]
  BBind        = (WHERE(STRUPCASE(Btags) EQ 'B'                ))[0]

  EAVind       = (WHERE(STRUPCASE(Etags) EQ 'ALONGV'           ))[0]
  ENBind       = (WHERE(STRUPCASE(Etags) EQ 'NEARB'            ))[0]
  EDSPind      = (WHERE(STRUPCASE(Etags) EQ 'DSP'              ))[0]
  EIENBind     = (WHERE(STRUPCASE(Etags) EQ 'INCLUDE_E_NEAR_B' ))[0]

  HJEeInd      = (WHERE(STRUPCASE(Htags) EQ 'JEE'              ))[0]
  HJeInd       = (WHERE(STRUPCASE(Htags) EQ 'JE'               ))[0]
  HJiInd       = (WHERE(STRUPCASE(Htags) EQ 'JI'               ))[0]

  IF have_included_pFlux THEN BEGIN
     PPind     = (WHERE(STRUPCASE(Ptags) EQ 'P'                ))[0]
     PVind     = (WHERE(STRUPCASE(Ptags) EQ 'V'                ))[0]
     PBind     = (WHERE(STRUPCASE(Ptags) EQ 'B'                ))[0]
  ENDIF ELSE BEGIN
     PPind     = -1
     PVind     = -1
     PBind     = -1
  ENDELSE

  ;;How many of each type?
  ;; nBTags       = N_ELEMENTS(Btags)
  ;; nETags       = N_ELEMENTS(Etags)
  ;; nHTags       = N_ELEMENTS(Htags)
  ;; nPTags       = N_ELEMENTS(Ptags)

  nBTags       = ( BPind   GE 0 ? 1 : 0 ) + ( BVind   GE 0 ? 1 : 0 ) + ( BBind    GE 0 ? 1 : 0 )
  nETags       = ( EAVind  GE 0 ? 1 : 0 ) + ( ENBInd  GE 0 ? 1 : 0 ) + ( EDSPind  GE 0 ? 1 : 0 ) 
  nHTags       = ( HJEeind GE 0 ? 1 : 0 ) + ( HJeInd  GE 0 ? 1 : 0 ) + ( HJiInd   GE 0 ? 1 : 0 )
  ;; nPTags       = N_ELEMENTS(Ptags)
  nPTags       = ( PPind   GE 0 ? 1 : 0 ) + ( PVind   GE 0 ? 1 : 0 ) + ( PBind    GE 0 ? 1 : 0 )

  RETURN,0

END

PRO DECLARE_ARRAYS,nOrbs,nBTags,nETags,nPTags,nHTags, $
                   orbArr,itvlArr,noENBArr, $
                   BDCArr,EDCArr,PDCArr, $
                   BACArr,EACArr,PACArr, $
                   HArr, $
                   BPTSArr,EPTSArr, HPTSArr, $
                   BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                   EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                   PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                   BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                   EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                   PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                   HAvg

  maxNElems    = 1e6

  ;;Pointers to time arrays
  BPTSArr      = PTRARR(nBTags)
  EPTSArr      = PTRARR(nETags)
  HPTSArr      = PTRARR(nETags)

  ;;Bro
  orbArr       = MAKE_ARRAY(maxNElems       ,/LONG ,VALUE=0) 
  itvlArr      = MAKE_ARRAY(maxNElems       ,/INTEG,VALUE=0) 
  noENBArr     = MAKE_ARRAY(2,maxNElems     ,/INTEG,VALUE=0) 


  ;;Arrays to store all outflow points
  BDCArr       = MAKE_ARRAY(maxNElems,nBTags,/FLOAT,VALUE=0.) 
  EDCArr       = MAKE_ARRAY(maxNElems,nETags,/FLOAT,VALUE=0.) 
  PDCArr       = MAKE_ARRAY(maxNElems,nPTags,/FLOAT,VALUE=0.) 

  BACArr       = MAKE_ARRAY(maxNElems,nBTags,/FLOAT,VALUE=0.) 
  EACArr       = MAKE_ARRAY(maxNElems,nETags,/FLOAT,VALUE=0.) 
  PACArr       = MAKE_ARRAY(maxNElems,nPTags,/FLOAT,VALUE=0.) 

  HArr         = MAKE_ARRAY(maxNElems,nHTags,/FLOAT,VALUE=0.) 



  BDCAvg       = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BDCAbsAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BDCPosAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BDCNegAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 

  EDCAvg       = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EDCAbsAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EDCPosAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EDCNegAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 

  PDCAvg       = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PDCAbsAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PDCPosAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PDCNegAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 

  BACAvg       = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BACAbsAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BACPosAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 
  BACNegAvg    = MAKE_ARRAY(nOrbs,nBTags,/FLOAT,VALUE=0.) 

  EACAvg       = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EACAbsAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EACPosAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 
  EACNegAvg    = MAKE_ARRAY(nOrbs,nETags,/FLOAT,VALUE=0.) 

  PACAvg       = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PACAbsAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PACPosAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 
  PACNegAvg    = MAKE_ARRAY(nOrbs,nPTags,/FLOAT,VALUE=0.) 

  HAvg         = MAKE_ARRAY(nOrbs,nHTags,/FLOAT,VALUE=0.) 

END

PRO LOCALE_ADJUSTMENTS,times, $
                       oflow_i,nOutflow, $
                       NORTH=north, $
                       SOUTH=south, $
                       DAY=day, $
                       NIGHT=night, $
                       MINILAT=minILAT


  IF ~(KEYWORD_SET(north) OR KEYWORD_SET(south) OR KEYWORD_SET(day) OR KEYWORD_SET(night)) THEN RETURN

  GET_FA_ORBIT,times,/TIME_ARRAY,/DEFINITIVE

  IF KEYWORD_SET(north) OR KEYWORD_SET(south) THEN BEGIN
     haveHemi = 1
     GET_DATA,'ILAT',DATA=ilat
     ilat = ilat.y
     
  ENDIF

  IF KEYWORD_SET(day) OR KEYWORD_SET(night) THEN BEGIN
     haveSide = 1
     GET_DATA,'MLT',DATA=mlt
     mlt = mlt.y
  ENDIF

  IF KEYWORD_SET(north) THEN BEGIN

     hemi_i  = WHERE(ilat GE minILAT,nHemi)
     
  ENDIF ELSE BEGIN

     IF KEYWORD_SET(south) THEN BEGIN

        hemi_i  = WHERE(ilat LE -1.*minILAT,nHemi)

     ENDIF ELSE BEGIN

        hemi_i  = WHERE(ABS(ilat) GE minILAT,nHemi)

     ENDELSE

  ENDELSE

  IF KEYWORD_SET(day) THEN BEGIN

     side_i    = WHERE(mlt GE 6.0 AND mlt LT 18.0 AND (ABS(ilat) GE minILAT),nSide)
     
  ENDIF ELSE BEGIN

     IF KEYWORD_SET(night) THEN BEGIN

        side_i    = WHERE(mlt GE 18.0 OR mlt LT 6.0 AND (ABS(ilat) GE minILAT),nSide)

     ENDIF

  ENDELSE

  IF (nSide GT 0) AND (nHemi GT 0) THEN BEGIN
     comb_i = CGSETINTERSECTION(hemi_i,side_i,COUNT=nComb)
  ENDIF ELSE BEGIN
     oflow_i = -1
     RETURN
  ENDELSE

  IF nComb EQ 0 THEN BEGIN
     oflow_i = -1
     RETURN
  ENDIF

  oflow_i = CGSETINTERSECTION(comb_i,oflow_i,COUNT=nOutflow)

  IF nOutflow EQ 0 THEN BEGIN
     oflow_i = -1
     RETURN
  ENDIF

END

PRO EXTRACT_STRANGEWAY__FIELDS_N_COMPANY,nTags, $
                                       DCArr,ACArr, $
                                       oFloOrbDCArr, $
                                       DCAvg, $
                                       DCPosAvg, $
                                       DCNegAvg, $
                                       DCAbsAvg, $
                                       oFloOrbACArr, $
                                       ACAvg, $
                                       ACPosAvg, $
                                       ACNegAvg, $
                                       ACAbsAvg, $
                                       totInds,orbCnt

  FOR l=0,nTags-1 DO BEGIN

     DCArr[totInds,l]  = ofloOrbDCArr[*,l]
     ACArr[totInds,l]  = ofloOrbACArr[*,l]

     safeDC  = WHERE(FINITE(ofloOrbDCArr[*,l]),nSafeDC)
     safeAC  = WHERE(FINITE(ofloOrbACArr[*,l]),nSafeAC)

     posDC   = CGSETINTERSECTION(safeDC,WHERE(ofloOrbDCArr[*,l] GT 0),COUNT=nPosDC)
     posAC   = CGSETINTERSECTION(safeAC,WHERE(ofloOrbACArr[*,l] GT 0),COUNT=nPosAC)

     negDC   = CGSETINTERSECTION(safeDC,WHERE(ofloOrbDCArr[*,l] LT 0),COUNT=nNegDC)
     negAC   = CGSETINTERSECTION(safeAC,WHERE(ofloOrbACArr[*,l] LT 0),COUNT=nNegAC)

     IF safeDC[0] NE -1 THEN BEGIN
        DCAvg   [orbCnt,l]  = MEAN(     (ofloOrbDCArr[*,l])[safeDC]  )
        DCABSAvg[orbCnt,l]  = MEAN( ABS((ofloOrbDCArr[*,l])[safeDC]) )
     ENDIF ELSE BEGIN
        DCAvg   [orbCnt,l]  = !VALUES.F_NaN
        DCABSAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF posDC[0] NE -1 THEN BEGIN
        DCPosAvg[orbCnt,l]  = MEAN( (ofloOrbDCArr[*,l])[posDC ] )
     ENDIF ELSE BEGIN
        DCPosAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF negDC[0] NE -1 THEN BEGIN
        DCNegAvg[orbCnt,l]  = MEAN( (ofloOrbDCArr[*,l])[negDC ] )
     ENDIF ELSE BEGIN
        DCNegAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF safeAC[0] NE -1 THEN BEGIN
        ACAvg[orbCnt,l]     = MEAN( (ofloOrbACArr[*,l])[safeAC] )
        ACABSAvg[orbCnt,l]  = MEAN( ABS((ofloOrbACArr[*,l])[safeAC]) )
     ENDIF ELSE BEGIN
        ACAvg[orbCnt,l]     = !VALUES.F_NaN
        ACABSAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF posAC[0] NE -1 THEN BEGIN
        ACPosAvg[orbCnt,l] = MEAN( (ofloOrbACArr[*,l])[posAC ] )
     ENDIF ELSE BEGIN
        ACPosAvg[orbCnt,l] = !VALUES.F_NaN
     ENDELSE

     IF negAC[0] NE -1 THEN BEGIN
        ACNegAvg[orbCnt,l] = MEAN( (ofloOrbACArr[*,l])[negAC ] )
     ENDIF ELSE BEGIN
        ACNegAvg[orbCnt,l] = !VALUES.F_NaN
     ENDELSE

  ENDFOR

END

PRO KILL_AVG_ARRAYS,BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                    EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                    PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                    BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                    EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                    PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                    HAvg

  BDCAvg       = !NULL
  BDCAbsAvg    = !NULL
  BDCPosAvg    = !NULL
  BDCNegAvg    = !NULL

  EDCAvg       = !NULL
  EDCAbsAvg    = !NULL
  EDCPosAvg    = !NULL
  EDCNegAvg    = !NULL

  PDCAvg       = !NULL
  PDCAbsAvg    = !NULL
  PDCPosAvg    = !NULL
  PDCNegAvg    = !NULL

  BACAvg       = !NULL
  BACAbsAvg    = !NULL
  BACPosAvg    = !NULL
  BACNegAvg    = !NULL

  EACAvg       = !NULL
  EACAbsAvg    = !NULL
  EACPosAvg    = !NULL
  EACNegAvg    = !NULL

  PACAvg       = !NULL
  PACAbsAvg    = !NULL
  PACPosAvg    = !NULL
  PACNegAvg    = !NULL

  HAvg         = !NULL

END

FUNCTION EXTRACT_STRANGEWAY_STATS__APPENDIX_A, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   PTS_STRUCT=pts, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   FOLD_INTERVALS=fold_intervals, $
   USE_INCLUDED_PFLUX=use_included_pFlux, $
   SAVE_PLOTS=save_plots, $
   PLOTDIR=plotDir, $
   PLOTS_PREFIX=plots_prefix, $
   SQUARE_WINDOW=square_window, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2

  ;;Some outflow defaults
  outflowMinLog10 = 5
  ptsMinOutflow   = 2
  allowableGap    = 2 ;seconds
  min_streakLen_t = 30 ;;At least 30, right?

  @strway_stuff

  @strangeway_2005__defaults__appendix_a.pro

  ;; ;;Outputs
  ;; outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/Appendix_A/'
  ;; hashFile     = 'Strangeway_et_al_2005__real_thing--outflow_intervals.sav'

  ;; IF KEYWORD_SET(use_eField_fit_variables) THEN BEGIN
  ;;    hashFile    +='--eFieldFits'
  ;; ENDIF

  ;; bonusSuff    = ''

  ;; IF ( ABS(energy_ions[0] - 4.)   LT 0.01 ) THEN bonusSuff += '--4eV_lower'
  ;; IF ( ABS(energy_ions[1] - 500.) LT 0.01 ) THEN bonusSuff += '--500eV_upper'

  ;; hashFile    += bonusSuff

  ;;Update hashfile name and outPlotName
  ;; plotPref = SETUP_STRANGEWAY_BRAMBLES_PLOTPREF($
  ;;            USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
  ;;            ONLY_FASTSRVY_DATA=only_128Ss_data, $
  ;;            INCLUDE_E_NEAR_B=include_E_near_B, $
  ;;            FULL_PFLUX_CALC=full_pFlux, $
  ;;            FIELDS_INTERP=do_fields_interp, $
  ;;            FIELDS_SPLINE=do_fields_spline)
  
  ;; hashFile    += plotPref

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

  tmpKey       = (swHash.Keys())[0]
  tmpStruct    = swHash[tmpKey,0]

  OK           = SETUP_TAGNAMES_AND_INDICES(swHash[tmpKey,0], $
                                            Bind,Eind,Hind,Pind, $
                                            Btags,Etags,Htags,Ptags, $
                                            nBTags,nETags,nHTags,nPTags, $
                                            BPind,BVind,BBind, $
                                            EAVind,ENBind,EDSPind,EIENBind, $
                                            HJEeInd,HJeInd,HJiInd, $
                                            PPind,PVind,PBind,have_included_pFlux)

;; SETUP_TAGNAMES_AND_INDICES( $
;;                  swHash[tmpKey,0], $
;;                  Bind,Eind,Hind, $
;;                  Btags,Etags,Htags,Ptags, $
;;                  nBTags,nETags,nHTags,nPTags, $
;;                  BPind,BVind,BBind, $
;;                  EAVind,ENBind,EDSPind,EIENBind, $
;;                  HJEeInd,HJeInd,HJiInd)
                                   

  IF (OK EQ -1) THEN RETURN,-1

  ;;Arrays to store orbit averages
  nOrbs        = N_ELEMENTS(swHash.Keys())
  orbStrtStop  = MAKE_ARRAY(2,nOrbs,/LONG)

  DECLARE_ARRAYS,nOrbs,nBTags,nETags,nPTags,nHTags, $
                 orbArr,itvlArr,noENBArr, $
                 BDCArr,EDCArr,PDCArr, $
                 BACArr,EACArr,PACArr, $
                 HArr, $
                 BPTSArr,EPTSArr, HPTSArr, $
                 BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                 EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                 PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                 BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                 EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                 PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                 HAvg

  totPtCnt     = 0 ;master counter
  orbCnt       = 0 ;orbit counter
  notUnivCnt   = 0 ;Number of intervals for which there is no universal time series
  FOREACH swayStruct, swHash, key DO BEGIN

     ;;Anything here?
     IF N_ELEMENTS(swayStruct[0]) EQ 0 THEN CONTINUE

     ;;How many?
     nItvls    = N_ELEMENTS(swayStruct)

     IF nItvls EQ 0 THEN CONTINUE

     nThisItvl     = MAKE_ARRAY(nItvls,/LONG)

     FOR k=0,nItvls-1 DO nThisItvl[k] = N_ELEMENTS(swayStruct[k].(0).(0).x)

     nThisOrb  = FIX(TOTAL(nThisItvl))

     ;;Points for this orbit
     ofloOrbItvlArr= MAKE_ARRAY(nThisOrb,/INTEGER,VALUE=0.) 

     ofloOrbBDCArr = MAKE_ARRAY(nThisOrb,nBTags,/FLOAT,VALUE=0.) 
     ofloOrbEDCArr = MAKE_ARRAY(nThisOrb,nETags,/FLOAT,VALUE=0.) 
     ofloOrbPDCArr = MAKE_ARRAY(nThisOrb,nPTags,/FLOAT,VALUE=0.) 

     ofloOrbBACArr = MAKE_ARRAY(nThisOrb,nBTags,/FLOAT,VALUE=0.) 
     ofloOrbEACArr = MAKE_ARRAY(nThisOrb,nETags,/FLOAT,VALUE=0.) 
     ofloOrbPACArr = MAKE_ARRAY(nThisOrb,nPTags,/FLOAT,VALUE=0.) 

     ofloOrbHArr   = MAKE_ARRAY(nThisOrb,nHTags,/FLOAT,VALUE=0.) 

     IF nThisOrb NE 0 THEN BEGIN

        ;;Now loop over intervals within this orbit
        orbPtCnt   = 0 ;;Keep track of how many we've gone over in this orbit
        FOR k=0,nItvls-1 DO BEGIN

           PRINT,FORMAT='(A0,T25,I5,", ",I2)',"Orbit, Interval :",key,k

           have_univ_TS  = BYTE(TAG_EXIST(swayStruct[k].(0).(0),'COMMONEST_TS'))

           IF have_univ_TS THEN BEGIN

              ;; PRINT,"UNIVERSAL"

              Puniv_TS   = PTR_NEW(swayStruct[k].(0).(0).(0))

              have_B_TS  = 1B
              have_E_TS  = 1B
              have_H_TS  = 1B

              PB_TS      = Puniv_TS
              PE_TS      = Puniv_TS
              PH_TS      = Puniv_TS

              BPTSArr[*] = Puniv_TS
              EPTSArr[*] = Puniv_TS
              HPTSArr[*] = Puniv_TS

              nHere      = N_ELEMENTS(*Puniv_TS)

           ENDIF ELSE BEGIN
              
              notUnivCnt++
              ;; PRINT,"NOT UNIVERSAL"

              have_B_TS  = BYTE(TAG_EXIST(swayStruct[k].(BInd).(0),'COMMON_TS'))
              have_E_TS  = BYTE(TAG_EXIST(swayStruct[k].(EInd).(0),'COMMON_TS'))
              have_H_TS  = BYTE(TAG_EXIST(swayStruct[k].(HInd).(0),'COMMON_TS'))

              IF have_B_TS THEN BEGIN
                 PB_TS          = PTR_NEW(swayStruct[k].(BInd).(0).(0))
                 BPTSArr[*]     = PB_TS
              ENDIF ELSE BEGIN
                 FOR p=0,nBTags-1 DO BEGIN
                    BPTSArr[p]  = PTR_NEW(swayStruct[k].(BInd).(p).(0))
                 ENDFOR
              ENDELSE

              IF have_E_TS THEN BEGIN
                 PE_TS          = PTR_NEW(swayStruct[k].(EInd).(0).(0))
                 BPTSArr[*]     = PB_TS
              ENDIF ELSE BEGIN
                 FOR p=0,nETags-1 DO BEGIN
                    EPTSArr[p]  = PTR_NEW(swayStruct[k].(EInd).(p).(0))
                 ENDFOR
              ENDELSE

              IF have_H_TS THEN BEGIN
                 PH_TS          = PTR_NEW(swayStruct[k].(HInd).(0).(0))
                 BPTSArr[*]     = PB_TS
              ENDIF ELSE BEGIN
                 FOR p=0,nETags-1 DO BEGIN
                    HPTSArr[p]  = PTR_NEW(swayStruct[k].(HInd).(p).(0))
                 ENDFOR
              ENDELSE

           ENDELSE

        ;; ENDFOR


           ;;Pick up fields, align time series
           dBp           = PTR_NEW(swayStruct[k].(Bind).(BPind))
           dBv           = PTR_NEW(swayStruct[k].(Bind).(BVind))
           dBB           = PTR_NEW(swayStruct[k].(Bind).(BBind))

           eAV           = PTR_NEW(swayStruct[k].(Eind).(EAVind))
           eNB           = PTR_NEW(swayStruct[k].(Eind).(ENBind))
           eDSP          = PTR_NEW(swayStruct[k].(Eind).(EDSPind))

           CASE 1 OF
              have_univ_TS: BEGIN ;;Already sammen
                 BclosE_i = INDGEN(N_ELEMENTS((*PUniv_TS)))
                 EclosB_i = INDGEN(N_ELEMENTS((*PUniv_TS)))
              END
              (have_B_TS AND have_E_TS): BEGIN
                 ;;Need to work on å sammenligne dem
                 BclosE_i = VALUE_CLOSEST2((*PB_TS),(*PE_TS))
                 EclosB_i = VALUE_CLOSEST2((*PE_TS),(*PB_TS))

                 BclosE_i = BclosE_i[WHERE(ABS( (*PB_TS)[BclosE_i] - (*PE_TS) ) LE 0.05)]
                 EclosB_i = EclosB_i[WHERE(ABS( (*PE_TS)[EclosB_i] - (*PB_TS) ) LE 0.05)]
              END
              ELSE: STOP
           ENDCASE

           IF ~KEYWORD_SET(full_pFlux) THEN BEGIN

              (*eNB).DC[*]  = 0.
              (*eNB).AC[*]  = 0.

           ENDIF ELSE BEGIN
              ;;Keep some stats if we're going to attempt the full pFlux sitiation
              ;;As always, I do mean sitiation
              IF ~swayStruct[k].(Eind).(EIENBind) THEN noENBArr[*,orbCnt+k] = [key,k]
           ENDELSE


           IF KEYWORD_SET(use_included_pFlux) THEN BEGIN

              pFB = PTR_NEW(swayStruct[k].(Pind).(PBind))
              pFP = PTR_NEW(swayStruct[k].(Pind).(PPind))

              IF SIZE(swayStruct[k].(Pind).(PVind),/TYPE) EQ 8 THEN BEGIN

                 pFV = PTR_NEW(swayStruct[k].(Pind).(PVind))

              ENDIF ELSE BEGIN

                 pFV = PTR_NEW(*pFB)
                 (*pFV).DC = 0.D
                 (*pFV).AC = 0.D

              ENDELSE

           ENDIF ELSE BEGIN
              ;;Calc Poynting flux

              ;;Poynting flux along B
              pFB    = PTR_NEW({DC:(*dBp).DC[BclosE_i]*(*eAV).DC[EclosB_i]/mu_0, $
                                AC:(*dBp).AC[BclosE_i]*(*eAV).AC[EclosB_i]/mu_0})
              

              ;;Poynting flux perp to B and to (Bxv)xB
              pFP    = PTR_NEW({DC:((*dBv).DC[BclosE_i]*(*eNB).DC[EclosB_i] - $
                                    (*dBB).DC[BclosE_i]*(*eAV).DC[EclosB_i])/mu_0, $
                                AC:((*dBv).AC[BclosE_i]*(*eNB).AC[EclosB_i] - $
                                    (*dBB).AC[BclosE_i]*(*eAV).AC[EclosB_i])/mu_0})

              ;;Negative sign comes out of S EQ 1/μ_0 * E x B for {b,v,p} "velocity-based" coord system
              pFV    = PTR_NEW({DC:(-1.)*(*eNB)[EclosB_i].DC*(*dBp)[BclosE_i].DC/mu_0, $
                                AC:(-1.)*(*eNB)[EclosB_i].AC*(*dBp)[BclosE_i].AC/mu_0})
              
              ;;Junk that nano prefix in nT
              (*pFB).DC *= 1e-9
              (*pFP).DC *= 1e-9
              (*pFV).DC *= 1e-9

              (*pFB).AC *= 1e-9
              (*pFP).AC *= 1e-9
              (*pFV).AC *= 1e-9

           ENDELSE

           ;;Outflow indices
           oflow_i = WHERE((ALOG10(ABS(swayStruct[k].(Hind).(HJiInd).y)) GE outflowMinLog10) AND $
                           (FINITE(swayStruct[k].(Hind).(HJiInd).y))                         AND $
                           (swayStruct[k].(Hind).(HJiInd).y GT 0),nOutflow)

           IF nOutflow LT ptsMinOutflow THEN CONTINUE

           LOCALE_ADJUSTMENTS,(*PUniv_TS), $
                              oflow_i,nOutflow, $
                              NORTH=north, $
                              SOUTH=south, $
                              DAY=day, $
                              NIGHT=night, $
                              MINILAT=minILAT

           IF nOutflow LT ptsMinOutflow THEN CONTINUE

           ;;Get outflow intervals
           GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE, $
              (*HPTSArr[HJiInd])[oflow_i],0, $
              NPTS=ptsMinOutflow, $
              MIN_T_STREAKLEN=min_streakLen_t, $
              GAP_TIME=allowableGap, $
              START_I=start_i, $
              STOP_I=stop_i, $
              STREAKLENS=lens

           IF start_i[0] EQ stop_i[0] THEN BEGIN
              PRINT,"No outflow streaks of sufficient length here ..."
              CONTINUE
           ENDIF

           ;;OK, now we know that there's going to be action. 
           ;;If we're this far, we're going to average all outflow points
           nStreaks  = N_ELEMENTS(start_i)
           nOfloPts  = FIX(TOTAL(lens) + nStreaks)

           PRINT,"n Outflow: " + STRCOMPRESS(nOfloPts,/REMOVE_ALL)

           ofloItvlBDCArr = MAKE_ARRAY(nOfloPts,nBTags,/FLOAT,VALUE=0.) 
           ofloItvlEDCArr = MAKE_ARRAY(nOfloPts,nETags,/FLOAT,VALUE=0.) 
           ofloItvlPDCArr = MAKE_ARRAY(nOfloPts,nPTags,/FLOAT,VALUE=0.) 

           ofloItvlBACArr = MAKE_ARRAY(nOfloPts,nBTags,/FLOAT,VALUE=0.) 
           ofloItvlEACArr = MAKE_ARRAY(nOfloPts,nETags,/FLOAT,VALUE=0.) 
           ofloItvlPACArr = MAKE_ARRAY(nOfloPts,nPTags,/FLOAT,VALUE=0.) 

           ofloItvlHArr   = MAKE_ARRAY(nOfloPts,nHTags,/FLOAT,VALUE=0.) 

           ofloItvlPtCnt  = 0

           FOR l=0,nStreaks-1 DO BEGIN

              ;; curInds  = [orbPtCnt:orbPtCnt+nThisItvl[k]-1]
              arrInds  = [ofloItvlPtCnt:ofloItvlPtCnt+lens[l]]
              strkInds  = [start_i[l]:stop_i[l]]

              ;; PRINT,FORMAT='(A0,I0,":",I0,A0)',"arrInds : [",arrInds[0],arrInds[-1],"]"
              ;; PRINT,FORMAT='(A0,I0,":",I0,A0)',"strkInds : [",strkInds[0],strkInds[-1],"]"

              ;;Fields DC
              ofloItvlBDCArr[arrInds,*] = [[(*dBp).DC[strkInds]], $
                                           [(*dBv).DC[strkInds]], $
                                           [(*dBB).DC[strkInds]]]
              ofloItvlEDCArr[arrInds,*] = [[(*eAV).DC[strkInds]], $
                                           [(*eNB).DC[strkInds]], $
                                           [(*eDSP).DC[strkInds]]]
              ofloItvlPDCArr[arrInds,*] = [[(*pFP).DC[strkInds]], $
                                           [(*pFV).DC[strkInds]], $
                                           [(*pFB).DC[strkInds]]]

              ;;Fields AC
              ofloItvlBACArr[arrInds,*] = [[(*dBp).AC[strkInds]], $
                                           [(*dBv).AC[strkInds]], $
                                           [(*dBB).AC[strkInds]]]
              ofloItvlEACArr[arrInds,*] = [[(*eAV).AC[strkInds]], $
                                           [(*eNB).AC[strkInds]], $
                                           [(*eDSP).AC[strkInds]]]
              ofloItvlPACArr[arrInds,*] = [[(*pFP).AC[strkInds]], $
                                           [(*pFV).AC[strkInds]], $
                                           [(*pFB).AC[strkInds]]]

              ;;Particles
              ofloItvlHArr[arrInds,*]   = [[swayStruct[k].(Hind).(HJEeInd).y[strkInds]], $
                                           [swayStruct[k].(Hind).(HJeInd).y[strkInds]], $
                                           [swayStruct[k].(Hind).(HJiInd).y[strkInds]]]
              ofloItvlPtCnt += lens[l] + 1

           ENDFOR

           tmpItvlInds = [orbPtCnt:orbPtCnt+ofloItvlPtCnt-1]
           tmpOfloInds = [0:(ofloItvlPtCnt-1)]
           
           PRINT,FORMAT='(A0,I0,":",I0,A0)',"itvlInds: [",tmpItvlInds[0],tmpItvlInds[-1],"]"
           PRINT,FORMAT='(A0,I0,":",I0,A0)',"ofloInds: [",tmpOfloInds[0],tmpOfloInds[-1],"]"

           ofloOrbItvlArr[tmpItvlInds]  = k

           ofloOrbBDCArr[tmpItvlInds,*] = ofloItvlBDCArr[tmpOfloInds,*]
           ofloOrbEDCArr[tmpItvlInds,*] = ofloItvlEDCArr[tmpOfloInds,*]
           ofloOrbPDCArr[tmpItvlInds,*] = ofloItvlPDCArr[tmpOfloInds,*]

           ofloOrbBACArr[tmpItvlInds,*] = ofloItvlBACArr[tmpOfloInds,*]
           ofloOrbEACArr[tmpItvlInds,*] = ofloItvlEACArr[tmpOfloInds,*]
           ofloOrbPACArr[tmpItvlInds,*] = ofloItvlPACArr[tmpOfloInds,*]

           ofloOrbHArr  [tmpItvlInds,*] = ofloItvlHArr  [tmpOfloInds,*]

           orbPtCnt += ofloItvlPtCnt

        ENDFOR

        tmpOrbInds = [0:orbPtCnt-1]

        ;;Shrink arrays for this orbit, then update master arrays and master counter. 
        ofloOrbItvlArr= ofloOrbItvlArr[tmpOrbInds]

        ofloOrbBDCArr = ofloOrbBDCArr[tmpOrbInds,*]
        ofloOrbEDCArr = ofloOrbEDCArr[tmpOrbInds,*]
        ofloOrbPDCArr = ofloOrbPDCArr[tmpOrbInds,*]

        ofloOrbBACArr = ofloOrbBACArr[tmpOrbInds,*]
        ofloOrbEACArr = ofloOrbEACArr[tmpOrbInds,*]
        ofloOrbPACArr = ofloOrbPACArr[tmpOrbInds,*]
                                     
        ofloOrbHArr   = ofloOrbHArr  [tmpOrbInds,*]

        orbStrtStop[*,orbCnt] = [totPtCnt,totPtCnt+orbPtCnt-1]

        tmpTotInds   = [totPtCnt:(totPtCnt+orbPtCnt-1)]

        ;;update orb and itvl arr
        itvlArr[tmpTotInds] = ofloOrbItvlArr
        orbArr[tmpTotInds]  = key

        ;;Loop over B-field array stuff
        EXTRACT_STRANGEWAY__FIELDS_N_COMPANY,nBTags, $
                                           BDCArr,BACArr, $
                                           oFloOrbBDCArr, $
                                           BDCAvg, $
                                           BDCPosAvg, $
                                           BDCNegAvg, $
                                           BDCAbsAvg, $
                                           oFloOrbBACArr, $
                                           BACAvg, $
                                           BACPosAvg, $
                                           BACNegAvg, $
                                           BACAbsAvg, $
                                           tmpTotInds,orbCnt

        ;;Loop over E-field array stuff
        EXTRACT_STRANGEWAY__FIELDS_N_COMPANY,nETags, $
                                           EDCArr,EACArr, $
                                           oFloOrbEDCArr, $
                                           EDCAvg, $
                                           EDCPosAvg, $
                                           EDCNegAvg, $
                                           EDCAbsAvg, $
                                           oFloOrbEACArr, $
                                           EACAvg, $
                                           EACPosAvg, $
                                           EACNegAvg, $
                                           EACAbsAvg, $
                                           tmpTotInds,orbCnt

        ;;Loop over Pflux array stuff
        EXTRACT_STRANGEWAY__FIELDS_N_COMPANY,nPTags, $
                                           PDCArr,PACArr, $
                                           oFloOrbPDCArr, $
                                           PDCAvg, $
                                           PDCPosAvg, $
                                           PDCNegAvg, $
                                           PDCAbsAvg, $
                                           oFloOrbPACArr, $
                                           PACAvg, $
                                           PACPosAvg, $
                                           PACNegAvg, $
                                           PACAbsAvg, $
                                           tmpTotInds,orbCnt

        ;;Loop over particle array stuff
        FOR l=0,nHTags-1 DO BEGIN

           HArr  [tmpTotInds,l]  = ofloOrbHArr[*,l]

           ;; safeDC                = WHERE(FINITE(ofloOrbHDCArr[*,l]),nSafeDC)
           ;; safeAC                = WHERE(FINITE(ofloOrbHACArr[*,l]),nSafeAC)

           safe                  = WHERE(FINITE(HArr[tmpTotInds,l]),nSafe)
           IF safe[0] NE -1 THEN BEGIN
              HAvg[orbCnt,l]     = MEAN( (HArr[tmpTotInds,l])[safe] )
           ENDIF ELSE BEGIN
              HAvg[orbCnt,l]     = !VALUES.F_NaN
           ENDELSE

        ENDFOR

        totPtCnt += orbPtCnt

     ENDIF


     orbCnt++
  ENDFOREACH
  PRINT,"N not-awesome interval: " + STRCOMPRESS(notUnivCnt,/REMOVE_ALL)

  ;;Final shrink
  dBAvg = CREATE_STRUCT('P',{DC:{avg:BDCAvg[*,0], $
                                 pos:BDCPosAvg[*,0], $
                                 neg:BDCNegAvg[*,0], $
                                 abs:BDCAbsAvg[*,0]}, $
                             AC:{avg:BACAvg[*,0], $
                                 pos:BACPosAvg[*,0], $
                                 neg:BACNegAvg[*,0], $
                                 abs:BACAbsAvg[*,0]}}, $
                        'V',{DC:{avg:BDCAvg[*,1], $
                                 pos:BDCPosAvg[*,1], $
                                 neg:BDCNegAvg[*,1], $
                                 abs:BDCAbsAvg[*,1]}, $
                             AC:{avg:BACAvg[*,1], $
                                 pos:BACPosAvg[*,1], $
                                 neg:BACNegAvg[*,1], $
                                 abs:BACAbsAvg[*,1]}}, $
                        'B',{DC:{avg:BDCAvg[*,2], $
                                 pos:BDCPosAvg[*,2], $
                                 neg:BDCNegAvg[*,2], $
                                 abs:BDCAbsAvg[*,2]}, $
                             AC:{avg:BACAvg[*,2], $
                                 pos:BACPosAvg[*,2], $
                                 neg:BACNegAvg[*,2], $
                                 abs:BACAbsAvg[*,2]}})

  EAvg = CREATE_STRUCT('AlongV',{DC:{avg:EDCAvg[*,0], $
                                     pos:EDCPosAvg[*,0], $
                                     neg:EDCNegAvg[*,0], $
                                     abs:EDCAbsAvg[*,0]}, $
                                 AC:{avg:EACAvg[*,0], $
                                     pos:EACPosAvg[*,0], $
                                     neg:EACNegAvg[*,0], $
                                     abs:EACAbsAvg[*,0]}}, $
                       'NearB',{DC:{avg:EDCAvg[*,1], $
                                    pos:EDCPosAvg[*,1], $
                                    neg:EDCNegAvg[*,1], $
                                    abs:EDCAbsAvg[*,1]}, $
                                AC:{avg:EACAvg[*,1], $
                                    pos:EACPosAvg[*,1], $
                                    neg:EACNegAvg[*,1], $
                                    abs:EACAbsAvg[*,1]}}, $
                       'DSP',{DC:{avg:EDCAvg[*,2], $
                                  pos:EDCPosAvg[*,2], $
                                  neg:EDCNegAvg[*,2], $
                                  abs:EDCAbsAvg[*,2]}, $
                              AC:{avg:EACAvg[*,2], $
                                  pos:EACPosAvg[*,2], $
                                  neg:EACNegAvg[*,2], $
                                  abs:EACAbsAvg[*,2]}})

  PAvg = CREATE_STRUCT('P',{DC:{avg:PDCAvg[*,0], $
                                pos:PDCPosAvg[*,0], $
                                neg:PDCNegAvg[*,0], $
                                abs:PDCAbsAvg[*,0]}, $
                            AC:{avg:PACAvg[*,0], $
                                pos:PACPosAvg[*,0], $
                                neg:PACNegAvg[*,0], $
                                abs:PACAbsAvg[*,0]}}, $
                       'V',{DC:{avg:PDCAvg[*,1], $
                                pos:PDCPosAvg[*,1], $
                                neg:PDCNegAvg[*,1], $
                                abs:PDCAbsAvg[*,1]}, $
                            AC:{avg:PACAvg[*,1], $
                                pos:PACPosAvg[*,1], $
                                neg:PACNegAvg[*,1], $
                                abs:PACAbsAvg[*,1]}}, $
                       'B',{DC:{avg:PDCAvg[*,2], $
                                pos:PDCPosAvg[*,2], $
                                neg:PDCNegAvg[*,2], $
                                abs:PDCAbsAvg[*,2]}, $
                            AC:{avg:PACAvg[*,2], $
                                pos:PACPosAvg[*,2], $
                                neg:PACNegAvg[*,2], $
                                abs:PACAbsAvg[*,2]}})

  HAvg = CREATE_STRUCT('JEe',{y:{avg:HAvg[*,0]}}, $
                       ;; pos:HPosAvg[*,0], $
                       ;; neg:HNegAvg[*,0], $
                       ;; abs:HAbsAvg[*,0]}, $
                       ;; AC:{avg:HACAvg[*,0], $
                       ;; pos:HACPosAvg[*,0], $
                       ;; neg:HACNegAvg[*,0], $
                       ;; abs:HACAbsAvg[*,0]}}, $
                       'Je',{y:{avg:HAvg[*,1]}}, $
                       ;; pos:HPosAvg[*,1], $
                       ;; neg:HNegAvg[*,1], $
                       ;; abs:HAbsAvg[*,1]}, $
                       ;; AC:{avg:HACAvg[*,1], $
                       ;; pos:HACPosAvg[*,1], $
                       ;; neg:HACNegAvg[*,1], $
                       ;; abs:HACAbsAvg[*,1]}}, $
                       'Ji',{y:{avg:HAvg[*,2]}})
  ;; pos:HPosAvg[*,2], $
  ;; neg:HNegAvg[*,2], $
  ;; abs:HAbsAvg[*,2]}, $
  ;; AC:{avg:HACAvg[*,2], $
  ;; pos:HACPosAvg[*,2], $
  ;; neg:HACNegAvg[*,2], $
  ;; abs:HACAbsAvg[*,2]}})

  avgStruct = {orbit  : (swHash.Keys()).ToArray(), $
               dB     : TEMPORARY(dBAvg), $
               E      : TEMPORARY(EAvg), $
               PFLUX  : TEMPORARY(PAvg), $
               PTCL   : TEMPORARY(HAvg)}

  KILL_AVG_ARRAYS,BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                  EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                  PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                  BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                  EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                  PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                  HAvg

  ;;All pts Arr
  orbArr       = orbArr [0:totPtCnt-1]
  itvlArr      = itvlArr[0:totPtCnt-1]

  BDCArr       = BDCArr[0:totPtCnt-1,*]
  EDCArr       = EDCArr[0:totPtCnt-1,*]
  PDCArr       = PDCArr[0:totPtCnt-1,*]

  BACArr       = BACArr[0:totPtCnt-1,*]
  EACArr       = EACArr[0:totPtCnt-1,*]
  PACArr       = PACArr[0:totPtCnt-1,*]

  HArr         = HArr  [0:totPtCnt-1,*]

  dBArr = CREATE_STRUCT('P',{DC:BDCArr[*,0], $
                             AC:BACArr[*,0]}, $
                        'V',{DC:BDCArr[*,1], $
                             AC:BACArr[*,1]}, $
                        'B',{DC:BDCArr[*,2], $
                             AC:BACArr[*,2]})

  EArr = CREATE_STRUCT('AlongV' ,{DC:EDCArr[*,0], $
                                  AC:EACArr[*,0]}, $
                       'NearB'  ,{DC:EDCArr[*,1], $
                                  AC:EACArr[*,1]}, $
                       'DSP'    ,{DC:EDCArr[*,2], $
                                  AC:EACArr[*,2]})

  PArr = CREATE_STRUCT('P',{DC:PDCArr[*,0], $
                            AC:PACArr[*,0]}, $
                       'V',{DC:PDCArr[*,1], $
                            AC:PACArr[*,1]}, $
                       'B',{DC:PDCArr[*,2], $
                            AC:PACArr[*,2]})

  HArr = CREATE_STRUCT('JEe',{y:HArr[*,0]}, $
                       'Je',{y:HArr[*,1]}, $
                       'Ji',{y:HArr[*,2]})

  pts = {orbit:orbArr, $
         interval:itvlArr, $
         dB:TEMPORARY(dBArr), $
         E:TEMPORARY(EArr), $
         PFLUX:TEMPORARY(PArr), $
         PTCL:TEMPORARY(HArr)}

  avgInd        = 0 ;;Just plain old
  avgTypeString = ''

  ;; posVal = 1
  ;; absVal = 1
  negVal = 1
  IF KEYWORD_SET(posVal) THEN BEGIN
     avgInd = 1
     avgTypeString = 'POS'
  ENDIF
  IF KEYWORD_SET(negVal) THEN BEGIN
     avgInd = 2
     avgTypeString = 'NEG'
  ENDIF
  IF KEYWORD_SET(absVal) THEN BEGIN
     avgInd = 3
     avgTypeString = 'ABS'
  ENDIF

  sw_i = SORT(avgStruct.orbit)

  finStruct   = {orbit     : avgStruct.orbit    [sw_i], $
                 ;; interval  : avgStruct.interval [sw_i], $   
                 ;; eAlongV   : avgStruct.eAlongV  [sw_i], $   
                 ;; dB_perp   : avgStruct.dB_perp  [sw_i], $   
                 pFAlongBDC: avgStruct.pFlux.B.DC.(avgInd) [sw_i], $
                 pFAlongPDC: avgStruct.pFlux.P.DC.(avgInd) [sw_i], $
                 pFAlongBAC: avgStruct.pFlux.B.AC.(avgInd) [sw_i], $
                 pFAlongPAC: avgStruct.pFlux.P.AC.(avgInd) [sw_i], $
                 DSPDC     : avgStruct.E.DSP.DC.(avgInd)   [sw_i], $
                 DSPAC     : avgStruct.E.DSP.AC.(avgInd)   [sw_i], $
                 je        : avgStruct.ptcl.je.y.avg   [sw_i], $
                 jee       : avgStruct.ptcl.jee.y.avg  [sw_i], $
                 ji        : avgStruct.ptcl.ji.y.avg   [sw_i]}



  IF ~KEYWORD_SET(no_plots) THEN BEGIN

     IF N_ELEMENTS(xQuants) EQ 0 THEN BEGIN
        xQuants = [1,2,3,4,5,6,7,8]
     ENDIF

     plotInfo  = {xQuants       : xQuants, $
                  xTitle        : ["", $
                                   "Poynting FluxB [DC] (mW/m^2)", $
                                   "Poynting FluxP [DC] (mW/m^2)", $
                                   "Poynting FluxB [AC] (mW/m^2)", $
                                   "Poynting FluxP [AC] (mW/m^2)", $
                                   "Average ELF amplitude [DC] (V/m)", $
                                   "Average ELF amplitude [AC] (V/m)", $
                                   "Average Electron Flux (#/cm$^2$/s)", $
                                   "Average Electron Energy Flux (mW/m$^2$)", $
                                   "Ion Flux (#/cm!U2!N/s)"] + avgTypeString, $
                  xRange        : [[0.,0.], $
                                   [1e-1,1e2], $
                                   [1e-1,1e2], $
                                   [1e-4,1e0], $
                                   [1e-4,1e0], $
                                   [1e-3,1e-1], $
                                   [1e-5,1e-2], $
                                   [1e7,1e10], $
                                   [1e-2,1e0], $                                    
                                   [1e6,1e10]], $
                  yTitle        : "Ion Flux (#/cm!U2!N/s)", $
                  yData         : finStruct.ji, $
                  yRange        : [1e6,1e10], $
                  plotNames     : ["", $
                                   "DC_Poynting_fluxB__vs__ionNumFlux", $
                                   "AC_Poynting_fluxB__vs__ionNumFlux", $
                                   "DC_Poynting_fluxP__vs__ionNumFlux", $
                                   "AC_Poynting_fluxP__vs__ionNumFlux", $
                                   "ELF_amplitudeDC__vs__ionNumFlux", $
                                   "ELF_amplitudeAC__vs__ionNumFlux", $
                                   "eNumFlux__vs__ionNumFlux", $
                                   "eFlux__vs__ionNumFlux", $
                                   "Ion Flux (#/cm!U2!N/s)"], $
                  canonPref     : 'Strangeway_2005_Appendix_A--', $
                  plotDirSuff   : '/Strangeway_et_al_2005--Appendix_A', $
                  plots_prefix  : (KEYWORD_SET(bonusSuff) ? bonusSuff : '') + $ 
                                  defs.statStr+'--'+defs.sideStr+'--'+defs.hemStr+'--' + $
                                  avgTypeString, $
                  verboten      : [0], $
                  navn_verboten : ["Orbit    (ind 0)"]}


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
