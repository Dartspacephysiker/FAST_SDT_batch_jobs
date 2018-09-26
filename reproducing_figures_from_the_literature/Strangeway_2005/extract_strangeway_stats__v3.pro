;;2018/07/28
FUNCTION SETUP_TAGNAMES_AND_INDICES,tmpStruct, $
                                    Bind,Eind,Hind,HMomind,Pind, $
                                    Btags,Etags,Htags,HMomtags,Ptags, $
                                    nBTags,nETags,nHTags,nHMomTags,nPTags, $
                                    BPind,BVind,BBind, $
                                    EAVind,ENBind,EDSPind,EIENBind, $
                                    HJEeInd,HJeInd,HJiInd, $
                                    HMomNeInd,HMomCEInd,HMomTInd, $
                                    HMomJEeInd,HMomJeInd, $
                                    PPind,PVind,PBind,have_included_pFlux, $
                                    HMOM_INDEX=HMom_index

  strucTags    = TAG_NAMES(tmpStruct)

  Bind         = WHERE(STRUPCASE(strucTags) EQ 'DB')
  Eind         = WHERE(STRUPCASE(strucTags) EQ 'E')
  Hind         = WHERE(STRUPCASE(strucTags) EQ 'PTCL')
  HMomind      = WHERE(STRUPCASE(strucTags) EQ 'PTCLMOMS')
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
  HMomtags     = TAG_NAMES(tmpStruct.(HMomind).(HMom_index))
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

  HMomNeInd    = (WHERE(STRUPCASE(HMomtags)EQ 'N'              ))[0]
  HMomCEInd    = (WHERE(STRUPCASE(HMomtags)EQ 'CHARE'          ))[0]
  HMomTInd     = (WHERE(STRUPCASE(HMomtags)EQ 'T'              ))[0]

  HMomJeInd    = (WHERE(STRUPCASE(HMomtags)EQ 'J'             ))[0]
  HMomJEeInd   = (WHERE(STRUPCASE(HMomtags)EQ 'JE'            ))[0]

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

  nHMomTags    = (HMomNeInd GE 0 ? 1 : 0 ) + (HMomCEInd GE 0 ? 1 : 0 ) + (HMomTInd  GE 0 ? 1 : 0 ) $
                 + (HMomJEeInd  GE 0 ? 1 : 0 ) + (HMomJeInd  GE 0 ? 1 : 0 )

  RETURN,0

END

PRO DECLARE_ARRAYS,nOrbs,nBTags,nETags,nPTags,nHTags,nHMomTags, $
                   orbArr,itvlArr,noENBArr, $
                   BDCArr,EDCArr,PDCArr, $
                   BACArr,EACArr,PACArr, $
                   HArr,HMomArr, $
                   BPTSArr,EPTSArr, HPTSArr, $
                   BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                   EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                   PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                   BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                   EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                   PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                   HAvg,HPosAvg,HNegAvg,HAbsAvg, $
                   HMomAvg,HMomPosAvg,HMomNegAvg,HMomAbsAvg, $
                   SW_infos

  COMMON SWSTATS_SWINFO,SWStat_SW_tmplt

  maxNElems    = 1e6

  ;;Pointers to time arrays
  BPTSArr      = PTRARR(nBTags)
  EPTSArr      = PTRARR(nETags)
  HPTSArr      = PTRARR(nHTags)

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

  HMomArr      = MAKE_ARRAY(maxNElems,nHMomTags,/FLOAT,VALUE=0.) 

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
  HPosAvg      = MAKE_ARRAY(nOrbs,nHTags,/FLOAT,VALUE=0.) 
  HNegAvg      = MAKE_ARRAY(nOrbs,nHTags,/FLOAT,VALUE=0.) 
  HAbsAvg      = MAKE_ARRAY(nOrbs,nHTags,/FLOAT,VALUE=0.) 

  HMomAvg      = MAKE_ARRAY(nOrbs,nHMomTags,/FLOAT,VALUE=0.) 
  HMomPosAvg   = MAKE_ARRAY(nOrbs,nHMomTags,/FLOAT,VALUE=0.) 
  HMomNegAvg   = MAKE_ARRAY(nOrbs,nHMomTags,/FLOAT,VALUE=0.) 
  HMomAbsAvg   = MAKE_ARRAY(nOrbs,nHMomTags,/FLOAT,VALUE=0.) 

  ;; Get SW conds
  SW_info = SWAY_STATS__ADD_SW_INFO(t1, $
                                    /INITIALIZE_STRUCT)

  SW_infos     = REPLICATE(SWStat_SW_tmplt,nOrbs)

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
        DCAvg   [orbCnt,l]  = MEAN(     (ofloOrbDCArr[*,l])[safeDC],/NAN)
        DCABSAvg[orbCnt,l]  = MEAN( ABS((ofloOrbDCArr[*,l])[safeDC]),/NAN)
     ENDIF ELSE BEGIN
        DCAvg   [orbCnt,l]  = !VALUES.F_NaN
        DCABSAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF posDC[0] NE -1 THEN BEGIN
        DCPosAvg[orbCnt,l]  = MEAN( (ofloOrbDCArr[*,l])[posDC ],/NAN)
     ENDIF ELSE BEGIN
        DCPosAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF negDC[0] NE -1 THEN BEGIN
        DCNegAvg[orbCnt,l]  = MEAN( (ofloOrbDCArr[*,l])[negDC ],/NAN)
     ENDIF ELSE BEGIN
        DCNegAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF safeAC[0] NE -1 THEN BEGIN
        ACAvg[orbCnt,l]     = MEAN( (ofloOrbACArr[*,l])[safeAC],/NAN)
        ACABSAvg[orbCnt,l]  = MEAN( ABS((ofloOrbACArr[*,l])[safeAC]),/NAN)
     ENDIF ELSE BEGIN
        ACAvg[orbCnt,l]     = !VALUES.F_NaN
        ACABSAvg[orbCnt,l]  = !VALUES.F_NaN
     ENDELSE

     IF posAC[0] NE -1 THEN BEGIN
        ACPosAvg[orbCnt,l] = MEAN( (ofloOrbACArr[*,l])[posAC ],/NAN)
     ENDIF ELSE BEGIN
        ACPosAvg[orbCnt,l] = !VALUES.F_NaN
     ENDELSE

     IF negAC[0] NE -1 THEN BEGIN
        ACNegAvg[orbCnt,l] = MEAN( (ofloOrbACArr[*,l])[negAC ],/NAN)
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
                    HAvg,HPosAvg,HNegAvg,HAbsAvg, $
                    HMomAvg,HMomPosAvg,HMomNegAvg,HMomAbsAvg

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
  HPosAvg      = !NULL
  HNegAvg      = !NULL
  HAbsAvg      = !NULL

  HMomAvg      = !NULL
  HMomPosAvg   = !NULL
  HMomNegAvg   = !NULL
  HMomAbsAvg   = !NULL

END

;; 20180801  Can provide minILAT as an array of form [[orbit,minILAT],...]

FUNCTION EXTRACT_STRANGEWAY_STATS__V3, $
   RESTORE_LAST_FILE=restore_last_file, $
   USERDEF_HASHFILE=userDef_hashFile, $
   AVERAGES=averages, $
   ;; INTEGRALS=integrals, $ ;meaningless
   SKIP_THESE_ORBS=skip_these_orbs, $
   HMOM__USE_LOSSCONE_NOT_ALL_PITCHA=HMom__use_losscone_not_all_pitcha, $
   AVERAGE_OVER_WHOLE_PASS=average_over_whole_pass, $
   PTS_STRUCT=pts, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   MINMLT=minMLT, $
   MAXMLT=maxMLT, $
   MINILAT=minILAT, $
   MAXILAT=maxILAT, $
   ;; FOLD_INTERVALS=fold_intervals, $ ;Is default
   USE_INCLUDED_PFLUX=use_included_pFlux, $
   INTERP_E_B_TSERIES_TO_MAKE_PFLUX=interp_E_B_tSeries, $
   SAVE_PLOTS=save_plots, $
   PLOTDIR=plotDir, $
   PLOTS_PREFIX=plots_prefix, $
   SQUARE_WINDOW=square_window, $
   NO_PLOTS=no_plots, $
   OUT_PLOTINFO=plotInfo

  COMPILE_OPT IDL2

  COMMON SWSTATS_SWINFO,SWStat_SW_tmplt

  ;;Some outflow defaults
  ;;The originals are here. I started experimenting 2017/05/20
  ;; outflowMinLog10 = 5  ;No longer relevant, since the new methodology does a gooder job
  ptsMinOutflow   = 1
  allowableGap    = 180 ;seconds
  ;; min_streakLen_t = 10 ;;At least 30, right?

  ;; outflowMinLog10 = 6.0
  ;; ptsMinOutflow   = 5
  ;; ;; allowableGap    = 2 ;seconds
  ;; min_streakLen_t = 5 ;;At least 30, right?

  @strway_stuff

  ;; Can change which database to use here! There are currently three (2018/07/27)
  @strangeway_2005__defaults__v3.pro

  IF N_ELEMENTS(maxILAT) EQ 0 THEN maxILAT = 90

  HMom_index = KEYWORD_SET(HMom__use_losscone_not_all_pitcha) 

  IF KEYWORD_SET(userDef_hashFile) THEN BEGIN
     PRINT,"ACTUALLY, userDef hashFile: ",userDef_hashFile

     hashFile = userDef_hashFile
  ENDIF

  defs = SETUP_STRANGEWAY_STATS__DEFAULTS($
         AVERAGES=averages, $
         INTEGRALS=integrals, $
         NORTH=north, $
         SOUTH=south, $
         DAY=day, $
         NIGHT=night, $
         MINMLT=minMLT, $
         MAXMLT=maxMLT)

  allowGapStr = STRING(FORMAT='("-allowGap",I0)',allowableGap)
  wholePassStr = ''
  IF KEYWORD_SET(average_over_whole_pass) THEN BEGIN
     wholePassStr = "-wholePass"
     allowableGap = 0
     allowGapStr = ''
  ENDIF

  HMomString = STRING(FORMAT='("-HMomIdx",I0)',HMom_index)

  lastFile = "last_sway_stats_v3_file" $
             +(hashFile.Replace(indivOrbPref,"")).Replace(".sav","")$
             +defs.statStr+"-"+defs.sideStr+"-"+defs.hemStr $
             +HMomString+allowGapStr+wholePassStr+".sav"

  IF FILE_TEST(outDir+lastFile) AND KEYWORD_SET(restore_last_file) THEN BEGIN

     PRINT,"Restoring last file we did ..."
     RESTORE,outDir+lastFile

  ENDIF ELSE BEGIN

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
                                               Bind,Eind,Hind,HMomind,Pind, $
                                               Btags,Etags,Htags,HMomtags,Ptags, $
                                               nBTags,nETags,nHTags,nHMomTags,nPTags, $
                                               BPind,BVind,BBind, $
                                               EAVind,ENBind,EDSPind,EIENBind, $
                                               HJEeInd,HJeInd,HJiInd, $
                                               HMomNeInd,HMomCEInd,HMomTInd, $
                                               HMomJEeInd,HMomJeInd, $
                                               PPind,PVind,PBind,have_included_pFlux, $
                                               HMOM_INDEX=HMom_index)

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

     DECLARE_ARRAYS,nOrbs,nBTags,nETags,nPTags,nHTags,nHMomTags, $
                    orbArr,itvlArr,noENBArr, $
                    BDCArr,EDCArr,PDCArr, $
                    BACArr,EACArr,PACArr, $
                    HArr,HMomArr, $
                    BPTSArr,EPTSArr, HPTSArr, $
                    BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                    EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                    PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                    BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                    EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                    PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                    HAvg,HPosAvg,HNegAvg,HAbsAvg, $
                    HMomAvg,HMomPosAvg,HMomNegAvg,HMomAbsAvg, $
                    SW_infos

     totPtCnt     = 0           ;master counter
     orbCnt       = 0           ;orbit counter
     notUnivCnt   = 0           ;Number of intervals for which there is no universal time series
     FOREACH tmpStruct, swHash, key DO BEGIN

        ;;Anything here?
        IF N_ELEMENTS(tmpStruct) EQ 0 THEN CONTINUE

        IF KEYWORD_SET(skip_these_orbs) THEN BEGIN
           ;; FOREACH skipper,skip_these_orbs DO BEGIN
           IF (WHERE(key EQ skip_these_orbs))[0] NE -1 THEN BEGIN
              PRINT,FORMAT='("Skipping orbit ",I0," ...")',key
              CONTINUE
           ENDIF
        ENDIF

        ;;How many?
        nItvls    = N_ELEMENTS(tmpStruct)

        IF nItvls EQ 0 THEN CONTINUE

        nThisItvl     = MAKE_ARRAY(nItvls,/LONG)

        FOR k=0,nItvls-1 DO nThisItvl[k] = N_ELEMENTS(tmpStruct[k].(0).(0).x)

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
        ofloOrbHMomArr= MAKE_ARRAY(nThisOrb,nHMomTags,/FLOAT,VALUE=0.) 

        IF nThisOrb NE 0 THEN BEGIN


           ;;Now loop over intervals within this orbit
           orbPtCnt   = 0 ;;Keep track of how many we've gone over in this orbit
           FOR k=0,nItvls-1 DO BEGIN

              ;;Pick up fields, align time series
              dBp        = PTR_NEW(tmpStruct[k].(Bind).(BPind))
              dBv        = PTR_NEW(tmpStruct[k].(Bind).(BVind))
              dBB        = PTR_NEW(tmpStruct[k].(Bind).(BBind))

              eAV        = PTR_NEW(tmpStruct[k].(Eind).(EAVind))
              eNB        = PTR_NEW(tmpStruct[k].(Eind).(ENBind))
              ;; eDSP       = PTR_NEW(tmpStruct[k].(Eind).(EDSPind))

              ;; STOP

              dsp = tmpStruct[k].(Eind).(EDSPind)
              dspTags = STRLOWCASE(TAG_NAMES(dsp))
              IF (WHERE(dspTags EQ "dc"))[0] EQ -1 THEN BEGIN
                 dspDat = dsp.y
                 dspInd = (WHERE(dspTags EQ "y"))[0]
              ENDIF ELSE BEGIN
                 dspDat = dsp.DC + dsp.AC
                 dspInd = (WHERE(dspTags EQ "dc"))[0]
              ENDELSE

              dsp.(dspInd) = TEMPORARY(dspDat)

              eDSP = PTR_NEW(TEMPORARY(dsp))

                    ;; CASE N_ELEMENTS(dspTags) OF
                    ;;    2: BEGIN

                    ;;       dspDat = (*eDSP).(1)
                    ;;    END
                    ;;    3: BEGIN
                    ;;       dspDat = (*eDSP).(1) + (*eDSP).(2)
                    ;;    END
                    ;; ENDCASE
                    ;; dspYInd = (WHERE(dspTags EQ "y" OR $
                    ;;              dspTags EQ "dc"))[0]
                    ;; eDSP    = PTR_NEW({y : DATA_CUT({x:(*eDSP).x,y:(*eDSP).(dspYInd)},(*pUniv_ts), $
                    ;; eDSP    = PTR_NEW({y : DATA_CUT({x:(*eDSP).x, $
                    ;;                                  y:TEMPORARY(dspDat)},(*pUniv_ts), $
                    ;;                                  COUNT=count, $
                    ;;                                  GAP_THRESH=gap_thresh, $
                    ;;                                  INTERP_GAP=interp_gap, $
                    ;;                                  GAP_DIST=gap_dist, $
                    ;;                                  MISSING=missing, $
                    ;;                                  IGNORE_NAN=ignore_nan)})
                    

              ;;Particles for hjar
              tmpJEe     = tmpStruct[k].(Hind).(HJEeInd)
              tmpJe      = tmpStruct[k].(Hind).(HJeInd)
              tmpJi      = tmpStruct[k].(Hind).(HJiInd)

              PRINT,FORMAT='(A0,T25,I5,", ",I2)',"Orbit, Interval :",key,k

              have_univ_TS  = BYTE(TAG_EXIST(tmpStruct[k].(0).(0),'COMMONEST_TS'))

              CASE 1 OF
                 have_univ_TS: BEGIN

                    PRINT,"UNIVERSAL"

                    Puniv_TS   = PTR_NEW(tmpStruct[k].(0).(0).(0))

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

                 END
                 KEYWORD_SET(interp_E_B_tSeries): BEGIN

                    have_B_TS  = BYTE(TAG_EXIST(tmpStruct[k].(BInd).(0),'COMMON_TS'))
                    have_E_TS  = BYTE(TAG_EXIST(tmpStruct[k].(EInd).(0),'COMMON_TS'))
                    have_H_TS  = BYTE(TAG_EXIST(tmpStruct[k].(HInd).(0),'COMMON_TS'))
                    ;; IF Pind NE -1 THEN BEGIN
                    ;;    have_P_TS  = BYTE(TAG_EXIST(tmpStruct[k].(PInd).(0),'COMMON_TS'))
                    ;; ENDIF

                    IF ~have_E_TS THEN BEGIN
                       have_E_TS = ARRAY_EQUAL(tmpStruct[k].(EInd).alongV.x,tmpStruct[k].(EInd).nearB.x)
                    ENDIF

                    IF have_B_TS THEN BEGIN
                       
                       ;;Interp time series
                       t1          = tmpStruct[k].(BInd).(0).x[0]
                       t2          = tmpStruct[k].(BInd).(0).x[-1]
                       Puniv_TS    = PTR_NEW(DOUBLE(LINDGEN(CEIL(t2-t1))+ROUND(t1)))

                       ;; have_B_TS   = 1B
                       ;; have_E_TS   = 1B
                       ;; have_H_TS   = 1B

                       PB_TS       = Puniv_TS
                       PE_TS       = Puniv_TS
                       PH_TS       = Puniv_TS

                       BPTSArr[*]  = Puniv_TS
                       EPTSArr[*]  = Puniv_TS
                       HPTSArr[*]  = Puniv_TS

                       nHere       = N_ELEMENTS(*Puniv_TS)

                    ENDIF ELSE BEGIN
                       STOP
                    ENDELSE

                    ;;Now that we have a universal time array, everyone gets interped to death

                    ;;Pick up fields, align time series
                    ;; dBp        = tmpStruct[k].(Bind).(BPind)
                    ;; dBv        = tmpStruct[k].(Bind).(BVind)
                    ;; dBB        = tmpStruct[k].(Bind).(BBind)

                    dBp        = PTR_NEW({x  : (*pUniv_ts), $
                                          DC : DATA_CUT({x:(*dBp).x,y:(*dBp).DC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan), $
                                          AC : DATA_CUT({x:(*dBp).x,y:(*dBp).AC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan), $
                                          COMMON_TS    : 1, $
                                          COMMONEST_TS : 1})

                    dBv        = PTR_NEW({DC : DATA_CUT({x:(*dBp).x,y:(*dBv).DC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan), $
                                          AC : DATA_CUT({x:(*dBp).x,y:(*dBv).AC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan)})

                    dBB        = PTR_NEW({DC : DATA_CUT({x:(*dBp).x,y:(*dBB).DC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan), $
                                          AC : DATA_CUT({x:(*dBp).x,y:(*dBB).AC},(*pUniv_ts), $
                                                        COUNT=count, $
                                                        GAP_THRESH=gap_thresh, $
                                                        INTERP_GAP=interp_gap, $
                                                        GAP_DIST=gap_dist, $
                                                        MISSING=missing, $
                                                        IGNORE_NAN=ignore_nan)})

                    IF have_E_TS THEN BEGIN

                       ;; eAV        = tmpStruct[k].(Eind).(EAVind)
                       ;; eNB        = tmpStruct[k].(Eind).(ENBind)

                       tmpETS     = (*eAV).x

                       eAV        = PTR_NEW({DC : DATA_CUT({x:tmpETS,y:(*eAV).DC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                             AC : DATA_CUT({x:tmpETS,y:(*eAV).AC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                             COMMON_TS : 1})

                       eNB        = PTR_NEW({DC : DATA_CUT({x:tmpETS,y:(*eNB).DC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                             AC : DATA_CUT({x:tmpETS,y:(*eNB).AC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan)})

                       tmpETS = !NULL
                       
                    ENDIF ELSE BEGIN
                       STOP
                    ENDELSE

                    ;; eDSP    = PTR_NEW({DC : DATA_CUT({x:(*eDSP).x,y:(*eDSP).DC},(*pUniv_ts), $
                    ;;                                  COUNT=count, $
                    ;;                                  GAP_THRESH=gap_thresh, $
                    ;;                                  INTERP_GAP=interp_gap, $
                    ;;                                  GAP_DIST=gap_dist, $
                    ;;                                  MISSING=missing, $
                    ;;                                  IGNORE_NAN=ignore_nan), $
                    ;;                    AC : DATA_CUT({x:(*eDSP).x,y:(*eDSP).AC},(*pUniv_ts), $
                    ;;                                  COUNT=count, $
                    ;;                                  GAP_THRESH=gap_thresh, $
                    ;;                                  INTERP_GAP=interp_gap, $
                    ;;                                  GAP_DIST=gap_dist, $
                    ;;                                  MISSING=missing, $
                    ;;                                  IGNORE_NAN=ignore_nan)})
                    
                    ;;Now interp particles
                    tS = !NULL
                    STR_ELEMENT,tmpJe,"x",tS
                    IF N_ELEMENTS(tS) EQ 0 THEN BEGIN
                       IF N_ELEMENTS(tmpJe.y) NE N_ELEMENTS(tmpJee.x) THEN STOP
                       tmpJe = {x : tmpJee.x, y : tmpJe.y}
                    ENDIF

                    tS = !NULL
                    STR_ELEMENT,tmpJi,"x",tS
                    IF N_ELEMENTS(tS) EQ 0 THEN BEGIN
                       IF N_ELEMENTS(tmpJi.y) NE N_ELEMENTS(tmpJee.x) THEN STOP
                       tmpJi = {x : tmpJee.x, y : tmpJi.y}
                    ENDIF

                    tmpJe    = {y : DATA_CUT(tmpJe,(*pUniv_ts), $
                                             COUNT=count, $
                                             GAP_THRESH=gap_thresh, $
                                             INTERP_GAP=interp_gap, $
                                             GAP_DIST=gap_dist, $
                                             MISSING=missing, $
                                             IGNORE_NAN=ignore_nan)}
                    
                    tmpJEe   = {y : DATA_CUT(tmpJEe,(*pUniv_ts), $
                                             COUNT=count, $
                                             GAP_THRESH=gap_thresh, $
                                             INTERP_GAP=interp_gap, $
                                             GAP_DIST=gap_dist, $
                                             MISSING=missing, $
                                             IGNORE_NAN=ignore_nan)}

                    tmpJi    = {y : DATA_CUT(tmpJi,(*pUniv_ts), $
                                             COUNT=count, $
                                             GAP_THRESH=gap_thresh, $
                                             INTERP_GAP=interp_gap, $
                                             GAP_DIST=gap_dist, $
                                             MISSING=missing, $
                                             IGNORE_NAN=ignore_nan)}

                    IF KEYWORD_SET(use_included_pFlux) THEN BEGIN
                       IF N_ELEMENTS(tmpStruct[k].(Bind).(BPind).x) NE N_ELEMENTS(tmpstruct[k].(Pind).(PBind).DC) THEN BEGIN
                          PRINT,"B-field and Poynting flux t Series should match. Maybe I'm comparing the wrong two, but they should all match anyway, right?"
                          STOP
                       ENDIF

                       pFB = PTR_NEW(tmpStruct[k].(Pind).(PBind))
                       pFP = PTR_NEW(tmpStruct[k].(Pind).(PPind))

                       pFB        = PTR_NEW({x  : (*pUniv_ts), $
                                             DC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                            y:(*pFB).DC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                             AC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                            y:(*pFB).AC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                          COMMON_TS    : 1, $
                                          COMMONEST_TS : 1})

                       pFP        = PTR_NEW({x  : (*pUniv_ts), $
                                             DC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                            y:(*pFP).DC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                             AC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                            y:(*pFP).AC},(*pUniv_ts), $
                                                           COUNT=count, $
                                                           GAP_THRESH=gap_thresh, $
                                                           INTERP_GAP=interp_gap, $
                                                           GAP_DIST=gap_dist, $
                                                           MISSING=missing, $
                                                           IGNORE_NAN=ignore_nan), $
                                          COMMON_TS    : 1, $
                                          COMMONEST_TS : 1})

                       IF SIZE(tmpStruct[k].(Pind).(PVind),/TYPE) EQ 8 THEN BEGIN

                          pFV = PTR_NEW(tmpStruct[k].(Pind).(PVind))

                          pFV        = PTR_NEW({x  : (*pUniv_ts), $
                                                DC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                               y:(*pFV).DC},(*pUniv_ts), $
                                                              COUNT=count, $
                                                              GAP_THRESH=gap_thresh, $
                                                              INTERP_GAP=interp_gap, $
                                                              GAP_DIST=gap_dist, $
                                                              MISSING=missing, $
                                                              IGNORE_NAN=ignore_nan), $
                                                AC : DATA_CUT({x:tmpStruct[k].(Bind).(BPind).x, $
                                                               y:(*pFV).AC},(*pUniv_ts), $
                                                              COUNT=count, $
                                                              GAP_THRESH=gap_thresh, $
                                                              INTERP_GAP=interp_gap, $
                                                              GAP_DIST=gap_dist, $
                                                              MISSING=missing, $
                                                              IGNORE_NAN=ignore_nan), $
                                                COMMON_TS    : 1, $
                                                COMMONEST_TS : 1})

                       ENDIF ELSE BEGIN

                          pFV = PTR_NEW(*pFB)
                          (*pFV).DC = 0.D
                          (*pFV).AC = 0.D

                       ENDELSE

                    ENDIF

                    ;;Now tell the world the joy
                    have_univ_TS = 1

                 END
                 ELSE: BEGIN

                    notUnivCnt++
                    ;; PRINT,"NOT UNIVERSAL"

                    have_B_TS  = BYTE(TAG_EXIST(tmpStruct[k].(BInd).(0),'COMMON_TS'))
                    have_E_TS  = BYTE(TAG_EXIST(tmpStruct[k].(EInd).(0),'COMMON_TS'))
                    have_H_TS  = BYTE(TAG_EXIST(tmpStruct[k].(HInd).(0),'COMMON_TS'))

                    IF ~have_E_TS THEN BEGIN
                       have_E_TS = ARRAY_EQUAL(tmpStruct[k].(EInd).alongV.x,tmpStruct[k].(EInd).nearB.x)
                    ENDIF

                    IF ~(have_B_TS AND have_E_TS AND have_H_TS) THEN BEGIN
                       PRINT,"Simply don't know what to tell you. I'm not cut out for this situation—gots to be able to interpolate or something!"
                       STOP
                    ENDIF

                    IF have_B_TS THEN BEGIN
                       PB_TS          = PTR_NEW(tmpStruct[k].(BInd).(0).(0))
                       BPTSArr[*]     = PB_TS
                    ENDIF ELSE BEGIN
                       FOR p=0,nBTags-1 DO BEGIN
                          BPTSArr[p]  = PTR_NEW(tmpStruct[k].(BInd).(p).(0))
                       ENDFOR
                    ENDELSE

                    IF have_E_TS THEN BEGIN
                       PE_TS          = PTR_NEW(tmpStruct[k].(EInd).(0).(0))
                       BPTSArr[*]     = PB_TS
                    ENDIF ELSE BEGIN
                       FOR p=0,nETags-1 DO BEGIN
                          EPTSArr[p]  = PTR_NEW(tmpStruct[k].(EInd).(p).(0))
                       ENDFOR
                    ENDELSE

                    IF have_H_TS THEN BEGIN
                       PH_TS          = PTR_NEW(tmpStruct[k].(HInd).(0).(0))
                       HPTSArr[*]     = PH_TS
                    ENDIF ELSE BEGIN
                       FOR p=0,nETags-1 DO BEGIN
                          HPTSArr[p]  = PTR_NEW(tmpStruct[k].(HInd).(p).(0))
                       ENDFOR
                    ENDELSE

                    ;; Now we interp, I guess

                 END
              ENDCASE

              ;; ENDFOR


              CASE 1 OF
                 have_univ_TS: BEGIN ;;Already sammen
                    BclosE_i = INDGEN(N_ELEMENTS((*PUniv_TS)))
                    EclosB_i = INDGEN(N_ELEMENTS((*PUniv_TS)))
                 END
                 (have_B_TS AND have_E_TS): BEGIN
                    ;;Need to work on å sammenligne dem
                    BclosE_i = VALUE_CLOSEST2((*PB_TS),(*PE_TS),/CONSTRAINED)
                    EclosB_i = VALUE_CLOSEST2((*PE_TS),(*PB_TS),/CONSTRAINED)

                    BclosE_i = BclosE_i[WHERE(ABS( (*PB_TS)[BclosE_i] - (*PE_TS) ) LE 0.2)]
                    EclosB_i = EclosB_i[WHERE(ABS( (*PE_TS)[EclosB_i] - (*PB_TS) ) LE 0.2)]
                 END
                 ELSE: STOP
              ENDCASE

              IF ~KEYWORD_SET(full_pFlux) THEN BEGIN

                 (*eNB).DC[*]  = 0.
                 (*eNB).AC[*]  = 0.

              ENDIF ELSE BEGIN
                 ;;Keep some stats if we're going to attempt the full pFlux sitiation
                 ;;As always, I do mean sitiation
                 IF ~tmpStruct[k].(Eind).(EIENBind) THEN noENBArr[*,orbCnt+k] = [key,k]
              ENDELSE

              IF KEYWORD_SET(use_included_pFlux) THEN BEGIN

                 IF ~KEYWORD_SET(interp_E_B_tSeries) THEN BEGIN

                    pFB = PTR_NEW(tmpStruct[k].(Pind).(PBind))
                    pFP = PTR_NEW(tmpStruct[k].(Pind).(PPind))

                    IF SIZE(tmpStruct[k].(Pind).(PVind),/TYPE) EQ 8 THEN BEGIN

                       pFV = PTR_NEW(tmpStruct[k].(Pind).(PVind))

                    ENDIF ELSE BEGIN

                       pFV = PTR_NEW(*pFB)
                       (*pFV).DC = 0.D
                       (*pFV).AC = 0.D

                    ENDELSE

                    ;; tmpJi    = {y : DATA_CUT({x:,y:tmpJi},(*pUniv_ts), $
                    ;;                          COUNT=count, $
                    ;;                          GAP_THRESH=gap_thresh, $
                    ;;                          INTERP_GAP=interp_gap, $
                    ;;                          GAP_DIST=gap_dist, $
                    ;;                          MISSING=missing, $
                    ;;                          IGNORE_NAN=ignore_nan)}
                 ENDIF

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
              ;; oflow_i = WHERE((ALOG10(ABS(tmpJi.y)) GE outflowMinLog10) AND $
              ;;                 (FINITE(tmpJi.y))                         AND $
              ;;                 (tmpJi.y GT 0),nOutflow)
              mustZap = WHERE(tmpJi.y LE 0)
              IF mustZap[0] NE -1 THEN BEGIN
                 PRINT,"NANNING NEGATIVE ION FLUXES"
                 tmpJi.y[mustZap] = !VALUES.F_NaN
                 ;; STOP
              ENDIF

              oflow_i = WHERE((FINITE(tmpJi.y))                         AND $
                              (tmpJi.y GT 0),nOutflow)
              ;; oflow_i = WHERE((FINITE(tmpJi.y))                         AND $
              ;;                 (tmpJi.y GT 5e5),nOutflow)
              ;; print,"First ji point:",tmpJi.y[0]

              tSerie = (N_ELEMENTS(PUniv_TS) GT 0 ? (*PUniv_TS) : (*PH_TS))
              ;; Add everything from 10 min before to 10 after
              ;; t10before = tSerie[oflow_i[0]]-300.D
              ;; ;; t10after  = tSerie[oflow_i[-1]]+300.D
              ;; t10after  = tSerie[oflow_i[-1]]

              ;; PRINT,"First data point: ",T2S(tSerie[0])
              ;; PRINT,"t10before       : ",T2S(t10before)

              ;; oflow_i   = WHERE(tSerie GE t10before AND tSerie LE t10after)

              ;; IF key EQ 8274 THEN STOP

              IF KEYWORD_SET(average_over_whole_pass) THEN BEGIN
                 oflow_i = LINDGEN(N_ELEMENTS(tmpJi.y))
              ENDIF

              IF nOutflow LT ptsMinOutflow THEN CONTINUE

              LOCALE_ADJUSTMENTS,tSerie, $
                                 oflow_i,nOutflow, $
                                 ORBIT=key, $
                                 NORTH=north, $
                                 SOUTH=south, $
                                 DAY=day, $
                                 MINMLT=minMLT, $
                                 MAXMLT=maxMLT, $
                                 NIGHT=night, $
                                 MINILAT=minILAT, $
                                 MAXILAT=maxILAT

              IF nOutflow LT ptsMinOutflow THEN CONTINUE

              ;;Get outflow intervals

              start_ii = !NULL  ;Dotsa reset these manually
              stop_ii  = !NULL
              lens     = !NULL
              GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE, $
                 (*HPTSArr[HJiInd])[oflow_i],0, $
                 NPTS=ptsMinOutflow, $
                 MIN_T_STREAKLEN=min_streakLen_t, $
                 GAP_TIME=allowableGap, $
                 START_I=start_ii, $
                 STOP_I=stop_ii, $
                 STREAKLENS=lens

              IF ~(ISA(start_ii) AND ISA(stop_ii)) THEN BEGIN
                 PRINT,"No outflow streaks of sufficient length here ..."
                 CONTINUE
              ENDIF

              IF start_ii[0] EQ stop_ii[0] THEN BEGIN
                 PRINT,"No outflow streaks of sufficient length here ..."
                 CONTINUE
              ENDIF

              ;;OK, now we know that there's going to be action. 
              ;;If we're this far, we're going to average all outflow points
              nPts      = (N_ELEMENTS(PUniv_TS) GT 0 ? N_ELEMENTS((*PUniv_TS)) : N_ELEMENTS((*PH_TS)))
              nStreaks  = N_ELEMENTS(start_ii)
              nOfloPts  = FIX(TOTAL(lens) + nStreaks)

              PRINT,"n Outflow/not: " + STRCOMPRESS(nOfloPts,/REMOVE_ALL) + '/' + STRCOMPRESS(nPts,/REMOVE_ALL) + ' (' + $
                    STRCOMPRESS(FLOAT(nOfloPts)/nPts*100.,/REMOVE_ALL) + '%)'

              ofloItvlBDCArr = MAKE_ARRAY(nOfloPts,nBTags,/FLOAT,VALUE=0.) 
              ofloItvlEDCArr = MAKE_ARRAY(nOfloPts,nETags,/FLOAT,VALUE=0.) 
              ofloItvlPDCArr = MAKE_ARRAY(nOfloPts,nPTags,/FLOAT,VALUE=0.) 

              ofloItvlBACArr = MAKE_ARRAY(nOfloPts,nBTags,/FLOAT,VALUE=0.) 
              ofloItvlEACArr = MAKE_ARRAY(nOfloPts,nETags,/FLOAT,VALUE=0.) 
              ofloItvlPACArr = MAKE_ARRAY(nOfloPts,nPTags,/FLOAT,VALUE=0.) 

              ofloItvlHArr   = MAKE_ARRAY(nOfloPts,nHTags,/FLOAT,VALUE=0.) 
              ofloItvlHMomArr= MAKE_ARRAY(nOfloPts,nHMomTags,/FLOAT,VALUE=0.) 

              ofloItvlPtCnt  = 0

              IF k EQ 0 THEN BEGIN
                 SW_infos[orbCnt] = SWAY_STATS__ADD_SW_INFO( $
                                    tmpStruct[0].(0).(0).(0)[0], $
                                    MINUTESBEFORE=minutesBefore, $
                                    MINUTESAVERAGE=minutesAverage)
              ENDIF

              FOR l=0,nStreaks-1 DO BEGIN

                 ;; curInds  = [orbPtCnt:orbPtCnt+nThisItvl[k]-1]

                 ;; Den gamle ordning, pre-2018-07-31
                 ;; arrInds  = [ofloItvlPtCnt:ofloItvlPtCnt+lens[l]]
                 ;; strkInds  = [oflow_i[start_ii[l]]:oflow_i[stop_ii[l]]]

                 ;; Den nye ordning post-2018-07-31
                 strkInds  = [oflow_i[start_ii[l]]:oflow_i[stop_ii[l]]]
                 arrInds  = strkInds-MIN(strkInds)+ofloItvlPtCnt

                 ;; PRINT,FORMAT='(A0,I0,":",I0,A0)',"arrInds : [",arrInds[0],arrInds[-1],"]"
                 ;; PRINT,FORMAT='(A0,I0,":",I0,A0)',"strkInds : [",strkInds[0],strkInds[-1],"]"

                 ;;Fields DC
                 ofloItvlBDCArr[arrInds,*] = [[(*dBp).DC[strkInds]], $
                                              [(*dBv).DC[strkInds]], $
                                              [(*dBB).DC[strkInds]]]
                 ofloItvlEDCArr[arrInds,*] = [[(*eAV).DC[strkInds]], $
                                              [(*eNB).DC[strkInds]], $
                                              ;; [(*eDSP).DC[strkInds]]]
                                              [(*eDSP).(dspInd)[strkInds]]]
                 ofloItvlPDCArr[arrInds,*] = [[(*pFP).DC[strkInds]], $
                                              [(*pFV).DC[strkInds]], $
                                              [(*pFB).DC[strkInds]]]

                 ;;Fields AC
                 ofloItvlBACArr[arrInds,*] = [[(*dBp).AC[strkInds]], $
                                              [(*dBv).AC[strkInds]], $
                                              [(*dBB).AC[strkInds]]]
                 ofloItvlEACArr[arrInds,*] = [[(*eAV).AC[strkInds]], $
                                              [(*eNB).AC[strkInds]], $
                                              ;; [(*eDSP).AC[strkInds]]]
                                              [(*eDSP).(dspInd)[strkInds]]]
                 ofloItvlPACArr[arrInds,*] = [[(*pFP).AC[strkInds]], $
                                              [(*pFV).AC[strkInds]], $
                                              [(*pFB).AC[strkInds]]]

                 ;;Particles
                 ofloItvlHArr[arrInds,*]   = [[tmpStruct[k].(Hind).(HJEeInd).y[strkInds]], $
                                              [tmpStruct[k].(Hind).(HJeInd).y[strkInds]], $
                                              [tmpJi.y[strkInds]]]

                 ofloItvlHMomArr[arrInds,*] = [[tmpStruct[k].(HMomind).(HMom_index).(HMomNeInd)[strkInds]], $
                                              [tmpStruct[k].(HMomind).(HMom_index).(HMomCEInd)[strkInds]], $
                                              [tmpStruct[k].(HMomind).(HMom_index).(HMomTInd)[strkInds]], $
                                              [tmpStruct[k].(HMomind).(HMom_index).(HMomJEeInd)[strkInds]], $
                                              [tmpStruct[k].(HMomind).(HMom_index).(HMomJeInd)[strkInds]]]

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

              ofloOrbHMomArr[tmpItvlInds,*] = ofloItvlHMomArr[tmpOfloInds,*]

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

           ofloOrbHMomArr= ofloOrbHMomArr[tmpOrbInds,*]

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
                 HAvg[orbCnt,l]     = MEAN( (HArr[tmpTotInds,l])[safe],/NAN)
                 HAbsAvg[orbCnt,l]  = MEAN( ABS((HArr[tmpTotInds,l])[safe]),/NAN)

                 ;; 2018/07/27 Just wanted to experiment with this
                 ;; HAvg[orbCnt,l]     = 10.D^(MEAN( (ALOG10(ABS((HArr[tmpTotInds,l]))))[safe],/NAN))

                 pos = CGSETINTERSECTION(safe,WHERE(ofloOrbHArr[*,l] GT 0),COUNT=nPos)
                 neg = CGSETINTERSECTION(safe,WHERE(ofloOrbHArr[*,l] LT 0),COUNT=nNeg)

                 IF pos[0] NE -1 THEN BEGIN
                    HPosAvg[orbCnt,l]  = MEAN( (HArr[tmpTotInds,l])[pos],/NAN)
                 ENDIF ELSE BEGIN
                    HPosAvg[orbCnt,l]  = !VALUES.F_NaN
                 ENDELSE

                 IF neg[0] NE -1 THEN BEGIN
                    HNegAvg[orbCnt,l]  = MEAN( (HArr[tmpTotInds,l])[neg],/NAN)
                 ENDIF ELSE BEGIN
                    HNegAvg[orbCnt,l]  = !VALUES.F_NaN
                 ENDELSE

              ENDIF ELSE BEGIN
                 HAvg[orbCnt,l]     = !VALUES.F_NaN
                 HPosAvg[orbCnt,l]  = !VALUES.F_NaN
                 HNegAvg[orbCnt,l]  = !VALUES.F_NaN
                 HAbsAvg[orbCnt,l]  = !VALUES.F_NaN

              ENDELSE

           ENDFOR

           ;;Loop over particle array STRUCT MOM DAD stuff
           FOR l=0,nHMomTags-1 DO BEGIN

              HMomArr  [tmpTotInds,l]  = ofloOrbHMomArr[*,l]

              ;; safeDC                = WHERE(FINITE(ofloOrbHDCArr[*,l]),nSafeDC)
              ;; safeAC                = WHERE(FINITE(ofloOrbHACArr[*,l]),nSafeAC)

              safe                  = WHERE(FINITE(HMomArr[tmpTotInds,l]),nSafe)
              IF safe[0] NE -1 THEN BEGIN
                 HMomAvg[orbCnt,l]     = MEAN( (HMomArr[tmpTotInds,l])[safe],/NAN)
                 HMomAbsAvg[orbCnt,l]  = MEAN( ABS((HMomArr[tmpTotInds,l])[safe]),/NAN)

                 ;; 2018/07/27 Just wanted to experiment with this
                 ;; HAvg[orbCnt,l]     = 10.D^(MEAN( (ALOG10(ABS((HMomArr[tmpTotInds,l]))))[safe],/NAN))

                 pos = CGSETINTERSECTION(safe,WHERE(ofloOrbHMomArr[*,l] GT 0),COUNT=nPos)
                 neg = CGSETINTERSECTION(safe,WHERE(ofloOrbHMomArr[*,l] LT 0),COUNT=nNeg)

                 IF pos[0] NE -1 THEN BEGIN
                    HMomPosAvg[orbCnt,l]  = MEAN( (HMomArr[tmpTotInds,l])[pos],/NAN)
                 ENDIF ELSE BEGIN
                    HMomPosAvg[orbCnt,l]  = !VALUES.F_NaN
                 ENDELSE

                 IF neg[0] NE -1 THEN BEGIN
                    HMomNegAvg[orbCnt,l]  = MEAN( (HMomArr[tmpTotInds,l])[neg],/NAN)
                 ENDIF ELSE BEGIN
                    HMomNegAvg[orbCnt,l]  = !VALUES.F_NaN
                 ENDELSE

              ENDIF ELSE BEGIN
                 HMomAvg[orbCnt,l]     = !VALUES.F_NaN
                 HMomPosAvg[orbCnt,l]  = !VALUES.F_NaN
                 HMomNegAvg[orbCnt,l]  = !VALUES.F_NaN
                 HMomAbsAvg[orbCnt,l]  = !VALUES.F_NaN

              ENDELSE

           ENDFOR

           totPtCnt += orbPtCnt

           ;; Get SW conds
           ;; SW_infos[orbCnt] = SWAY_STATS__ADD_SW_INFO( $
           ;;                    tmpStruct[0].(0).(0).(0), $
           ;;                    MINUTESBEFORE=minutesBefore, $
           ;;                    MINUTESAVERAGE=minutesAverage)

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
                          'NearB' ,{DC:{avg:EDCAvg[*,1], $
                                        pos:EDCPosAvg[*,1], $
                                        neg:EDCNegAvg[*,1], $
                                        abs:EDCAbsAvg[*,1]}, $
                                    AC:{avg:EACAvg[*,1], $
                                        pos:EACPosAvg[*,1], $
                                        neg:EACNegAvg[*,1], $
                                        abs:EACAbsAvg[*,1]}}, $
                          ;; 'DSP'   ,{DC:{avg:EDCAvg[*,2], $
                          ;;               pos:EDCPosAvg[*,2], $
                          ;;               neg:EDCNegAvg[*,2], $
                          ;;               abs:EDCAbsAvg[*,2]}, $
                          ;;           AC:{avg:EACAvg[*,2], $
                          ;;               pos:EACPosAvg[*,2], $
                          ;;               neg:EACNegAvg[*,2], $
                          ;;               abs:EACAbsAvg[*,2]}})
                          'DSP'   ,{y:{avg:EDCAvg[*,2], $
                                        pos:EDCPosAvg[*,2], $
                                        neg:EDCNegAvg[*,2], $
                                        abs:EDCAbsAvg[*,2]}})

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

     HAvg = CREATE_STRUCT('JEe',{y:{avg:HAvg[*,0], $
                                    pos:HPosAvg[*,0], $
                                    neg:HNegAvg[*,0], $
                                    abs:HAbsAvg[*,0]}}, $
                          ;; abs:HAbsAvg[*,0]}, $
                          ;; AC:{avg:HACAvg[*,0], $
                          ;; pos:HACPosAvg[*,0], $
                          ;; neg:HACNegAvg[*,0], $
                          ;; abs:HACAbsAvg[*,0]}}, $
                          'Je',{y:{avg:HAvg[*,1], $
                                   pos:HPosAvg[*,1], $
                                   neg:HNegAvg[*,1], $
                                   abs:HAbsAvg[*,1]}}, $
                          ;; abs:HAbsAvg[*,1]}, $
                          ;; AC:{avg:HACAvg[*,1], $
                          ;; pos:HACPosAvg[*,1], $
                          ;; neg:HACNegAvg[*,1], $
                          ;; abs:HACAbsAvg[*,1]}}, $
                          'Ji',{y:{avg:HAvg[*,2], $
                                   pos:HPosAvg[*,2], $
                                   neg:HNegAvg[*,2], $
                                   abs:HAbsAvg[*,2]}})
     ;; pos:HPosAvg[*,2], $
     ;; neg:HNegAvg[*,2], $
     ;; abs:HAbsAvg[*,2]}, $
     ;; AC:{avg:HACAvg[*,2], $
     ;; pos:HACPosAvg[*,2], $
     ;; neg:HACNegAvg[*,2], $
     ;; abs:HACAbsAvg[*,2]}})

     HMomAvg = CREATE_STRUCT('densE',{y:{avg:HMomAvg[*,0], $
                                         pos:HMomPosAvg[*,0], $
                                         neg:HMomNegAvg[*,0], $
                                         abs:HMomAbsAvg[*,0]}}, $
                             ;; abs:HMomAbsAvg[*,0]}, $
                             ;; AC:{avg:HMomACAvg[*,0], $
                             ;; pos:HMomACPosAvg[*,0], $
                             ;; neg:HMomACNegAvg[*,0], $
                             ;; abs:HMomACAbsAvg[*,0]}}, $
                             'CEe',{y:{avg:HMomAvg[*,1], $
                                       pos:HMomPosAvg[*,1], $
                                       neg:HMomNegAvg[*,1], $
                                       abs:HMomAbsAvg[*,1]}}, $
                             ;; abs:HMomAbsAvg[*,1]}, $
                             ;; AC:{avg:HMomACAvg[*,1], $
                             ;; pos:HMomACPosAvg[*,1], $
                             ;; neg:HMomACNegAvg[*,1], $
                             ;; abs:HMomACAbsAvg[*,1]}}, $
                             'Te',{y:{avg:HMomAvg[*,2], $
                                   pos:HMomPosAvg[*,2], $
                                   neg:HMomNegAvg[*,2], $
                                      abs:HMomAbsAvg[*,2]}}, $
                             'JEe',{y:{avg:HMomAvg[*,3], $
                                       pos:HMomPosAvg[*,3], $
                                       neg:HMomNegAvg[*,3], $
                                       abs:HMomAbsAvg[*,3]}}, $
                             'Je',{y:{avg:HMomAvg[*,4], $
                                       pos:HMomPosAvg[*,4], $
                                       neg:HMomNegAvg[*,4], $
                                       abs:HMomAbsAvg[*,4]}})

     avgStruct = {orbit  : (swHash.Keys()).ToArray(), $
                  dB     : TEMPORARY(dBAvg), $
                  E      : TEMPORARY(EAvg), $
                  PFLUX  : TEMPORARY(PAvg), $
                  PTCL   : TEMPORARY(HAvg), $
                  PTCLMom: TEMPORARY(HMomAvg)}

     KILL_AVG_ARRAYS,BDCAvg,BDCAbsAvg,BDCPosAvg,BDCNegAvg, $
                     EDCAvg,EDCAbsAvg,EDCPosAvg,EDCNegAvg, $
                     PDCAvg,PDCAbsAvg,PDCPosAvg,PDCNegAvg, $
                     BACAvg,BACAbsAvg,BACPosAvg,BACNegAvg, $
                     EACAvg,EACAbsAvg,EACPosAvg,EACNegAvg, $
                     PACAvg,PACAbsAvg,PACPosAvg,PACNegAvg, $
                     HAvg,HPosAvg,HNegAvg,HAbsAvg, $
                     HMomAvg,HMomPosAvg,HMomNegAvg,HMomAbsAvg

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

     HMomArr      = HMomArr[0:totPtCnt-1,*]

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
                          ;; 'DSP'    ,{DC:EDCArr[*,2], $
                          ;;            AC:EACArr[*,2]})
                          'DSP'    ,{y:EDCArr[*,2]})

     PArr = CREATE_STRUCT('P',{DC:PDCArr[*,0], $
                               AC:PACArr[*,0]}, $
                          'V',{DC:PDCArr[*,1], $
                               AC:PACArr[*,1]}, $
                          'B',{DC:PDCArr[*,2], $
                               AC:PACArr[*,2]})

     HArr = CREATE_STRUCT('JEe',{y:HArr[*,0]}, $
                          'Je',{y:HArr[*,1]}, $
                          'Ji',{y:HArr[*,2]})

     HMomArr = CREATE_STRUCT('densE',{y:HMomArr[*,0]}, $
                             'CEe',{y:HMomArr[*,1]}, $
                             'Te',{y:HMomArr[*,2]}, $
                             'JEe',{y:HMomArr[*,3]}, $
                             'Je',{y:HMomArr[*,4]})

     pts = {orbit:orbArr, $
            interval:itvlArr, $
            dB:TEMPORARY(dBArr), $
            E:TEMPORARY(EArr), $
            PFLUX:TEMPORARY(PArr), $
            PTCL:TEMPORARY(HArr), $
            PTCLMom:TEMPORARY(HMomArr)}


     PRINT,"Saving to " + lastFile + " ..."
     SAVE,avgStruct,pts, $
          FILENAME=outDir+lastFile

  ENDELSE

  PRINT,FORMAT='("Lastfile: ",A0)',lastFile

  quit = 0
  WHILE ~quit DO BEGIN

     validr1 = 0
     WHILE ~validr1 DO BEGIN
        PRINT,'What type of avg?'
        PRINT,'0: reguree'
        PRINT,'1: posVals'
        PRINT,'2: negVals'
        PRINT,'3: absVals'
        READ,avgInd

        CASE avgInd OF
           0: BEGIN
              validr1 = 1

              avgTypeString = ''
           END
           1: BEGIN
              validr1 = 1
              
              avgTypeString = 'POS'

              posVal = 1
              negVal = 0
              absVal = 0
           END
           2: BEGIN
              validr1 = 1
              
              avgTypeString = 'NEG'

              posVal = 0
              negVal = 1
              absVal = 0
           END
           3: BEGIN
              validr1 = 1
              
              avgTypeString = 'ABS'

              posVal = 0
              negVal = 0
              absVal = 1
           END
           ELSE: BEGIN
              PRINT,"WRONG!"
           END
        ENDCASE
     ENDWHILE

     ;; avgInd        = 0 ;;Just plain old
     ;; avgTypeString = ''

     ;; posVal = 1
     ;; absVal = 1
     ;; negVal = 1
     ;; IF KEYWORD_SET(posVal) THEN BEGIN
     ;;    ;; avgInd = 1
     ;; ENDIF
     ;; IF KEYWORD_SET(negVal) THEN BEGIN
     ;;    ;; avgInd = 2
     ;; ENDIF
     ;; IF KEYWORD_SET(absVal) THEN BEGIN
     ;;    ;; avgInd = 3
     ;; ENDIF

     PRINT,avgTypeString

     sw_i = SORT(avgStruct.orbit)

     jiOutflowers = KEYWORD_SET(average_over_whole_pass) ? $
                    avgStruct.ptcl.ji.y.avg   [sw_i]     : $
                    avgStruct.ptcl.ji.y.avg   [sw_i]

     finStruct   = {orbit     : avgStruct.orbit    [sw_i], $
                    ;; interval  : avgStruct.interval [sw_i], $   
                    eAlongVAC : avgStruct.e.AlongV.AC.(avgInd) [sw_i], $   
                    dB_perpAC : avgStruct.dB.p.AC.(avgInd) [sw_i], $   
                    pFAlongBDC: avgStruct.pFlux.B.DC.(avgInd) [sw_i], $
                    pFAlongPDC: avgStruct.pFlux.P.DC.(avgInd) [sw_i], $
                    pFAlongBAC: avgStruct.pFlux.B.AC.(avgInd) [sw_i], $
                    pFAlongPAC: avgStruct.pFlux.P.AC.(avgInd) [sw_i], $
                    DSPDC     : avgStruct.E.DSP.y.(avgInd)[sw_i], $
                    ;; DSPAC     : avgStruct.E.DSP.AC.(avgInd)   [sw_i], $
                    ;; je        : avgStruct.ptcl.je.y.(avgInd)   [sw_i], $
                    ;; jee       : avgStruct.ptcl.jee.y.(avgInd)  [sw_i], $
                    je        : avgStruct.ptclMom.je.y.(avgInd)   [sw_i], $
                    jee       : avgStruct.ptclMom.jee.y.(avgInd)  [sw_i], $
                    ji        : jiOutflowers, $
                    densE     : avgStruct.ptclMom.densE.y.(avgInd)  [sw_i], $
                    ;; densE     : 2.134e-14*((avgStruct.ptclMom.je.y.(avgInd))^(1.5D)/(avgStruct.ptclMom.jee.y.(avgInd))^(0.5D))  [sw_i], $ ;Strangeway dens
                    pFBDCAC   : (avgStruct.pFlux.B.DC.(1) $
                                 *(avgStruct.pFlux.B.AC.(3))^(1.5D)) [sw_i], $
                    denspFBAC : ((avgStruct.ptclMom.densE.y.(0))^(1.27) $
                                 *(avgStruct.pFlux.B.AC.(3))^(1.38D)) [sw_i]}

     ;; IF KEYWORD_SET(south) THEN BEGIN
     ;;    finStruct.dB_perpAC  *= -1.
     ;;    finStruct.pFAlongBDC *= -1.
     ;;    finStruct.pFAlongBAC *= -1.
     ;;    finStruct.pFAlongPDC *= -1.
     ;;    finStruct.pFAlongPAC *= -1.
     ;; ENDIF

     IF N_ELEMENTS(xQuants) EQ 0 THEN BEGIN
        ;; xQuants = [1,2,3,4,5,6,7,8,9,11]
        ;; xQuants = [1,2,3,5,8,9,11,12]
        ;; xQuants = [2,3,5,8,11,12,13]
        xQuants = [2,3,5,8,11]
     ENDIF

     plotInfo  = {xQuants       : xQuants, $
                  xTitle        : ["", $
                                   "EAlongV [AC] (mV/m)", $
                                   "Cross-track B [AC] (nT)", $
                                   "Poynting FluxB [DC] (mW/m^2)", $
                                   "Poynting FluxP [DC] (mW/m^2)", $
                                   "Poynting FluxB [AC] (mW/m^2)", $ ;xQuant5
                                   "Poynting FluxP [AC] (mW/m^2)", $
                                   "Average ELF amplitude [DC] (V/m)", $
                                   ;; "Average ELF amplitude [AC] (V/m)", $
                                   "Average Electron Flux (#/cm$^2$/s)", $
                                   "Average Electron Energy Flux (mW/m$^2$)", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "Average Electron Density (cm$^-3$)", $
                                   "PFB[DC]*PFB[AC]!U1.5!N", $
                                   "dens*PFB[AC]!U1.5!N"] $ ;xQuant12
                  + avgTypeString, $
                  xRange        : [[0.,0.], $
                                   [1e0,1e3], $
                                   [1e-1,3e1], $
                                   [1e-1,1e2], $
                                   [1e-1,1e2], $
                                   [1e-4,1e0], $
                                   [1e-4,1e0], $
                                   [1e-3,1e-1], $
                                   ;; [1e-5,1e-2], $
                                   [1e7,1e10], $
                                   [1e-2,1e0], $                                    
                                   [1e6,1e10], $                                    
                                   [1e-1,1e2], $                                    
                                   [1e-4,1e1], $                                    
                                   [1e-5,1e1]], $
                  yTitle        : "Ion Flux (#/cm!U2!N/s)", $
                  yData         : finStruct.ji, $
                  yRange        : [1e6,1e10], $
                  plotNames     : ["", $
                                   "EAlongVAC__vs__ionNumFlux", $
                                   "CrossTrackBAC__vs__ionNumFlux", $
                                   "DC_Poynting_fluxB__vs__ionNumFlux", $
                                   "AC_Poynting_fluxB__vs__ionNumFlux", $
                                   "DC_Poynting_fluxP__vs__ionNumFlux", $
                                   "AC_Poynting_fluxP__vs__ionNumFlux", $
                                   "ELF_amplitudeDC__vs__ionNumFlux", $
                                   ;; "ELF_amplitudeAC__vs__ionNumFlux", $
                                   "eNumFlux__vs__ionNumFlux", $
                                   "eFlux__vs__ionNumFlux", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "dens_vs_ionNumFlux", $
                                   "magicPFlux", $
                                   "densPFBAC"], $
                  canonPref     : 'Strangeway_2005__v3--', $
                  plotDirSuff   : '/Strangeway_et_al_2005__v3', $
                  plots_prefix  : (KEYWORD_SET(bonusSuff) ? bonusSuff : '') + $ 
                  defs.statStr+'--'+defs.sideStr+'--'+defs.hemStr+'--' + $
                  avgTypeString, $
                  verboten      : [0], $
                  navn_verboten : ["Orbit    (ind 0)"]}

     IF ~KEYWORD_SET(no_plots) THEN BEGIN

        PLOT_STRANGEWAY_STATS, $
           finStruct, $
           PLOTINFO=plotInfo, $
           OUT_PLOTARR=plotArr, $
           SQUARE_WINDOW=square_window, $
           SAVE_PLOTS=save_plots, $
           PLOTDIR=plotDir

     ENDIF

     validr1 = 0
     WHILE ~validr1 DO BEGIN
        PRINT,"Do another? (y/n)"
        response = ''
        READ,response
        CASE STRMID(STRUPCASE(response),0,1) OF
           'Y': BEGIN
              validr1 = 1
           END
           'N': BEGIN
              PRINT,'Quitting ...'
              quit    = 1
              validr1 = 1
           END
           ELSE: BEGIN
              PRINT,'Say yes or no, son!'
           END
        ENDCASE
     ENDWHILE

  ENDWHILE


  RETURN,finStruct

END
