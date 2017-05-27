;2017/05/22
PRO STRANGEWAY_3BANDS__STITCH_FILES, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   USE_JE_TBOUNDS=use_Je_tBounds, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   INCLUDE_PARTICLES=include_particles, $
   DECIMATE_E_AND_B__THEN_CALC_PFLUX=decimate_eb_calc_pFlux, $
   SKIPDSP=skipDSP, $
   MAKE_DB=make_dB, $      
   MAKE_EFIELD=make_eField, $
   MAKE_PFLUX=make_pFlux, $
   MAKE_MAGFLAGS=make_magFlags, $
   MAKE_PTCL=make_ptcl, $
   MAKE_EPHEM=make_ephem

  COMPILE_OPT IDL2,STRICTARRSUBS

  @strangeway_3bands__defaults__8hz.pro

  interp_4Hz_to_1s     = 1
  haveACHigh           = 1

  originating_routine  = 'STRANGEWAY_3BANDS__STITCH_FILES'

  ;;1997
  ;; startOrb             = 1436
  ;; stopOrb              = 5382
  
  startOrb          = 1000
  stopOrb           = 7705

  ;; make_dB              = 1
  ;; make_eField          = 1
  ;; make_pFlux           = 1
  ;; make_magFlags        = 1
  ;; ;; make_ptcl            = 
  ;; make_ephem           = 1

  dBSuff               = 'dB'
  eFieldSuff           = 'eField'
  pFluxSuff            = 'pFlux'
  magFlagsSuff         = 'magFlags'
  ephemSuff            = 'ephem'

  itvlTypeName         = 'magItvl'
  IF KEYWORD_SET(use_Je_tBounds) THEN BEGIN
     itvlTypeName      = 'EESAItvl'
  ENDIF

  maitreFilSuff        = STRING(FORMAT='("__orbs_",I0,"-",I0,"_",A0)',startOrb,stopOrb,itvlTypeName)
  maitreFil            = STRSPLIT(hashFile,'.sav',/REGEX,/EXTRACT) 
  IF N_ELEMENTS(maitreFil) GT 1 THEN BEGIN
     maitreFil         = STRJOIN(maitreFil)
  ENDIF
  maitreFil           += maitreFilSuff + '.sav'

  number_of_intervals  = 10     ;max

  ptsPerOrb            = 2500L
  maxNPts              = ptsPerOrb * (stopOrb-startOrb+1) ;Assume so many points per orbit
  typisk               = MAKE_ARRAY(maxNPts,/FLOAT)
  typisk8              = MAKE_ARRAY(8,maxNPts,/FLOAT)

  sMemNames            = !NULL
  tmpltNames           = !NULL
  ;; varNames             = !NULL

  IF haveACHigh THEN BEGIN

     IF KEYWORD_SET(make_dB) THEN BEGIN

        PRINT,'Making dBTmplt ...'
        dBTmplt        = {p      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          v      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          B      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}}

        sMemNames      = [sMemNames,'dB']
        tmpltNames     = [tmpltNames,'dBTmplt']

     ENDIF

     IF KEYWORD_SET(make_eField) THEN BEGIN

        PRINT,'Making eTmplt ...'
        eTmplt         = {AlongV : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          NearB  : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          dsp    : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}

        sMemNames      = [sMemNames,'e']
        tmpltNames     = [tmpltNames,'eTmplt']

     ENDIF

     IF KEYWORD_SET(make_pFlux) THEN BEGIN

        PRINT,'Making pFluxTmplt ...'
        pFluxTmplt        = CREATE_STRUCT('p',{DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                                          'v',(KEYWORD_SET(full_pFlux) ? {DC:typisk8,AC:typisk8,ACHigh:typisk8} : 0B), $
                                          'b',{DC:typisk8,AC:typisk8,ACHigh:typisk8})

        sMemNames      = [sMemNames,'pFlux']
        tmpltNames     = [tmpltNames,'pFluxTmplt']

     ENDIF

  ENDIF ELSE BEGIN

     PRINT,"So you do NOT have ACHigh, Right?"
     WAIT,3

     IF KEYWORD_SET(make_dB) THEN BEGIN

        PRINT,'Making dBTmplt ...'
        dBTmplt           = {p      : {DC:typisk8,AC:typisk8}, $
                             v      : {DC:typisk8,AC:typisk8}, $
                             B      : {DC:typisk8,AC:typisk8}}

        sMemNames      = [sMemNames,'dB']
        tmpltNames     = [tmpltNames,'dBTmplt']

     ENDIF

     IF KEYWORD_SET(make_eField) THEN BEGIN

        PRINT,'Making eTmplt ...'
        eTmplt            = {AlongV : {DC:typisk8,AC:typisk8}, $
                             NearB  : {DC:typisk8,AC:typisk8}, $
                             dsp    : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}

        sMemNames      = [sMemNames,'e']
        tmpltNames     = [tmpltNames,'eTmplt']

     ENDIF

     IF KEYWORD_SET(make_pFlux) THEN BEGIN

        PRINT,'Making pFluxTmplt ...'
        pFluxTmplt     = CREATE_STRUCT('p',{DC:typisk8,AC:typisk8}, $
                                       'v',(KEYWORD_SET(full_pFlux) ? {DC:typisk8,AC:typisk8} : 0B), $
                                       'b',{DC:typisk8,AC:typisk8})

        sMemNames      = [sMemNames,'pFlux']
        tmpltNames     = [tmpltNames,'pFluxTmplt']

     ENDIF

  ENDELSE

  IF KEYWORD_SET(make_ptcl) THEN BEGIN

     PRINT,'Making ptclTmplt ...'
     ptclTmplt         = ( KEYWORD_SET(include_particles) ? {jEe:typisk,je:typisk,ji:typisk} : 0B )

     sMemNames         = [sMemNames,'pFlux']
     tmpltNames        = [tmpltNames,'pFluxTmplt']

  ENDIF
  
  IF KEYWORD_SET(make_magFlags) THEN BEGIN

     PRINT,'Making magFlagsTmplt ...'
     magFlagsTmplt     = {x : MAKE_ARRAY(maxNPts/5,/DOUBLE), $
                          y : MAKE_ARRAY(maxNPts/5,/UINT)}

  ENDIF

  IF KEYWORD_SET(make_ephem) THEN BEGIN

     PRINT,'Making ephemTmplt ...'
     ephemTmplt           = {time      : MAKE_ARRAY(maxNPts,/DOUBLE), $
                             orbit     : MAKE_ARRAY(maxNPts,/LONG), $  
                             fa_pos    : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                             alt       : typisk, $
                             ilat      : typisk, $
                             ;; ilng      : typisk, $
                             mlt       : typisk, $
                             fa_vel    : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                             ;; bfoot     : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                             magRatio  : typisk, $
                             lat       : typisk, $
                             lng       : typisk}
     ;; flat      : typisk, $
     ;; flng      : typisk, $
     ;; b_model   : MAKE_ARRAY(maxNPts,3,/FLOAT)}, $

  ENDIF
  
  PRINT,"Making info ..."
  info                 = {full_pFlux             : BYTE(KEYWORD_SET(full_pFlux)), $
                          decimate_eb_calc_pFlux : BYTE(KEYWORD_SET(decimate_eb_calc_pFlux)), $
                          interp_4Hz_to_1s       : BYTE(KEYWORD_SET(interp_4Hz_to_1s      )), $
                          include_E_near_B       : BYTE(KEYWORD_SET(include_E_near_B)), $
                          eField_fit_variables   : BYTE(KEYWORD_SET(use_eField_fit_variables)), $
                          skipDSP                : BYTE(KEYWORD_SET(skipDSP)), $
                          originating_routine    : originating_routine, $
                          date                   : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)}


  ;; leMaitre             = {dB        : TEMPORARY(dBTmplt), $
  ;;                         e         : TEMPORARY(eTmplt), $
  ;;                         ptcl      : TEMPORARY(ptclTmplt), $
  ;;                         pFlux     : TEMPORARY(pFluxTmplt), $
  ;;                         magFlags  : TEMPORARY(magFlagsTmplt), $
  ;;                         ;; ephem     : TEMPORARY(ephemTmplt), $
  ;;                         info      : TEMPORARY(info)}

  dB         = KEYWORD_SET(make_dB)       ? TEMPORARY(dBTmplt)        : !NULL
  e          = KEYWORD_SET(make_eField)   ? TEMPORARY(eTmplt)         : !NULL
  ptcl       = KEYWORD_SET(make_ptcl)     ? TEMPORARY(ptclTmplt)      : !NULL
  pFlux      = KEYWORD_SET(make_pFlux)    ? TEMPORARY(pFluxTmplt)     : !NULL
  magFlags   = KEYWORD_SET(make_magFlags) ? TEMPORARY(magFlagsTmplt)  : !NULL
  ephem      = KEYWORD_SET(make_ephem)    ? TEMPORARY(ephemTmplt)     : !NULL

  typisk     = !NULL
  typisk8    = !NULL
  
  datInd     = 0L
  magFlagInd = 0L

  PRINT,"Creating " + maitreFil
  FOR orb=startOrb,stopOrb DO BEGIN

     FOR jj=0,number_of_intervals-1 DO BEGIN

        itvlString   = STRCOMPRESS(jj,/REMOVE_ALL)
        tmpFile      = indivOrbPref + STRING(FORMAT='(I0)',orb) + '__' + itvlTypeName + '_' + itvlString + '.sav'

        IF ~FILE_TEST(outDir+tmpFile) THEN BEGIN

           IF jj EQ 0 THEN PRINT,"Couldn't get salty for orbit " + STRING(FORMAT='(I0)',orb) + "!"

           BREAK

        ENDIF

        RESTORE,outDir+tmpFile

        nPtsHere     = N_ELEMENTS(tmpStruct.ephem.time)
        nMagFlagHere = N_ELEMENTS(tmpStruct.magFlags.x)

        IF nPtsHere LE 1 THEN STOP

        PRINT,FORMAT='(I0,", ",I0,", ",I0,", ",I0)',orb,jj,nPtsHere,nMagFlagHere

        ;;Most of these have screwed-up ephemeris info
        IF N_ELEMENTS(SIZE(tmpStruct.ephem.fa_vel,/DIM)) NE 2 THEN BEGIN

           PRINT,"Getting correct ephem info for this interval ..."
           GET_FA_ORBIT,tmpStruct.ephem.time,/TIME_ARRAY,/DEFINITIVE,/ALL,STRUC=ephem

           STR_ELEMENT,tmpStruct,'ephem.fa_pos',ephem.fa_pos,/ADD_REPLACE
           STR_ELEMENT,tmpStruct,'ephem.fa_vel',ephem.fa_vel,/ADD_REPLACE
           STR_ELEMENT,tmpStruct,'ephem.bfoot',ephem.bfoot,/ADD_REPLACE
           STR_ELEMENT,tmpStruct,'ephem.b_model',ephem.b_model,/ADD_REPLACE

           SAVE,tmpStruct,FILENAME=outDir+tmpFile

        ENDIF

        inds                                = [datInd:(datInd+nPtsHere-1)]
        MFInds                              = [magFlagInd:(magFlagInd+nMagFlagHere-1)]

        IF KEYWORD_SET(make_dB) THEN BEGIN

           dB.p.DC[*,inds]         = tmpStruct.dB.p.DC
           dB.p.AC[*,inds]         = tmpStruct.dB.p.AC
           dB.v.DC[*,inds]         = tmpStruct.dB.v.DC
           dB.v.AC[*,inds]         = tmpStruct.dB.v.AC
           dB.B.DC[*,inds]         = tmpStruct.dB.B.DC
           dB.B.AC[*,inds]         = tmpStruct.dB.B.AC

           IF haveACHigh THEN BEGIN

              dB.p.ACHigh[*,inds]     = tmpStruct.dB.p.ACHigh
              dB.v.ACHigh[*,inds]     = tmpStruct.dB.v.ACHigh
              dB.B.ACHigh[*,inds]     = tmpStruct.dB.B.ACHigh

           ENDIF

        ENDIF

        IF KEYWORD_SET(make_eField) THEN BEGIN

           e.AlongV.DC[*,inds]     = tmpStruct.e.AlongV.DC
           e.AlongV.AC[*,inds]     = tmpStruct.e.AlongV.AC
           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              e.NearB.DC[*,inds]   = tmpStruct.e.NearB.DC
              e.NearB.AC[*,inds]   = tmpStruct.e.NearB.AC
           ENDIF

           IF haveACHigh THEN BEGIN

              e.AlongV.ACHigh[*,inds] = tmpStruct.e.AlongV.ACHigh
              IF KEYWORD_SET(include_E_near_B) THEN BEGIN
                 e.NearB.ACHigh[*,inds]  = tmpStruct.e.NearB.ACHigh
              ENDIF

           ENDIF

        ENDIF
        
        IF KEYWORD_SET(make_pFlux) THEN BEGIN

           pFlux.p.DC[*,inds]      = tmpStruct.pFlux.p.DC
           pFlux.p.AC[*,inds]      = tmpStruct.pFlux.p.AC
           pFlux.B.DC[*,inds]      = tmpStruct.pFlux.B.DC
           pFlux.B.AC[*,inds]      = tmpStruct.pFlux.B.AC

           IF haveACHigh THEN BEGIN

              pFlux.p.ACHigh[*,inds]  = tmpStruct.pFlux.p.ACHigh
              pFlux.B.ACHigh[*,inds]  = tmpStruct.pFlux.B.ACHigh

           ENDIF

        ENDIF

        IF KEYWORD_SET(make_magFlags) THEN BEGIN

           magFlags.x[MFInds]      = tmpStruct.magFlags.x
           magFlags.y[MFInds]      = tmpStruct.magFlags.y

        ENDIF


        IF KEYWORD_SET(make_ephem) THEN BEGIN

           ;;Calc mag ratio
           mag1                             = (tmpStruct.ephem.b_model[*,0]*tmpStruct.ephem.b_model[*,0]+ $
                                               tmpStruct.ephem.b_model[*,1]*tmpStruct.ephem.b_model[*,1]+ $
                                               tmpStruct.ephem.b_model[*,2]*tmpStruct.ephem.b_model[*,2])^0.5
           mag2                             = (tmpStruct.ephem.bfoot[*,0]*tmpStruct.ephem.bfoot[*,0]+ $
                                               tmpStruct.ephem.bfoot[*,1]*tmpStruct.ephem.bfoot[*,1]+ $
                                               tmpStruct.ephem.bfoot[*,2]*tmpStruct.ephem.bfoot[*,2])^0.5
           ratio                            = (mag2/mag1)


           ephem.time[inds]        = tmpStruct.ephem.time   
           ephem.orbit[inds]       = tmpStruct.ephem.orbit  
           ephem.fa_pos[inds,*]    = tmpStruct.ephem.fa_pos 
           ephem.alt[inds]         = tmpStruct.ephem.alt    
           ephem.ilat[inds]        = tmpStruct.ephem.ilat   
           ;; ephem.ilng[inds]        = tmpStruct.ephem.ilng   
           ephem.mlt[inds]         = tmpStruct.ephem.mlt    
           ephem.fa_vel[inds,*]    = tmpStruct.ephem.fa_vel 
           ;; ephem.bfoot[inds,*]  = tmpStruct.ephem.bfoot  
           ephem.lat[inds]         = tmpStruct.ephem.lat    
           ephem.lng[inds]         = tmpStruct.ephem.lng    
           ;; ephem.flat[inds]        = tmpStruct.ephem.flat   
           ;; ephem.flng[inds]        = tmpStruct.ephem.flng   
           ;; ephem.b_model[inds,*]   = tmpStruct.ephem.b_model
           ephem.magRatio[inds]    = TEMPORARY(ratio)

        ENDIF

        datInd                          += TEMPORARY(nPtsHere)
        magFlagInd                      += TEMPORARY(nMagFlagHere)

     ENDFOR

  ENDFOR

  finalInds   = [0:(datInd-1)]
  finalMFInds = [0:(magFlagInd-1)]

  IF haveACHigh THEN BEGIN

     IF KEYWORD_SET(make_dB) THEN BEGIN

        dB       = {p : {DC:dB.p.DC[*,finalInds], $
                         AC:dB.p.AC[*,finalInds], $
                         ACHigh:dB.p.ACHigh[*,finalInds]}, $
                    v : {DC:dB.v.DC[*,finalInds], $
                         AC:dB.v.AC[*,finalInds], $
                         ACHigh:dB.v.ACHigh[*,finalInds]}, $
                    B : {DC:dB.B.DC[*,finalInds], $
                         AC:dB.B.AC[*,finalInds], $
                         ACHigh:dB.B.ACHigh[*,finalInds]}}

     ENDIF


     IF KEYWORD_SET(make_eField) THEN BEGIN

        eAlongV  = {DC:e.alongV.DC[*,finalInds], $
                    AC:e.alongV.AC[*,finalInds], $
                    ACHigh:e.alongV.ACHigh[*,finalInds]}

        eNearB   = KEYWORD_SET(include_E_near_B)        ? $
                   {DC:e.nearB.DC[*,finalInds], $
                    AC:e.nearB.AC[*,finalInds], $
                    ACHigh:e.nearB.ACHigh[*,finalInds]} : $
                   0B

        dsp      = KEYWORD_SET(skipDSP) ? $
                   0B                   : $
                   {DC:e.dsp.DC[*,finalInds], $
                    AC:e.dsp.AC[*,finalInds], $
                    ACHigh:e.dsp.ACHigh[*,finalInds]}

     ENDIF

     IF KEYWORD_SET(make_pFlux) THEN BEGIN

        pFP         = {DC:pFlux.p.DC[*,finalInds], $
                       AC:pFlux.p.AC[*,finalInds], $
                       ACHigh:pFlux.p.ACHigh[*,finalInds]}
        
        pFB         = {DC:pFlux.B.DC[*,finalInds], $
                       AC:pFlux.B.AC[*,finalInds], $
                       ACHigh:pFlux.B.ACHigh[*,finalInds]}
        
        pFV         = KEYWORD_SET(full_pFlux)              ? $
                      {DC:pFlux.v.DC[*,finalInds], $
                       AC:pFlux.v.AC[*,finalInds], $
                       ACHigh:pFlux.v.ACHigh[*,finalInds]} : $
                      0B

     ENDIF
     
  ENDIF ELSE BEGIN

     IF KEYWORD_SET(make_dB) THEN BEGIN

        dBStruct    = {p : {DC:dB.p.DC[*,finalInds], $
                            AC:dB.p.AC[*,finalInds]}, $
                       v : {DC:dB.v.DC[*,finalInds], $
                            AC:dB.v.AC[*,finalInds]}, $
                       B : {DC:dB.B.DC[*,finalInds], $
                            AC:dB.B.AC[*,finalInds]}}

     ENDIF

     IF KEYWORD_SET(make_eField) THEN BEGIN

        eAlongV     = {DC : e.alongV.DC[*,finalInds], $
                       AC : e.alongV.AC[*,finalInds]}

        eNearB      = KEYWORD_SET(include_E_near_B)    ? $
                      {DC : e.nearB.DC[*,finalInds], $
                       AC : e.nearB.AC[*,finalInds]}   : $
                      0B

        dsp         = KEYWORD_SET(skipDSP) ? $
                      0B                   : $
                      {DC : e.dsp.DC[*,finalInds], $
                       AC : e.dsp.AC[*,finalInds]}

     ENDIF

     IF KEYWORD_SET(make_pFlux) THEN BEGIN

        pFP         = {DC : pFlux.p.DC[*,finalInds], $
                       AC : pFlux.p.AC[*,finalInds]}
        
        pFB         = {DC : pFlux.B.DC[*,finalInds], $
                       AC : pFlux.B.AC[*,finalInds]}
        
        pFV         = KEYWORD_SET(full_pFlux)        ? $
                      {DC : pFlux.v.DC[*,finalInds], $
                       AC : pFlux.v.AC[*,finalInds]} : $
                      0B

     ENDIF

  ENDELSE

  ;; CASE 1 OF
  ;;    KEYWORD_SET(include_E_near_B) AND ~KEYWORD_SET(skipDSP): BEGIN
  ;;    END
  ;;    KEYWORD_SET(include_E_near_B) AND KEYWORD_SET(skipDSP): BEGIN
  ;;       eStruct     = {alongV               : TEMPORARY(eAlongV), $
  ;;                      nearB                : {DC:e.nearB.DC[*,finalInds], $
  ;;                                              AC:e.nearB.AC[*,finalInds], $
  ;;                                              ACHigh:e.nearB.ACHigh[*,finalInds]}, $
  ;;                      dsp                  : 0B}

  ;;    END
  ;;    KEYWORD_SET(skipDSP): BEGIN
  ;;       eStruct     = {alongV               : {DC:e.alongV.DC[*,finalInds], $
  ;;                                              AC:e.alongV.AC[*,finalInds], $
  ;;                                              ACHigh:e.alongV.ACHigh[*,finalInds]}, $
  ;;                      dsp                  : 0B}

  ;;    END
  ;; ENDCASE

  IF KEYWORD_SET(make_eField) THEN BEGIN

     eStruct     = {alongV : TEMPORARY(eAlongV), $
                    nearB  : TEMPORARY(eNearB), $
                    dsp    : TEMPORARY(dsp)}

  ENDIF

  IF KEYWORD_SET(make_ptcl) THEN BEGIN

     ptcl        = KEYWORD_SET(include_particles)      ? $
                   {jEe:ptcl.jEe[finalInds], $
                    je:ptcl.je[finalInds], $
                    ji:ptcl.ji[finalInds]}    : $
                   0B
     
  ENDIF

  IF KEYWORD_SET(make_pFlux) THEN BEGIN

     pFluxStruct    = {p : TEMPORARY(pFP), $
                       v : TEMPORARY(pFV), $
                       B : TEMPORARY(pFB)}

  ENDIF
  
  IF KEYWORD_SET(make_magFlags) THEN BEGIN

     magFlags       = {x : magFlags.x[finalMFInds], $
                       y : magFlags.y[finalMFInds]}

  ENDIF

  IF KEYWORD_SET(make_ephem) THEN BEGIN

     ephem          = {time      : ephem.time[finalInds], $
                       orbit     : ephem.orbit[finalInds], $  
                       fa_pos    : ephem.fa_pos[finalInds,*], $
                       alt       : ephem.alt [finalInds], $
                       ilat      : ephem.ilat[finalInds], $
                       ;; ilng      : ephem.ilng[finalInds], $
                       mlt       : ephem.mlt [finalInds], $
                       fa_vel    : ephem.fa_vel[finalInds,*], $
                       ;; bfoot     : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                       magRatio  : ephem.magRatio[finalInds], $
                       lat       : ephem.lat     [finalInds], $
                       lng       : ephem.lng     [finalInds]}
     ;; flat      : ephem.flat    [finalInds], $
     ;; flng      : ephem.flng    [finalInds], $
     ;; b_model   : ephem.b_model [finalInds,*]}, $

  ENDIF

  IF KEYWORD_SET(make_dB) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + dBSuff + '.sav'
     PRINT,"Saving dB to " + thisFile + ' ...'
     SAVE,dB,FILENAME=outDir+thisFile

  ENDIF
  
  IF KEYWORD_SET(make_eField) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + eFieldSuff + '.sav'
     PRINT,"Saving eField to " + thisFile + ' ...'
     SAVE,e,FILENAME=outDir+thisFile

  ENDIF
  
  IF KEYWORD_SET(make_pFlux) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + pFluxSuff + '.sav'
     PRINT,"Saving pFlux to " + thisFile + ' ...'
     SAVE,pFlux,FILENAME=outDir+thisFile

  ENDIF
  
  IF KEYWORD_SET(make_magFlags) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + magFlagsSuff + '.sav'
     PRINT,"Saving magFlags to " + thisFile + ' ...'
     SAVE,magFlags,FILENAME=outDir+thisFile

  ENDIF

  IF KEYWORD_SET(make_ptcl) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + ptclSuff + '.sav'
     PRINT,"Saving ptcl to " + thisFile + ' ...'
     SAVE,ptcl,FILENAME=outDir+thisFile

  ENDIF
  
  IF KEYWORD_SET(make_ephem) THEN BEGIN

     thisFile = STRSPLIT(maitreFil,'.sav',/REGEX,/EXTRACT) + '-' + ephemSuff + '.sav'
     PRINT,"Saving ephem to " + thisFile + ' ...'
     SAVE,ephem,FILENAME=outDir+thisFile

  ENDIF

  ;; leMaitre    = {dB       : TEMPORARY(dBStruct), $
  ;;                e        : TEMPORARY(eStruct), $
  ;;                ptcl     : TEMPORARY(ptcl), $
  ;;                pFlux    : TEMPORARY(pFluxStruct), $
  ;;                magFlags : TEMPORARY(magFlags), $
  ;;                ;; ephem    : TEMPORARY(ephem), $
  ;;                info     : leMaitre.info}


  ;; SAVE,leMaitre,FILENAME=outDir+maitreFil

END
