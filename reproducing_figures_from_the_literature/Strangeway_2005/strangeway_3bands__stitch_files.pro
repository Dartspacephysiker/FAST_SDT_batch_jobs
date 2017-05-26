;2017/05/22
PRO STRANGEWAY_3BANDS__STITCH_FILES, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   USE_JE_TBOUNDS=use_Je_tBounds, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   INCLUDE_PARTICLES=include_particles, $
   DECIMATE_E_AND_B__THEN_CALC_PFLUX=decimate_eb_calc_pFlux, $
   SKIPDSP=skipDSP

  COMPILE_OPT IDL2,STRICTARRSUBS

  @strangeway_3bands__defaults__8hz.pro

  interp_4Hz_to_1s     = 1
  haveACHigh           = 0

  originating_routine  = 'STRANGEWAY_3BANDS__STITCH_FILES'

  ;;1997
  startOrb             = 1436
  stopOrb              = 5382
  
  ;; startOrb             = 1000
  ;; stopOrb              = 5800

  itvlTypeName         = 'magItvl'
  IF KEYWORD_SET(use_Je_tBounds) THEN BEGIN
     itvlTypeName      = 'EESAItvl'
  ENDIF

  maitreFilSuff        = STRING(FORMAT='("__orbs_",I0,"-",I0,"_",A0)',startOrb,stopOrb,itvlTypeName)
  maitreFil            = STRSPLIT(hashFile,'.sav',/REGEX,/EXTRACT) + maitreFilSuff + '.sav'

  number_of_intervals  = 10     ;max

  ptsPerOrb            = 2500L
  maxNPts              = ptsPerOrb * (stopOrb-startOrb+1) ;Assume so many points per orbit
  typisk               = MAKE_ARRAY(maxNPts,/FLOAT)
  typisk8              = MAKE_ARRAY(8,maxNPts,/FLOAT)

  IF haveACHigh THEN BEGIN

     PRINT,'Making dBTmplt ...'
     dBTmplt           = {p      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          v      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          B      : {DC:typisk8,AC:typisk8,ACHigh:typisk8}}

     PRINT,'Making eTmplt ...'
     eTmplt            = {AlongV : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          NearB  : {DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                          dsp    : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}

     PRINT,'Making pFluxTmplt ...'
     pFluxTmplt        = CREATE_STRUCT('p',{DC:typisk8,AC:typisk8,ACHigh:typisk8}, $
                                       'v',(KEYWORD_SET(full_pFlux) ? {DC:typisk8,AC:typisk8,ACHigh:typisk8} : 0B), $
                                       'b',{DC:typisk8,AC:typisk8,ACHigh:typisk8})
  ENDIF ELSE BEGIN

     PRINT,"So you do NOT have ACHigh, Right?"
     WAIT,3

     PRINT,'Making dBTmplt ...'
     dBTmplt           = {p      : {DC:typisk8,AC:typisk8}, $
                          v      : {DC:typisk8,AC:typisk8}, $
                          B      : {DC:typisk8,AC:typisk8}}

     PRINT,'Making eTmplt ...'
     eTmplt            = {AlongV : {DC:typisk8,AC:typisk8}, $
                          NearB  : {DC:typisk8,AC:typisk8}, $
                          dsp    : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}

     PRINT,'Making pFluxTmplt ...'
     pFluxTmplt        = CREATE_STRUCT('p',{DC:typisk8,AC:typisk8}, $
                                       'v',(KEYWORD_SET(full_pFlux) ? {DC:typisk8,AC:typisk8} : 0B), $
                                       'b',{DC:typisk8,AC:typisk8})

  ENDELSE

  PRINT,'Making ptclTmplt ...'
  ptclTmplt            = ( KEYWORD_SET(include_particles) ? {jEe:typisk,je:typisk,ji:typisk} : 0B )

     
  PRINT,'Making magFlagsTmplt ...'
  magFlagsTmplt        = {x : MAKE_ARRAY(maxNPts/5,/DOUBLE), $
                          y : MAKE_ARRAY(maxNPts/5,/UINT)}

  ;; PRINT,'Making ephemTmplt ...'
  ;; ephemTmplt           = {time      : MAKE_ARRAY(maxNPts,/DOUBLE), $
  ;;                         orbit     : MAKE_ARRAY(maxNPts,/LONG), $  
  ;;                         fa_pos    : MAKE_ARRAY(maxNPts,3,/FLOAT), $
  ;;                         alt       : typisk, $
  ;;                         ilat      : typisk, $
  ;;                         ;; ilng      : typisk, $
  ;;                         mlt       : typisk, $
  ;;                         fa_vel    : MAKE_ARRAY(maxNPts,3,/FLOAT), $
  ;;                         ;; bfoot     : MAKE_ARRAY(maxNPts,3,/FLOAT), $
  ;;                         magRatio  : typisk, $
  ;;                         lat       : typisk, $
  ;;                         lng       : typisk}
  ;;                        ;; flat      : typisk, $
  ;;                        ;; flng      : typisk, $
  ;;                        ;; b_model   : MAKE_ARRAY(maxNPts,3,/FLOAT)}, $

  PRINT,"Making info ..."
  info                 = {full_pFlux             : BYTE(KEYWORD_SET(full_pFlux)), $
                          decimate_eb_calc_pFlux : BYTE(KEYWORD_SET(decimate_eb_calc_pFlux)), $
                          interp_4Hz_to_1s       : BYTE(KEYWORD_SET(interp_4Hz_to_1s      )), $
                          include_E_near_B       : BYTE(KEYWORD_SET(include_E_near_B)), $
                          eField_fit_variables   : BYTE(KEYWORD_SET(use_eField_fit_variables)), $
                          skipDSP                : BYTE(KEYWORD_SET(skipDSP)), $
                          originating_routine    : originating_routine, $
                          date                   : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)}


  leMaitre             = {dB        : TEMPORARY(dBTmplt), $
                          e         : TEMPORARY(eTmplt), $
                          ptcl      : TEMPORARY(ptclTmplt), $
                          pFlux     : TEMPORARY(pFluxTmplt), $
                          magFlags  : TEMPORARY(magFlagsTmplt), $
                          ;; ephem     : TEMPORARY(ephemTmplt), $
                          info      : TEMPORARY(info)}

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

        leMaitre.dB.p.DC[*,inds]            = tmpStruct.dB.p.DC
        leMaitre.dB.p.AC[*,inds]            = tmpStruct.dB.p.AC
        leMaitre.dB.v.DC[*,inds]            = tmpStruct.dB.v.DC
        leMaitre.dB.v.AC[*,inds]            = tmpStruct.dB.v.AC
        leMaitre.dB.B.DC[*,inds]            = tmpStruct.dB.B.DC
        leMaitre.dB.B.AC[*,inds]            = tmpStruct.dB.B.AC
        leMaitre.e.AlongV.DC[*,inds]        = tmpStruct.e.AlongV.DC
        leMaitre.e.AlongV.AC[*,inds]        = tmpStruct.e.AlongV.AC
        IF KEYWORD_SET(include_E_near_B) THEN BEGIN
           leMaitre.e.NearB.DC[*,inds]      = tmpStruct.e.NearB.DC
           leMaitre.e.NearB.AC[*,inds]      = tmpStruct.e.NearB.AC
        ENDIF
        leMaitre.pFlux.p.DC[*,inds]         = tmpStruct.pFlux.p.DC
        leMaitre.pFlux.p.AC[*,inds]         = tmpStruct.pFlux.p.AC
        leMaitre.pFlux.B.DC[*,inds]         = tmpStruct.pFlux.B.DC
        leMaitre.pFlux.B.AC[*,inds]         = tmpStruct.pFlux.B.AC

        IF haveACHigh THEN BEGIN

           leMaitre.dB.p.ACHigh[*,inds]        = tmpStruct.dB.p.ACHigh
           leMaitre.dB.v.ACHigh[*,inds]        = tmpStruct.dB.v.ACHigh
           leMaitre.dB.B.ACHigh[*,inds]        = tmpStruct.dB.B.ACHigh
           leMaitre.e.AlongV.ACHigh[*,inds]    = tmpStruct.e.AlongV.ACHigh
           IF KEYWORD_SET(include_E_near_B) THEN BEGIN
              leMaitre.e.NearB.ACHigh[*,inds]  = tmpStruct.e.NearB.ACHigh
           ENDIF
           leMaitre.pFlux.p.ACHigh[*,inds]     = tmpStruct.pFlux.p.ACHigh
           leMaitre.pFlux.B.ACHigh[*,inds]     = tmpStruct.pFlux.B.ACHigh

        ENDIF

        leMaitre.magFlags.x[MFInds]         = tmpStruct.magFlags.x
        leMaitre.magFlags.y[MFInds]         = tmpStruct.magFlags.y

        ;;Calc mag ratio
        ;; mag1                                = (tmpStruct.ephem.b_model[*,0]*tmpStruct.ephem.b_model[*,0]+ $
        ;;                                        tmpStruct.ephem.b_model[*,1]*tmpStruct.ephem.b_model[*,1]+ $
        ;;                                        tmpStruct.ephem.b_model[*,2]*tmpStruct.ephem.b_model[*,2])^0.5
        ;; mag2                                = (tmpStruct.ephem.bfoot[*,0]*tmpStruct.ephem.bfoot[*,0]+ $
        ;;                                        tmpStruct.ephem.bfoot[*,1]*tmpStruct.ephem.bfoot[*,1]+ $
        ;;                                        tmpStruct.ephem.bfoot[*,2]*tmpStruct.ephem.bfoot[*,2])^0.5
        ;; ratio                               = (mag2/mag1)


        ;; leMaitre.ephem.time[inds]        = tmpStruct.ephem.time   
        ;; leMaitre.ephem.orbit[inds]       = tmpStruct.ephem.orbit  
        ;; leMaitre.ephem.fa_pos[inds,*]    = tmpStruct.ephem.fa_pos 
        ;; leMaitre.ephem.alt[inds]         = tmpStruct.ephem.alt    
        ;; leMaitre.ephem.ilat[inds]        = tmpStruct.ephem.ilat   
        ;; ;; leMaitre.ephem.ilng[inds]        = tmpStruct.ephem.ilng   
        ;; leMaitre.ephem.mlt[inds]         = tmpStruct.ephem.mlt    
        ;; leMaitre.ephem.fa_vel[inds,*]    = tmpStruct.ephem.fa_vel 
        ;; ;; leMaitre.ephem.bfoot[inds,*]  = tmpStruct.ephem.bfoot  
        ;; leMaitre.ephem.lat[inds]         = tmpStruct.ephem.lat    
        ;; leMaitre.ephem.lng[inds]         = tmpStruct.ephem.lng    
        ;; ;; leMaitre.ephem.flat[inds]        = tmpStruct.ephem.flat   
        ;; ;; leMaitre.ephem.flng[inds]        = tmpStruct.ephem.flng   
        ;; ;; leMaitre.ephem.b_model[inds,*]   = tmpStruct.ephem.b_model
        ;; leMaitre.ephem.magRatio[inds]    = TEMPORARY(ratio)

        datInd                          += TEMPORARY(nPtsHere)
        magFlagInd                      += TEMPORARY(nMagFlagHere)

     ENDFOR

  ENDFOR

  finalInds   = [0:(datInd-1)]
  finalMFInds = [0:(magFlagInd-1)]

  IF haveACHigh THEN BEGIN
     dBStruct    = {p : {DC:leMaitre.dB.p.DC[*,finalInds], $
                         AC:leMaitre.dB.p.AC[*,finalInds], $
                         ACHigh:leMaitre.dB.p.ACHigh[*,finalInds]}, $
                    v : {DC:leMaitre.dB.v.DC[*,finalInds], $
                         AC:leMaitre.dB.v.AC[*,finalInds], $
                         ACHigh:leMaitre.dB.v.ACHigh[*,finalInds]}, $
                    B : {DC:leMaitre.dB.B.DC[*,finalInds], $
                         AC:leMaitre.dB.B.AC[*,finalInds], $
                         ACHigh:leMaitre.dB.B.ACHigh[*,finalInds]}}

     eAlongV     = {DC:leMaitre.e.alongV.DC[*,finalInds], $
                    AC:leMaitre.e.alongV.AC[*,finalInds], $
                    ACHigh:leMaitre.e.alongV.ACHigh[*,finalInds]}

     eNearB      = KEYWORD_SET(include_E_near_B)                 ? $
                   {DC:leMaitre.e.nearB.DC[*,finalInds], $
                    AC:leMaitre.e.nearB.AC[*,finalInds], $
                    ACHigh:leMaitre.e.nearB.ACHigh[*,finalInds]} : $
                   0B

     dsp         = KEYWORD_SET(skipDSP) ? $
                   0B                   : $
                   {DC:leMaitre.e.dsp.DC[*,finalInds], $
                    AC:leMaitre.e.dsp.AC[*,finalInds], $
                    ACHigh:leMaitre.e.dsp.ACHigh[*,finalInds]}

     pFP         = {DC:leMaitre.pFlux.p.DC[*,finalInds], $
                    AC:leMaitre.pFlux.p.AC[*,finalInds], $
                    ACHigh:leMaitre.pFlux.p.ACHigh[*,finalInds]}
     
     pFB         = {DC:leMaitre.pFlux.B.DC[*,finalInds], $
                    AC:leMaitre.pFlux.B.AC[*,finalInds], $
                    ACHigh:leMaitre.pFlux.B.ACHigh[*,finalInds]}
     
     pFV         = KEYWORD_SET(full_pFlux)                       ? $
                   {DC:leMaitre.pFlux.v.DC[*,finalInds], $
                    AC:leMaitre.pFlux.v.AC[*,finalInds], $
                    ACHigh:leMaitre.pFlux.v.ACHigh[*,finalInds]} : $
                   0B

  ENDIF ELSE BEGIN

     dBStruct    = {p : {DC:leMaitre.dB.p.DC[*,finalInds], $
                         AC:leMaitre.dB.p.AC[*,finalInds]}, $
                    v : {DC:leMaitre.dB.v.DC[*,finalInds], $
                         AC:leMaitre.dB.v.AC[*,finalInds]}, $
                    B : {DC:leMaitre.dB.B.DC[*,finalInds], $
                         AC:leMaitre.dB.B.AC[*,finalInds]}}

     eAlongV     = {DC : leMaitre.e.alongV.DC[*,finalInds], $
                    AC : leMaitre.e.alongV.AC[*,finalInds]}

     eNearB      = KEYWORD_SET(include_E_near_B)                 ? $
                   {DC : leMaitre.e.nearB.DC[*,finalInds], $
                    AC : leMaitre.e.nearB.AC[*,finalInds]} : $
                   0B

     dsp         = KEYWORD_SET(skipDSP) ? $
                   0B                   : $
                   {DC : leMaitre.e.dsp.DC[*,finalInds], $
                    AC : leMaitre.e.dsp.AC[*,finalInds]}

     pFP         = {DC : leMaitre.pFlux.p.DC[*,finalInds], $
                    AC : leMaitre.pFlux.p.AC[*,finalInds]}
     
     pFB         = {DC : leMaitre.pFlux.B.DC[*,finalInds], $
                    AC : leMaitre.pFlux.B.AC[*,finalInds]}
     
     pFV         = KEYWORD_SET(full_pFlux)                       ? $
                   {DC : leMaitre.pFlux.v.DC[*,finalInds], $
                    AC : leMaitre.pFlux.v.AC[*,finalInds]} : $
                   0B

  ENDELSE

  ;; CASE 1 OF
  ;;    KEYWORD_SET(include_E_near_B) AND ~KEYWORD_SET(skipDSP): BEGIN
  eStruct     = {alongV : TEMPORARY(eAlongV), $
                 nearB  : TEMPORARY(eNearB), $
                 dsp    : TEMPORARY(dsp)}

  ;;    END
  ;;    KEYWORD_SET(include_E_near_B) AND KEYWORD_SET(skipDSP): BEGIN
  ;;       eStruct     = {alongV               : TEMPORARY(eAlongV), $
  ;;                      nearB                : {DC:leMaitre.e.nearB.DC[*,finalInds], $
  ;;                                              AC:leMaitre.e.nearB.AC[*,finalInds], $
  ;;                                              ACHigh:leMaitre.e.nearB.ACHigh[*,finalInds]}, $
  ;;                      dsp                  : 0B}

  ;;    END
  ;;    KEYWORD_SET(skipDSP): BEGIN
  ;;       eStruct     = {alongV               : {DC:leMaitre.e.alongV.DC[*,finalInds], $
  ;;                                              AC:leMaitre.e.alongV.AC[*,finalInds], $
  ;;                                              ACHigh:leMaitre.e.alongV.ACHigh[*,finalInds]}, $
  ;;                      dsp                  : 0B}

  ;;    END
  ;; ENDCASE

  ptcl              = KEYWORD_SET(include_particles)      ? $
                      {jEe:leMaitre.ptcl.jEe[finalInds], $
                       je:leMaitre.ptcl.je[finalInds], $
                       ji:leMaitre.ptcl.ji[finalInds]}    : $
                      0B

  pFluxStruct       = {p : TEMPORARY(pFP), $
                       v : TEMPORARY(pFV), $
                       B : TEMPORARY(pFB)}

  magFlags          = {x : leMaitre.magFlags.x[finalMFInds], $
                       y : leMaitre.magFlags.y[finalMFInds]}

  ;; ephem             = {time      : leMaitre.ephem.time[finalInds], $
  ;;                      orbit     : leMaitre.ephem.orbit[finalInds], $  
  ;;                      fa_pos    : leMaitre.ephem.fa_pos[finalInds,*], $
  ;;                      alt       : leMaitre.ephem.alt [finalInds], $
  ;;                      ilat      : leMaitre.ephem.ilat[finalInds], $
  ;;                      ;; ilng      : leMaitre.ephem.ilng[finalInds], $
  ;;                      mlt       : leMaitre.ephem.mlt [finalInds], $
  ;;                      fa_vel    : leMaitre.ephem.fa_vel[finalInds,*], $
  ;;                      ;; bfoot     : MAKE_ARRAY(maxNPts,3,/FLOAT), $
  ;;                      magRatio  : leMaitre.ephem.magRatio[finalInds], $
  ;;                      lat       : leMaitre.ephem.lat     [finalInds], $
  ;;                      lng       : leMaitre.ephem.lng     [finalInds]}
  ;;                     ;; flat      : leMaitre.ephem.flat    [finalInds], $
  ;;                     ;; flng      : leMaitre.ephem.flng    [finalInds], $
  ;;                     ;; b_model   : leMaitre.ephem.b_model [finalInds,*]}, $

  leMaitre    = {dB       : TEMPORARY(dBStruct), $
                 e        : TEMPORARY(eStruct), $
                 ptcl     : TEMPORARY(ptcl), $
                 pFlux    : TEMPORARY(pFluxStruct), $
                 magFlags : TEMPORARY(magFlags), $
                 ;; ephem    : TEMPORARY(ephem), $
                 info     : leMaitre.info}


  SAVE,leMaitre,FILENAME=outDir+maitreFil

END
