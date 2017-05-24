;2017/05/22
PRO STRANGEWAY_3BANDS__STITCH_FILES, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   USE_JE_TBOUNDS=use_Je_tBounds, $
   INCLUDE_PARTICLES=include_particles, $
   DECIMATE_E_AND_B__THEN_CALC_PFLUX=decimate_eb_calc_pFlux, $
   SKIPDSP=skipDSP

  COMPILE_OPT IDL2,STRICTARRSUBS

  @strangeway_3bands__defaults__8hz.pro

  interp_4Hz_to_1s     = 1

  originating_routine  = 'STRANGEWAY_3BANDS__STITCH_FILES'

  ;;1997
  ;; startOrb             = 1436
  ;; stopOrb              = 5382
  
  startOrb             = 1000
  stopOrb              = 9936

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
  leMaitre             = {dB    : {p                       : {DC:typisk,AC:typisk}, $
                                   v                       : {DC:typisk,AC:typisk}, $
                                   B                       : {DC:typisk,AC:typisk}}, $
                          e     : {AlongV                  : {DC:typisk,AC:typisk}, $
                                   ;; NearB                   : TEMPORARY(eNB), $
                                   dsp                     : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}, $
                          ptcl  : ( KEYWORD_SET(include_particles) ? {jEe:typisk,je:typisk,ji:typisk} : 0B ), $
                          pFlux : CREATE_STRUCT('p',{DC:typisk,AC:typisk}, $
                                                'v',(KEYWORD_SET(full_pFlux) ? {DC:typisk,AC:typisk} : 0B), $
                                                'b',{DC:typisk,AC:typisk}), $
                          magFlags : {x                    : MAKE_ARRAY(maxNPts/5,/DOUBLE), $
                                      y                    : MAKE_ARRAY(maxNPts/5,/UINT)}, $
                          ephem : {time                    : MAKE_ARRAY(maxNPts,/DOUBLE), $
                                   orbit                   : MAKE_ARRAY(maxNPts,/LONG), $  
                                   fa_pos                  : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                                   alt                     : typisk, $
                                   ilat                    : typisk, $
                                   ;; ilng                    : typisk, $
                                   mlt                     : typisk, $
                                   fa_vel                  : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                                   ;; bfoot                   : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                                   magRatio                : typisk, $
                                   lat                     : typisk, $
                                   lng                     : typisk, $
                                   ;; flat                    : typisk, $
                                   ;; flng                    : typisk, $
                                   b_model                 : MAKE_ARRAY(maxNPts,3,/FLOAT)}, $
                          info   : {full_pFlux             : BYTE(KEYWORD_SET(full_pFlux)), $
                                    decimate_eb_calc_pFlux : BYTE(KEYWORD_SET(decimate_eb_calc_pFlux)), $
                                    interp_4Hz_to_1s       : BYTE(KEYWORD_SET(interp_4Hz_to_1s      )), $
                                    include_E_near_B       : BYTE(KEYWORD_SET(include_E_near_B)), $
                                    eField_fit_variables   : BYTE(KEYWORD_SET(use_eField_fit_variables)), $
                                    skipDSP                : BYTE(KEYWORD_SET(skipDSP)), $
                                    originating_routine    : originating_routine, $
                                    date                   : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)}}

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

        PRINT,FORMAT='(I0,", ",I0,", ",I0)',orb,jj,nPtsHere

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

        inds                             = [datInd:(datInd+nPtsHere-1)]
        MFInds                           = [magFlagInd:(magFlagInd+nMagFlagHere-1)]

        leMaitre.dB.p.DC[inds]           = tmpStruct.dB.p.DC
        leMaitre.dB.p.AC[inds]           = tmpStruct.dB.p.AC
        leMaitre.dB.v.DC[inds]           = tmpStruct.dB.v.DC
        leMaitre.dB.v.AC[inds]           = tmpStruct.dB.v.AC
        leMaitre.dB.B.DC[inds]           = tmpStruct.dB.B.DC
        leMaitre.dB.B.AC[inds]           = tmpStruct.dB.B.AC
        leMaitre.e.AlongV.DC[inds]       = tmpStruct.e.AlongV.DC
        leMaitre.e.AlongV.AC[inds]       = tmpStruct.e.AlongV.AC
        leMaitre.pFlux.p.DC[inds]        = tmpStruct.pFlux.p.DC
        leMaitre.pFlux.p.AC[inds]        = tmpStruct.pFlux.p.AC
        leMaitre.pFlux.B.DC[inds]        = tmpStruct.pFlux.B.DC
        leMaitre.pFlux.B.AC[inds]        = tmpStruct.pFlux.B.AC
        leMaitre.magFlags.x[MFInds]      = tmpStruct.magFlags.x
        leMaitre.magFlags.y[MFInds]      = tmpStruct.magFlags.y

        ;;Calc mag ratio
        mag1                             = (tmpStruct.ephem.b_model[*,0]*tmpStruct.ephem.b_model[*,0]+ $
                                            tmpStruct.ephem.b_model[*,1]*tmpStruct.ephem.b_model[*,1]+ $
                                            tmpStruct.ephem.b_model[*,2]*tmpStruct.ephem.b_model[*,2])^0.5
        mag2                             = (tmpStruct.ephem.bfoot[*,0]*tmpStruct.ephem.bfoot[*,0]+ $
                                            tmpStruct.ephem.bfoot[*,1]*tmpStruct.ephem.bfoot[*,1]+ $
                                            tmpStruct.ephem.bfoot[*,2]*tmpStruct.ephem.bfoot[*,2])^0.5
        ratio                            = (mag2/mag1)


        leMaitre.ephem.time[inds]        = tmpStruct.ephem.time   
        leMaitre.ephem.orbit[inds]       = tmpStruct.ephem.orbit  
        leMaitre.ephem.fa_pos[inds,*]    = tmpStruct.ephem.fa_pos 
        leMaitre.ephem.alt[inds]         = tmpStruct.ephem.alt    
        leMaitre.ephem.ilat[inds]        = tmpStruct.ephem.ilat   
        ;; leMaitre.ephem.ilng[inds]        = tmpStruct.ephem.ilng   
        leMaitre.ephem.mlt[inds]         = tmpStruct.ephem.mlt    
        leMaitre.ephem.fa_vel[inds,*]    = tmpStruct.ephem.fa_vel 
        ;; leMaitre.ephem.bfoot[inds,*]  = tmpStruct.ephem.bfoot  
        leMaitre.ephem.lat[inds]         = tmpStruct.ephem.lat    
        leMaitre.ephem.lng[inds]         = tmpStruct.ephem.lng    
        ;; leMaitre.ephem.flat[inds]        = tmpStruct.ephem.flat   
        ;; leMaitre.ephem.flng[inds]        = tmpStruct.ephem.flng   
        leMaitre.ephem.b_model[inds,*]   = tmpStruct.ephem.b_model
        leMaitre.ephem.magRatio[inds]    = TEMPORARY(ratio)

        datInd                          += TEMPORARY(nPtsHere)

     ENDFOR

  ENDFOR

  finalInds   = [0:(datInd-1)]
  finalMFInds = [0:(magFlagInd-1)]

  leMaitre    = {dB       : {p                    : {DC:leMaitre.dB.p.DC[finalInds],AC:leMaitre.dB.p.AC[finalInds]}, $
                             v                    : {DC:leMaitre.dB.v.DC[finalInds],AC:leMaitre.dB.v.AC[finalInds]}, $
                             B                    : {DC:leMaitre.dB.B.DC[finalInds],AC:leMaitre.dB.B.AC[finalInds]}}, $
                 e        : {AlongV               : {DC:leMaitre.e.alongV.DC[finalInds],AC:leMaitre.e.alongV.AC[finalInds]}, $
                             ;; NearB                   : TEMPORARY(eNB), $
                             dsp                  : KEYWORD_SET(skipDSP) ?  0B : {DC:typisk,AC:typisk}}, $
                 ptcl     : ( KEYWORD_SET(include_particles) ? {jEe:leMaitre.ptcl.jEe[finalInds], $
                                                                je:leMaitre.ptcl.je[finalInds], $
                                                                ji:leMaitre.ptcl.ji[finalInds]} : 0B ), $
                 pFlux    : {p                    : {DC:leMaitre.pFlux.p.DC[finalInds],AC:leMaitre.pFlux.p.AC[finalInds]}, $
                             v                    : KEYWORD_SET(full_pFlux) ? {DC:leMaitre.pFlux.v.DC[finalInds],AC:leMaitre.pFlux.v.AC[finalInds]} : 0B, $
                             B                    : {DC:leMaitre.pFlux.B.DC[finalInds],AC:leMaitre.pFlux.B.AC[finalInds]}}, $
                 magFlags : {x                    : leMaitre.magFlags.x[finalMFInds], $
                             y                    : leMaitre.magFlags.y[finalMFInds]}, $
                 ephem    : {time                 : leMaitre.ephem.time[finalInds], $
                             orbit                : leMaitre.ephem.orbit[finalInds], $  
                             fa_pos               : leMaitre.ephem.fa_pos[finalInds,*], $
                             alt                  : leMaitre.ephem.alt [finalInds], $
                             ilat                 : leMaitre.ephem.ilat[finalInds], $
                             ;; ilng                 : leMaitre.ephem.ilng[finalInds], $
                             mlt                  : leMaitre.ephem.mlt [finalInds], $
                             fa_vel               : leMaitre.ephem.fa_vel[finalInds,*], $
                             ;; bfoot                : MAKE_ARRAY(maxNPts,3,/FLOAT), $
                             magRatio             : leMaitre.ephem.magRatio[finalInds], $
                             lat                  : leMaitre.ephem.lat     [finalInds], $
                             lng                  : leMaitre.ephem.lng     [finalInds], $
                             ;; flat                 : leMaitre.ephem.flat    [finalInds], $
                             ;; flng                 : leMaitre.ephem.flng    [finalInds], $
                             b_model              : leMaitre.ephem.b_model [finalInds,*]}, $
                 info     : leMaitre.info}


  SAVE,leMaitre,FILENAME=outDir+maitreFil

END
