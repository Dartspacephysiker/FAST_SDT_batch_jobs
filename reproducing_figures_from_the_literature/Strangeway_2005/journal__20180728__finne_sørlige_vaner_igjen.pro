;2018/07/28
PRO JOURNAL__20180728__FINNE_SøRLIGE_VANER_IGJEN

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__fastloc_vars.pro

  @common__fastloc_espec_vars.pro

  for_eSpec_DBs = 1

  LOAD_FASTLOC_AND_FASTLOC_TIMES,FOR_ESPEC_DBS=for_eSpec_DBs

  CASE 1 OF
     KEYWORD_SET(for_eSpec_DBs): BEGIN
        fl = TEMPORARY(FL_eSpec__fastLoc)
     END
     ELSE: BEGIN
        fl = TEMPORARY(FL__fastLoc)
     END
  ENDCASE

  south_i = WHERE(fl.ilat LT 0)

  uniqOrb      = fl.orbit[south_i[UNIQ(fl.orbit[south_i],SORT(fl.orbit[south_i]))]]

  alt = uniqOrb * 0.
  mlt = uniqOrb * 0.
  ilat = uniqOrb * 0.
  FOREACH orb,uniqOrb,ind DO BEGIN
     tmpI = WHERE((fl.orbit EQ orb),nTmp)

     ;; tmpII = WHERE((fl.ilat[tmpI] LT 0 ) AND $
     ;;               (fl.mlt [tmpI] GE 11 ) AND $
     ;;               (fl.mlt [tmpI] LE 15),nTmpII)
     tmpII = WHERE((fl.ilat[tmpI] LT -70) AND $
                   (fl.ilat[tmpI] GT -85) AND $
                   (fl.mlt [tmpI] GE 11 ) AND $
                   (fl.mlt [tmpI] LE 15),nTmpII)

     IF nTmpII GT 0 THEN BEGIN
        alt[ind] = MAX(fl.alt[tmpI[tmpII]],indiii)

        mlt[ind]  = fl.mlt[tmpI[tmpII[indiii]]]
        ilat[ind] = fl.ilat[tmpI[tmpII[indiii]]]

        IF alt[ind] GT 3000 THEN PRINT,FORMAT='(I5,TR3,F7.2,TR3,F5.2,TR3,F5.1,TR3,I4)', $
                                       orb,alt[ind],mlt[ind],ilat[ind],nTmpII
     ENDIF
  ENDFOREACH

  CASE 1 OF
     KEYWORD_SET(for_eSpec_DBs): BEGIN
        fl = TEMPORARY(FL_eSpec__fastLoc)
     END
     ELSE: BEGIN
        fl = TEMPORARY(FL__fastLoc)
     END
  ENDCASE
  STOP
  
END
