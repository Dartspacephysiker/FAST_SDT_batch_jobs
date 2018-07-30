;2018/07/28
PRO JOURNAL__20180728__FINNE_SøRLIGE_VANER_IGJEN

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__fastloc_vars.pro

  LOAD_FASTLOC_AND_FASTLOC_TIMES

  fl = FL__fastLoc

  south_i = WHERE(fl.ilat LT 0)

  uniqOrb      = fl.orbit[south_i[UNIQ(fl.orbit[south_i],SORT(fl.orbit[south_i]))]]

  alt = uniqOrb * 0.
  mlt = uniqOrb * 0.
  ilat = uniqOrb * 0.
  FOREACH orb,uniqOrb,ind DO BEGIN
     tmpI = WHERE((fl.orbit EQ orb),nTmp)

     tmpII = WHERE((fl.ilat[tmpI] LT 0 ) AND $
                   (fl.mlt [tmpI] GE 9 ) AND $
                   (fl.mlt [tmpI] LE 16),nTmpII)

     IF nTmpII GT 0 THEN BEGIN
        alt[ind] = MAX(fl.alt[tmpI[tmpII]],indiii)

        mlt[ind]  = fl.mlt[tmpI[tmpII[indiii]]]
        ilat[ind] = fl.ilat[tmpI[tmpII[indiii]]]

        IF alt[ind] GT 3000 THEN PRINT,orb,"  ",alt[ind],"  ",mlt[ind],"  ",ilat[ind]
     ENDIF
  ENDFOREACH

  FOREACH orb,uniqorb,ind DO IF alt[ind] GT 3000 THEN PRINT,orb,"  ",alt[ind],"  ",mlt[ind],"  ",ilat[ind]

  STOP
  
END
