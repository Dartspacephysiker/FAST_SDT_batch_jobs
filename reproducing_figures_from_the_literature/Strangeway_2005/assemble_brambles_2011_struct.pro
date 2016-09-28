;;09/27/16
FUNCTION ASSEMBLE_BRAMBLES_2011_STRUCT,tS_1s, $
                                       orbit, $
                                       tmpDatStruct, $
                                       tmp1sStruct, $
                                       minILAT, $
                                       SDT_NAMES=SDT_names, $
                                       SANS_INTEGRATION=sans_integration

  GET_FA_ORBIT,tS_1s,/TIME_ARRAY,/DEFINITIVE,/ALL
  GET_DATA,'fa_vel',DATA=vel
  speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  ;;get position of each mag point
  position         = MAKE_ARRAY(N_ELEMENTS(tS_1s),/DOUBLE,VALUE=0.0D)
  speed_point_inds = VALUE_CLOSEST2(vel.x,tS_1s)
  speed_mag_point  = speed[speed_point_inds]
  position[1:-1]   = TOTAL((tS_1s[1:-1]-tS_1s[0:-2])*speed_mag_point,/CUMULATIVE)

  GET_DATA,SDT_names.MLT          ,DATA=mlt
  GET_DATA,SDT_names.ILAT         ,DATA=ilat
  GET_DATA,SDT_names.ALT          ,DATA=alt
  GET_DATA,SDT_names.E_ALONG_V_AC ,DATA=eAVsc
  GET_DATA,SDT_names.dB_ac_interp ,DATA=dB_perp
  GET_DATA,SDT_names.pFlux_ac     ,DATA=pFluxB1s
  ;; GET_DATA',SDT_names.Je        ,DATA=Je
  GET_DATA,SDT_names.pFluxP_ac    ,DATA=pFluxP
  ;; GET_DATA',SDT_names.Ji        ,DATA=Ji
  ;; GET_DATA',SDT_names.DSP_integ ,DATA=dsp

  ;;Check time series, if you like
  PRINT,ARRAY_EQUAL(eavsc.x[*,1],db_perp.x[*,1])
  PRINT,ARRAY_EQUAL(eavsc.x[*,1],pFluxB1s.x[*,1])
  PRINT,ARRAY_EQUAL(db_perp.x[*,1],pFluxB1s.x[*,1])
  PRINT,ARRAY_EQUAL(pFluxP.x[*,1],pFluxB1s.x[*,1])
  ;; PRINT,ARRAY_EQUAL(je.x,pFluxB1s.x[*,1])
  ;; PRINT,ARRAY_EQUAL(je.x,pFluxP.x)
  ;; PRINT,ARRAY_EQUAL(je.x,ji.x)
  ;; PRINT,ARRAY_EQUAL(pFluxP.x,ji.x)
  ;; PRINT,ARRAY_EQUAL(pFluxP.x,dsp.x)
  ;; PRINT,ARRAY_EQUAL(je.x,dsp.x)
  ;; PRINT,ARRAY_EQUAL(ji.x,dsp.x)

  ;;Reduce data
  time        = tS_1s           ;ephem stuff
  mlt         = mlt.y
  ilat        = ilat.y
  alt         = alt.y

  eAlongV     = eAVsc.y[*,1]    ;fields
  dB_perp     = dB_perp.y[*,1]
  pFAlongB    = pFluxB1s.y[*,1]
  ;; je          = je.y            ;particles
  pFAlongP    = pFluxP.y
  pFAlongBAbs = ABS(pFluxB1s.y[*,1])
  pFAlongPAbs = ABS(pFluxP.y[*,1])
  ;; ji          = ji.y
  ;; dsp         = dsp.y

  ;;Safety
  eAlongV  [WHERE(~FINITE(eAlongV  ))]  = 0.0
  dB_perp  [WHERE(~FINITE(dB_perp  ))]  = 0.0
  pFAlongB [WHERE(~FINITE(pFAlongB ))]  = 0.0
  ;; je       [WHERE(~FINITE(je       ))]  = 0.0     
  pFAlongP [WHERE(~FINITE(pFAlongP ))]  = 0.0    
  pFAlongBAbs [WHERE(~FINITE(pFAlongBAbs ))]  = 0.0
  pFAlongPAbs [WHERE(~FINITE(pFAlongPAbs ))]  = 0.0    
  ;; ji       [WHERE(~FINITE(ji       ))]  = 0.0     
  ;; dsp      [WHERE(~FINITE(dsp      ))]  = 0.0     

  ;;Indices divvying up hemispheres as well as day/nightside
  day_i    = WHERE(mlt GE 6.0 AND mlt LT 18.0 AND (ABS(ilat) GE minILAT),nDay)
  ngt_i    = WHERE(mlt GE 18.0 OR mlt LT 6.0  AND (ABS(ilat) GE minILAT),nNgt)

  all_i    = WHERE(ABS(ilat) GE minILAT,nAll)
  north_i  = WHERE(ilat GE minILAT,nNorth)
  south_i  = WHERE(ilat LE -1.*minILAT,nSouth)

  dayN_i   = CGSETINTERSECTION(day_i,north_i,COUNT=nDayN)
  ngtN_i   = CGSETINTERSECTION(ngt_i,north_i,COUNT=nNgtN)
  dayS_i   = CGSETINTERSECTION(day_i,south_i,COUNT=nDayS)
  ngtS_i   = CGSETINTERSECTION(ngt_i,south_i,COUNT=nNgtS)

  @init_brambles_stats

  IF day_i[0] NE -1 THEN BEGIN

     eAlongVAvg_day    = MEAN(eAlongV[day_i])
     dB_perpAvg_day    = MEAN(dB_perp[day_i])
     pFAlongBAvg_day   = MEAN(pFAlongB[day_i])
     pFAlongPAvg_day   = MEAN(pFAlongP[day_i])
     pFAlongBAbsAvg_day   = MEAN(pFAlongBAbs[day_i])
     pFAlongPAbsAvg_day   = MEAN(pFAlongPAbs[day_i])

     ;; jeAvg_day         = MEAN(je[day_i])
     ;; JiAvg_day         = MEAN(Ji[day_i])

     ;; dspAvg_day        = MEAN(dsp[day_i])

     IF dayN_i[0] NE -1 THEN BEGIN

        dayN_tRange       = [MIN(time[dayN_i]),MAX(time[dayN_i])]
        dayN_len          = MAX(position[dayN_i])-MIN(position[dayN_i])

        eAlongVAvg_dayN   = MEAN(eAlongV[dayN_i])
        dB_perpAvg_dayN   = MEAN(dB_perp[dayN_i])
        pFAlongBAvg_dayN  = MEAN(pFAlongB[dayN_i])
        pFAlongPAvg_dayN  = MEAN(pFAlongP[dayN_i])
        pFAlongBAbsAvg_dayN  = MEAN(pFAlongBAbs[dayN_i])
        pFAlongPAbsAvg_dayN  = MEAN(pFAlongPAbs[dayN_i])

        ;; jeAvg_dayN        = MEAN(je[dayN_i])
        ;; JiAvg_dayN        = MEAN(Ji[dayN_i])

        ;; dspAvg_dayN        = MEAN(dsp[dayN_i])

     ENDIF

     IF dayS_i[0] NE -1 THEN BEGIN

        eAlongVAvg_dayS   = MEAN(eAlongV[dayS_i])
        dB_perpAvg_dayS   = MEAN(dB_perp[dayS_i])
        pFAlongBAvg_dayS  = MEAN(pFAlongB[dayS_i])
        pFAlongPAvg_dayS  = MEAN(pFAlongP[dayS_i])
        pFAlongBAbsAvg_dayS  = MEAN(pFAlongBAbs[dayS_i])
        pFAlongPAbsAvg_dayS  = MEAN(pFAlongPAbs[dayS_i])

        ;; jeAvg_dayS        = MEAN(je[dayS_i])
        ;; JiAvg_dayS        = MEAN(Ji[dayS_i])

        ;; dspAvg_dayS        = MEAN(dsp[dayS_i])

     ENDIF

     day_len           = dayN_len + dayS_len

     IF ~KEYWORD_SET(sans_integration) THEN BEGIN

        IF N_ELEMENTS(day_i) GT 1 THEN BEGIN

           eAlongVInt_day    = INT_TABULATED(position[day_i],eAlongV[day_i])/day_len
           dB_perpInt_day    = INT_TABULATED(position[day_i],dB_perp[day_i])/day_len
           pFAlongBInt_day   = INT_TABULATED(position[day_i],pFAlongB[day_i])/day_len
           pFAlongPInt_day   = INT_TABULATED(position[day_i],pFAlongP[day_i])/day_len
           pFAlongBAbsInt_day   = INT_TABULATED(position[day_i],pFAlongBAbs[day_i])/day_len
           pFAlongPAbsInt_day   = INT_TABULATED(position[day_i],pFAlongPAbs[day_i])/day_len

           ;; JeInt_day         = INT_TABULATED(position[day_i],Je[day_i])/day_len
           ;; JiInt_day         = INT_TABULATED(position[day_i],Ji[day_i])/day_len

           ;; dspInt_day         = INT_TABULATED(position[day_i],dsp[day_i])/day_len

        ENDIF

        IF N_ELEMENTS(dayN_i) GT 1 THEN BEGIN

           eAlongVInt_dayN   = INT_TABULATED(position[dayN_i],eAlongV[dayN_i])/dayN_len
           dB_perpInt_dayN   = INT_TABULATED(position[dayN_i],dB_perp[dayN_i])/dayN_len
           pFAlongBInt_dayN  = INT_TABULATED(position[dayN_i],pFAlongB[dayN_i])/dayN_len
           pFAlongPInt_dayN  = INT_TABULATED(position[dayN_i],pFAlongP[dayN_i])/dayN_len
           pFAlongBAbsInt_dayN  = INT_TABULATED(position[dayN_i],pFAlongBAbs[dayN_i])/dayN_len
           pFAlongPAbsInt_dayN  = INT_TABULATED(position[dayN_i],pFAlongPAbs[dayN_i])/dayN_len

           ;; JeInt_dayN        = INT_TABULATED(position[dayN_i],Je[dayN_i])/dayN_len
           ;; JiInt_dayN        = INT_TABULATED(position[dayN_i],Ji[dayN_i])/dayN_len

           ;; dspInt_dayN        = INT_TABULATED(position[dayN_i],dsp[dayN_i])/dayN_len

        ENDIF

        IF N_ELEMENTS(dayS_i) GT 1 THEN BEGIN

           eAlongVInt_dayS   = INT_TABULATED(position[dayS_i],eAlongV[dayS_i])/dayS_len
           dB_perpInt_dayS   = INT_TABULATED(position[dayS_i],dB_perp[dayS_i])/dayS_len
           pFAlongBInt_dayS  = INT_TABULATED(position[dayS_i],pFAlongB[dayS_i])/dayS_len
           pFAlongPInt_dayS  = INT_TABULATED(position[dayS_i],pFAlongP[dayS_i])/dayS_len
           pFAlongBAbsInt_dayS  = INT_TABULATED(position[dayS_i],pFAlongBAbs[dayS_i])/dayS_len
           pFAlongPAbsInt_dayS  = INT_TABULATED(position[dayS_i],pFAlongPAbs[dayS_i])/dayS_len

           ;; JeInt_dayS        = INT_TABULATED(position[dayS_i],Je[dayS_i])/dayS_len
           ;; JiInt_dayS        = INT_TABULATED(position[dayS_i],Ji[dayS_i])/dayS_len

           ;; dspInt_dayS        = INT_TABULATED(position[dayS_i],dsp[dayS_i])/dayS_len

        ENDIF

     ENDIF

  ENDIF

  IF ngt_i[0] NE -1 THEN BEGIN

     eAlongVAvg_ngt    = MEAN(eAlongV[ngt_i])
     dB_perpAvg_ngt    = MEAN(dB_perp[ngt_i])
     pFAlongBAvg_ngt   = MEAN(pFAlongB[ngt_i])
     pFAlongPAvg_ngt   = MEAN(pFAlongP[ngt_i])
     pFAlongBAbsAvg_ngt   = MEAN(pFAlongBAbs[ngt_i])
     pFAlongPAbsAvg_ngt   = MEAN(pFAlongPAbs[ngt_i])

     ;; jeAvg_ngt         = MEAN(je[ngt_i])
     ;; JiAvg_ngt         = MEAN(Ji[ngt_i])

     ;; dspAvg_ngt         = MEAN(dsp[ngt_i])

     IF ngtN_i[0] NE -1 THEN BEGIN

        ngtN_tRange       = [MIN(time[ngtN_i]),MAX(time[ngtN_i])]
        ngtN_len          = MAX(position[ngtN_i])-MIN(position[ngtN_i])

        eAlongVAvg_ngtN   = MEAN(eAlongV[ngtN_i])
        dB_perpAvg_ngtN   = MEAN(dB_perp[ngtN_i])
        pFAlongBAvg_ngtN  = MEAN(pFAlongB[ngtN_i])
        pFAlongPAvg_ngtN  = MEAN(pFAlongP[ngtN_i])
        pFAlongBAbsAvg_ngtN  = MEAN(pFAlongBAbs[ngtN_i])
        pFAlongPAbsAvg_ngtN  = MEAN(pFAlongPAbs[ngtN_i])

        ;; jeAvg_ngtN        = MEAN(je[ngtN_i])
        ;; JiAvg_ngtN        = MEAN(Ji[ngtN_i])

        ;; dspAvg_ngtN        = MEAN(dsp[ngtN_i])

     ENDIF

     IF ngtS_i[0] NE -1 THEN BEGIN

        eAlongVAvg_ngtS   = MEAN(eAlongV[ngtS_i])
        dB_perpAvg_ngtS   = MEAN(dB_perp[ngtS_i])
        pFAlongBAvg_ngtS  = MEAN(pFAlongB[ngtS_i])
        pFAlongPAvg_ngtS  = MEAN(pFAlongP[ngtS_i])
        pFAlongBAbsAvg_ngtS  = MEAN(pFAlongBAbs[ngtS_i])
        pFAlongPAbsAvg_ngtS  = MEAN(pFAlongPAbs[ngtS_i])

        ;; jeAvg_ngtS        = MEAN(je[ngtS_i])
        ;; JiAvg_ngtS        = MEAN(Ji[ngtS_i])

        ;; dspAvg_ngtS        = MEAN(dsp[ngtS_i])

     ENDIF

     ngt_len           = ngtN_len + ngtS_len

     IF ~KEYWORD_SET(sans_integration) THEN BEGIN

        IF N_ELEMENTS(ngt_i) GT 1 THEN BEGIN

           eAlongVInt_ngt    = INT_TABULATED(position[ngt_i],eAlongV[ngt_i])/ngt_len
           dB_perpInt_ngt    = INT_TABULATED(position[ngt_i],dB_perp[ngt_i])/ngt_len
           pFAlongBInt_ngt   = INT_TABULATED(position[ngt_i],pFAlongB[ngt_i])/ngt_len
           pFAlongPInt_ngt   = INT_TABULATED(position[ngt_i],pFAlongP[ngt_i])/ngt_len
           pFAlongBAbsInt_ngt   = INT_TABULATED(position[ngt_i],pFAlongBAbs[ngt_i])/ngt_len
           pFAlongPAbsInt_ngt   = INT_TABULATED(position[ngt_i],pFAlongPAbs[ngt_i])/ngt_len

           ;; JeInt_ngt         = INT_TABULATED(position[ngt_i],Je[ngt_i])/ngt_len
           ;; JiInt_ngt         = INT_TABULATED(position[ngt_i],Ji[ngt_i])/ngt_len

           ;; dspInt_ngt         = INT_TABULATED(position[ngt_i],dsp[ngt_i])/ngt_len

        ENDIF

        IF N_ELEMENTS(ngtN_i) GT 1 THEN BEGIN

           eAlongVInt_ngtN   = INT_TABULATED(position[ngtN_i],eAlongV[ngtN_i])/ngtN_len
           dB_perpInt_ngtN   = INT_TABULATED(position[ngtN_i],dB_perp[ngtN_i])/ngtN_len
           pFAlongBInt_ngtN  = INT_TABULATED(position[ngtN_i],pFAlongB[ngtN_i])/ngtN_len
           pFAlongPInt_ngtN  = INT_TABULATED(position[ngtN_i],pFAlongP[ngtN_i])/ngtN_len
           pFAlongBAbsInt_ngtN  = INT_TABULATED(position[ngtN_i],pFAlongBAbs[ngtN_i])/ngtN_len
           pFAlongPAbsInt_ngtN  = INT_TABULATED(position[ngtN_i],pFAlongPAbs[ngtN_i])/ngtN_len

           ;; JeInt_ngtN        = INT_TABULATED(position[ngtN_i],Je[ngtN_i])/ngtN_len
           ;; JiInt_ngtN        = INT_TABULATED(position[ngtN_i],Ji[ngtN_i])/ngtN_len

           ;; dspInt_ngtN        = INT_TABULATED(position[ngtN_i],dsp[ngtN_i])/ngtN_len

        ENDIF

        IF N_ELEMENTS(ngtS_i) GT 1 THEN BEGIN

           eAlongVInt_ngtS   = INT_TABULATED(position[ngtS_i],eAlongV[ngtS_i])/ngtS_len
           dB_perpInt_ngtS   = INT_TABULATED(position[ngtS_i],dB_perp[ngtS_i])/ngtS_len
           pFAlongBInt_ngtS  = INT_TABULATED(position[ngtS_i],pFAlongB[ngtS_i])/ngtS_len
           pFAlongPInt_ngtS  = INT_TABULATED(position[ngtS_i],pFAlongP[ngtS_i])/ngtS_len
           pFAlongBAbsInt_ngtS  = INT_TABULATED(position[ngtS_i],pFAlongBAbs[ngtS_i])/ngtS_len
           pFAlongPAbsInt_ngtS  = INT_TABULATED(position[ngtS_i],pFAlongPAbs[ngtS_i])/ngtS_len

           ;; JeInt_ngtS        = INT_TABULATED(position[ngtS_i],Je[ngtS_i])/ngtS_len
           ;; JiInt_ngtS        = INT_TABULATED(position[ngtS_i],Ji[ngtS_i])/ngtS_len

           ;; dspInt_ngtS        = INT_TABULATED(position[ngtS_i],dsp[ngtS_i])/ngtS_len

        ENDIF

     ENDIF

  ENDIF

  IF north_i[0] NE -1 THEN BEGIN

     north_len      = dayN_len + ngtN_len

     eAlongVAvg_N   = MEAN(eAlongV[north_i])
     dB_perpAvg_N   = MEAN(dB_perp[north_i])
     pFAlongBAvg_N  = MEAN(pFAlongB[north_i])
     pFAlongPAvg_N  = MEAN(pFAlongP[north_i])
     pFAlongBAbsAvg_N  = MEAN(pFAlongBAbs[north_i])
     pFAlongPAbsAvg_N  = MEAN(pFAlongPAbs[north_i])

     ;; jeAvg_N        = MEAN(je[north_i])
     ;; JiAvg_N        = MEAN(Ji[north_i])

     ;; dspAvg_N        = MEAN(dsp[north_i])

     IF ~KEYWORD_SET(sans_integration) THEN BEGIN

        IF N_ELEMENTS(north_i) GT 1 THEN BEGIN

           eAlongVInt_N   = INT_TABULATED(position[north_i],eAlongV[north_i])/north_len
           dB_perpInt_N   = INT_TABULATED(position[north_i],dB_perp[north_i])/north_len
           pFAlongBInt_N  = INT_TABULATED(position[north_i],pFAlongB[north_i])/north_len
           pFAlongPInt_N  = INT_TABULATED(position[north_i],pFAlongP[north_i])/north_len
           pFAlongBAbsInt_N  = INT_TABULATED(position[north_i],pFAlongBAbs[north_i])/north_len
           pFAlongPAbsInt_N  = INT_TABULATED(position[north_i],pFAlongPAbs[north_i])/north_len

           ;; JeInt_N        = INT_TABULATED(position[north_i],Je[north_i])/north_len
           ;; JiInt_N        = INT_TABULATED(position[north_i],Ji[north_i])/north_len

           ;; dspInt_N        = INT_TABULATED(position[north_i],dsp[north_i])/north_len

        ENDIF

     ENDIF

  ENDIF

  IF south_i[0] NE -1 THEN BEGIN

     south_len      = dayS_len + ngtS_len

     eAlongVAvg_S   = MEAN(eAlongV[south_i])
     dB_perpAvg_S   = MEAN(dB_perp[south_i])
     pFAlongBAvg_S  = MEAN(pFAlongB[south_i])
     pFAlongPAvg_S  = MEAN(pFAlongP[south_i])
     pFAlongBAbsAvg_S  = MEAN(pFAlongBAbs[south_i])
     pFAlongPAbsAvg_S  = MEAN(pFAlongPAbs[south_i])

     ;; jeAvg_S        = MEAN(je[south_i])
     ;; JiAvg_S        = MEAN(Ji[south_i])

     ;; dspAvg_S        = MEAN(dsp[south_i])

     IF ~KEYWORD_SET(sans_integration) THEN BEGIN

        IF N_ELEMENTS(south_i) GT 1 THEN BEGIN

           eAlongVInt_S   = INT_TABULATED(position[south_i],eAlongV[south_i])/south_len
           dB_perpInt_S   = INT_TABULATED(position[south_i],dB_perp[south_i])/south_len
           pFAlongBInt_S  = INT_TABULATED(position[south_i],pFAlongB[south_i])/south_len
           pFAlongPInt_S  = INT_TABULATED(position[south_i],pFAlongP[south_i])/south_len
           pFAlongBAbsInt_S  = INT_TABULATED(position[south_i],pFAlongBAbs[south_i])/south_len
           pFAlongPAbsInt_S  = INT_TABULATED(position[south_i],pFAlongPAbs[south_i])/south_len

           ;; JeInt_S        = INT_TABULATED(position[south_i],Je[south_i])/south_len
           ;; JiInt_S        = INT_TABULATED(position[south_i],Ji[south_i])/south_len

           ;; dspInt_S        = INT_TABULATED(position[south_i],dsp[south_i])/south_len

        ENDIF

     ENDIF

  ENDIF

  IF all_i[0] NE -1 THEN BEGIN

     all_len      = north_len+south_len

     eAlongVAvg   = MEAN(eAlongV[all_i])
     dB_perpAvg   = MEAN(dB_perp[all_i])
     pFAlongBAvg  = MEAN(pFAlongB[all_i])
     pFAlongPAvg  = MEAN(pFAlongP[all_i])
     pFAlongBAbsAvg  = MEAN(pFAlongBAbs[all_i])
     pFAlongPAbsAvg  = MEAN(pFAlongPAbs[all_i])

     ;; jeAvg        = MEAN(je[all_i])
     ;; JiAvg        = MEAN(Ji[all_i])

     ;; dspAvg        = MEAN(dsp[all_i])

     IF ~KEYWORD_SET(sans_integration) THEN BEGIN

        IF N_ELEMENTS(all_i) GT 1 THEN BEGIN

           eAlongVInt   = INT_TABULATED(position[all_i],eAlongV[all_i])/all_len
           dB_perpInt   = INT_TABULATED(position[all_i],dB_perp[all_i])/all_len
           pFAlongBInt  = INT_TABULATED(position[all_i],pFAlongB[all_i])/all_len
           pFAlongPInt  = INT_TABULATED(position[all_i],pFAlongP[all_i])/all_len
           pFAlongBAbsInt  = INT_TABULATED(position[all_i],pFAlongBAbs[all_i])/all_len
           pFAlongPAbsInt  = INT_TABULATED(position[all_i],pFAlongPAbs[all_i])/all_len

           ;; JeInt        = INT_TABULATED(position[all_i],Je[all_i])/all_len
           ;; JiInt        = INT_TABULATED(position[all_i],Ji[all_i])/all_len

           ;; dspInt        = INT_TABULATED(position[all_i],dsp[all_i])/all_len

        ENDIF

     ENDIF

  ENDIF

  tmpStruct = {orbit:orbit, $
               time:time, $
               avg:{Both:{both:{N:nAll, $
                                eAlongV:eAlongVAvg, $
                                dB_perp:dB_perpAvg, $
                                pFAlongB:pFAlongBAvg, $
                                pFAlongP:pFAlongPAvg, $
                                pFAlongBAbs:pFAlongBAbsAvg, $
                                pFAlongPAbs:pFAlongPAbsAvg}, $
                                ;; je:jeAvg, $
                                ;; Ji:JiAvg, $
                                ;; dsp:dspAvg}, $
                          day:{N:nDay, $
                               eAlongV:eAlongVAvg_day, $
                               dB_perp:dB_perpAvg_day, $
                               pFAlongB:pFAlongBAvg_day, $
                               pFAlongP:pFAlongPAvg_day, $
                               pFAlongBAbs:pFAlongBAbsAvg_day, $
                               pFAlongPAbs:pFAlongPAbsAvg_day}, $
                               ;; je:jeAvg_day, $
                               ;; Ji:JiAvg_day, $
                               ;; dsp:dspAvg_day}, $
                          ngt:{N:nNgt, $
                               eAlongV:eAlongVAvg_ngt, $
                               dB_perp:dB_perpAvg_ngt, $
                               pFAlongB:pFAlongBAvg_ngt, $
                               pFAlongP:pFAlongPAvg_ngt, $ ;, $
                               pFAlongBAbs:pFAlongBAbsAvg_ngt, $
                               pFAlongPAbs:pFAlongPAbsAvg_ngt}}, $ ;, $
                               ;; je:jeAvg_ngt, $
                               ;; Ji:JiAvg_ngt, $
                               ;; dsp:dspAvg_ngt}}, $
                    North:{both:{N:nNorth, $
                                 eAlongV:eAlongVAvg_N, $
                                 dB_perp:dB_perpAvg_N, $
                                 pFAlongB:pFAlongBAvg_N, $
                                 pFAlongP:pFAlongPAvg_N, $
                                 pFAlongBAbs:pFAlongBAbsAvg_N, $
                                 pFAlongPAbs:pFAlongPAbsAvg_N}, $
                                 ;; je:jeAvg_N, $
                                 ;; Ji:JiAvg_N, $
                                 ;; dsp:dspAvg_N}, $
                           day:{N:nDayN, $
                                eAlongV:eAlongVAvg_dayN, $
                                dB_perp:dB_perpAvg_dayN, $
                                pFAlongB:pFAlongBAvg_dayN, $
                                pFAlongP:pFAlongPAvg_dayN, $
                                pFAlongBAbs:pFAlongBAbsAvg_dayN, $
                                pFAlongPAbs:pFAlongPAbsAvg_dayN}, $
                                ;; je:jeAvg_dayN, $
                                ;; Ji:JiAvg_dayN, $
                                ;; dsp:dspAvg_dayN}, $
                           ngt:{N:nNgtN, $
                                eAlongV:eAlongVAvg_ngtN, $
                                dB_perp:dB_perpAvg_ngtN, $
                                pFAlongB:pFAlongBAvg_ngtN, $
                                pFAlongP:pFAlongPAvg_ngtN, $
                                pFAlongBAbs:pFAlongBAbsAvg_ngtN, $
                                pFAlongPAbs:pFAlongPAbsAvg_ngtN}}, $
                                ;; je:jeAvg_ngtN, $
                                ;; Ji:JiAvg_ngtN, $
                                ;; dsp:dspAvg_ngtN}}, $
                    South:{both:{N:nSouth, $
                                 eAlongV:eAlongVAvg_S, $
                                 dB_perp:dB_perpAvg_S, $
                                 pFAlongB:pFAlongBAvg_S, $
                                 pFAlongP:pFAlongPAvg_S, $
                                 pFAlongBAbs:pFAlongBAbsAvg_S, $
                                 pFAlongPAbs:pFAlongPAbsAvg_S}, $
                                 ;; je:jeAvg_S, $
                                 ;; Ji:JiAvg_S, $
                                 ;; dsp:dspAvg_S}, $
                           day:{N:nDayS, $
                                eAlongV:eAlongVAvg_dayS, $
                                dB_perp:dB_perpAvg_dayS, $
                                pFAlongB:pFAlongBAvg_dayS, $
                                pFAlongP:pFAlongPAvg_dayS, $
                                pFAlongBAbs:pFAlongBAbsAvg_dayS, $
                                pFAlongPAbs:pFAlongPAbsAvg_dayS}, $
                                ;; je:jeAvg_dayS, $
                                ;; Ji:JiAvg_dayS, $
                                ;; dsp:dspAvg_dayS}, $
                           ngt:{N:nNgtS, $
                                eAlongV:eAlongVAvg_ngtS, $
                                dB_perp:dB_perpAvg_ngtS, $
                                pFAlongB:pFAlongBAvg_ngtS, $
                                pFAlongP:pFAlongPAvg_ngtS, $
                                pFAlongBAbs:pFAlongBAbsAvg_ngtS, $
                                pFAlongPAbs:pFAlongPAbsAvg_ngtS}}}, $
                                ;; je:jeAvg_ngtS, $
                                ;; Ji:JiAvg_ngtS, $
                                ;; dsp:dspAvg_ngtS}}}, $
               int:{Both:{both:{len:all_len, $
                                eAlongV:eAlongVInt, $
                                dB_perp:dB_perpInt, $
                                pFAlongB:pFAlongBInt, $
                                pFAlongP:pFAlongPInt, $
                                pFAlongBAbs:pFAlongBAbsInt, $
                                pFAlongPAbs:pFAlongPAbsInt}, $
                                ;; je:jeInt, $
                                ;; Ji:JiInt, $
                                ;; dsp:dspInt}, $
                          day:{len:day_len, $
                               eAlongV:eAlongVInt_day, $
                               dB_perp:dB_perpInt_day, $
                               pFAlongB:pFAlongBInt_day, $
                               pFAlongP:pFAlongPInt_day, $
                               pFAlongBAbs:pFAlongBAbsInt_day, $
                               pFAlongPAbs:pFAlongPAbsInt_day}, $
                               ;; je:jeInt_day, $
                               ;; Ji:JiInt_day, $
                               ;; dsp:dspInt_day}, $
                          ngt:{len:ngt_len, $
                               eAlongV:eAlongVInt_ngt, $
                               dB_perp:dB_perpInt_ngt, $
                               pFAlongB:pFAlongBInt_ngt, $
                               pFAlongP:pFAlongPInt_ngt, $
                               pFAlongBAbs:pFAlongBAbsInt_ngt, $
                               pFAlongPAbs:pFAlongPAbsInt_ngt}}, $
                               ;; je:jeInt_ngt, $
                               ;; Ji:JiInt_ngt, $
                               ;; dsp:dspInt_ngt}}, $
                    North:{both:{len:north_len, $
                                 eAlongV:eAlongVInt_N, $
                                 dB_perp:dB_perpInt_N, $
                                 pFAlongB:pFAlongBInt_N, $
                                 pFAlongP:pFAlongPInt_N, $
                                 pFAlongBAbs:pFAlongBAbsInt_N, $
                                 pFAlongPAbs:pFAlongPAbsInt_N}, $
                                 ;; je:jeInt_N, $
                                 ;; Ji:JiInt_N, $
                                 ;; dsp:dspInt_N}, $
                           day:{len:dayN_len, $
                                eAlongV:eAlongVInt_dayN, $
                                dB_perp:dB_perpInt_dayN, $
                                pFAlongB:pFAlongBInt_dayN, $
                                pFAlongP:pFAlongPInt_dayN, $
                                pFAlongBAbs:pFAlongBAbsInt_dayN, $
                                pFAlongPAbs:pFAlongPAbsInt_dayN}, $
                                ;; je:jeInt_dayN, $
                                ;; Ji:JiInt_dayN, $
                                ;; dsp:dspInt_dayN}, $
                           ngt:{len:ngtN_len, $
                                eAlongV:eAlongVInt_ngtN, $
                                dB_perp:dB_perpInt_ngtN, $
                                pFAlongB:pFAlongBInt_ngtN, $
                                pFAlongP:pFAlongPInt_ngtN, $
                                pFAlongBAbs:pFAlongBAbsInt_ngtN, $
                                pFAlongPAbs:pFAlongPAbsInt_ngtN}}, $
                                ;; je:jeInt_ngtN, $
                                ;; Ji:JiInt_ngtN, $
                                ;; dsp:dspInt_ngtN}}, $
                    South:{both:{len:south_len, $
                                 eAlongV:eAlongVInt_S, $
                                 dB_perp:dB_perpInt_S, $
                                 pFAlongB:pFAlongBInt_S, $
                                 pFAlongP:pFAlongPInt_S, $
                                 pFAlongBAbs:pFAlongBAbsInt_S, $
                                 pFAlongPAbs:pFAlongPAbsInt_S}, $
                                 ;; je:jeInt_S, $
                                 ;; Ji:JiInt_S, $
                                 ;; dsp:dspInt_S}, $
                           day:{len:dayS_len, $
                                eAlongV:eAlongVInt_dayS, $
                                dB_perp:dB_perpInt_dayS, $
                                pFAlongB:pFAlongBInt_dayS, $
                                pFAlongP:pFAlongPInt_dayS, $
                                pFAlongBAbs:pFAlongBAbsInt_dayS, $
                                pFAlongPAbs:pFAlongPAbsInt_dayS}, $
                                ;; je:jeInt_dayS, $
                                ;; Ji:JiInt_dayS, $
                                ;; dsp:dspInt_dayS}, $
                           ngt:{len:ngtS_len, $
                                eAlongV:eAlongVInt_ngtS, $
                                dB_perp:dB_perpInt_ngtS, $
                                pFAlongB:pFAlongBInt_ngtS, $
                                pFAlongP:pFAlongPInt_ngtS, $
                                pFAlongBAbs:pFAlongBAbsInt_ngtS, $
                                pFAlongPAbs:pFAlongPAbsInt_ngtS}}}, $
                                ;; je:jeInt_ngtS, $
                                ;; Ji:JiInt_ngtS, $
                                ;; dsp:dspInt_ngtS}}}, $
               ephem:{mlt:mlt, $
                      ilat:ilat, $
                      alt:alt, $
                      position:position, $
                      speed:speed, $
                      day_i:day_i, $
                      ngt_i:ngt_i, $
                      north_i:north_i, $
                      south_i:south_i, $
                      dayN_i:dayN_i, $
                      ngtN_i:ngtN_i, $
                      dayS_i:dayS_i, $
                      ngtS_i:ngtS_i, $
                      day_len:day_len, $
                      ngt_len:ngt_len, $
                      all_len:all_len, $
                      north_len:north_len, $
                      south_len:south_len, $
                      dayN_len:dayN_len, $
                      ngtN_len:ngtN_len, $
                      dayS_len:dayS_len, $
                      ngtS_len:ngtS_len}, $
               data:CREATE_STRUCT(tmpDatStruct,"one_s",tmp1sStruct)}

  RETURN,tmpStruct

END
