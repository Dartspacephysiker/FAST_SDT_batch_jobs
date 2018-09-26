;2018/09/25
PRO LOCALE_ADJUSTMENTS,times, $
                       oflow_i,nOutflow, $
                       ORBIT=orbit, $
                       NORTH=north, $
                       SOUTH=south, $
                       DAY=day, $
                       NIGHT=night, $
                       MINMLT=minMLT, $
                       MAXMLT=maxMLT, $
                       MINILAT=minILAT, $
                       MAXILAT=maxILAT

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~(KEYWORD_SET(north) OR KEYWORD_SET(south) OR KEYWORD_SET(day) OR KEYWORD_SET(night)) THEN RETURN

  GET_FA_ORBIT,times,/TIME_ARRAY,/DEFINITIVE

  IF KEYWORD_SET(north) OR KEYWORD_SET(south) THEN BEGIN
     haveHemi = 1
     GET_DATA,'ILAT',DATA=ilat
     ilat = ilat.y
     
  ENDIF

  IF KEYWORD_SET(day) OR KEYWORD_SET(night) OR KEYWORD_SET(minMLT) OR KEYWORD_SET(maxMLT) THEN BEGIN
     haveSide = 1
     GET_DATA,'MLT',DATA=mlt
     mlt = mlt.y
  ENDIF

  CASE N_ELEMENTS(SIZE(minILAT,/DIMENSIONS)) OF
     0: 
     1: BEGIN

        tmpMinILAT = minILAT

     END
     2: BEGIN

        this = WHERE(orbit EQ minILAT[0,*],nThis)

        IF nThis NE 1 THEN STOP

        tmpMinILAT = minILAT[1,this]

     END
  ENDCASE

  IF tmpMinILAT LT 0 THEN BEGIN
     PRINT,FORMAT='("Flipping sign of tmpMinILAT = ",F4.1,"; it should be pos in any case, you know ...")',tmpMinILAT
     tmpMinILAT = tmpMinILAT*(-1)
  ENDIF

  CASE N_ELEMENTS(SIZE(maxILAT,/DIMENSIONS)) OF
     0: 
     1: BEGIN

        IF N_ELEMENTS(maxILAT) EQ 0 THEN BEGIN
           tmpMaxILAT = 90
        ENDIF ELSE BEGIN

           tmpMaxILAT = maxILAT

        ENDELSE

     END
     2: BEGIN

        this = WHERE(orbit EQ maxILAT[0,*],nThis)

        IF nThis NE 1 THEN STOP

        tmpMaxILAT = maxILAT[1,this]

     END
  ENDCASE

  IF tmpMaxILAT LT 0 THEN BEGIN
     PRINT,FORMAT='("Flipping sign of tmpMaxILAT = ",F4.1,"; it should be pos in any case, you know ...")',tmpMaxILAT
     tmpMaxILAT = tmpMaxILAT*(-1)
  ENDIF

  IF KEYWORD_SET(north) THEN BEGIN

     IF KEYWORD_SET(tmpMaxILAT) THEN BEGIN
        hemi_i  = WHERE((ilat GE tmpMinILAT) AND (ilat LE tmpMaxILAT),nHemi)
     ENDIF ELSE BEGIN
        hemi_i  = WHERE(ilat GE tmpMinILAT,nHemi)
     ENDELSE

  ENDIF ELSE IF KEYWORD_SET(south) THEN BEGIN

     IF KEYWORD_SET(tmpMaxILAT) THEN BEGIN
        hemi_i  = WHERE((ilat LE -1.*tmpMinILAT) AND (ilat GE -1.*tmpMaxILAT),nHemi)
     ENDIF ELSE BEGIN
        hemi_i  = WHERE(ilat LE -1.*tmpMinILAT,nHemi)
     ENDELSE

  ENDIF ELSE BEGIN

     hemi_i  = WHERE(ABS(ilat) GE tmpMinILAT,nHemi)

  ENDELSE

  CASE 1 OF
     KEYWORD_SET(day): BEGIN
        side_i    = WHERE(mlt GE 6.0 AND mlt LT 18.0 AND (ABS(ilat) GE tmpMinILAT),nSide)   
     END
     KEYWORD_SET(night): BEGIN
        side_i    = WHERE(mlt GE 18.0 OR mlt LT 6.0 AND (ABS(ilat) GE tmpMinILAT),nSide)
     END
     (N_ELEMENTS(minMLT) GT 0 OR N_ELEMENTS(maxMLT) GT 0): BEGIN

        side_i = GET_MLT_INDS(dbStruct,minMLT,maxMLT, $
                              DIRECT_MLTS=mlt, $
                              N_MLT=nSide, $
                              /BATCH_MODE)

        ;; side_i = CGSETINTERSECTION(side_i,WHERE(ABS(ilat) 

     END
  ENDCASE

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
