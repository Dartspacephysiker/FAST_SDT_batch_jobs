;;01/24/17
PRO CLEANUP_STRUCT,struct, $
                   FINITE=finite, $
                   SORT_TIME=sort_time, $
                   UNIQ_TIME=uniq_time, $
                   MUY_RAPIDO=muy_rapido, $
                   REMOVE_TIME=remove_time, $
                   KEPT_I=kept_i, $
                   SUCCESS=success

  COMPILE_OPT IDL2

  ;;Assume we hose it
  success = 0

  tags = TAG_NAMES(struct)
  nTags = N_ELEMENTS(tags)

  IF nTags GT 2 THEN BEGIN
     PRINT,"Sorry, I only do TWO tags, buddy"
     RETURN
  ENDIF

  nMemb = N_ELEMENTS(struct.x)
  IF nMemb NE N_ELEMENTS(struct.y) THEN BEGIN
     PRINT,"You've got a problem: Unequal numbers of members"
     RETURN
  ENDIF

  good_i = LINDGEN(nMemb)

  IF KEYWORD_SET(muy_rapido) THEN BEGIN

     good_i = CGSETINTERSECTION(good_i,WHERE(FINITE(struct.y),nFinite))
     IF nFinite EQ 0 THEN RETURN

     struct = {x:struct.x[good_i],y:struct.y[good_i]}


     uniq_i = UNIQ(struct.x,SORT(struct.x))
     struct = {x:struct.x[uniq_i],y:struct.y[uniq_i]}

     kept_i = good_i[uniq_i]

     IF KEYWORD_SET(remove_time) THEN BEGIN
        struct = struct.y
     ENDIF

     success = 1

     RETURN
  ENDIF

  IF KEYWORD_SET(finite) THEN BEGIN

     good_i = CGSETINTERSECTION(good_i,WHERE(FINITE(struct.y),nFinite))

     IF nFinite EQ 0 THEN BEGIN
        PRINT,"This whole thing is junk!"
        RETURN
     ENDIF ELSE BEGIN
        PRINT,"Junking " + STRCOMPRESS(nMemb-nFinite) + " non-finite junk pieces"
        struct = {x:struct.x[good_i],y:struct.y[good_i]}
     ENDELSE

  ENDIF

  IF KEYWORD_SET(sort_time) THEN BEGIN

     sort_i = SORT(struct.x)
     struct = {x:struct.x[sort_i],y:struct.y[sort_i]}

  ENDIF

  IF KEYWORD_SET(uniq_time) THEN BEGIN

     uniq_i = UNIQ(struct.x)
     struct = {x:struct.x[uniq_i],y:struct.y[uniq_i]}

  ENDIF

END
