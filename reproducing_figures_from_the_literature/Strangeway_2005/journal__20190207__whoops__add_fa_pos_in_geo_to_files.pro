;2019/02/07
PRO JOURNAL__20190207__WHOOPS__ADD_FA_POS_IN_GEO_TO_FILES, $
   HERE_THEY_ARE=here_they_are

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/'
  fPref = 'Orbit_'
  fSuff = '-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav'

  coordDir = '/SPENCEdata/Research/database/temps/'

  IF N_ELEMENTS(here_they_are) GT 0 THEN BEGIN
     orbits = here_they_are
  ENDIF ELSE BEGIN

     doNHSep1998Orbs = 1
     doSHJan1999Orbs = 0

     IF doNHSep1998Orbs THEN BEGIN
        orbits = INDGEN(33)+8260
     ENDIF ELSE IF doSHJan1999Orbs THEN BEGIN
        orbits = [ $ ;; INDGEN(2,START=9291), $
                 ;; INDGEN(5,START=9300), $
                 ;; INDGEN(3,START=9313), $
                 ;; 9322,9324,9333,9336,9346,9354, $
                 ;; INDGEN(5,START=9376), $
                 9387,9389, $
                 INDGEN(2,START=9390), $
                 INDGEN(2,START=9401), $
                 9409,9411,9420, $
                 INDGEN(2,START=9422), $
                 9430, $
                 INDGEN(3,START=9432), $
                 9441,9443,9444,9452,9454,9457,9463,9465,9472, $
                 INDGEN(7,START=9474), $
                 9483, $
                 INDGEN(6,START=9485), $
                 9493,9495, $
                 INDGEN(3,START=9497), $
                 9504,9506,9517,9519]
     ENDIF

  ENDELSE

  nOrbits = N_ELEMENTS(orbits)

  R_E              = 6371.2D    ;Earth radius in km, from IGRFLIB_V2.pro

  FOR k=0,nOrbits-1 DO BEGIN
     
     orbit = orbits[k]
     orbStr = STRING(FORMAT='(I0)',orbit)

     print,"Orbit " + orbStr


     GEO_MAG_filename = "tmpGEOMAGcoords_"+orbStr+".sav"
     tryFile = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/Orbit_'+orbStr+'-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav'

     RESTORE,coordDir+GEO_MAG_filename
     RESTORE,tryFile

     ;; STOP

     ;; RLEN_GEO=SQRT(TOTAL(coords.geo^2.,1)) ;dist from GEO origin in km

     geoCoords = [[geo.alt+r_e],[geo.lon],[geo.lat]]

     tNames = TAG_NAMES(db)
     IF (WHERE(STRUPCASE(tNames) EQ "FA_POS_GEO"))[0] EQ -1 THEN BEGIN

        ;; db = CREATE_STRUCT(db,"FA_POS_GEO",TRANSPOSE(coords.GEO))
        db = CREATE_STRUCT(db,"FA_POS_GEO",TEMPORARY(geoCoords))

     ENDIF ELSE BEGIN
        db.fa_pos_geo = TEMPORARY(geoCoords)
     ENDELSE

     PRINT,"Have to uncomment"
     ;; STOP
     SAVE,iMom,db,FILENAME=tryFile

     ;; useDB = db

     ;; times = useDB.time
     ;; nTot = N_ELEMENTS(times)

     ;; GEIcoords   = {orbit              :useDB.orbit, $
     ;;                fa_pos             :useDB.fa_pos, $
     ;;                alt                :useDB.alt, $
     ;;                lat                :useDB.lat, $
     ;;                lng                :useDB.lng, $
     ;;                fa_vel             :useDB.fa_vel, $
     ;;                pos_and_vel_coords :'GEI (per GET_FA_ORBIT)', $
     ;;                orig_routineName   :'BLIG'}

     ;; orig_routineName = "ODoyle"

     ;; IF FILE_TEST(coordDir+GEO_MAG_filename) THEN BEGIN

     ;;    IF KEYWORD_SET(overwrite_if_no_GEI2GEO) THEN BEGIN

     ;;       RESTORE,coordDir+GEO_MAG_filename

     ;;       tNames = STRUPCASE(TAG_NAMES(coords))

     ;;       IF (WHERE(tNames EQ "GEI2GEO_COORD"))[0] NE -1 THEN BEGIN
     ;;          PRINT,"Already have GEI2GEO stuff! Out ..."
     ;;          CONTINUE
     ;;       ENDIF ELSE BEGIN
     ;;          PRINT,"Not have GEI2GEO yet -- skal bli!"
     ;;       ENDELSE

     ;;    ENDIF ELSE IF KEYWORD_SET(force_overwrite) THEN BEGIN
     ;;       PRINT,"Forced overwrite!"
     ;;    ENDIF ELSE BEGIN
     ;;       PRINT,"File exists: " + outDir+outFile
     ;;       PRINT,"Tell me you want it."
     ;;       STOP
     ;;    ENDELSE

     ;; ENDIF

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; Time strings

     ;; divFactor     = 10000L     ;No more than 10000 at once
     ;; timeStr    = MAKE_ARRAY(nTot,/STRING)
     ;; FOR kk=0L,(nTot/divFactor) DO BEGIN
     ;;    ind1       = kk*divFactor
     ;;    ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
     ;;    PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
     ;;    tempI      = [ind1:ind2]
     ;;    timeStr[tempI] = TIME_TO_STR(times[tempI],/MSEC)
     ;; ENDFOR

     ;; CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS, $
     ;;    timeStr, $
     ;;    times, $
     ;;    ;; GEI_FILE=GEI_coord_filename, $
     ;;    GEI_DIR=coordDir, $
     ;;    GEI_STRUCT_NAME=defGEIStructName, $
     ;;    IN_GEI_STRUCT=GEICoords, $
     ;;    INCLUDE_GEI_TO_GEO_MATRIX=include_gei_to_geo_matrix, $
     ;;    OUTFILE=GEO_MAG_filename, $
     ;;    OUTDIR=coordDir, $
     ;;    ORIG_ROUTINENAME=orig_routineName, $
     ;;    FORCE_OVERWRITE=force_overwrite, $
     ;;    OVERWRITE_IF_NO_GEI_TO_GEO=overwrite_if_no_GEI2GEO


     ;; PRINT,"GOT IT!"

  ENDFOR

END
