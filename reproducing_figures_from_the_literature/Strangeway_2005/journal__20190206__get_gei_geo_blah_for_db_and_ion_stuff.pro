;2019/02/06
;; Version with lots of unnecessary details related to FASTDB_COORDINATE_CONVERSION__SINGLE at bottom of file
PRO JOURNAL__20190206__GET_GEI_GEO_BLAH_FOR_DB_AND_ION_STUFF, $
   HERE_ARE_MY_ORBITS=here_they_are, $
   INCLUDE_GEI_TO_GEO_MATRIX=include_gei_to_geo_matrix

  COMPILE_OPT IDL2,STRICTARRSUBS

  coordDir = '/SPENCEdata/Research/database/temps/'

  IF N_ELEMENTS(include_gei_to_geo_matrix) EQ 0 THEN include_gei_to_geo_matrix = 1
  ;; force_overwrite = 0
  overwrite_if_no_GEI2GEO = 1

  IF N_ELEMENTS(here_they_are) GT 0 THEN BEGIN
     orbits = here_they_are
  ENDIF ELSE BEGIN

     doNHSep1998Orbs = 0
     doSHJan1999Orbs = 1

     IF doNHSep1998Orbs THEN BEGIN
        orbits = INDGEN(33)+8260
     ENDIF ELSE IF doSHJan1999Orbs THEN BEGIN
        orbits = [ $;; INDGEN(2,START=9291), $
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

  IF N_ELEMENTS(defGEIStructName) EQ 0 THEN defGEIStructName = 'GEICoords'

  nOrbits = N_ELEMENTS(orbits)

  FOR k=0,nOrbits-1 DO BEGIN
     
     orbit = orbits[k]
     orbStr = STRING(FORMAT='(I0)',orbit)

     tryFile = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/Orbit_'+orbStr+'-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav'

     print,"Orbit " + orbStr

     RESTORE,tryFile

     useDB = db

     times = useDB.time
     nTot = N_ELEMENTS(times)

     GEIcoords   = {orbit              :useDB.orbit, $
                    fa_pos             :useDB.fa_pos, $
                    alt                :useDB.alt, $
                    lat                :useDB.lat, $
                    lng                :useDB.lng, $
                    fa_vel             :useDB.fa_vel, $
                    pos_and_vel_coords :'GEI (per GET_FA_ORBIT)', $
                    orig_routineName   :'BLIG'}

     GEO_MAG_filename = "tmpGEOMAGcoords_"+orbStr+".sav"
     orig_routineName = "ODoyle"

     IF FILE_TEST(coordDir+GEO_MAG_filename) THEN BEGIN

        IF KEYWORD_SET(overwrite_if_no_GEI2GEO) THEN BEGIN

           RESTORE,coordDir+GEO_MAG_filename

           tNames = STRUPCASE(TAG_NAMES(coords))

           IF (WHERE(tNames EQ "GEI2GEO_COORD"))[0] NE -1 THEN BEGIN
              PRINT,"Already have GEI2GEO stuff! Out ..."
              CONTINUE
           ENDIF ELSE BEGIN
              PRINT,"Not have GEI2GEO yet -- skal bli!"
           ENDELSE

        ENDIF ELSE IF KEYWORD_SET(force_overwrite) THEN BEGIN
           PRINT,"Forced overwrite!"
        ENDIF ELSE BEGIN
           PRINT,"File exists: " + outDir+outFile
           PRINT,"Tell me you want it."
           STOP
        ENDELSE

     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Time strings

     divFactor     = 10000L     ;No more than 10000 at once
     timeStr    = MAKE_ARRAY(nTot,/STRING)
     FOR kk=0L,(nTot/divFactor) DO BEGIN
        ind1       = kk*divFactor
        ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
        PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
        tempI      = [ind1:ind2]
        timeStr[tempI] = TIME_TO_STR(times[tempI],/MSEC)
     ENDFOR

     CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS, $
        timeStr, $
        times, $
        ;; GEI_FILE=GEI_coord_filename, $
        GEI_DIR=coordDir, $
        GEI_STRUCT_NAME=defGEIStructName, $
        IN_GEI_STRUCT=GEICoords, $
        INCLUDE_GEI_TO_GEO_MATRIX=include_gei_to_geo_matrix, $
        OUTFILE=GEO_MAG_filename, $
        OUTDIR=coordDir, $
        ORIG_ROUTINENAME=orig_routineName, $
        FORCE_OVERWRITE=force_overwrite, $
        OVERWRITE_IF_NO_GEI_TO_GEO=overwrite_if_no_GEI2GEO


     PRINT,"GOT IT!"

  ENDFOR

END

;; VERSION WITH LOTS OF UNNECESSARY DETAILS

;; PRO JOURNAL__20190206__GET_GEI_GEO_BLAH_FOR_DB_AND_ION_STUFF
;; 
;;   COMPILE_OPT IDL2,STRICTARRSUBS
;; 
;;   doNHSep1998Orbs = 0
;;   doSHJan1999Orbs = 1
;; 
;;   coordDir = '/SPENCEdata/Research/database/temps/'
;; 
;;   IF doNHSep1998Orbs THEN BEGIN
;;      orbits = INDGEN(33)+8260
;;   ENDIF ELSE IF doSHJan1999Orbs THEN BEGIN
;;      orbits = [INDGEN(2,START=9291), $
;;                INDGEN(5,START=9300), $
;;                INDGEN(3,START=9313), $
;;                9322,9324,9333,9336,9346,9354, $
;;                INDGEN(5,START=9376), $
;;                9387,9389, $
;;                INDGEN(2,START=9390), $
;;                INDGEN(2,START=9401), $
;;                9409,9411,9420, $
;;                INDGEN(2,START=9422), $
;;                9430, $
;;                INDGEN(3,START=9432), $
;;                9441,9443,9444,9452,9454,9457,9463,9465,9472, $
;;                INDGEN(7,START=9474), $
;;                9483, $
;;                INDGEN(6,START=9485), $
;;                9493,9495, $
;;                INDGEN(3,START=9497), $
;;                9504,9506,9517,9519]
;;   ENDIF
;; 
;;   nOrbits = N_ELEMENTS(orbits)
;; 
;;   FOR k=0,nOrbits-1 DO BEGIN
;;      
;;      orbit = orbits[k]
;;      ;; orbit = useDB.orbit[nTot/2]
;;      orbStr = STRING(FORMAT='(I0)',orbit)
;; 
;;      tryFile = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/Orbit_'+orbStr+'-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav'
;; 
;;      print,"Orbit " + orbStr
;; 
;;      RESTORE,tryFile
;; 
;;      useDB = db
;; 
;;      times = useDB.time
;;      nTot = N_ELEMENTS(times)
;; 
;;      GEIcoords   = {orbit              :useDB.orbit, $
;;                     fa_pos             :useDB.fa_pos, $
;;                     alt                :useDB.alt, $
;;                     lat                :useDB.lat, $
;;                     lng                :useDB.lng, $
;;                     fa_vel             :useDB.fa_vel, $
;;                     pos_and_vel_coords :'GEI (per GET_FA_ORBIT)', $
;;                     orig_routineName   :'BLIG'}
;; 
;; 
;;      ;; timeFile = coordDir + "tmpTimeFile.sav"
;;      ;; GEI_coord_filename = "tmpGEIcoords.sav"
;;      GEO_MAG_filename = "tmpGEOMAGcoords_"+orbStr+".sav"
;;      orig_routineName = "ODoyle"
;; 
;;      ;; create_timeStamps = 1
;;      ;; get_GEI_coords = 1
;;      ;; do_GEO_MAG_conversions = 1
;; 
;;      IF N_ELEMENTS(defGEIStructName) EQ 0 THEN defGEIStructName = 'GEICoords'
;; 
;;      ;; IF N_ELEMENTS(R_E             ) EQ 0 THEN R_E              = 6371.2D ;Earth radius in km, from IGRFLIB_V2.pro
;; 
;;      ;; IF N_ELEMENTS(altitude_max    ) EQ 0 THEN altitude_max     = 4400 ;in km, and risk nothing
;;      ;; IF N_ELEMENTS(allow_fl_trace  ) EQ 0 THEN allow_fl_trace   = 1B   ;Allow fieldline tracing for AACGM_v2?
;;      ;; IF N_ELEMENTS(check_if_exists ) EQ 0 THEN check_if_exists  = 1B
;; 
;;      ;;Var names
;;      ;; IF N_ELEMENTS(in_names) EQ 0 THEN BEGIN
;;      ;;    in_names = {GEOSph       : 'GEOSph_arr'   , $
;;      ;;                AACGMSph     : 'AACGMSph_arr' , $
;;      ;;                GEOStruct    : 'GEO'          , $
;;      ;;                AACGMStruct  : 'AACGM'        , $
;;      ;;                coordStruct  : 'GEICoords'    , $
;;      ;;                timeStr      : 'timeTmpStr'   , $
;;      ;;                DBInd        : 'remaining_i'}
;;      ;; ENDIF
;; 
;;      ;; IF N_ELEMENTS(defNames) EQ 0 THEN BEGIN 
;;      ;;    defNames = {AACGMSph    : 'AACGMSph'      , $
;;      ;;                AACGMStruct : 'AACGMStruct'   , $
;;      ;;                restrictVar : 'restrict_ii'   , $
;;      ;;                DBInd       : 'DBInds'        , $
;;      ;;                DBIndName   : 'db_i'}
;;      ;; ENDIF
;; 
;; 
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      ;; Time strings
;; 
;;      divFactor     = 10000L     ;No more than 10000 at once
;;      timeStr    = MAKE_ARRAY(nTot,/STRING)
;;      FOR kk=0L,(nTot/divFactor) DO BEGIN
;;         ind1       = kk*divFactor
;;         ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
;;         PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
;;         tempI      = [ind1:ind2]
;;         timeStr[tempI] = TIME_TO_STR(times[tempI],/MSEC)
;;      ENDFOR
;; 
;;      ;; STOP
;; 
;;      CONVERT_GEI_COORDS_TO_GEO_AND_MAG_COORDS, $
;;         timeStr, $
;;         times, $
;;         ;; GEI_FILE=GEI_coord_filename, $
;;         GEI_DIR=coordDir, $
;;         GEI_STRUCT_NAME=defGEIStructName, $
;;         IN_GEI_STRUCT=GEICoords, $
;;         OUTFILE=GEO_MAG_filename, $
;;         OUTDIR=coordDir, $
;;         ORIG_ROUTINENAME=orig_routineName
;; 
;; 
;;      PRINT,"GOT IT!"
;; 
;;   ENDFOR
;; 
;;   ;; RESTORE,coordDir+GEO_MAG_filename
;; 
;;   ;; IF N_ELEMENTS(coords.time) NE nTot THEN STOP
;; 
;;   ;; FASTDB_COORDINATE_CONVERSION__SINGLE,times, $
;;   ;;                                      TIME_STRINGS=timeStr, $
;;   ;;                                      CREATE_TIMESTAMPS=create_timeStamps, $
;;   ;;                                      GET_GEI_COORDS=get_GEI_coords, $
;;   ;;                                      DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
;;   ;;                                      DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
;;   ;;                                      GET_DIPOLETILT_DATA=get_dipoleTilt_data, $
;;   ;;                                      ;; STITCH_FILES=stitch_files, $
;;   ;;                                      ORIG_ROUTINENAME=orig_routineName, $
;;   ;;                                      COORDFILE=GEO_MAG_filename, $
;;   ;;                                      GEI_COORD_FILENAME=GEI_coord_filename, $
;;   ;;                                      DPTILT_FILENAME=dpTilt_filename, $
;;   ;;                                      COORDDIR=coordDir, $
;;   ;;                                      TIMEFILE=timeFile, $
;;   ;;                                      SAVE_GEI_COORDS=save_GEI_coords, $
;;   ;;                                      ;; EPHEMFILEINDARR=ephemFileIndArr, $
;;   ;;                                      ;; TMPFILE=tmpFile, $
;;   ;;                                      ;; OUTFILE=outFile, $ ;;for AACGM
;;   ;;                                      R_E=R_E, $
;;   ;;                                      ALTITUDE_MAX=altitude_max, $
;;   ;;                                      ALLOW_FL_TRACE=allow_FL_trace, $
;;   ;;                                      CHECK_IF_EXISTS=check_if_exists, $
;;   ;;                                      ;; CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
;;   ;;                                      ;; NOTALTITUDE_SUFF=notAltitude_suff, $
;;   ;;                                      CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varNames_and_resave_outFiles, $
;;   ;;                                      FORCE_NEWCHECKITVL=force_newCheckItvl, $
;;   ;;                                      USER__RESTRICT_II=user__restrict_i, $
;;   ;;                                      IN_NAMES=in_names, $
;;   ;;                                      DEFNAMES=defNames
;; 
