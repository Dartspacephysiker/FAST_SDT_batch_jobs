;2019/01/17
PRO JOURNAL__20190117__WHEN_WAS_FAST_SKYHIGH_AND_ON_DAYSIDE

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; LOAD_FASTLOC_AND_FASTLOC_TIMES,fastLoc,fastloc_times,/NO_MEMORY_LOAD
  LOAD_FASTLOC_AND_FASTLOC_TIMES,fastLoc,fastloc_times, $
                                 FOR_ESPEC_DBS=for_eSpec_DBs, $
                                 FOR_ESPEC__GIGANTE=for_eSpec__gigante, $
                                 /NO_MEMORY_LOAD

  minMLT = 9
  maxMLT = 17

  ;; dayside = 1

  minILAT = 60
  maxILAT = 90

  hemi = 'NORTH'

  IF hemi EQ 'SOUTH' THEN BEGIN

     tmp = minILAT
     minILAT = (-1.) * maxILAT
     maxILAT = (-1.) * TEMPORARY(tmp)

  ENDIF

  altRange = [3500,4200]

  mlt_i  = GET_MLT_INDS(fastLoc, $
                        minMLT, $
                        maxMLT, $
                        DAYSIDE=dayside, $
                        NIGHTSIDE=nightside, $
                        N_MLT=n_mlt, $
                        N_OUTSIDE_MLT=n_outside_MLT, $
                        USE_LNG=use_Lng, $
                        LUN=lun)

  ilat_i    = GET_ILAT_INDS(fastLoc, $
                            minILAT, $
                            maxILAT, $
                            hemi, $
                            N_ILAT=n_ilat, $
                            N_NOT_ILAT=n_not_ilat, $
                            LUN=lun)
  region_i  = CGSETINTERSECTION(ilat_i,mlt_i)


  alt_i     = GET_ALTITUDE_INDS( $
              fastLoc, $
              altRange[0], $
              altRange[1],LUN=lun)


  region_i          = CGSETINTERSECTION(region_i,alt_i)

  uniqOrbs_ii = UNIQ(fastLoc.orbit[region_i],SORT(fastLoc.orbit[region_i]))

  uniqOrbs = fastLoc.orbit[region_i[uniqOrbs_ii]]

  nUniq = N_ELEMENTS(uniqOrbs)
  PRINT,FORMAT='(A5,TR2,A8,TR2,A23,TR2,A5,TR2,A8,TR2,A8,TR2,A8)', $
        "#", $
        "Orbit", $
        "Time", $
        "NPunkt", $
        "MLT", $
        "ILAT", $
        "ALT"
  FOR k=0,nUniq-1 DO BEGIN
     ind = region_i[uniqOrbs_ii[k]]

     nPunkt = N_ELEMENTS(WHERE(fastLoc.orbit[region_i] EQ fastLoc.orbit[ind]))

     PRINT,FORMAT='(I5,TR2,I8,TR2,A23,TR2,I5,TR2,F8.2,TR2,F8.2,TR2,F8.2)', $
           k, $
           fastLoc.orbit[ind], $
           fastLoc.time[ind], $
           nPunkt, $
           fastLoc.mlt[ind], $
           fastLoc.ilat[ind], $
           fastLoc.alt[ind]

  ENDFOR

  STOP


END
