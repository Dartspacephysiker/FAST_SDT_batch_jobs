;;12/05/16
;;Looping makes it much faster
PRO JOURNAL__20170102__MAKE_MAPRATIO_DB_FOR_ESPEC_GIGANTE

  COMPILE_OPT IDL2

  @common__newell_espec.pro

  outDir    = '/SPENCEdata/Research/database/FAST/dartdb/saves/mapratio_dbs/'
  outFile   = 'mapratio_for_eSpec_gigante--20170102--500-23999--Je_times.sav'

  ;; LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime,/DO_CHASTDB
  ;; LOAD_FASTLOC_AND_FASTLOC_TIMES,DB_TFILE=db_tFile,/FOR_ESPEC_DBS
  LOAD_NEWELL_ESPEC_DB,/GIGANTE,/DONT_MAP_TO_100KM,/REDUCED_DB

  ;; FASTLOC_E__delta_t = !NULL
  ;; fastLoc_info = (TEMPORARY(FL_eSpec__fastLoc)).info

  PRINT,'Getting mapRatios for fastLoc_intervals5 DB ...'

  info  = NEWELL__eSpec.info
  times = (TEMPORARY(NEWELL__eSpec)).x
  nTot  = N_ELEMENTS(times)
  nIter = nTot/1e6

  mapRatio = { mag1  : FLTARR(nTot), $
               mag2  : FLTARR(nTot), $
               ratio : FLTARR(nTot), $
               info  : info}

  FOR k=0,nIter DO BEGIN
     
     ;;Set up the indices
     tmpInds = [(k*1e6):(((k*1e6)+999999) < (nTot-1))]

     PRINT,FORMAT='("Inds ",I0," through ",I0)',tmpInds[0],tmpInds[-1]

     ;;Get 'em
     GET_FA_ORBIT,times[tmpInds],/TIME_ARRAY,/ALL
     GET_DATA,'ILAT',DATA=ilat
     ;; GET_DATA,'ORBIT',data=ilat


     ;;Scale electron energy flux to 100km, pos flux earthward
     GET_DATA,'B_model',DATA=bMod
     GET_DATA,'BFOOT',DATA=bFoot

     mag1      = (bMod.y[*,0]*bMod.y[*,0]+ $
                  bMod.y[*,1]*bMod.y[*,1]+ $
                  bMod.y[*,2]*bMod.y[*,2])^0.5
     mag2      = (bFoot.y[*,0]*bFoot.y[*,0]+ $
                  bFoot.y[*,1]*bFoot.y[*,1]+ $
                  bFoot.y[*,2]*bFoot.y[*,2])^0.5
     ratio     = mag2/mag1


     mapRatio.mag1[tmpInds]  = FLOAT(mag1 )
     mapRatio.mag2[tmpInds]  = FLOAT(mag2 )
     mapRatio.ratio[tmpInds] = FLOAT(ratio)

     ;; PRINT,"Saving thesens: " + outFile+'_tmp'+STRCOMPRESS(k,/REMOVE_ALL)
     ;; SAVE,mapRatio,FILENAME=outDir+outFile+'_tmp'+STRCOMPRESS(k,/REMOVE_ALL)

  ENDFOR

  PRINT,'Saving ' + outFile + ' ...'
  SAVE,mapRatio,FILENAME=outDir+outFile

END


