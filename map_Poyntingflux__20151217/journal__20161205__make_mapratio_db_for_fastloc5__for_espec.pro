;;12/05/16
;;Looping makes it much faster
PRO JOURNAL__20161205__MAKE_MAPRATIO_DB_FOR_FASTLOC5__FOR_ESPEC

  COMPILE_OPT IDL2

  @common__fastloc_espec_vars.pro

  outDir    = '/SPENCEdata/Research/database/FAST/dartdb/saves/mapratio_dbs/'
  outFile   = 'mapratio_for_fastLoc_intervals5--20161129--500-16361--Je_times.sav'

  ;; LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime,/DO_CHASTDB
  LOAD_FASTLOC_AND_FASTLOC_TIMES,DB_TFILE=db_tFile,/FOR_ESPEC_DBS

  HELP,FASTLOC_E__delta_t
  FASTLOC_E__delta_t = !NULL
  fastLoc_info = (TEMPORARY(FL_eSpec__fastLoc)).info

  STR_ELEMENT,fastLoc_info,'DB_TFILE',db_tFile,/ADD_REPLACE

  PRINT,'Getting mapRatios for fastLoc_intervals5 DB ...'

  nTot  = N_ELEMENTS(FASTLOC_E__times)
  nIter = nTot/1e6

  mapRatio = { mag1  : FLTARR(nTot), $
               mag2  : FLTARR(nTot), $
               ratio : FLTARR(nTot), $
               info  : fastLoc_info}

  FOR k=0,nIter DO BEGIN
     
     ;;Set up the indices
     tmpInds = [(k*1e6):(((k*1e6)+999999) < (nTot-1))]

     PRINT,FORMAT='("Inds ",I0," through ",I0)',tmpInds[0],tmpInds[-1]

     ;;Get 'em
     GET_FA_ORBIT,FASTLOC_E__times[tmpInds],/TIME_ARRAY,/ALL
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

