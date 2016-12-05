;;12/05/16
PRO JOURNAL__20161205__MAKE_MAPRATIO_DB_FOR_FASTLOC5__FOR_ESPEC

  COMPILE_OPT IDL2

  @common__fastloc_espec_vars.pro

  outDir    = '/SPENCEdata/Research/database/FAST/dartdb/saves/mapratio_dbs/'
  outFile   = 'mapratio_for_fastLoc_intervals5--20161129--500-16361--Je_times.sav'

  ;; LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime,/DO_CHASTDB
  LOAD_FASTLOC_AND_FASTLOC_TIMES,DB_TFILE=db_tFile,/FOR_ESPEC_DBS

  HELP,FASTLOC_E__delta_t
  fastLoc_info = (TEMPORARY(FL_eSpec__fastLoc)).info

  STR_ELEMENT,fastLoc_info,'DB_TFILE',db_tFile,/ADD_REPLACE

  PRINT,'Getting mapRatios for fastLoc_intervals4 DB ...'
  GET_FA_ORBIT,FASTLOC_E__times,/TIME_ARRAY,/ALL
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


  mapRatio  = { mag1: mag1, $
                mag2: mag2, $
                ratio: ratio, $
                info: fastLoc_info}
                ;; times: cdbTime, $
                ;; orbit: .orbit}

  PRINT,'Saving ' + outFile + ' ...'
  SAVE,mapRatio,FILENAME=outDir+outFile

END

