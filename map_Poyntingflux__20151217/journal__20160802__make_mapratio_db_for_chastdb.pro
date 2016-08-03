;;08/02/16
;;In order to recreate a figure
PRO JOURNAL__20160802__MAKE_MAPRATIO_DB_FOR_CHASTDB

  COMPILE_OPT IDL2
  
  outDir    = '/SPENCEdata/Research/database/FAST/dartdb/saves/mapratio_dbs/'
  outFile   = 'mapratio_for_chastDB--20160802.dat'

  LOAD_MAXIMUS_AND_CDBTIME,maximus,cdbTime,/DO_CHASTDB

  GET_FA_ORBIT,cdbTime,/TIME_ARRAY,/ALL
  get_data,'ILAT',DATA=ilat
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
                times: cdbTime, $
                orbit: maximus.orbit}

  PRINT,'Saving ' + outFile + ' ...'
  SAVE,mapRatio,FILENAME=outDir+outFile

END

