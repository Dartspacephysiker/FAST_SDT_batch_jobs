;;06/11/16
;;This'll make a new mapRatio DB for us
PRO JOURNAL__20160611__MAKE_NEW_MAPRATIO_DB

  COMPILE_OPT IDL2
  
  minOrb = 500
  maxOrb = 16361

  TIC

  FOR orbit_num=minOrb,maxOrb DO BEGIN
     clock = TIC("orb_"+STRCOMPRESS(orbit_num,/REMOVE_ALL))
     GET_MAPPED_BFIELDVALS_FOR_ALFVENDB_TIMES__UCLA_DESPIN__201605_UPDATES,orbit_num  
     TOC,clock
  ENDFOR
  TOC

END
