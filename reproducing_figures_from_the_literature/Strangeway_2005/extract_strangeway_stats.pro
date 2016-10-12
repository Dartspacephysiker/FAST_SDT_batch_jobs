;;09/24/16
FUNCTION EXTRACT_STRANGEWAY_STATS, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night

  COMPILE_OPT IDL2

  defStat = 2 ;average

  CASE 1 OF
     KEYWORD_SET(averages): BEGIN
        stat   = 2
     END
     KEYWORD_SET(integrals): BEGIN
        stat   = 3
     END
     ELSE: BEGIN
        stat   = defStat
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(day): BEGIN
        side   = 1
     END
     KEYWORD_SET(night): BEGIN
        side   = 2
     END
     ELSE: BEGIN
        side   = 0
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(north): BEGIN
        hemi   = 1
     END
     KEYWORD_SET(south): BEGIN
        hemi   = 2
     END
     ELSE: BEGIN
        hemi   = 0 ;both
     END
  ENDCASE



  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/'
  hashFile     = 'Strangeway_et_al_2005__DC_params.sav'

  IF FILE_TEST(outDir+hashFile) THEN BEGIN
     PRINT,"Restoring hash file ..."
     RESTORE,outDir+hashFile

     ;; CASE (WHERE((swHash.Keys()).ToArray() EQ orbit))[0] OF
     ;;    -1: BEGIN
     ;;    END
     ;;    ELSE: BEGIN
     ;;    END
     ;; ENDCASE

  ENDIF ELSE BEGIN
     PRINT,'Nothing here! Returning ...'
     RETURN,-1
  ENDELSE

  maxNElems    = 1e6

  orbArr       = MAKE_ARRAY(N_ELEMENTS(swHash) ,/FLOAT,VALUE=0.) 
  eAlongVArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  dB_perpArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  pFAlongBArr  = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)
  jeArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      
  jeeArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)     
  jiArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      

  nCount       = 0
  orbCnt       = 0
  FOREACH value, swHash, key DO BEGIN

     tmpThing  = ((value.(stat)).(hemi)).(side)

     nHere     = N_ELEMENTS(tmpThing.eAlongV)

     IF nHere EQ 0 THEN CONTINUE

     curInds   = [nCount:nCount+nHere-1]


    orbArr      [orbCnt ] = key
    eAlongVArr  [curInds] = tmpThing.eAlongV   
    dB_perpArr  [curInds] = tmpThing.dB_perp   
    pFAlongBArr [curInds] = tmpThing.pFAlongB  
    jeArr       [curInds] = tmpThing.je        
    jeeArr      [curInds] = tmpThing.jee       
    jiArr       [curInds] = tmpThing.ji        

    nCount    += nHere
    orbCnt++
  ENDFOREACH

  finStruct = {orbit     : orbArr                   , $
               eAlongV   : eAlongVArr  [0:nCount-1] , $  
               dB_perp   : dB_perpArr  [0:nCount-1] , $  
               pFAlongB  : pFAlongBArr [0:nCount-1] , $ 
               je        : jeArr       [0:nCount-1] , $       
               jee       : jeeArr      [0:nCount-1] , $      
               ji        : jiArr       [0:nCount-1]}


  that = PLOT(finStruct.pfalongb,finStruct.ji, $
              XLOG=1, $
              YLOG=1, $
              LINESTYLE='', $
              SYMBOL='o', $
              /SYM_FILLED, $
              XRANGE=[1e-1,1e2], $
              YRANGE=[1e6,1e10])

  RETURN,finStruct

END
