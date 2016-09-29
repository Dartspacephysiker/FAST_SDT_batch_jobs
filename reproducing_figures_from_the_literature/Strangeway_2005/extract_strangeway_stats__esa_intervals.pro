;;09/24/16
FUNCTION EXTRACT_STRANGEWAY_STATS__ESA_INTERVALS, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   FOLD_INTERVALS=fold_intervals, $
   SAVE_PLOTS=save_plots, $
   PLOTDIR=plotDir, $
   PLOTS_PREFIX=plots_prefix, $
   SQUARE_WINDOW=square_window, $
   NO_PLOTS=no_plots

  COMPILE_OPT IDL2

  defs = SETUP_STRANGEWAY_STATS__DEFAULTS($
         AVERAGES=averages, $
         INTEGRALS=integrals, $
         NORTH=north, $
         SOUTH=south, $
         DAY=day, $
         NIGHT=night)

  outDir       = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Strangeway_2005/'
  hashFile     = 'Strangeway_et_al_2005__DC_params--ESA_intervals.sav'

  bonusSuff    = '--500eV_ions'

  hashFile    += bonusSuff

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
  maxNItvls    = 30S

  orbArr       = MAKE_ARRAY(maxNElems          ,/LONG ,VALUE=0.) 
  itvlArr      = MAKE_ARRAY(maxNElems          ,/INTEG,VALUE=0.) 
  eAlongVArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  dB_perpArr   = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.) 
  pFAlongBArr  = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)
  jeArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      
  jeeArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)     
  jiArr        = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)      
  dspArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)
  lenArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)
  nPtArr       = MAKE_ARRAY(maxNElems          ,/FLOAT,VALUE=0.)

  nCount       = 0
  orbCnt       = 0
  FOREACH value, swHash, key DO BEGIN

     nItvls    = N_ELEMENTS(value)

     ;; tmpOrb_i  = MAKE_ARRAY(2,maxNItvls,/LONG)

     IF nItvls EQ 0 THEN CONTINUE

     CASE KEYWORD_SET(fold_intervals) OF
        1: BEGIN

           CASE defs.stat OF
              2: BEGIN

                 ;;Get the number of points here
                 nItvl = MAKE_ARRAY(nItvls,/LONG)
                 FOR k=0,nItvls-1 DO nItvl[k] = ((value[k].(defs.stat)).(defs.hemi)).(defs.side).N

                 nThisOrb = FIX(TOTAL(nItvl))

                 IF nThisOrb NE 0 THEN BEGIN

                    ;;Make temporary arrays
                    tmp_eAlongV   = MAKE_ARRAY(nThisOrb,/FLOAT) 
                    tmp_dB_perp   = MAKE_ARRAY(nThisOrb,/FLOAT) 
                    tmp_pFAlongB  = MAKE_ARRAY(nThisOrb,/FLOAT)
                    tmp_je        = MAKE_ARRAY(nThisOrb,/FLOAT)      
                    tmp_jee       = MAKE_ARRAY(nThisOrb,/FLOAT)     
                    tmp_ji        = MAKE_ARRAY(nThisOrb,/FLOAT)      
                    tmp_dsp       = MAKE_ARRAY(nThisOrb,/FLOAT)     

                    tmptmp        = 0
                    FOR k=0,nItvls-1 DO BEGIN
                       tmpThing   = ((value[k].(defs.stat)).(defs.hemi)).(defs.side)

                       nHere      = N_ELEMENTS(tmpThing.eAlongV)

                       IF nHere GT 0 THEN BEGIN

                          curInds    = [tmptmp:tmptmp+nItvl[k]-1]

                          tmp_eAlongV  [curInds] = tmpThing.eAlongV 
                          tmp_dB_perp  [curInds] = tmpThing.dB_perp 
                          tmp_pFAlongB [curInds] = tmpThing.pFAlongB
                          tmp_je       [curInds] = tmpThing.je      
                          tmp_jee      [curInds] = tmpThing.jee     
                          tmp_ji       [curInds] = tmpThing.ji      
                          tmp_dsp      [curInds] = tmpThing.dsp     
                          
                          tmptmp += nItvl[k]
                       ENDIF

                    ENDFOR
                 
                    tmp_eAlongV  = TOTAL(tmp_eAlongV )/nThisOrb
                    tmp_dB_perp  = TOTAL(tmp_dB_perp )/nThisOrb
                    tmp_pFAlongB = TOTAL(tmp_pFAlongB)/nThisOrb
                    tmp_je       = TOTAL(tmp_je      )/nThisOrb
                    tmp_jee      = TOTAL(tmp_jee     )/nThisOrb
                    tmp_ji       = TOTAL(tmp_ji      )/nThisOrb
                    tmp_dsp      = TOTAL(tmp_dsp     )/nThisOrb
                 ENDIF ELSE BEGIN
                    tmp_eAlongV  = 0.0
                    tmp_dB_perp  = 0.0
                    tmp_pFAlongB = 0.0
                    tmp_je       = 0.0
                    tmp_jee      = 0.0
                    tmp_ji       = 0.0
                    tmp_dsp      = 0.0
                 ENDELSE

              END
              3: BEGIN

                 tmp_eAlongV   = 0. 
                 tmp_dB_perp   = 0. 
                 tmp_pFAlongB  = 0.
                 tmp_je        = 0.      
                 tmp_jee       = 0.     
                 tmp_ji        = 0.      
                 tmp_dsp       = 0.     

                 lenItvl       = MAKE_ARRAY(nItvls,/LONG)
                 FOR k=0,nItvls-1 DO BEGIN
                    lenItvl[k] = ((value[k].(defs.stat)).(defs.hemi)).(defs.side).len
                 ENDFOR

                 lenThisOrb    = TOTAL(lenItvl)

                 FOR k=0,nItvls-1 DO BEGIN
                    tmpThing   = ((value[k].(defs.stat)).(defs.hemi)).(defs.side)

                    nHere      = N_ELEMENTS(tmpThing.eAlongV)

                    IF nHere GT 0 THEN BEGIN

                       ;;"UN-integrate" these fellers
                       tmp_eAlongV   += ( tmpThing.eAlongV  * lenItvl[k] )
                       tmp_dB_perp   += ( tmpThing.dB_perp  * lenItvl[k] )
                       tmp_pFAlongB  += ( tmpThing.pFAlongB * lenItvl[k] )
                       tmp_je        += ( tmpThing.je       * lenItvl[k] )
                       tmp_jee       += ( tmpThing.jee      * lenItvl[k] )
                       tmp_ji        += ( tmpThing.ji       * lenItvl[k] )
                       tmp_dsp       += ( tmpThing.dsp      * lenItvl[k] )

                    ENDIF

                 ENDFOR

                 tmp_eAlongV   /= lenThisOrb
                 tmp_dB_perp   /= lenThisOrb
                 tmp_pFAlongB  /= lenThisOrb
                 tmp_je        /= lenThisOrb
                 tmp_jee       /= lenThisOrb
                 tmp_ji        /= lenThisOrb
                 tmp_dsp       /= lenThisOrb

              END
           ENDCASE

           orbArr      [orbCnt ] = key
           itvlArr     [orbCnt ] = TOTAL(INDGEN(nItvls)+1)
           eAlongVArr  [orbCnt ] = tmp_eAlongV   
           dB_perpArr  [orbCnt ] = tmp_dB_perp   
           pFAlongBArr [orbCnt ] = tmp_pFAlongB  
           jeArr       [orbCnt ] = tmp_je        
           jeeArr      [orbCnt ] = tmp_jee       
           jiArr       [orbCnt ] = tmp_ji        
           dspArr      [orbCnt ] = tmp_dsp

           nCount++

        END
        ELSE: BEGIN

           FOR k=0,nItvls-1 DO BEGIN

              tmpThing  = ((value[k].(defs.stat)).(defs.hemi)).(defs.side)

              nHere     = N_ELEMENTS(tmpThing.eAlongV)

              IF nHere EQ 0 THEN CONTINUE

              curInds   = [nCount:nCount+nHere-1]


              orbArr      [curInds ] = key
              itvlArr     [curInds] = k + 1
              eAlongVArr  [curInds] = tmpThing.eAlongV   
              dB_perpArr  [curInds] = tmpThing.dB_perp   
              pFAlongBArr [curInds] = tmpThing.pFAlongB  
              jeArr       [curInds] = tmpThing.je        
              jeeArr      [curInds] = tmpThing.jee       
              jiArr       [curInds] = tmpThing.ji        
              dspArr      [curInds] = tmpThing.dsp
              CASE defs.stat OF
                 2: BEGIN       ;averages
                    nPtarr[curInds] = tmpThing.N
                 END
                 3: BEGIN
                    lenArr[curInds] = tmpThing.len
                 END
                 ELSE: STOP
              ENDCASE

              nCount    += nHere

              ;; tmpOrb_i[*,k] = [curInds[0],curInds[-1]]

           ENDFOR

        END
     ENDCASE


     orbCnt++
  ENDFOREACH

  finStruct = {orbit     : orbArr      [0:nCount-1] , $  
               interval  : itvlArr     [0:nCount-1] , $  
               eAlongV   : eAlongVArr  [0:nCount-1] , $  
               dB_perp   : dB_perpArr  [0:nCount-1] , $  
               pFAlongB  : pFAlongBArr [0:nCount-1] , $ 
               je        : jeArr       [0:nCount-1] , $       
               jee       : jeeArr      [0:nCount-1] , $      
               ji        : jiArr       [0:nCount-1] , $
               dsp       : dspArr      [0:nCount-1] }

  sw_i = SORT(finStruct.orbit)
  
  finStruct   = {orbit     : finStruct.orbit    [sw_i], $
                 interval  : finStruct.interval [sw_i], $   
                 eAlongV   : finStruct.eAlongV  [sw_i], $   
                 dB_perp   : finStruct.dB_perp  [sw_i], $   
                 pFAlongB  : finStruct.pFAlongB [sw_i], $
                 je        : finStruct.je       [sw_i], $
                 jee       : finStruct.jee      [sw_i], $
                 ji        : (-1.)*finStruct.ji [sw_i], $
                 dsp       : finStruct.dsp      [sw_i]}

  IF ~KEYWORD_SET(no_plots) THEN BEGIN

     IF N_ELEMENTS(xQuants) EQ 0 THEN BEGIN
        xQuants = [4,5,6,8]
     ENDIF

     plotInfo  = {xQuants       : xQuants, $
                  xTitle        : ["", $
                                   "", $
                                   "$\Delta$B [DC] (nT)", $
                                   "E along V$_{sc}$ [DC] (mV/m)", $
                                   "Poynting Flux [DC] (mW/m^2)", $
                                   "Average Electron Flux (#/cm$^2$/s)", $
                                   "Average Electron Energy Flux (mW/m$^2$)", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "Average ELF amplitude (V/m)"], $
                  xRange        : [[0.,0.], $
                                   [0.,0.], $
                                   [0.,0.], $
                                   [0.,0.], $
                                   [1e-1,1e2], $
                                   [1e7,1e10], $
                                   [1e-2,1e0], $
                                   [1e6,1e10], $
                                   [1e-3,1e-1]], $
                  yTitle        : "Ion Flux (#/cm!U2!N/s)", $
                  yData         : finStruct.ji, $
                  yRange        : [1e6,1e10], $
                  plotNames     : ["", $
                                   "", $
                                   "$dB__vs__ionNumFlux", $
                                   "E_along_V__vs__ionNumFlux", $
                                   "DC_Poynting_flux__vs__ionNumFlux", $
                                   "eNumFlux__vs__ionNumFlux", $
                                   "eFlux__vs__ionNumFlux", $
                                   "Ion Flux (#/cm!U2!N/s)", $
                                   "ELF_amplitude__vs__ionNumFlux"], $
                  canonPref     : 'Strangeway_et_al_2005--', $
                  plotDirSuff   : '/Strangeway_2005', $
                  plots_prefix  : (KEYWORD_SET(bonusSuff) ? bonusSuff : '') + $ 
                                  defs.statStr+'--'+defs.sideStr+'--'+defs.hemStr+'--', $
                  verboten      : [0,1,2,3], $
                  navn_verboten : ["Orbit    (ind 0)", $
                                   "Interval (ind 1)", $
                                   "EalongV  (ind 2)", $                               
                                   "dB       (ind 3)"]}


     PLOT_STRANGEWAY_STATS, $
        finStruct, $
        PLOTINFO=plotInfo, $
        OUT_PLOTARR=plotArr, $
        SQUARE_WINDOW=square_window, $
        SAVE_PLOTS=save_plots, $
        PLOTDIR=plotDir

  ENDIF

  RETURN,finStruct

END

