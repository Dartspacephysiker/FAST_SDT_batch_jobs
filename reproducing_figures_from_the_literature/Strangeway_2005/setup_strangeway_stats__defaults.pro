;;09/28/16
FUNCTION SETUP_STRANGEWAY_STATS__DEFAULTS, $
   AVERAGES=averages, $
   INTEGRALS=integrals, $
   NORTH=north, $
   SOUTH=south, $
   DAY=day, $
   NIGHT=night, $
   MINMLT=minMLT, $
   MAXMLT=maxMLT

  COMPILE_OPT IDL2

  defStat = 2                   ;average
  defSStr = 'Avg'

  CASE 1 OF
     KEYWORD_SET(averages): BEGIN
        stat    = 2
        statStr = 'Avg'
     END
     KEYWORD_SET(integrals): BEGIN
        stat    = 3
        statStr = 'Integ'
     END
     ELSE: BEGIN
        stat    = defStat
        statStr = defSStr 
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(day): BEGIN
        side    = 1
        sideStr = 'day'
     END
     KEYWORD_SET(night): BEGIN
        side    = 2
        sideStr = 'night'
     END
     ELSE: BEGIN

        CASE 1 OF
           (N_ELEMENTS(minMLT) GT 0 AND N_ELEMENTS(maxMLT) GT 0): BEGIN

              sideStr = STRING(FORMAT='("-",I02,"-",I02,"MLT")', $
                               minMLT, $
                               maxMLT)
              side = 3

           END
           N_ELEMENTS(minMLT) GT 0: BEGIN

              sideStr = STRING(FORMAT='("-",I02,"-",I02,"MLT")', $
                               minMLT, $
                               24)
              side = 3

           END
           N_ELEMENTS(maxMLT) GT 0: BEGIN

              sideStr = STRING(FORMAT='("-",I02,"-",I02,"MLT")', $
                               0, $
                               maxMLT)
              side = 3

           END
           ELSE: BEGIN
              side    = 0
              sideStr = 'day_n_night'
           END
        ENDCASE
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(north): BEGIN
        hemi   = 1
        hemStr = 'North'
     END
     KEYWORD_SET(south): BEGIN
        hemi   = 2
        hemStr = 'South'
     END
     ELSE: BEGIN
        hemi   = 0              ;both
        hemStr = 'Both'
        PRINT,"Both hemispheres ..."
     END
  ENDCASE


  RETURN,{stat:stat, $
          side:side, $
          hemi:hemi, $
          statStr:statStr, $
          sideStr:sideStr, $
          hemStr:hemStr}

END
