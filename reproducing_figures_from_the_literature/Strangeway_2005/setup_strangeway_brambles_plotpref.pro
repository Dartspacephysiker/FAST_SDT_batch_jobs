;;09/29/16
FUNCTION SETUP_STRANGEWAY_BRAMBLES_PLOTPREF, $
   USE_EFIELD_FIT_VARIABLES=use_eField_fit_variables, $
   ONLY_FASTSRVY_DATA=only_128Ss_data, $
   INCLUDE_E_NEAR_B=include_E_near_B, $
   FULL_PFLUX_CALC=full_pFlux, $
   FIELDS_INTERP=do_fields_interp, $
   FIELDS_SPLINE=do_fields_spline

  COMPILE_OPT IDL2


  plotPref = ''

  IF KEYWORD_SET(include_E_near_B) THEN BEGIN

     plotPref      += '--also_E_near_B'

  ENDIF

  IF KEYWORD_SET(full_pFlux) THEN BEGIN

     plotPref      += '--full_pFlux'

  ENDIF

  CASE 1 OF
     KEYWORD_SET(use_eField_fit_variables): BEGIN
        plotPref    += '--eFieldFit'
     END
     ELSE: BEGIN
     END
  ENDCASE

  IF KEYWORD_SET(only_128Ss_data) THEN BEGIN

     plotPref       += '--128Ss_only'

  ENDIF

  RETURN,plotPref

  IF KEYWORD_SET(do_fields_interp) THEN BEGIN
     plotPref      += '--fields_interp'
  ENDIF

  IF KEYWORD_SET(do_fields_spline) THEN BEGIN
     plotPref      += '--fields_spline'
  ENDIF


END
