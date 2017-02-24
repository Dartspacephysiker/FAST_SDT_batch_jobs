;;2017/02/24
PRO SUMPLOTS_AND_B_PLUS_J__GET_FILENAME, $
   ORBIT=orbit, $
   EEB_OR_EES=eeb_or_ees, $
   IEB_OR_IES=ieb_or_ies, $
   SKIP_DESPIN=skip_despin, $
   PLOTPREF=plotPref, $
   SAVESUFF=saveSuff, $
   BONUSSUFF=bonusSuff, $
   ANCILLARY_PLOTS=ancillary_plots, $
   OUTPLOTNAME=outPlotName, $
   SAVEFILE=saveFile
   
  COMPILE_OPT IDL2

  IF ~KEYWORD_SET(eeb_or_ees) THEN BEGIN
     eeb_or_ees     = 'eeb'
  ENDIF
  IF ~KEYWORD_SET(ieb_or_ies) THEN BEGIN
     ieb_or_ies     = 'ieb'
  ENDIF

  despunStr         = KEYWORD_SET(skip_despin) ? '-no_B_despin' : ''

  IF ~KEYWORD_SET(plotPref) THEN plotPref = ''
  IF ~KEYWORD_SET(saveSuff) THEN saveSuff = ''

  orbStr            = STRCOMPRESS(orbit,/REMOVE_ALL)
  ee_ie_string      = '-' + eeb_or_ees + '-' + ieb_or_ies

  outPlotName       = 'Orb_' + orbStr + plotPref + ee_ie_string + despunStr
  saveFile          = 'Orbit_' + orbStr + '-B_and_J-' + $
                      GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + $
                      ee_ie_string + despunStr + $
                      saveSuff + '.sav'


  IF N_ELEMENTS(ancillary_plots) EQ 0 THEN ancillary_plots = 1
  IF KEYWORD_SET(ancillary_plots) THEN BEGIN
     outPlotName   += '-with_ancillaries'
  ENDIF

  ;;Alternative
  IF KEYWORD_SET(bonusSuff) THEN BEGIN
     outPlotName      += bonusSuff
     saveFile         += bonusSuff
  ENDIF

END
