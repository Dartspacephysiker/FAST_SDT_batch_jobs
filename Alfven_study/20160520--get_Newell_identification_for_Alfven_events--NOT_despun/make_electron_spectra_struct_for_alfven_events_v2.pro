;2016/05/20
;Explanation of variable names
;interval       : If the electron spectrum time falls between the recored alfven wave start and stop times, matched[i] = interval num. If we just
;                   had to associate the nearest electron spectrum (in time) with an Alfven event, matched[i] = 255-interval-1.
;                   The special value 255 means there was no match, i.e., this event is an orphan.
;nhits          : The number of times each alfven wave event was matched with an electron spectrum. If nHits is greater than one, that
;                   means the event got matched twice!
;hit_nSpectra   : For each hit, the number of spectra associated with it is given. So, TOTAL(alf_elec_spectra.hit_nSpectra) is nSpectra
FUNCTION MAKE_ELECTRON_SPECTRA_STRUCT_FOR_ALFVEN_EVENTS_V2,orbit,nSpectra,alfvenDBindices_for_spectra,alf_times,matched,nHits,hit_nSpectra,alf_spectra

  ;; events = { x:[events.x,tempEvent.x], $
  ;;            mono:[events.mono,tempEvent.mono], $
  ;;            broad:[events.broad,tempEvent.broad], $
  ;;            diffuse:[events.diffuse,tempEvent.diffuse], $
  ;;            Je:[events.Je,tempEvent.Je], $
  ;;            Jee:[events.Jee,tempEvent.Jee], $
  ;;            time_i:[events.time_i,tempEvent.time_i], $
  ;;            Ji:[events.Ji,tempEvent.Ji], $
  ;;            Jei:[events.Jei,tempEvent.Jei], $
  ;;            nBad_iSpec:[events.nBad_eSpec,tempEvent.nBad_eSpec], $
  ;;            nBad_eSpec:[events.nBad_iSpec,tempEvent.nBad_iSpec]}

  alf_eSpec = {orbit:orbit, $
               time:alf_times, $
               interval:matched, $  
               nHits:nHits, $
               hit_nSpectra:hit_nSpectra, $
               alf_indices:alfvenDBindices_for_spectra, $
               ;; structs:MAKE_BLANK_ESPEC_STRUCTS(nSpectra), $
               ident:alf_spectra} ;, $
               ;; could_be_fraud:MAKE_ARRAY(nSpectra,/BYTE,VALUE=0)}

  RETURN,alf_eSpec

END