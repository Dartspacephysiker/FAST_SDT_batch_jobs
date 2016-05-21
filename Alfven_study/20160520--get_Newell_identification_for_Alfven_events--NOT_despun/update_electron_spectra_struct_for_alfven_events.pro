PRO UPDATE_ELECTRON_SPECTRA_STRUCT_FOR_ALFVEN_EVENTS,alf_eSpec,eSpecs_parsed,parsed_eSpec_inds,n_to_update,nSpectra_visited,could_be_fraud
  
  i_spectra = nSpectra_visited

  FOR k=0,n_to_update-1 DO BEGIN

     ;;temp inds
     alf_eSpec_i = i_spectra+k
     parsed_temp_i = parsed_eSpec_inds[k]

     ;;update dat
     ;; alf_eSpec.structs[alf_eSpec_i] = {x:eSpecs_parsed.x[parsed_temp_i], $
     ;;                                   mono:eSpecs_parsed.mono[parsed_temp_i], $
     ;;                                   broad:eSpecs_parsed.broad[parsed_temp_i], $
     ;;                                   diffuse:eSpecs_parsed.diffuse[parsed_temp_i], $
     ;;                                   Je:eSpecs_parsed.Je[parsed_temp_i], $
     ;;                                   Jee:eSpecs_parsed.Jee[parsed_temp_i], $
     ;;                                   nBad_eSpec:eSpecs_parsed.nBad_eSpec[parsed_temp_i]}
     alf_eSpec.structs.x[alf_eSpec_i]           = eSpecs_parsed.x[parsed_temp_i]
     alf_eSpec.structs.mono[alf_eSpec_i]        = eSpecs_parsed.mono[parsed_temp_i]
     alf_eSpec.structs.broad[alf_eSpec_i]       = eSpecs_parsed.broad[parsed_temp_i]
     alf_eSpec.structs.diffuse[alf_eSpec_i]     = eSpecs_parsed.diffuse[parsed_temp_i]
     alf_eSpec.Structs.Je[alf_eSpec_i]          = eSpecs_parsed.Je[parsed_temp_i]
     alf_eSpec.Structs.Jee[alf_eSpec_i]         = eSpecs_parsed.Jee[parsed_temp_i]
     alf_eSpec.structs.nBad_eSpec[alf_eSpec_i]  = eSpecs_parsed.nBad_eSpec[parsed_temp_i]

     IF N_ELEMENTS(could_be_fraud) GT 0 THEN alf_eSpec.could_be_fraud[alf_eSpec_i] = could_be_fraud[k]
  ENDFOR


END