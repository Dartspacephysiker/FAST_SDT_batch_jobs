PRO CAT_ALF_ESPEC,final_eSpec,final_ident_hash,alf_eSpec


  tempIdent       = alf_eSpec.ident
  
  tempIdent       = CREATE_STRUCT(tempIdent,'ALF_INDICES',alf_eSpec.alf_indices)     
  tempIdx         = alf_eSpec.alf_indices[UNIQ(alf_eSpec.alf_indices)]

  alf_eSpec       = {orbit:alf_eSpec.orbit, $
                     interval:alf_eSpec.interval, $
                     time:alf_eSpec.time, $
                     idx:tempIdx, $
                     nHits:alf_eSpec.nHits, $
                     hit_nSpectra:alf_eSpec.hit_nSpectra} ;, $
                     ;; ident_idx:0} ;, $
;                     ident:tempIdent}
  
  tempHash        = ORDEREDHASH(alf_eSpec.orbit,tempIdent)

  IF N_ELEMENTS(final_eSpec) EQ 0 THEN BEGIN
     final_eSpec  = alf_eSpec
     final_ident_hash = tempHash
  ENDIF ELSE BEGIN
     
     n_events     = N_ELEMENTS(alf_eSpec.time)
     final_eSpec  = {orbit:[final_eSpec.orbit,REPLICATE(alf_eSpec.orbit,n_events)], $
                     interval:[final_eSpec.interval,alf_eSpec.interval], $  
                     time:[final_eSpec.time,alf_eSpec.time], $
                     idx:[final_eSpec.idx,alf_eSpec.idx], $
                     nHits:[final_eSpec.nHits,alf_eSpec.nHits], $
                     hit_nSpectra:[final_eSpec.hit_nSpectra,alf_eSpec.hit_nSpectra]}
                     ;; ident:[final_eSpec.ident,alf_eSpec.ident]} ;, $
     final_ident_hash = final_ident_hash + tempHash

  ENDELSE  
END