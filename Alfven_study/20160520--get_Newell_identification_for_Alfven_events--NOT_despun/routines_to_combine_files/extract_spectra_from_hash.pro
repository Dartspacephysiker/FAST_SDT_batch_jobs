PRO EXTRACT_SPECTRA_LIST_FROM_HASH,final_ident_hash,orb_nums,extracted_spectra_list,alf_indices


  final_ident_hash[this[0:10]]

  ;; extracted_spectra_list = (final_ident_hash[orb_nums]).values()
  extracted_spectra_hash = final_ident_hash[orb_nums]

  
  ;; FOREACH struct,extracted_spectra_list, key DO BEGIN
  
  FOREACH struct,extracted_spectra_hash, key DO BEGIN
     

  ENDFOR

  

END