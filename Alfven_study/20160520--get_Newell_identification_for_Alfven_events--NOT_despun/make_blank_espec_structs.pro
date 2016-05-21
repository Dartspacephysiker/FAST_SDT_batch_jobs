;2016/05/21 Dad's birthday!!!
;Called by MAKE_ELECTRON_SPECTRA_STRUCT_FOR_ALFVEN_EVENTS as part of effort to figure out what these alfven events are
FUNCTION MAKE_BLANK_ESPEC_STRUCTS,nSpectra

  event = { x:MAKE_ARRAY(nSpectra,/DOUBLE), $
            mono:MAKE_ARRAY(nSpectra,/BYTE), $
            broad:MAKE_ARRAY(nSpectra,/BYTE), $
            diffuse:MAKE_ARRAY(nSpectra,/BYTE), $
            Je:MAKE_ARRAY(nSpectra,/FLOAT), $
            Jee:MAKE_ARRAY(nSpectra,/FLOAT), $
            nBad_eSpec:MAKE_ARRAY(nSpectra,/BYTE)}
  
    RETURN,event

END