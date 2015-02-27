;02/26/2015
; This pro is meant to define an "Alfven event" object that allows quick access to the data products
; associated with a given Alfven event. This pretty much amounts to getters and plotters for B-field,
; E-field, Poynting flux estimates, and whatever other sorts of data we might be interested in. 
; The data products needed to get B-field data should simply be 

function alfEvent::init,eventNum,orbNum,maximus

  ;; init pointers for B-field and E-field data based on orbit number
  self.BField    = ptr_new(/allocate)  
  self.EField    = ptr_new(/allocate)  
  self.tSeries_B = ptr_new(/allocate)  
  self.PoyntEst  = ptr_new(/allocate)  

  ;; data is

  return,1
end


;********
;GETTERS
;********

function alfEvent::get_BField,BFout

  if n_elements(*(self.BField)) ne 0 then BFout=*(self.BField)

  return,BFout

end

function alfEvent::get_EField,EFout

  if n_elements(*(self.EField)) ne 0 then EFout=*(self.EField)

  return,EFout

end

function alfEvent::get_tSeries_B,tSBout

  if n_elements(*(self.tSeries_B)) ne 0 then tSBout=*(self.tSeries_B)

  return,tSBout

end


function alfEvent::get_PoyntEst,PEout

  if n_elements(*(self.PoyntEst)) ne 0 then PEout=*(self.PoyntEst)

  return,PEout

end

;********
;PLOTTERS
;********

pro alfEvent::plot_BField

  IF n_elements(*(self.BField)) NE 0 AND n_elements(*(self.tSeries_B)) NE 0 THEN BEGIN
     cgPlot,(self.tSeries_B-self.tSeries_B[0]),self.BField,TITLE="Time since" + time_to_str(tSeries_B[0])
  ENDIF ELSE print,"No BField data available! How bizarre..."

  RETURN

end

pro alfEvent::plot_EField

  IF n_elements(*(self.EField)) NE 0 AND n_elements(*(self.tSeries_B)) NE 0 THEN BEGIN
     cgPlot,(self.tSeries_B-self.tSeries_B[0]),self.EField,TITLE="Time since" + time_to_str(tSeries_B[0])
  ENDIF ELSE print,"No EField data available! How bizarre..."

  RETURN

end

pro alfEvent::plot_PoyntEst

  IF n_elements(*(self.PoyntEst)) NE 0 AND n_elements(*(self.tSeries_B)) NE 0 THEN BEGIN
     cgPlot,(self.tSeries_B-self.tSeries_B[0]),self.PoyntEst,TITLE="Time since" + time_to_str(tSeries_B[0])
  ENDIF ELSE print,"No PoyntEst data available! How bizarre..."

  RETURN

end

;**********
;Definisjon
;**********

pro alfEvent__define
 
  void={alfEvent,eventNum:0.,orbit:0., $              ;Orbit number, event number
        start_t:"UNDEFINED",stop_t:"UNDEFINED", $     ;Start and stop time
        BField:ptr_new(),EField:ptr_new(), $          ;Fields
        tSeries_B:ptr_new(), $                        ;Time series of BField data
        PoyntEst:ptr_new()}                           ;Poynting flux estimate

  return 

end