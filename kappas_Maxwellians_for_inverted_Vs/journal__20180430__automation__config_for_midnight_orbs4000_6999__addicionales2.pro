;2018/04/30

minMMidnight20180430 = 19
maxMMidnight20180430 = 5

IF MLT GT minMMidnight20180430 OR MLT LT maxMMidnight20180430 THEN BEGIN

   IF orbit EQ 5773 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 600
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5686 and nToSkip EQ 2 THEN BEGIN
      minElecEnergy = 300
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5593 NO REDO
   IF orbit EQ 6787 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 900
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5560 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5552 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5824 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5647 NO REDO
;; 5484 NO REDO
   IF orbit EQ 4273 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6982 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 700
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5711 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 800
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5634 NO REDO
   IF orbit EQ 5589 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 100
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5513 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5709 NO REDO
   IF orbit EQ 5643 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 600
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5625 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 130
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5515 NO REDO
   IF orbit EQ 5906 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 400
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5932 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

;; 5722 NOREDO
;; 6971 NOREDO
;; 5717 NOREDO
   IF orbit EQ 5440 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 600
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5848 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 600
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5481 and nToSkip EQ 1 THEN BEGIN
      minElecEnergy = 900
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5780 THEN BEGIN
      
      use_electron_tBounds = 1
      IF use_electron_tBounds THEN BEGIN


         dato = '1998-02-06/'
         edgeries = [100,300,100]

         energy_electrons    = [[edgeries[0],energy_electrons[1]], $
                                [edgeries[1],energy_electrons[1]], $
                                [edgeries[2],energy_electrons[1]]]

         moment_energyArr    = [[energy_electrons[*,2]],[energy_electrons[*,2]],[10,2.4e4]]

         energy_electron_tBounds = dato +  [['18:08:35','18:08:50'], $
                                            ['18:08:50','18:09:43'], $
                                            ['18:09:43','18:10:00']]  


         min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                    energy  : edgeries, $
                                    forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

      ENDIF

   ENDIF
   
   IF orbit EQ 5417 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6778 and nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 600
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6712 and nToSkip EQ 1 THEN BEGIN
      minElecEnergy = 900
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF
   
ENDIF

minMMidnight20180430 = !NULL
maxMMidnight20180430 = !NULL
