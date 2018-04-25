;2018/04/25

minMMidnight20180425 = 19
maxMMidnight20180425 = 5

IF MLT GT minMMidnight20180425 OR MLT LT maxMMidnight20180425 THEN BEGIN

   IF orbit EQ 4328 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 400
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5483 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5621 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 400
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF


   IF orbit EQ 5624 AND nToSkip EQ 0 THEN BEGIN
      
      dato = '1998-01-23/'

      minElecEnergy       = 60. ;50 is too low; I tried it 
      energy_electrons[0] = minElecEnergy

      edgeries            = [30.,150.]

      energy_electrons    = [[edgeries[0],energy_electrons[1]], $
                             [edgeries[1],energy_electrons[1]]]

      moment_energyArr    = [[energy_electrons],[energy_electrons],[10,2.4e4]]

      energy_electron_tBounds = dato + [['08:23:58','08:24:01.4'], $ ;lb is 30
                                        ['08:24:01.4','08:25:40']]   ;lb is 150.

      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E1]
      ;; max_peak_energyArr  = [1E4,2e4,1.0E3]

      min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                 energy  : edgeries, $
                                 forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

      use_peakE_bounds_for_moment_calc = [1,0,0]
      peakE_bounds_indShift = [-1,0]

      cAP__iu_pot_tids = dato + [['08:24:04','08:24:34.8'], $
                                 ['08:24:50','08:24:57.5'], $
                                 ['08:25:04.9','08:25:30']]
      ;; cAP_tRanges = dato + [['12:01:24.3','12:01:28.76'], $
      ;;                                ['12:01:33.1','12:01:35.7']]

      spectra_average_interval = 2
      cAP_tRanges = dato + [['08:24:04','08:24:34.8']]

   ENDIF

   IF orbit EQ 5627 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 200
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5640 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5701 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 900
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5805 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5816 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 800
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 5846 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6722 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6767 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6808 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6904 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6953 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 5000
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 6964 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 500
      energy_electrons[0] = minElecEnergy
      min_peak_energy = minElecEnergy
      min_peak_energyArr = [minElecEnergy,1E2,1E2]
   ENDIF

ENDIF

minMMidnight20180425 = !NULL
maxMMidnight20180425 = !NULL
