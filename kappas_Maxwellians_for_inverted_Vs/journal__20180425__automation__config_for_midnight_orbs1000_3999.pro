;2018/04/25

minMMidnight20180425 = 19
maxMMidnight20180425 = 5

IF MLT GT minMMidnight20180425 OR MLT LT maxMMidnight20180425 THEN BEGIN

   IF orbit EQ 1579 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 1607 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 8E1
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1612 AND nToSkip EQ 0 THEN BEGIN
      
      dato = '1997-01-17/'
      add_parm_errors = 1

      spectra_average_interval = 2

      ;; debug__skip_to_this_time  = '1997-01-17/12:01:00'
      ;; debug__break_on_this_time = '1997-01-17/12:01:12.5'

      minElecEnergy        = 115. ;50 is too low; I tried it 
      use_electron_tBounds = 0

      use_peak_energy_bounds_for_moment_calc = 0
      peakE_bounds_indShift = [-2,0]

      ;; cAP_tRanges = dato + [['12:00:29.79','12:00:48.7'], $
      ;;                       ['12:01:22.75','12:01:29.073']]

      ;; Want stats from full kappa interval
      cAP_tRanges = dato + [['12:00:29.79','12:00:48.7'], $
                            ['12:00:55','12:01:29.073']]

      cAP__iu_pot_tids = dato + [['12:00:27.5','12:00:39.'], $
                                 ['12:00:40.0','12:00:49'], $
                                 ['12:01:09.0','12:01:13'], $
                                 ['12:01:18.5','12:01:30'], $
                                 ['12:01:32.0','12:01:47']]


      IF KEYWORD_SET(add_parm_errors) THEN BEGIN
         kSum__add_parm_errors_from_file      = 1
         kSum__add_parm_errors__nRolls        = 10000
         kSum__add_parm_errors__use_most_prob = 1
      ENDIF

      energy_electrons[0] = minElecEnergy

      IF use_electron_tBounds THEN BEGIN

         ;; edgeries            = [300.,90.,40.,65.]
         edgeries            = [300.,90.,125.,115.]

         energy_electrons    = [[edgeries[0],energy_electrons[1]], $
                                [edgeries[1],energy_electrons[1]], $
                                [edgeries[2],energy_electrons[1]], $
                                [edgeries[3],energy_electrons[1]]]

         moment_energyArr    = [[energy_electrons[*,2]],[energy_electrons[*,2]],[10,2.4e4]]

         energy_electron_tBounds = dato + [['12:00:25','12:00:45'], $ ;lb is 300
                                           ['12:00:45','12:01:15'], $ ;lb is 90
                                           ['12:01:15','12:01:17'], $ ;lb is 40
                                           ['12:01:17','12:01:50']]   ;lb is 65

         min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                    energy  : edgeries, $
                                    forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

      ENDIF ELSE BEGIN
         moment_energyArr    = [[energy_electrons],[energy_electrons],[10,2.4e4]]
      ENDELSE


      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,7E0]
      max_peak_energyArr  = [1E4,2e4,1.0E3]

      IF KEYWORD_SET(use_peak_energy_bounds_for_moment_calc) THEN BEGIN
         use_peakE_bounds_for_moment_calc = [1,0,0]
      ENDIF

   ENDIF

   IF orbit EQ 1662 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1664 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1694 AND nToSkip EQ 0 THEN BEGIN
      
      ;;    isMajic             = 2E2
      ;;    energy_electrons[0] = isMajic
      ;;    min_peak_energy     = isMajic
      ;;    min_peak_energyArr  = [isMajic,isMajic,isMajic]

      minElecEnergy       = 2E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
      max_peak_energyArr  = [2e4,2e4,4E3]

      cAP__iu_pot_tids    = '1997-01-25/' + [['02:02:00','02:02:27.5'], $
                                             ['02:02:58','02:03:33']]

      ;; use_peakE_bounds_for_moment_calc = [1,0,0]
      ;; peakE_bounds_indShift            = [-2,0]

      cAP_tRanges         = cAP__iu_pot_tids

      spectra_average_interval = 4

   ENDIF

   IF orbit EQ 1696 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1697 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 9E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 1719 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1719 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1720 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 6E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1727 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 6E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1728 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1731 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1734 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1743 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1744 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1747 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1750 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1753 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1754 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1756 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 6E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1758 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1760 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1775 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1776 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1797 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1801 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 2E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1805 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1829 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1835 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 6E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,5E1]
   ENDIF

   IF orbit EQ 1849 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,5E1]
   ENDIF

   IF orbit EQ 1861 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 2E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1869 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1870 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,5E1]
   ENDIF

   IF orbit EQ 1875 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,5E1]
   ENDIF

   IF orbit EQ 1906 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1909 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 1944 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1945 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy = KEYWORD_SET(majicInterval) ? 5E2 : 2E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 1947 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 3353 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 4E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2006 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2008 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 2E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2009 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 2030 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 2082 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 2045 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 2072 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 2078 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 1.2E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 2082 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 2685 AND nToSkip EQ 0 THEN BEGIN

      minElecEnergy       = 80.
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,700]

      spectra_average_interval = 1

   ENDIF

   IF orbit EQ 2891 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2891 AND nToSkip EQ 1 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2892 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 2968 AND nToSkip EQ 0 THEN BEGIN

      minElecEnergy        = 1.1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,700]
      max_peak_energyArr  = [2e4,2e4,4E3]

      cAP__iu_pot_tids    = '1997-05-22/' + [['22:39:30','22:40:05'], $
                                             ['22:40:15','22:40:26'], $
                                             ['22:40:32','22:40:45']]
   ENDIF

   IF orbit EQ 3061 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 300.
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      ;; min_peak_energyArr  = [minElecEnergy,1E2,700]
      ;; max_peak_energyArr  = [2e4,2e4,4E3]

   ENDIF
   
   IF orbit EQ 3075 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 3086 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 3099 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 2E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   ;; Second interval, 'round 04:57
   IF orbit EQ 3166 AND nToSkip EQ 1 THEN BEGIN
      ;; minElecEnergy       = 80
      ;; energy_electrons[0] = minElecEnergy
      ;; min_peak_energy     = minElecEnergy
      ;; min_peak_energyArr  = [minElecEnergy,1E2,700]

      edgeries         = [70,100,300]

      energy_electrons = [[edgeries[0],energy_electrons[1]], $
                          [edgeries[1],energy_electrons[1]], $
                          [edgeries[2],energy_electrons[1]]]

      moment_energyArr = [[energy_electrons[*,2]],[energy_electrons[*,2]],[10,2.4e4]]

      dato                    = '1997-06-10/'
      energy_electron_tBounds = dato + [['04:57:20'  ,'04:57:57.5'], $ ;lb is 70
                                        ['04:57:57.5','04:58:30'], $   ;lb is 100
                                        ['04:58:30','04:59:20']]       ;lb is 300
      min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                 energy  : edgeries, $
                                 forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

   ENDIF

   IF orbit EQ 3157 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 800
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,700]
   ENDIF

   IF orbit EQ 3167 AND nToSkip EQ 0 THEN BEGIN

      minElecEnergy       = 300
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,700]

      spectra_average_interval = 6

   ENDIF

   IF orbit EQ 3368 AND nToSkip EQ 0 THEN BEGIN

      minElecEnergy       = 200.
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,700]

      spectra_average_interval = 4

   ENDIF

   IF orbit EQ 3218 AND nToSkip EQ 0 THEN BEGIN

      minElecEnergy       = 70.
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
      ;; max_peak_energyArr  = [2e4,2e4,4E3]

   ENDIF

   IF orbit EQ 3231 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 6E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]
   ENDIF

   IF orbit EQ 3353 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 5E2
      energy_electrons[0] = minElecEnergy
      ;; min_peak_energy     = minElecEnergy
      ;; min_peak_energyArr  = [minElecEnergy,1E2,1E2]

      edgeries            = [500.,150.]

      dato = '1997-06-27/'

      energy_electrons    = [[edgeries[0],energy_electrons[1]], $
                             [edgeries[1],energy_electrons[1]]]

      moment_energyArr    = [[energy_electrons],[energy_electrons],[10,2.4e4]]

      energy_electron_tBounds = dato + [['12:50:30','12:52:05'], $ ;lb is 500
                                        ['12:52:05','12:53:00']]   ;lb is 150.

      min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                 energy  : edgeries, $
                                 forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

   ENDIF

   IF orbit EQ 3369 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 3725 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 7E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

   IF orbit EQ 3759 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 3E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 3760 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 80.
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

      tmpDate             = '1997-08-04/'
      cAP_tRanges = tmpDate + [['03:49:50','03:50:02'], $
                               ['03:50:38','03:50:49']]

   ENDIF

   IF orbit EQ 3781 AND nToSkip EQ 0 THEN BEGIN
      
      minElecEnergy       = 8E2
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,100]

   ENDIF

   IF orbit EQ 3918 AND nToSkip EQ 0 THEN BEGIN
      minElecEnergy       = 1E3
      energy_electrons[0] = minElecEnergy
      min_peak_energy     = minElecEnergy
      min_peak_energyArr  = [minElecEnergy,1E2,1E2]
   ENDIF

ENDIF

minMMidnight20180425 = !NULL
maxMMidnight20180425 = !NULL
