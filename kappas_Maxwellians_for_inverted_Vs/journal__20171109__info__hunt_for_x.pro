;; JOURNAL__20171109__INFO_HUNT_FOR_X
;;10/11/16

orbs     = [1767, $
            1764, $
            1757]                    ;, $
            ;; 1849, $
            ;; 1773, $
            ;; 1789, $
            ;; 0000]

orbTimes = [ $
           ['97-01-31/20:08:30','97-01-31/20:10:10'], $
           ['97-01-31/13:30:30','97-01-31/13:31:40'], $
           ['97-01-30/21:55:50','97-01-30/21:56:43']]
           ;; ['97-02-02/21:01:56','97-02-02/21:02:14'] $ ;Checking out the total moment thing

orbBurstTimes = LIST( $
                ['97-02-07/20:49:30','97-02-07/20:50:10'],$
                ['97-01-31/13:30:43','97-01-31/13:31:10'], $
                ['97-01-30/21:55:58','97-01-30/21:56:30'] $
                    )

bonusPrefs = [ $
             '-hunter-1-1767', $
             '-hunter-1-1764', $
             '-hunter-1-1757']


kStats_startStops__ees = LIST( $
                         ;;Numbers 0 and 1
                         LIST('1997-01-31/' + [['20:08:30','20:10:10']]), $
                         LIST('1997-01-31/' + [['13:30:43','13:31:33']]), $
                         LIST('1997-01-30/' + [['21:55:58','21:56:30']]))

kStats_startStops__eeb = LIST( $
                         ;;Numbers 0 and 1
                         [0], $
                         LIST('1997-01-31/' + [['13:30:43','13:31:10']]), $
                         LIST('1997-01-30/' + [['21:56:05','21:56:21']]))


cAP_tRanges_list              = LIST($
                                ;;
                                ;;Number 0
                                '1997-01-31/' + $
                                ['20:49:50', $
                                 '20:50:11'], $
                                '1997-01-31/' + ['13:30:43','13:31:10'], $
                                '1997-01-30/' + ['21:56:05','21:56:21'])

eeb_or_ees__recommande        = LIST('ees', $
                                     'eeb', $
                                     'eeb')

energy_electrons__recommande  = LIST([5D2,3.0D4], $
                                     [6D2,3.0D4], $
                                     [6D2,3.0D4])
min_peak_energy_recommande    = LIST(500, $
                                     700, $
                                     600)

spectra_average_interval_list = LIST(eeb_or_ees__recommande[0] EQ 'eeb' ? 2 : 2, $
                                     eeb_or_ees__recommande[1] EQ 'eeb' ? 5 : 2, $
                                     eeb_or_ees__recommande[1] EQ 'eeb' ? 5 : 2)

