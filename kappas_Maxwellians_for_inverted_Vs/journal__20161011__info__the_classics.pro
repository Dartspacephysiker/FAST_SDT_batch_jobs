;;10/11/16

orbs     = [1843, $
            1849, $
            1773, $
            1789, $
            0000]

orbTimes = [ $
           ['1997-02-07/' + ['20:49:30','20:50:12']], $
           ['1997-02-08/' + ['10:11:22','10:11:52']], $
           ;; ['1997-02-01/' + ['09:25:50','09:27:30']], $
           ['1997-02-01/' + ['09:26:10','09:27:15']], $
           ;; ['1997-02-02/' + ['20:59:30','21:07:30']] $ 
           ;; ['1997-02-02/' + ['21:03:42.5','21:03:58.5']] $
           ;; ['1997-02-02/' + ['21:01:45','21:04:40']] $ ;Checking out the total moment thing
           ;; ['1997-02-02/' + ['21:01:56','21:02:14']] $ ;Checking out the total moment thing
           ;; ['1997-02-02/' + ['21:01:11','21:04:45']] $ ;;2017/11/21
           ;; ['1997-02-02/' + ['21:03:40','21:04:00']] $ ;;2017/11/21 segment 0
           ['1997-02-02/' + ['21:01:40','21:02:20']] $ ;;2017/11/21 segment 1
           ]

orbBurstTimes = LIST( $
                [ $ ;1
                ['1997-02-07/' + ['20:49:30','20:50:10']] $
                ],$

                ;; ['1997-02-08/' + ['10:11:22','10:11:52']], $ ;2
                '1997-02-08/' + [ $
                ['10:11:26.5','10:11:27.2'], $
                ['10:11:31','10:11:35'] $
                                ], $
   
                ['1997-02-01/' + ['09:26:05','09:27:27']], $ ;3

                [ $ ;4
                ['1997-02-02/' + ['21:01:22','21:01:38.8']], $ 
                ['1997-02-02/' + ['21:01:56','21:02:28']], $ 
                ['1997-02-02/' + ['21:03:42.5','21:03:58.5']], $
                ['1997-02-02/' + ['21:04:49.5','21:05:06.5']], $
                ['1997-02-02/' + ['21:05:25','21:05:55']] $
                ] $
                )

bonusPrefs = [ $
             '-classics-1-Ergun_et_al_1998', $
             '-classics-2-McFadden_et_al_1998', $
             '-classics-3-Elphic_et_al_1998', $
             '-classics-4-Carlson_et_al_2001' $
             ]


kStats_startStops__ees = LIST( $
                         ;;Numbers 0 and 1
                         [0],[0], $
                         ;;
                         ;;Number 2
                         LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
                                               ['09:26:55','09:27:05']]), $
                         ;;
                         ;;Number 3
                         LIST('1997-02-02/' + [ $;; ['21:01:50','21:02:15'], $ ;These are for the downward current regions
                                               ;; ['21:27:05','09:27:15'], $
                         ;; ['21:03:42','21:03:58']]), $
                         ;; ['21:03:42','21:03:58']]), $ ;checking out the total moment thing
                         ;; ['21:01:59','21:02:15']]), $ ;checking out the total moment thing
                         ;; ['21:01:11','21:04:45']]), $ ;;2017/11/21
                         ;; ['21:03:40','21:04:00']]), $ ;;2017/11/21 segment 0
                         ['21:01:40','21:02:20']]), $ ;;2017/11/21 segment 1
                         [0], $
                         [0],[0],[0],[0],[0], $
                         [0],[0],[0],[0],[0], $
                         [0],[0], $
                         [0], $
                         [0], $
                         [0])

kStats_startStops__eeb = LIST( $
                         ;;Numbers 0 and 1
                         [0], $
                         LIST('1997-02-08/' + [['10:11:26.5','10:11:27.2'], $
                                               ['10:11:32.0','10:11:34.032']]), $
                         ;;
                         ;;Number 2
                         LIST('1997-02-01/' + [['09:26:12','09:26:23'], $ ;;These are the money times that seem to give good fits
                                               ['09:26:53','09:27:07.5']]), $
                         ;;
                         ;;Number 3 (Carlson)
                         LIST('1997-02-02/' + [['21:01:56','21:02:15'], $
                                               ['21:01:56','21:02:15'], $
                                               ['21:03:42','21:03:58']]), $ ;, $ ;;These are the money times that seem to give good fits
                                               ;; ['09:26:53','09:27:07.5']]), $
                         [0], $
                         [0],[0],[0],[0],[0], $
                         [0],[0],[0],[0],[0], $
                         [0],[0], $
                         [0], $
                         [0], $
                         [0])

;; cAP_tRanges_list              = LIST('1997-02-07/' + $
;;                                      [['20:49:45', $
;;                                        '20:49:52'], $
;;                                       ['20:49:56', $
;;                                        '20:50:09']], $
;; ;; '1997-02-07/'+['20:49:41','20:50:10'], $
;;                                      [0], $
;;                                      ;; '1997-02-01/'+['09:26:14.0','09:27:05.4'])
;;                                      '1997-02-01/'+[['09:26:14.2','09:26:23.0'], $
;;                                                     ['09:26:55.0','09:27:05.0']])
;;                                      ;; '1997-02-01/'+[['09:26:14.0','09:26:44.0'], $
;;                                      ;;                ['09:26:51.0','09:27:05.4']])

cAP_tRanges_list              = LIST($
                                ;;
                                ;;Number 0
                                '1997-02-07/' + $
                                ['20:49:50', $
                                  '20:50:11'], $
                                ;; [['20:49:56', $
                                ;;   '20:50:09'], $
                                 ;; ['20:49:26', $
                                 ;;  '20:49:46']], $
                                ;; '1997-02-07/'+['20:49:41','20:50:10'], $ 
                                ;;
                                ;;Number 1
                                ;; '1997-02-08/' + ['10:11:25.5','10:11:27.2'], $
                                ;; '1997-02-08/' + ['10:11:32.84','10:11:34.032'], $
                                ;;
                                '1997-02-08/' + ['10:11:22','10:11:52'], $
                                ;;Number 2
                                ;; '1997-02-01/'+['09:26:14.0','09:27:05.4'])

                                ;; '1997-02-01/'+[['09:26:14.2','09:26:23.0'], $
                                ;;                ['09:26:55.0','09:27:05.0']], $

                                ;; '1997-02-01/'+[['09:26:14.2','09:26:23.0']], $

                                '1997-02-01/'+[['09:26:55.5','09:27:03.5']], $ ;GRL interval
                                ;; '1997-02-01/'+[['09:26:11.0','09:26:20.0'], $ ;Spence checkout 2018/02/21
                                ;;                ['09:26:55.5','09:27:04']], $
                                ;;
                                ;;Number 3
                                ;; '1997-02-02/'+ ['21:03:42','21:03:58'])
                                ;; '1997-02-02/'+[['21:02:02.0','21:02:14']])
                                ;; '1997-02-02/'+[['21:01:58.0','21:02:14']]) ;;2017/04/22
                                ;; '1997-02-02/'+[['21:02:01.0','21:02:13']]) ;;2017/04/22
                                ;; '1997-02-02/'+[['21:01:11','21:04:45']]) ;;2017/11/21
                                ;; '1997-02-02/'+[['21:03:40','21:04:00']]) ;;2017/11/21 segment 0
                                '1997-02-02/'+[['21:01:40','21:02:20']]) ;;2017/11/21 segment 1
                                ;; '1997-02-02/'+[['21:02:01','21:02:10']])
                                ;; '1997-02-02/'+ ['21:01:42','21:03:58']) ;Checking out the total moment thing

eeb_or_ees__recommande        = LIST('ees','ees','ees','ees')

energy_electrons__recommande  = LIST([5D2,3.01D4],[5D2,3.01D4],[3D2,3.01D4],[6D2,3.01D4])
min_peak_energy_recommande    = LIST(500,1D3,600,600)

;; spectra_average_interval_list = LIST(4,!NULL,2)
spectra_average_interval_list = LIST(2, $
                                     eeb_or_ees__recommande[1] EQ 'eeb' ? 2 : 2, $
                                     2, $ ;1773 GRL
                                     ;; 3, $ ;1773 Quite contrary
                                     ;; 4, $ ;1773 Quite contrary 2
                                     eeb_or_ees__recommande[3] EQ 'eeb' ? 24 : 1)

cAP__add_iu_pot               = LIST( $
                                1, $ ;1843
                                1, $ ;1849
                                1, $ ;1773
                                1)   ;1789
                                      
  ;; Any necessary mods to kappa_mpfit_paramInfo?
  @common__kappa_mpfit_paraminfo.pro

  kFit__DEFSINITIALIZED = 0
  EXPLICIT_DERIVATIVES  = 1
  @kappa_mpfit_param_defaults.pro


