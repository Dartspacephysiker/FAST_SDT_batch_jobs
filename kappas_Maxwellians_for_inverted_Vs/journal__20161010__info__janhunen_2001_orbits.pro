;;10/10/16

orbs     = [3091, $             ;The Janhunen orbits
            3123, $
            3147, $
            3219, $
            3370, $
            3498, $
            0000, $
            7628, $
            0000, $
            11002, $            ;The Dombeck orbits
            11076, $
            11097, $
            11109, $
            11024, $
            11056, $
            11067, $
            1771, $             ;The bonus orbits
            1770, $
            6717, $
            5805, $
            5825, $
            1713, $
            5616, $
            12136]

orbTimes = [ $
           ;; ['1997-06-03/' + ['06:28:20','06:31:50']], $ ;The Janhunen orbits
           ['1997-06-03/' + ['06:28:50','06:31:00']], $ ;The Janhunen orbits
           ;; ['1997-06-06/' + ['05:29:10','05:33:10']], $                
           ['1997-06-06/' + ['05:30:55','05:32:06']], $ ;2017/11/21 for burst itvl
           ['1997-06-08/' + ['10:41:10','10:44:10']], $
           ['1997-06-15/' + ['02:34:10','02:36:50']], $
           ['1997-06-29/' + ['01:33:20','01:37:30']], $
           ['1997-07-10/' + ['21:38:40','21:45:05']], $
           ['1997-07-20/' + ['16:20:00','16:24:00']], $
           ['1998-07-27/' + ['06:38:00','06:42:00']], $
           ['1998-08-11/' + ['02:56:00','03:00:00']], $
           ['1999-06-02/' + ['22:42:10','22:47:50']], $ ;11002 ;The Dombeck orbits
           ['1999-06-09/' + ['17:54:10','18:01:08']], $  ;11076
           ['1999-06-11/' + ['16:14:00','16:17:00']], $  ;11097
           ['1999-06-12/' + ['18:43:48','18:48:46']], $  ;11109
           ['1999-06-12/' + ['18:43:48','18:48:46']], $  ;11024
           ['1999-06-12/' + ['18:43:48','18:48:46']], $  ;11056
           ['1999-06-12/' + ['18:43:48','18:48:46']], $  ;11067
           ['1997-02-01/' + ['04:58:45','05:00:10']], $  ;1771  - The Bonus orbits--giving ten on either side to see if that makes mag quantities less crazy
           ['1997-02-01/' + ['02:44:35','02:46:00']], $  ;1770
           ['1998-05-04/' + ['06:44:10','06:45:01']], $  ;6717  - The Chaston et al. [2006] interval
           ['1998-02-09/' + ['01:38:40','01:42:40']], $  ;5805  - World's longest continual obs of monoenergetic aurora
           ;; ['1998-02-09/' + ['01:42:40','01:46:40']], $  ;5805  - World's longest continual obs of monoenergetic aurora
           ['1998-02-10/' + ['21:58:20','22:00:30']], $  ;5825  - Big current, strict mono
           ['1997-01-26/' + ['20:13:20','20:14:30']], $  ;1713  - Semi-big current, strict mono
           ['1998-01-22/' + ['14:34:35','14:35:25']], $  ;5615  - Semi-big current, strict mono
           ['1999-09-15/' + ['02:10:00','02:38:34']]  $  ;12136 - KH candidáto
           ]

orbBurstTimes = LIST( $
                [ $ ;1 --The Janhunen orbits
                ['1997-06-03/' + ['06:28:33.5','06:29:27']] $ 
                ], $
                [ $;2               
                ;; ['1997-06-06/' + ['05:29:10','05:33:10']] $
                ['1997-06-06/' + ['05:30:55','05:32:06']] $ ;2017/11/21 for burst itvl
                ], $                                         
                [ $ ;3
                ['1997-06-08/' + ['10:41:00','10:45:00']], $
                ['1997-06-08/' + ['10:42:31','10:43:25']] $
                ], $
                [ $ ;4
                ['1997-06-15/' + ['02:34:51.8','02:35:28.3']], $
                ['1997-06-15/' + ['02:35:46.05','02:36:04.73']], $
                ['1997-06-15/' + ['02:36:05.8','02:36:49.25']] $
                ], $                                          
                [ $ ;5
                ['1997-06-29/' + ['01:35:02.7','01:35:35.6']] $
                ],$                                            
                ['1997-07-10/' + ['21:38:55','21:39:10']], $   ;6
                ['1997-07-20/' + ['16:20:00','16:24:00']], $   ;7
                ['1998-07-27/' + ['06:38:00','06:42:00']], $   ;8
                ['1998-08-11/' + ['02:56:00','03:00:00']], $   ;9
                ['1999-06-09/' + ['17:56:57','17:57:14']], $   ;10 ;;11002 ;;The Dombeck orbits 
                ['1999-06-09/' + ['17:54:00','18:01:01']], $   ;11 ;;11076
                ['1999-06-11/' + ['16:14:00','16:17:00']], $   ;12 ;;11097
                ['1999-06-09/' + ['17:56:57','17:57:14']], $   ;13 ;;11024
                ['1999-06-09/' + ['17:56:57','17:57:14']], $   ;14
                ['1999-06-09/' + ['17:56:57','17:57:14']], $   ;15
                ['1999-06-09/' + ['17:56:57','17:57:14']], $   ;16
                ['1997-02-01/' + ['04:59:14','05:00:04']], $   ;17 ;;The bonus orbits
                ['1997-02-01/' + ['02:45:25','02:45:58']], $   ;18
                ['1998-05-04/' + ['06:44:10','06:45:01']], $   ;19 ;;The Chaston et al. [2006] interval
                ['1998-02-09/' + ['01:40:01','01:40:33']], $   ;20 ;;The world's longest continual observation of monoenergetic aurora
                ['1998-02-10/' + ['01:40:01','01:40:33']], $   ;21 Big current, strict mono
                ['1997-01-26/' + ['01:40:01','01:40:33']], $   ;22 Semi-big current, strict mono
                ['1998-01-22/' + ['14:34:39','14:35:10']], $   ;23 Semi-big current, strict mono
                '1999-09-15/'+[['02:11:51.5','02:12:11.6'],['02:23:27.5','02:24:28']]  $ ;24 KH candidáto
                )

southArr = [ $
           0,0,0,0,0,0,0,0,0, $ ;00–09 : The Janhunen orbits
           1,1,1,1,1,1,1, $     ;10–16 : The Dombeck orbits
           0,0, $
           1, $                 ;18    : 6717 - The Chaston interval
           0, $                 ;19    : 5805 - The huge mono event
           0, $                 ;The big current, strict mono event
           0, $
           0, $
           0]

bonusPrefs = [ $
             '--Janhunen_evt--1--NM', $
             '--Janhunen_evt--2--NM', $
             '--Janhunen_evt--3--NM', $
             '--Janhunen_evt--4--QM', $
             '--Janhunen_evt--5--QM', $
             '--Janhunen_evt--6--Diffuse', $
             '--Janhunen_evt--7--Diffuse', $
             '--Janhunen_evt--8--Diffuse', $
             '--Janhunen_evt--9--Diffuse', $
             '--Dombeck_evt--0--Mono--decr_with_ILAT', $
             '--Dombeck_evt--1--Mono--decr_with_ILAT', $
             '--Dombeck_evt--2--Mono--decr_with_ILAT', $
             '--Dombeck_evt--3--Mono--decr_with_ILAT', $
             '--Dombeck_evt--4--Mono--decr_with_ILAT', $
             '--Dombeck_evt--5--Mono--decr_with_ILAT', $
             '--Dombeck_evt--6--Mono--decr_with_ILAT', $
             '--Bonus--0--MonoQQ', $
             '--Bonus--1--MonoQQ', $
             '--Chaston_2006--0',  $
             '--Bonus--2--huge_mono',  $
             '--Bonus--3--monoS_bigCurrent',  $
             '--Bonus--4--monoS_bigCurrent',  $
             '--Bonus--5--monoS_bigCurrent',  $
             '--Bonus--6--KH_candidáto'   $
             ]

kStats_startStops__ees = LIST(['1997-06-03/' + ['06:28:50','06:31:00']], $ ;00 : 3091
                              ['1997-06-06/' + ['05:30:55','05:32:06']], $ ;01 : 3123
                              [0],[0],[0], $                               ;02–04
                              [0],[0],[0],[0],['1999-06-02/' + ['22:42:10','22:47:50']], $         ;05–09
                              ['1999-06-09/' + ['17:54:00','18:01:01']],[0],[0],[0],[0],[0], $     ;10–15
                              [0],[0], $                                                           ;16–17
                              '1997-02-01/' + [['02:45:32.5','02:45:44'], $                        ;18
                                               ['02:44:44.5','02:45:02']], $                       ;
                              '1998-02-09/'+[['01:40:21','01:40:52'],['01:41:17.5','01:41:30']], $ ;19
                              [0], $                                                               ;20
                              ;; '1998-02-10/'+[['21:59:15','21:59:30'],['21:59:50','21:59:57.5']] $
                              '1998-02-10/'+[['21:59:15','21:59:30']], $
                              '1997-01-26/'+[['21:59:15','21:59:30']], $
                              '1998-01-22/'+[['14:34:40','14:35:20']], $
                              '1999-09-15/'+[['02:08:54','02:16:00'],['02:23:00','02:26:30']]  $
                             )

kStats_startStops__eeb = LIST([0], $
                              ['1997-06-06/' + ['05:30:55','05:32:06']], $ ;3123
                              [0],[0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0], $
                              '1997-02-01/' + [['02:45:32.5','02:45:44'], $
                               ['02:44:44.5','02:45:02']], $
                              [0], $
                              '1998-02-09/' + [['01:41:17.5','01:41:30']], $
                              [0], $
                              [0], $
                              '1998-01-22/'+[['14:34:39','14:35:10']], $
                              '1999-09-15/'+[['02:11:51.5','02:12:11.6'],['02:23:27.5','02:24:28']]  $
                             )
nHjar = N_ELEMENTS(orbTimes[0,*])

;; cAP_tRanges_list              = LIST($
;;                                 ['1997-06-03/' + ['06:28:50','06:31:00']], $ ;3091
;;                                 ['1997-06-06/' + ['05:30:55','05:32:06']]) ;3123 , $ ;The Janhunen orbits
cAP_tRanges_list = LIST()
FOR k=0,nHjar-1 DO cAP_tRanges_list.Add,orbTimes[*,k]

cAP_tRanges_list[2] = ['1997-06-08/' + ['10:42:10','10:42:25']]

;; eeb_or_ees__recommande        = LIST('ees','eeb','ees','ees')
eeb_or_ees__recommande        = LIST()
FOR k=0,nHjar-1 DO eeb_or_ees__recommande.Add,'ees'

energy_electrons__recommande  = LIST([5D2,3.1D4],[7D2,3.1D4],[3D2,3.1D4],[6D2,3.1D4])
FOR k=N_ELEMENTS(energy_electrons__recommande),nHjar-1 DO energy_electrons__recommande.Add,[5E2,3.1E4]
energy_electrons__recommande[19] = [200,3.1D4] ;19 - 5805

min_peak_energy_recommande    = LIST(500,9D2,300,300)
FOR k=N_ELEMENTS(min_peak_energy_recommande),nHjar-1 DO min_peak_energy_recommande.Add,500
min_peak_energy_recommande[19] = [150.] ;19 -5805

;; spectra_average_interval_list = LIST(4,!NULL,2)
spectra_average_interval_list = LIST(2, $
                                     eeb_or_ees__recommande[1] EQ 'eeb' ? 8 : 2, $
                                     2, $
                                     eeb_or_ees__recommande[3] EQ 'eeb' ? 24 : 1)
FOR k=N_ELEMENTS(spectra_average_interval_list),nHjar-1 DO spectra_average_interval_list.Add,2

cAP__add_iu_pot               = LIST( $
                                0, $ ;3091
                                0, $ ;
                                0, $ ;
                                0)   ;
FOR k=N_ELEMENTS(cAP__add_iu_pot),nHjar-1 DO cAP__add_iu_pot.Add,0

cAP__iu_pot_tids              = LIST(0,0,0,0)
FOR k=N_ELEMENTS(cAP__iu_pot_tids),nHjar-1 DO cAP__iu_pot_tids.Add,0

  ;; Any necessary mods to kappa_mpfit_paramInfo?
  @common__kappa_mpfit_paraminfo.pro

  kFit__DEFSINITIALIZED = 0
  EXPLICIT_DERIVATIVES  = 1
  @kappa_mpfit_param_defaults.pro
