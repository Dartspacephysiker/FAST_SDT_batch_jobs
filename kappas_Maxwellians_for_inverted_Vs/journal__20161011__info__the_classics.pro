;;10/11/16

orbs     = [1843, $
            1849, $
            1773, $
            1789, $
            0000]

orbTimes = [ $
           ['97-02-07/20:49:30','97-02-07/20:50:10'], $
           ['97-02-08/10:11:22','97-02-08/10:11:52'], $
           ['97-02-01/09:25:50','97-02-01/09:27:30'], $
           ['97-02-02/20:59:30','97-02-02/21:07:30'] $ 
           ]

orbBurstTimes = LIST( $
                [ $ ;1
                ['97-02-07/20:49:30','97-02-07/20:50:10'] $
                ],$

                ['97-02-08/10:11:22','97-02-08/10:11:52'], $ ;2

                ['97-02-01/09:26:05','97-02-01/09:27:27'], $ ;3

                [ $ ;4
                ['97-02-02/21:01:22','97-02-02/21:01:38.8'], $ 
                ['97-02-02/21:01:55','97-02-02/21:02:28'], $ 
                ['97-02-02/21:03:42.5','97-02-02/21:03:58.5'], $
                ['97-02-02/21:04:49.5','97-02-02/21:05:06.5'], $
                ['97-02-02/21:05:25','97-02-02/21:05:55'] $
                ] $
                )

bonusPrefs = [ $
             '--classics--1--Ergun_et_al_1998', $
             '--classics--2--McFadden_et_al_1998', $
             '--classics--3--Elphic_et_al_1998', $
             '--classics--4--Carlson_et_al_2001' $
             ]


kStats_startStops__ees = LIST([0],[0],LIST('1997-02-01/' + [['09:25:50','09:26:10'], $ ;These are for the downward current regions
                                                       ['09:27:05','09:27:15']]), $
                              [0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0], $
                              [0], $
                              [0], $
                              [0])

kStats_startStops__eeb = LIST([0],[0],LIST('1997-02-01/' + [['09:26:12','09:26:23'], $ ;;These are the money times that seem to give good fits
                                                       ['09:26:53','09:27:07.5']]), $
                              [0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0],[0],[0],[0], $
                              [0],[0], $
                              [0], $
                              [0], $
                              [0])