;2017/02/23
;This is entirely ripped off from Strangeway's batch_summary.pro, gifted to me by that beautiful human, Jack Vernetti
;So says my Alf wave DB (via JOURNAL__20161024__LOOK_FOR_STREAKS_OF_ALFVEN_WAVES__BELLAN_METHOD):
;
;this way:
;JOURNAL__20170224__ORBIT_10832__FOR_PRE8_PROCEEDINGS,/SKIP_DB_EAST_PANEL,/SAVE_B_AND_J_DATA,/ADD_TIMEBAR,TIMEBAR_THICK=2.5
PRO JOURNAL__20170224__ORBIT_10832__FOR_PRE8_PROCEEDINGS, $
   TPLT_VARS=tPlt_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   SCREEN_PLOT=screen_plot, $
   USE_DB_FAC=use_db_fac, $
   SKIP_DESPIN=skip_despin, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   TPLOT_RIGHTNOW=tPlot_rightNow, $
   SAVE_B_AND_J_DATA=save_B_and_J_data, $
   ANCILLARY_PLOTS=ancillary_plots, $
   ADD_TIMEBAR=add_timebar, $
   TIMEBAR_THICK=timeBar_thick, $
   SKIP_DB_EAST_PANEL=skip_dB_east_panel


  eeb_or_ees        = 'eeb'
  ieb_or_ies        = 'ieb'

  plotPref          = '-PRE_VIII-Fig_1'
  saveSuff          = '-with_sc_pot'

  orbit             = 10832

  dato              = '1999-05-18/'
  ;; t1                = dato + '07:25:00'
  ;; t2                = dato + '07:29:00'

  ;; t1                = dato + '06:50:35'
  ;; t2                = dato + '06:51:25'

  t1                = dato + '06:50:45'
  t2                = dato + '06:51:20'

  ;; timeBar_times     = dato + ['06:50:51.705','06:51:13.791']
  ;; timeBar_times     = dato + ['06:50:46.5','06:51:13.791']
  timeBar_times     = dato + ['06:50:57.0','06:51:08.0'  ]

  ;; energy_electrons  = [0.,30000.]
  ;; energy_ions       = [0.,30000.]
  ;; ion_angle         = [180,360]

  plotDirSuff       = '/wave_vector_estimation/PREVIII_proceedings'

  SUMPLOTS_AND_B_PLUS_J_DATA_FOR_BELLAN_SINGLE_SC_ANALYSIS, $
     T1=t1, $
     T2=t2, $
     ORBIT=orbit, $
     TIMEBAR_TIMES=timeBar_times, $
     EEB_OR_EES=eeb_or_ees, $
     IEB_OR_IES=ieb_or_ies, $
     ENERGY_ELECTRONS=energy_electrons, $
     ENERGY_IONS=energy_ions, $
     ION_ANGLE=ion_angle, $
     TPLT_VARS=tPlt_vars, $
     PLOT_NORTH=plot_north, $
     PLOT_SOUTH=plot_south, $
     TLIMIT_NORTH=tlimit_north, $
     TLIMIT_SOUTH=tlimit_south, $
     TLIMIT_ALL=tlimit_all, $
     SCREEN_PLOT=screen_plot, $
     USE_DB_FAC=use_db_fac, $
     SKIP_DESPIN=skip_despin, $
     NO_BLANK_PANELS=no_blank_panels, $
     SAVE_PNG=save_png, $
     SAVE_PS=save_ps, $
     PLOTPREF=plotPref, $
     TPLOT_RIGHTNOW=tPlot_rightNow, $
     SAVE_B_AND_J_DATA=save_B_and_J_data, $
     SAVESUFF=saveSuff, $
     SAVEDIR=saveDir, $
     ANCILLARY_PLOTS=ancillary_plots, $
     BONUSSUFF=bonusSuff, $
     PLOTDIRSUFF=plotDirSuff, $
     ADD_TIMEBAR=add_timebar, $
     TIMEBAR_THICK=timeBar_thick, $
     SKIP_DB_EAST_PANEL=skip_dB_east_panel

END




