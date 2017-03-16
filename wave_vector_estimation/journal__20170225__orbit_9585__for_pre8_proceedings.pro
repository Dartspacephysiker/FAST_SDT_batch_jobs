;2017/02/23
;This is entirely ripped off from Strangeway's batch_summary.pro, gifted to me by that beautiful human, Jack Vernetti
;So says my Alf wave DB (via JOURNAL__20161024__LOOK_FOR_STREAKS_OF_ALFVEN_WAVES__BELLAN_METHOD):
;
;9627     1999-01-27/11:32:42.211  1999-01-27/11:33:24.344  42.133      115       0.36637   21.549    -34.942
;                                                                                                      ^^
;2017/03/09 Trying to figure out how serious the issue is. Only have 1 min of data loaded in SDT with config UIcfg.Orb_9585__test_1__a_la_Elphic_1998_AND_Fig_1__thelonious
;The command:
;bonusSuff = '-20170225journal__1minLoaded'
;JOURNAL__20170225__ORBIT_9585__FOR_PRE8_PROCEEDINGS,/SAVE_B_AND_J_DATA,/SAVE_EPS,/ADD_TIMEBAR,TIMEBAR_THICK=2.0,/SKIP_DB_EAST_PANEL,BONUSSUFF=bonusSuff
;Next:
;bonusSuff = '-20170225journal__50minLoaded'
;JOURNAL__20170225__ORBIT_9585__FOR_PRE8_PROCEEDINGS,/SAVE_B_AND_J_DATA,/SAVE_EPS,/ADD_TIMEBAR,TIMEBAR_THICK=2.0,/SKIP_DB_EAST_PANEL,BONUSSUFF=bonusSuff
;
;2017/03/13 This way, doggie:
;JOURNAL__20170225__ORBIT_9585__FOR_PRE8_PROCEEDINGS,/SKIP_DB_EAST_PANEL,/SAVE_B_AND_J_DATA,/ADD_TIMEBAR,TIMEBAR_THICK=2.5
PRO JOURNAL__20170225__ORBIT_9585__FOR_PRE8_PROCEEDINGS, $
   TPLT_VARS=tPlt_vars, $
   PLOT_NORTH=plot_north, $
   PLOT_SOUTH=plot_south, $
   TLIMIT_NORTH=tlimit_north, $
   TLIMIT_SOUTH=tlimit_south, $
   TLIMIT_ALL=tlimit_all, $
   ION_ANGLE=ion_angle, $
   SCREEN_PLOT=screen_plot, $
   USE_DB_FAC=use_db_fac, $
   SKIP_DESPIN=skip_despin, $
   NO_BLANK_PANELS=no_blank_panels, $
   SAVE_PNG=save_png, $
   SAVE_PS=save_ps, $
   SAVE_EPS=save_eps, $
   TPLOT_RIGHTNOW=tPlot_rightNow, $
   BONUSSUFF=bonusSuff, $
   SAVE_B_AND_J_DATA=save_B_and_J_data, $
   ANCILLARY_PLOTS=ancillary_plots, $
   ADD_TIMEBAR=add_timebar, $
   TIMEBAR_THICK=timeBar_thick, $
   SKIP_DB_EAST_PANEL=skip_dB_east_panel

  eeb_or_ees        = 'eeb'
  ieb_or_ies        = 'ieb'

  skip_dB_east_panel = 1

  plotPref          = '-PRE_VIII-Fig_1'
  saveSuff          = '-with_sc_pot'

  orbit             = 9585

  dato              = '1999-01-23/'

  t1                = dato + '14:50:35'
  t2                = dato + '14:51:15'

  timeBar_times     = dato + ['14:50:56','14:51:07']

  ;; energy_electrons  = [0.,30000.]
  ;; energy_ions       = [0.,30000.]
  ;; ion_angle         = [180,360]

  plotDirSuff       = '/wave_vector_estimation/PREVIII_proceedings'

  EESA_lims         = [1D7,1D10]
  IESA_lims         = [1D6,1D8]
  Ienergy_dE_lims   = [1D6,1D9]

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
     EESA_LIMS=EESA_lims, $
     EESA_ANGLE_DE_LIMS=Eangle_dE_lims, $
     EESA_ENERGY_DE_LIMS=Eenergy_dE_lims, $
     IESA_LIMS=IESA_lims, $     
     IESA_ANGLE_DE_LIMS=Iangle_dE_lims, $
     IESA_ENERGY_DE_LIMS=Ienergy_dE_lims, $
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
     SAVE_EPS=save_eps, $
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

