;;2017/02/25
PRO JOURNAL__20170225__ORBIT_6717__FOR_PRE8_PROCEEDINGS, $
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
   ADD_TIMEBAR=add_timebar

  eeb_or_ees        = 'eeb'
  ieb_or_ies        = 'ieb'

  plotPref          = '-PRE_VIII-Fig_1'
  saveSuff          = '-fixed_currents-with_sc_pot'

  orbit             = 6717

  dato              = '98-05-04/'
  ;; CASE STRUPCASE(hemi) OF
  ;;    'NORTH': BEGIN

  ;;       t1          = dato + '06:44:13'  
  ;;       t2          = dato + '06:44:56.5'

  ;;       timeBar_times = dato + ['14:50:56','14:51:06']

  ;;    END
  ;;    'SOUTH': BEGIN

        t1          = dato + '06:44:31'  
        t2          = dato + '06:45:00'

        timeBar_times = dato + ['06:44:48','06:44:56.5']

  ;;    END
  ;; ENDCASE

  plotDirSuff       = '/PREVIII_proceedings'

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
     ADD_TIMEBAR=add_timebar

END
