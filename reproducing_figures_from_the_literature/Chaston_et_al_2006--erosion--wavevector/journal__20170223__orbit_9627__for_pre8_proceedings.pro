;2017/02/23
;This is entirely ripped off from Strangeway's batch_summary.pro, gifted to me by that beautiful human, Jack Vernetti
;So says my Alf wave DB (via JOURNAL__20161024__LOOK_FOR_STREAKS_OF_ALFVEN_WAVES__BELLAN_METHOD):
;
;9627     1999-01-27/11:32:42.211  1999-01-27/11:33:24.344  42.133      115       0.36637   21.549    -34.942
;                                                                                                      ^^
;                                                                                                      ||
; create a summary plot of:
; SFA (AKR)
; DSP (VLF)
; Eesa Energy
; Eesa Angle
; Iesa Energy
; Iesa Angle
; E fit along V (Southern hemisphere corrected)
; dB_fac_v (dB_fac and dB_SM also stored)
;
; Returns:
; tPlt_vars  - array of tplot variables
; tlimit_north - tlimits for northern hemisphere
; tlimit_south - tlimits for southern hemisphere
; tlimit_all -  tlimits for all the data
PRO JOURNAL__20170223__ORBIT_9627__FOR_PRE8_PROCEEDINGS, $
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
   TPLOT_RIGHTNOW=tPlot_rightNow, $
   SAVE_B_AND_J_DATA=save_B_and_J_data, $
   ANCILLARY_PLOTS=ancillary_plots, $
   ADD_TIMEBAR=add_timebar

  eeb_or_ees        = 'eeb'
  ieb_or_ies        = 'ieb'

  plotPref          = '-PRE_VIII-Fig_1'
  saveSuff          = '-with_sc_pot'

  orbit             = 9627

  dato              = '1999-01-27/'
  t1                = dato + '11:32:34.2'
  t2                = dato + '11:33:26.5'

  ;; timeBar_times     = dato + ['11:32:42.211','11:33:24.344']
  ;; timeBar_times     = dato + ['11:32:44','11:33:09']
  timeBar_times     = dato + ['11:32:52','11:33:09']

  plotDirSuff       = '/PREVIII_proceedings'

  EESA_lims         = [1D7,1D10]
  IESA_lims         = [1D5,1D8]

  ;; PLOT_FA_CROSSING,ORBIT=orbit,/MAGPOLE,/SOUTH
  ;; PLOT_FA_CROSSING,TMIN=STR_TO_TIME(timebar_times[0]),TMAX=STR_TO_TIME(timebar_times[1]),/magpole,/south,/zoom,/fill,/WHOLE

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
     IESA_LIMS=IESA_lims, $     
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
     ADD_TIMEBAR=add_timebar

END


