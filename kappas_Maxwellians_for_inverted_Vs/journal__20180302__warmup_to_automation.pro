;2018/03/02
PRO JOURNAL__20180302__WARMUP_TO_AUTOMATION,orbit, $
   BOTH_RESTORE_FITFILE_AND_NO_REMAKE_JV_MASTERFILE=both_restore_fitFile_and_no_remake_jv_masterfile, $
   RESTORE_FITFILE=restore_fitFile, $
   RESTORE_JV_MASTERFILE=restore_jv_masterFile, $
   NO1DPLOTSPLEASE=no1DPlotsPlease, $
   NOSTRANGEWAYSUMMARY=noStrangewaySummary, $
   NOKAPPASUMMARY=noKappaSummary, $
   NOCURPOTPLOTSPLEASE=noCurPotPlotsPlease, $
   ONLY_SOUTH=only_south, $
   MIN_ALTITUDE=min_altitude, $
   BATCH_MODE=batch_mode

  COMPILE_OPT IDL2,STRICTARRSUBS

;; dummy=LABEL_DATE(DATE_FORMAT=['%I:%S']) & times = UTC_TO_JULDAY(curPotList[0].time) & window=WINDOW(DIMENSIONS=[1000,800]) & magcplot = plot(times,magcurrent,NAME='Magnetometer',TITLE='Current obs beginning ' + T2S(curPotList[0].time[0],/MS),XTICKFORMAT='LABEL_DATE',XTICKUNITS='Time',XRANGE=xRange,XTITLE='Tid',/CURRENT,XTICKLEN=1.0,YTICKLEN=1.0,XSUBTICKLEN=0.01,YSUBTICKLEN=0.01) & downE=PLOT(times,curPotList[0].cur,NAME='Downward e!U-!N',COLOR='BLUE',/OVERPLOT) & allcurplot=PLOT(times,curPotList[0].cur+curPotList[1].cur+curPotList[2].cur,NAME='All',LINESTYLE='--',COLOR='RED',XTICKFORMAT='LABEL_DATE',/OVERPLOT) & ionPlot=PLOT(times,curPotList[2].cur,NAME='Upward i!U+!N',LINESTYLE='-.',COLOR='Green',THICK=2,XTICKFORMAT='LABEL_DATE',/OVERPLOT) & axes=magcplot.axes & axes[0].major=ROUND((xRange[1]-xRange[0])*24*60*60/15.)+1 & axes[0].minor=2 & axes[2].major = axes[0].major & axes[2].minor = axes[0].minor & legend=LEGEND(TARGET=[magcplot,downE,allcurplot,ionPlot])

  ;; manual_restore_masterFile = 0
  ;; manual_restore_fitFile   = 1

  ;; batch_mode = 1
  routName = 'JOURNAL__20180302__WARMUP_TO_AUTOMATION'
  bonusPref    = '-TESTRUN-20180302'

  ;;get orbTimes here
  ;;@journal__20161010__info__janhunen_2001_orbits.pro

  GET_FA_SDT_ORBIT,orbit
  ;; orbit = KEYWORD_SET(orbit) ? orbit : 1746

  addSec_on_either_side             = 20
  only_1D_fits                      = 0

  checkForSkippers = 0
  nToSkip = 0

  dateToCheck = '20180328'
  dirForCheck = '/SPENCEdata/software/sdt/batch_jobs/plots/'+dateToCheck+'/kappa_fits/'
  orbDir = STRING(FORMAT='("Orbit_",I0)',orbit)
  IF FILE_TEST(dirForCheck+orbDir,/DIRECTORY) AND KEYWORD_SET(checkForSkippers) THEN BEGIN

     skipFiles = FILE_SEARCH(dirForCheck+orbDir,'Kappa_summary--*eps')
     sFileTids = STRMID(skipFiles, $
                        STRLEN(dirForCheck+orbDir+'/'+'Kappa_summary--'+STRING(FORMAT='(I0)',orbit)+bonusPref+'--'), $
                        8)
     nToSkip = N_ELEMENTS(UNIQ(sFileTids,SORT(sFileTids)))

  ENDIF

  READ_KAPPA_BATCH_SETUP_FILE, $
     orbit,MLT,ILAT,ALT,t1Str,t2Str,t_streakLen,nPts,dt_avg,avg_current, $
     NTOSKIP=nToSkip, $
     /PRINT_SUMMARY

  IF KEYWORD_SET(only_south) AND ILAT GT 0 THEN BEGIN
     PRINT,"Sorry, this orbit is in the Norf"
     RETURN
  ENDIF

  IF KEYWORD_SET(min_altitude) THEN IF ALT LT min_altitude THEN BEGIN
     PRINT,FORMAT='(A0,I0,A0,I0,A0)', $
           "Sorry, the altitude of this pass (", $
           ALT, $
           " km) is below the thresh @ ", $
           min_altitude, $
           " km. Returning!"
     RETURN
  ENDIF

  cAP_tRanges  = [t1Str,t2Str]

  IF (orbit EQ 1612 OR orbit EQ 5548) AND nToSkip EQ 0 THEN addSec_on_either_side = 0 ;;More stuff below for this orbit!

  ;; Now add some buffer time to the sides
  t1  = S2T(t1Str)
  t2  = S2T(t2Str)
  t1 -= addSec_on_either_side
  t2 += addSec_on_either_side

  ;; 2018/03/12 For super low kappa
  IF orbit EQ 1607 AND nToSkip EQ 0 THEN BEGIN
     tmpDate     = '1997-01-17/'
     t1          = S2T(tmpDate + '01:03:50')
     t2          = S2T(tmpDate + '01:06:15')
     cAP_tRanges = tmpDate + ['01:04:56','01:05:55']
     ;;More stuff for orbit 1607 below
  ENDIF

  IF (orbit EQ 3931 OR orbit EQ 3218) AND nToSkip EQ 0 THEN BEGIN
     t1 += 25
  ENDIF

  ;; 2018/03/14 For maybe-super-low low kappa
  IF orbit EQ 1945 AND nToSkip EQ 0 THEN BEGIN
     tmpDate     = '1997-02-17/'
     ;; t1          = S2T(tmpDate + '07:14:15')
     ;; t2          = S2T(tmpDate + '07:15:05')
     ;; cAP_tRanges = tmpDate + ['07:14:38','07:14:47.5']
     ;; cAP__iu_pot_tids = tmpDate + ['07:14:22.5','07:14:38']
     cAP__iu_pot_tids = tmpDate + [['07:14:22.5','07:14:38'], $
                                   ['07:13:52','07:13:57.5']]
     ;;More stuff for orbit 1945 below
     nTRanges = N_ELEMENTS(cAP__iu_pot_tids[0,*])
     majicInterval = 0
  ENDIF

  IF orbit EQ 1835 AND nToSkip EQ 0 THEN BEGIN
     tmpDate     = '1997-02-07/'
     eeb_or_ees  = 'eeb'

     cAP_tRanges = tmpDate + ['03:02:03','03:02:19']
     ;; cAP__iu_pot_tids = tmpDate + ['07:14:22.5','07:14:38']
     cAP__iu_pot_tids = tmpDate + ['03:02:06','03:02:18.5']

     t1Str = cAP_tRanges[0]
     t2Str = cAP_tRanges[1]
     t1    = S2T(t1Str)
     t2    = S2T(t2Str)
     
     spectra_average_interval = 4

  ENDIF

  t1Str        = T2S(t1,/MS)
  t2Str        = T2S(t2,/MS)
  PRINT,t1Str
  kStats__tids = [t1Str,t2Str]

  ;; Options
  fit1D__sourceCone_energy_spectrum = 1

  fit1D__nFlux                      = 1
  fit2D__nFlux                      = 0

  fit__linear_energy_shift          = 1

  fit1D__weighting                  = 2 ;1 = lin 2 = square
  fit1D__clampTemperature           = 0
  fit1D__clampDensity               = 0

  add_oneCount_curve                = 1

  daPlots_cAP                       = KEYWORD_SET(noCurPotPlotsPlease) ? 0 : 1
  fit1D__save_plotSlices            = KEYWORD_SET(no1DPlotsPlease) ? 0 : 1
  fit1D__save_every_nth_plot        = 4
  fit1D__save_if_kappa_below        = 3.
  fit1D__combine_plotslices_in_PDF  = 1
  fit2D__save_all_plots             = 0
  fit2D__show_each_candidate        = 0
  fit2D__show_only_data             = 0
  fit2D__weighting                  = 2 ;1 = lin 2 = square
  fit2D__clampTemperature           = 0
  fit2D__clampDensity               = 0
  fit2D__estimate_sourceCone_from_dist = 0B
  fit2D__extend_fitStruct_eRange    = 0 ;to 50 keV, je crois?

  fit2D__temperature_type  = 'PAR' ;or 'AVG'
  ;;PostScript options
  timeBars                 = 1

  eps                      = 1

  ;; spectrogram_units        = 'flux'
  spectrogram_units        = 'eflux'

  show_Strangeway_summary  = KEYWORD_SET(noStrangewaySummary) ? 0 : 1
  sway__save_ps            = 1
  sway__add_kappa_panel    = 0
  sway__add_chare_panel    = 1
  sway__add_Newell_panel   = 1
  sway__add_iu_pot         = 1
  sway__log_kappaPlot      = 0
  sway__spectrogram_units  = spectrogram_units

  show_kappa_summary  = KEYWORD_SET(noKappaSummary) ? 0 : 1
  kSum__save_ps       = 1
  kSum__add_parm_errors_from_file = 0
  kSum__add_parm_errors__nRolls = 10000
  kSum__add_parm_errors__use_most_prob = 1
  kSum__chi2Bounds    = [0.,15.]
  ;; kSum__spectrogram_units = 'flux'
  kSum__spectrogram_units = spectrogram_units

  kSum__convert_to_Newell_interp = 1
  kSum__add_chi2_line = 5       ;give value at which you'd like line
  kSum__add_meas_T_and_N = 1
  kSum__GRL           = 1
  kSum__oPlot_pot     = 1

  kStats__save_stuff   = 1

  save_diff_eFlux_file = 1
  load_diff_eFlux_file = 1
  ;; restore_fitFile      = (N_ELEMENTS(manual_restore_fitFile) GT 0 ? manual_restore_fitFile : KEYWORD_SET(both_restore_fitFile_and_no_remake_jv_masterfile))
  restore_fitFile      = KEYWORD_SET(manual_restore_fitFile) OR $
                         KEYWORD_SET(restore_fitFile       ) OR $
                         KEYWORD_SET(both_restore_fitFile_and_no_remake_jv_masterfile)
  restore_jv_masterFile = KEYWORD_SET(manual_restore_masterFile) OR $
                          KEYWORD_SET(restore_jv_masterFile    ) OR $
                          KEYWORD_SET(both_restore_fitFile_and_no_remake_jv_masterfile)

  jv_theor__also_eFlux = 0
  jv_theor__only_eFlux = 0

  electron_angleRange  = 'lc'
  energy_electrons     = [3e2,3.1e4]
  ;; electron_lca         = [150,-150]
  ;; electron_lca         = 'lc'
  min_peak_energy      = 300
  max_peak_energy      = !NULL

  IF orbit EQ 1607 AND nToSkip EQ 0 THEN BEGIN
     energy_electrons[0] = 8E1
     min_peak_energy     = 8E1
     min_peak_energyArr  = [8E1,1E2,1E2]
  ENDIF

  IF orbit EQ 1612 AND nToSkip EQ 0 THEN BEGIN
     
     dato = '1997-01-17/'

     majicEnergy         = 115.  ;50 is too low; I tried it 
     energy_electrons[0] = majicEnergy

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

     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,7E0]
     max_peak_energyArr  = [1E4,2e4,1.0E3]

     min_peak_energy_tStruct = {tBounds : energy_electron_tBounds, $
                                energy  : edgeries, $
                                forWhom : MAKE_ARRAY(N_ELEMENTS(edgeries),VALUE=0)}

     use_peakE_bounds_for_moment_calc = [1,0,0]
     peakE_bounds_indShift = [-1,0]

     cAP__iu_pot_tids = dato + [['12:00:27.5','12:00:39.'], $
                                ['12:00:40.0','12:00:49'], $
                                ['12:01:09.0','12:01:13'], $
                                ['12:01:18.5','12:01:30'], $
                                ['12:01:32.0','12:01:47']]
     ;; cAP_tRanges = dato + [['12:01:24.3','12:01:28.76'], $
     ;;                                ['12:01:33.1','12:01:35.7']]

     spectra_average_interval = 2
     ;; cAP_tRanges = dato + [['12:01:10.7','12:01:15.2'], $
     ;;                       ['12:01:21.1','12:01:29.4']]
     ;; cAP_tRanges = dato + [['12:01:11','12:01:20']]
     cAP_tRanges = dato + [['12:00:29.79','12:00:48.7'], $
                           ['12:01:10.75','12:01:22.125']]

  ENDIF

  IF orbit EQ 1694 AND nToSkip EQ 0 THEN BEGIN
     
  ;;    isMajic             = 2E2
  ;;    energy_electrons[0] = isMajic
  ;;    min_peak_energy     = isMajic
  ;;    min_peak_energyArr  = [isMajic,isMajic,isMajic]

     majicEnergy         = 2E2
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,100]
     max_peak_energyArr  = [2e4,2e4,4E3]

     cAP__iu_pot_tids    = '1997-01-25/' + [['02:02:00','02:02:27.5'], $
                                            ['02:02:58','02:03:33']]

     use_peakE_bounds_for_moment_calc = [1,0,0]
     peakE_bounds_indShift            = [-1,0]

     cAP_tRanges         = cAP__iu_pot_tids

     spectra_average_interval = 2

  ENDIF

  IF orbit EQ 1835 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy = 6E2
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,5E1]
     max_peak_energyArr  = [2e4,2e4,2E3]

  ENDIF

  IF orbit EQ 1945 AND nToSkip EQ 0 THEN BEGIN
     majicEnergy = KEYWORD_SET(majicInterval) ? 5E2 : 2E2
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,1E2]
  ENDIF

  IF orbit EQ 2968 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy         = 1.1E3
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,700]
     max_peak_energyArr  = [2e4,2e4,4E3]

     cAP__iu_pot_tids    = '1997-05-22/' + [['22:39:30','22:40:05'], $
                                            ['22:40:15','22:40:26'], $
                                            ['22:40:32','22:40:45']]
  ENDIF

  IF orbit EQ 3167 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy         = 300
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,700]

     spectra_average_interval = 6

  ENDIF

  IF orbit EQ 3368 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy         = 200.
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,700]

     spectra_average_interval = 4

  ENDIF

  IF orbit EQ 2685 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy         = 80.
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,700]

     spectra_average_interval = 1

  ENDIF

  IF orbit EQ 3218 AND nToSkip EQ 0 THEN BEGIN

     majicEnergy         = 70.
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,100]
     ;; max_peak_energyArr  = [2e4,2e4,4E3]

  ENDIF

  IF orbit EQ 3760 AND nToSkip EQ 0 THEN BEGIN
     
     majicEnergy         = 80.
     energy_electrons[0] = majicEnergy
     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,100]

     tmpDate             = '1997-08-04/'
     cAP_tRanges = tmpDate + [['03:49:50','03:50:02'], $
                              ['03:50:38','03:50:49']]

  ENDIF

  IF orbit EQ 5624 AND nToSkip EQ 0 THEN BEGIN
     
     dato = '1998-01-23/'

     majicEnergy         = 60.  ;50 is too low; I tried it 
     energy_electrons[0] = majicEnergy

     edgeries            = [30.,150.]

     energy_electrons    = [[edgeries[0],energy_electrons[1]], $
                            [edgeries[1],energy_electrons[1]]]

     moment_energyArr    = [[energy_electrons],[energy_electrons],[10,2.4e4]]

     energy_electron_tBounds = dato + [['08:23:58','08:24:01.4'], $ ;lb is 30
                                       ['08:24:01.4','08:25:40']]   ;lb is 150.

     min_peak_energy     = majicEnergy
     min_peak_energyArr  = [majicEnergy,1E2,1E1]
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

  ;;survey window
  eeb_or_ees           = N_ELEMENTS(eeb_or_ees) GT 0 ? eeb_or_ees : 'ees'
  spectra_average_interval = N_ELEMENTS(spectra_average_interval) GT 0 ? spectra_average_interval : 2

  ;;Thresholds for inclusion
  ;; chi2_thresh          = 1.5e4
  chi2_over_dof_thresh = 100
  lowDens_thresh       = 0.05
  diffEflux_thresh     = 3D6
  nPkAbove_dEF_thresh  = 3

  ;; fit2D__density_angleRange = 'ALL__EXCL_ATM'
  fit2D__density_angleRange = 'ALL__EXCL_ATM'
  fit2D__temperature_angleRange = 'LC'
  fit2D__faConductance_angleRange = 'LC'

  ;;Current and potential analysis
  curAndPot_analysis        = 1

  cAP__iu_pot_tids          = N_ELEMENTS(cAP__iu_pot_tids) GT 0 ? cAP__iu_pot_tids : 0
  cAP__add_iu_pot           = KEYWORD_SET(cAP__iu_pot_tids)

  ;; remake_masterFile         = (N_ELEMENTS(manual_restore_masterFile) GT 0 ? ~manual_restore_masterFile : $
  ;;                              ~(KEYWORD_SET(both_restore_fitFile_and_no_remake_jv_masterfile) $
  ;;                                OR ~KEYWORD_SET(restore_jv_masterFile)))
  cAP_struct = { $
               remake_masterFile : ~restore_jv_masterFile, $
               map_to_100km : 1, $
               use_all_currents : 0B, $
               use_ed_current : 1B, $
               use_iu_current : 0B, $
               use_eu_current : 0B, $
               use_mag_current : 0B, $
               use_char_en_for_downPot : 0B, $
               T_plusMinusFac_for_pot  : 0L, $
               use_peak_en_for_downPot : 1B, $
               add_iu_pot : cAP__add_iu_pot, $
               iu_pot_tids : cAP__iu_pot_tids, $
               tRanges : cAP_tRanges, $
               ;; moment_energyArr : [[100,3.0e4],[100,3.0e4],[100,2.4e4]]
               moment_energyArr : N_ELEMENTS(moment_energyArr) GT 0 ? moment_energyArr : $
               [[energy_electrons],[energy_electrons],[100,2.4e4]], $
               ;; use_peakE_bounds_for_moment_calc : 1B, $
               plot_j_v_potBar : 0B, $
               plot_jv_a_la_Elphic : daPlots_cAP, $
               plot_T_and_N : 0B, $
               plot_j_v_and_theory : 0B, $
               plot_j_v__fixed_t_and_n : daPlots_cAP, $
               plot_j_v_map__r_b_and_kappa__fixed_t_and_n : daPlots_cAP, $
               plot_en_specs : 0B, $
               en_specs__movie : 0B, $
               jv_theor__R_B_init : 30, $
               jv_theor__kappa_init : 10, $
               jv_theor__kappaLims  : [1.520,11], $
               jv_theor__also_eFlux : KEYWORD_SET(jv_theor__also_eFlux), $
               jv_theor__only_eFlux : KEYWORD_SET(jv_theor__only_eFlux), $
               ;; jv_theor__TempLims       : [0,0], $
               ;; jv_theor__DensLims      : [0,0], $
               ;; jv_theor__magRatioLims  : [2,100], $

               ;;JV theory options

               ;; jv_theor__fit_je         : 1, $
               jv_theor__fit_both : 0, $
               jv_theor__Liemohn_and_Khazanov_dens : KEYWORD_SET(dens__Liemohn_Khaz), $

               use_msph_sourcecone_for_dens : [1,0,0], $
               use_msph_sourcecone_for_temp : [0,0,0], $
               temperature_type             : fit2D__temperature_type, $
               aRange__temp_e_down         : fit2D__temperature_angleRange, $
               ;; eRange__temp_list         :, $
               use_energies_above_peak_for_temp : [1,0,0], $
               msph_sourcecone_halfWidth : 30, $
               ;; msph_sourcecone_halfWidth : msph_sourcecone_halfWidth, $
               all_pitchAngles : 0, $
               allPitch_except_atm_lc : 0, $
               ;; jv_theor__initial_source_R_E : 5.0D, $
               jv_theor__initial_source__Polar : 1, $
               ;; jv_theor__initial_source__equator : 0, $
               ;; jv_theor__iterative_game : 0, $
               ;; jv_theor__itergame_NFac   : 3.0, $
               jv_theor__itergame_tie_R_B_and_dens : 1, $
               in_bonusPref                        : bonusPref, $
               plots_in_buffer                     : 1}

  IF KEYWORD_SET(min_peak_energy_tStruct) THEN BEGIN
     STR_ELEMENT,cAP_struct,'min_peak_energy_tStruct',min_peak_energy_tStruct,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(max_peak_energy_tStruct) THEN BEGIN
     STR_ELEMENT,cAP_struct,'max_peak_energy_tStruct',max_peak_energy_tStruct,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(min_peak_energyArr) THEN BEGIN
     STR_ELEMENT,cAP_struct,'min_peak_energyArr',min_peak_energyArr,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(max_peak_energyArr) THEN BEGIN
     STR_ELEMENT,cAP_struct,'max_peak_energyArr',max_peak_energyArr,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(use_peakE_bounds_for_moment_calc) THEN BEGIN
     STR_ELEMENT,cAP_struct,'use_peakE_bounds_for_moment_calc',use_peakE_bounds_for_moment_calc,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(peakE_bounds_indShift) THEN BEGIN
     STR_ELEMENT,cAP_struct,'peakE_bounds_indShift',peakE_bounds_indShift,/ADD_REPLACE
  ENDIF
  
  IF KEYWORD_SET(aRange__dens_e_down) THEN BEGIN
     STR_ELEMENT,cAP_struct,'aRange__dens_e_down',aRange__dens_e_down,/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(timeBars) AND KEYWORD_SET(cAP_struct) THEN IF (WHERE(TAG_NAMES(cAP_struct) EQ 'TRANGES'))[0] NE -1 THEN BEGIN
     timeBars                  = cAP_struct.tRanges
  ENDIF

  show_post_plots      = 0
  save_postKappa_plots = 0
  close_kp_after_save  = 0

  n_below_peak1D       = KEYWORD_SET(fit__JE_over_E) ? -1 : -1
  n_above_peak1D       = 30
  n_below_peak2D       = KEYWORD_SET(fit__JE_over_E) ? -1 : -1
  n_above_peak2D       = 30
  phi__use_energy_before_peak = 1 ;the Kaeppler et al. [2014] thing (see bottom of second par, page 10,170)
  
  KAPPA_FITTER_BLACKBOX,orbit, $
                        ELECTRON_SOURCECONEANGLE=electron_angleRange, $
                        ;; ELECTRON_LOSSCONEANGLE=electron_lca, $
                        MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                        ENERGY_ELECTRONS=energy_electrons, $
                        ENERGY_ELECTRON_TBOUNDS=energy_electron_tBounds, $
                        UPGOING=upgoing, $
                        MIN_PEAK_ENERGY=min_peak_energy, $
                        MAX_PEAK_ENERGY=max_peak_energy, $
                        PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
                        PHI__USE_ENERGY_BEFORE_PEAK=phi__use_energy_before_peak, $
                        EEB_OR_EES=eeb_or_ees, $
                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                        CHI2_THRESHOLD=chi2_thresh, $
                        CHI2_OVER_DOF_THRESHOLD=chi2_over_dof_thresh, $
                        HIGHDENSITY_THRESHOLD=highDens_thresh, $
                        LOWDENSITY_THRESHOLD=lowDens_thresh, $
                        DIFFEFLUX_THRESHOLD=diffEflux_thresh, $
                        N_PEAKS_ABOVE_DEF_THRESHOLD=nPkAbove_dEF_thresh, $
                        RESTORE_FITFILE=restore_fitFile, $
                        T1STR=t1Str, $
                        T2STR=t2Str, $
                        SHOW_POST_PLOTS=show_post_plots, $
                        ONLY_1D_FITS=only_1D_fits, $
                        FIT__LINEAR_ENERGY_SHIFT=fit__linear_energy_shift, $
                        FIT__JE_OVER_E=fit__JE_over_E, $
                        FIT__LES__TAKE_STOCK_OF_RB=fit__LES__take_stock_of_RB, $
                        FIT1D__N_BELOW_PEAK=n_below_peak1D, $
                        FIT1D__N_ABOVE_PEAK=n_above_peak1D, $
                        FIT1D__SOURCECONE_ENERGY_SPECTRUM=fit1D__sourceCone_energy_spectrum, $
                        FIT1D__NFLUX=fit1D__nFlux, $
                        FIT1D__WEIGHTING=fit1D__weighting, $
                        FIT1D__CLAMPTEMPERATURE=fit1D__clampTemperature, $
                        FIT1D__CLAMPDENSITY=fit1D__clampDensity, $
                        FIT1D__SAVE_PLOTSLICES=fit1D__save_plotSlices, $
                        FIT1D__SAVE_EVERY_NTH_PLOT=fit1D__save_every_nth_plot, $
                        FIT1D__SAVE_IF_KAPPA_BELOW=fit1D__save_if_kappa_below, $
                        FIT1D__COMBINE_PLOTSLICES_IN_PDF=fit1D__combine_plotslices_in_PDF, $
                        FIT2D__N_BELOW_PEAK=n_below_peak2D, $
                        FIT2D__N_ABOVE_PEAK=n_above_peak2D, $
                        FIT2D__SHOW_EACH_CANDIDATE=fit2D__show_each_candidate, $
                        FIT2D__SHOW_ONLY_DATA=fit2D__show_only_data, $
                        FIT2D__SAVE_ALL_PLOTS=fit2D__save_all_plots, $
                        FIT2D__WEIGHTING=fit2D__weighting, $
                        FIT2D__CLAMPTEMPERATURE=fit2D__clampTemperature, $
                        FIT2D__CLAMPDENSITY=fit2D__clampDensity, $
                        FIT2D__DENSITY_ANGLERANGE=fit2D__density_angleRange, $
                        FIT2D__TEMPERATURE_ANGLERANGE=fit2D__temperature_angleRange, $
                        FIT2D__FACONDUCTANCE_ANGLERANGE=fit2D__faConductance_angleRange, $
                        FIT2D__ESTIMATE_DENS_ARANGE_FROM_DIST=fit2D__estimate_sourceCone_from_dist, $
                        FIT2D__TEMPERATURE_TYPE=fit2D__temperature_type, $
                        FIT2D__EXTEND_FITSTRUCT_ERANGE=fit2D__extend_fitStruct_eRange, $
                        FIT2D__NFLUX=fit2D__nFlux, $
                        ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                        SAVE_POSTKAPPA_PLOTS=save_postKappa_plots, $
                        SAVEKAPPA_BONUSPREF=bonusPref, $
                        CLOSE_KAPPAPLOTS_AFTER_SAVE=close_kp_after_save, $
                        PLOTDIR=plotDir, $
                        SHOW_STRANGEWAY_SUMMARY=show_Strangeway_summary, $
                        SWAY__SAVE_PS=sway__save_ps, $
                        SWAY__SAVE_PNG=sway__save_png, $
                        SWAY__ADD_KAPPA_PANEL=sway__add_kappa_panel, $
                        SWAY__ADD_CHARE_PANEL=sway__add_chare_panel, $
                        SWAY__ADD_NEWELL_PANEL=sway__add_Newell_panel, $
                        SWAY__ADD_IU_POT=sway__add_iu_pot, $
                        SWAY__LOG_KAPPAPLOT=sway__log_kappaPlot, $
                        SWAY__SPECTROGRAM_UNITS=sway__spectrogram_units, $
                        SHOW_KAPPA_SUMMARY=show_kappa_summary, $
                        KSUM__EANGLE=kSum__eAngle, $
                        KSUM__SAVE_PS=kSum__save_ps, $
                        KSUM__SAVE_PNG=kSum__save_png, $
                        KSUM__CONV_DESPECS_TO_NEWELL_INTERP=kSum__convert_to_Newell_interp, $
                        KSUM__ADD_CHI2_LINE=kSum__add_chi2_line, $
                        KSUM__CHI2BOUNDS=kSum__chi2Bounds, $
                        KSUM__ADD_MEASURED_T_AND_N=kSum__add_meas_T_and_N, $
                        KSUM__GRL=kSum__GRL, $
                        KSUM__OPLOT_POT=kSum__oPlot_pot, $
                        KSUM__SPECTROGRAM_UNITS=kSum__spectrogram_units, $
                        KSUM__ADD_PARM_ERRORS_FROM_FILE=kSum__add_parm_errors_from_file, $
                        KSUM__ADD_PARM_ERRORS__NROLLS=kSum__add_parm_errors__nRolls, $
                        KSUM__ADD_PARM_ERRORS__USE_MOST_PROB=kSum__add_parm_errors__use_most_prob, $
                        OUT_FIT2DK=fit2DK, $
                        OUT_FIT2DGAUSS=fit2DG, $
                        OUT_KAPPAFIT1DSTRUCTS=kappaFit1Ds, $
                        OUT_GAUSSFIT1DSTRUCTS=gaussFit1Ds, $
                        FIT2D_KAPPA_INF_LIST=fit2DKappa_inf_list, $
                        FIT2D_GAUSS_INF_LIST=fit2DGauss_inf_list, $
                        SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file, $
                        LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file, $
                        KAPPA_STATS__SAVE_STUFF=kStats__save_stuff, $
                        KAPPA_STATS__INCLUDE_THESE_STARTSTOPS=kStats__tids,$
                        DEBUG__SKIP_TO_THIS_TIME=debug__skip_to_this_time, $
                        DEBUG__BREAK_ON_THIS_TIME=debug__break_on_this_time, $
                        ORIGINATING_ROUTINE=routName, $
                        CURANDPOT_ANALYSIS=curAndPot_analysis, $
                        CAP_STRUCT=cAP_struct, $
                        TIMEBARS=timeBars, $
                        EPS=eps, $
                        BATCH_MODE=batch_mode

END
