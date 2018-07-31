;2018/07/30
;; How badly does ion drift affect things? Få se
PRO JOURNAL__20180730__INVESTIGATE_SH_ORB_2351

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; GET_E_PERP,'1997-03-26/21:15:00','1997-03-26/21:35:00'

  upDownMinRatio = 2
  minNumQualifyingEChannels = 3
  enforce_this_sample_rate = 5.0

  save_ps = 1
  ;; only_leeward_ions = 1
  ;; only_cone_ions = 1

  JOURNAL__20180720__LOOK_AT_CONIC_VS_ALL_FLUX_RATIOS, $
     UPDOWNMINRATIO=upDownMinRatio, $
     MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
     SAVE_PS=save_ps, $
     NO_PLOTS=no_plots, $
     ;; /QUIT_IF_FILE_EXISTS, $
     ONLY_LEEWARD_IONS=only_leeward_ions, $
     ONLY_CONE_IONS=only_cone_ions, $
     ENFORCE_THIS_SAMPLE_RATE=enforce_this_sample_rate, $
     ESPECALL=eSpec, $
     ESPECUP=eSpecUp, $
     ESPECDOWN=eSpecDown, $
     UPDOWNRATIOSPEC=upDownRatioSpec, $
     UPALLRATIOSPEC=upAllRatioSpec, $
     EBOUND=eBound, $
     IONMOMSTRUCT=ionMomStruct, $
     IONUPJ=ionUpJ, $
     UP_ARANGEN=up_aRangeN, $
     DOWN_ARANGEN=down_aRangeN, $
     UP_ARANGES=up_aRangeS, $
     DOWN_ARANGES=down_aRangeS, $
     MISLYKTES=mislyktes


  t1Str = '1997-03-26/21:22:30'
  t1 = S2T(t1Str)
  dat = GET_FA_IES_C(t1,/ADVANCE)

  ;; CONTOUR2D,dat,/POLAR,/LABEL
  CONTOUR2D,dat,/POLAR,/LABEL,LIMITS={xRange:[-3,3],yRange:[-3,3]},/FILL,NCONT=10

  ;; PLOT_FA_CROSSING,ORBIT=2351,/SOUTH,/ALMANAC_INFO,/FILL,/MAGPOLE,XMARK='1997-03-26/21:22:30'

  STOP

END
