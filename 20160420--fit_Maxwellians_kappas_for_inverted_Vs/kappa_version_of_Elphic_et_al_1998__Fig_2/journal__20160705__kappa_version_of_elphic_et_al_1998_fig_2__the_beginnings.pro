;;07/05/16
;;Elphic et al. [1998] use orbit 1773 between 0925:40 and 0929:30 UT on February 1, 1997. I don't have that whole interval
;;saved, but maybe I have enough. I hope I do.
PRO JOURNAL__20160705__KAPPA_VERSION_OF_ELPHIC_ET_AL_1998_FIG_2__THE_BEGINNINGS

  COMPILE_OPT IDL2

  datDir  = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/kappa_fits/'
  datFile = '20160616--eSpec_eflux__and_nFlux--ees--orb_1773__1997-02-01-09_26_20__000-1997-02-01-09_26_50__000.sav'

  RESTORE,datDir+datFile

END
