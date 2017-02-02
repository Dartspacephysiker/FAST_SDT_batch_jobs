;2016/07/18 OK, now we've got three solid fits from 
PRO JOURNAL__20160718__ORBIT_1849__ANGULAR_DENSITY_DISTRIBUTIONS__INTERP_THE_BAD_ANGLES

  COMPILE_OPT idl2

  inDir   = '~/software/sdt/batch_jobs/saves_output_etc/'
  fitFile = '20160718--McFadden_et_al_1998--Kappa_fits_and_Gauss_fits--ees--fit_each_angle--solid_fits_from_three_times.sav'

  RESTORE,inDir+fitFile

  ;; dataStrs  = MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthPackage[0],TIME_INDS=[0:2])
  dataStrs  = synthPackage[0,0:2]
  kappaStrs = synthPackage[1,0:2]
  gaussStrs = synthPackage[2,0:2]

  ;; kappaStrs = MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthPackage[1],TIME_INDS=[0:2])
  ;; gaussStrs = MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX(synthPackage[2],TIME_INDS=[0:2])


  time_index = 0
  ind        = 0

  curKappas  = !NULL

  PARSE_KAPPA_FIT_STRUCTS,kappaFits, $
                          A=a, $
                          TIME=time, $
                          TINDEX=tIndex, $
                          BULKANGLEINF=bulkAngleInf, $
                          STRUCT_A=Astruct, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus

   ;; plot(angles[0:14],astruct.temp[0:14],SYMBOL='*',LINESTYLE=6)
   ;; plot(angles[0:14],astruct.n[0:14],SYMBOL='*',LINESTYLE=6)

  STOP
END