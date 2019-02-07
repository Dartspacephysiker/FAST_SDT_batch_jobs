;2019/02/07
PRO JOURNAL__20190207__TRY_CONVERTING_B_GEI_TO_B_GEO

  COMPILE_OPT IDL2,STRICTARRSUBS

  orbit = 8276
  orbStr = STRING(FORMAT='(I0)',orbit)

  tryDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/'
  coordDir = '/SPENCEdata/Research/database/temps/'

  tryFile = 'Orbit_'+orbStr+'-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav'

;; /SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_et_al_2005/V3/rawProds/Orbit_8276-rawProds__dB_and_ions__upDownRat5_thresh5e5_fracBelow0_75.sav

  RESTORE,tryDir+tryFile
  RESTORE,coordDir+"tmpGEOMAGcoords_"+orbStr+".sav"

  posGEI = db.fa_pos
  posGEO = TRANSPOSE(coords.GEO)


  R_E = 6371.2D

  testInd = 0
  GEOPosMag = SQRT(TOTAL(coords.geo[*,testInd]^2.))
  GEIPosMag = SQRT(TOTAL(db.fa_pos[testInd,*]^2.))
  ;; PRINT,GEIPosMag-GEOPosMag
  PRINT,GEIPosMag-R_E,dB.alt[testInd]

  STOP

END
