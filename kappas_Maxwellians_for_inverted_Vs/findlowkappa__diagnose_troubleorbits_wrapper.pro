; Test wrapper for IDL access from "sdt_batch"

; If you are running on a 32-bit machine,
; uncomment the next line:
; idl -32

;device,decomposed=0
@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

; To get pro in the PATH, we need this line:
!PATH='/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs:'+!PATH
      

;*****************************
;Put code here:

JOURNAL__20180302__WARMUP_TO_AUTOMATION,orbit, $
   /RESTORE_FITFILE_AND_REMAKE_JV_MASTERFILE

IF N_ELEMENTS(orbit GT 0) THEN SPAWN,"cat outIDL.findlowkappa__diagnose_troubleorbits_wrapper.pro errIDL.findlowkappa__diagnose_troubleorbits_wrapper.pro > /SPENCEdata/software/sdt/batch_jobs/txtOutput/findlowkappa/" + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + "-troubleOrbit_" + STRING(FORMAT='(I0)',orbit) + ".txt"

;*****************************
;End routine:

exit

end
