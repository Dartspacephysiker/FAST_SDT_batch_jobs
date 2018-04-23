; Test wrapper for IDL access from "sdt_batch"

; If you are running on a 32-bit machine,
; uncomment the next line:
; idl -32

; The following commented out lines are probably
; not needed for calling "alfven_stats_5":
;device,decomposed=0
@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

; To get "alfven_stats_5.pro" in the PATH, we need this line:
!PATH='/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs:'+!PATH
      

;*****************************
;Put code here:

JOURNAL__20180423__AUTOMATION_FOR_DAWNDUSK,orbit,/NOCURPOTPLOTSPLEASE,/BATCH_MODE,CHECKFORSKIPPERS=1

IF N_ELEMENTS(orbit GT 0) THEN SPAWN,"cat outIDL.findlowkappa__dawnandduskside_wrapper.pro errIDL.findlowkappa__dawnandduskside_wrapper.pro > /SPENCEdata/software/sdt/batch_jobs/txtOutput/findlowkappa/" + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + "-Orbit_" + STRING(FORMAT='(I0)',orbit) + ".txt"

;*****************************
;End routine:

exit

end
