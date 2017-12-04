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
!PATH='/SPENCEdata/software/sdt/batch_jobs/eesa_time_intervals:'+!PATH
      

;*****************************
;Put code here:

;; ALFVEN_STATS_5__ELECTRON_SPEC_IDENTIFICATION_V2,/BATCH_MODE,/INCLUDE_IONS
GET_EESA_INTERVAL_TIMES_AND_SAVE,/GET_DIFF_EFLUX_TOO

;*****************************
;End routine:

exit

end
