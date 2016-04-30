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
;!PATH='/home/spencerh/software/sdt/batch_jobs/Alfven_study/as5_14F:'+!PATH
!PATH='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/as5--add_later_orbs:'+!PATH
      

;*****************************
;Put code here:

ALFVEN_STATS_5__LATERORBS,/KEEP_ALFVEN_ONLY,/BELOW_AURORAL_OVAL,/DONTSHOWPLOTS,/UCLA_MAG_DESPIN

;*****************************
;End routine:

exit

end
