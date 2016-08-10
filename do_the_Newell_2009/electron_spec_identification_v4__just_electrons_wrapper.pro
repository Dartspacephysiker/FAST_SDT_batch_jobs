; Test wrapper for IDL access from "sdt_batch"

; If you are running on a 32-bit machine,
; uncomment the next line:
; idl -32

; The following commented out lines are probably
; not needed for calling "alfven_stats_5":
;device,decomposed=0
;@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

; To get "alfven_stats_5.pro" in the PATH, we need this line:
;!PATH='/home/spencerh/software/sdt/batch_jobs/Alfven_study/as5_14F:'+!PATH
;; !PATH='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun:'+!PATH
!PATH='/SPENCEdata/software/sdt/batch_jobs/do_the_Newell_2009:'+!PATH
      

;*****************************
;Put code here:

ELECTRON_SPEC_IDENTIFICATION_V4__JUST_ELECTRONS,EEB_OR_EES='ees'

;*****************************
;End routine:

exit

end
