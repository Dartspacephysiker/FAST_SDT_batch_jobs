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

; To get "teams_explore.pro" in the PATH, we need this line:
!PATH='/home/spencerh/software/sdt/batch_jobs/TEAMS/:'+!PATH
      

;*****************************
;Put code here:

fast_t_summary,BW=bw,k0=k0


;*****************************
;End routine:

exit
end
