; Test wrapper for IDL access from "sdt_batch"

; If you are running on a 32-bit machine,
; uncomment the next line:
; idl -32

; The following commented out lines are probably
; not needed for calling "alfven_Stats_3":
;device,decomposed=0
;@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

; To get "get_and_plot_jmag_jesa.pro" in the PATH, we need this line:
!PATH='/home/spencerh/software/sdt/batch_jobs/Alfven_study_14F/get_magcurrent_plots:'+!PATH
      

;*****************************
;Put code here:

get_and_plot_jmag_jesa,/do_chastplots

;*****************************
;End routine:

exit
end
