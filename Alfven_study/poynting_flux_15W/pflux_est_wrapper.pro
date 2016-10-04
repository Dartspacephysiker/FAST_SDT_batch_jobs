; Test wrapper for IDL access from "sdt_batch"

; If you are running on a 32-bit machine,
; uncomment the next line:
; idl -32

; The following commented out lines are probably
; not needed for calling "pflux_estimation":
;device,decomposed=0
@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

; To get "pflux_estimation.pro" in the PATH, we need this line:
;!PATH='/home/spencerh/software/sdt/batch_jobs/Alfven_study/as5_14F:'+!PATH
!PATH='/SPENCEdata/software/sdt/batch_jobs/Alfven_study/poynting_flux_15W/:'+!PATH
      

.compile alfven_stats_6_spectral

;*****************************
;Put code here:

ALFVEN_STATS_6_SPECTRAL,/BIGWINDOW

;*****************************
;End routine:

exit
end
