; Test wrapper for IDL access from "sdt_batch"

; The following commented out lines are probably
; not needed for calling "alfven_stats_5":
;device,decomposed=0
@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

!PATH='/home/spencerh/software/sdt/batch_jobs/reproducing_figures_from_the_literature/Strangeway_2005:'+!PATH
      

;*****************************
;Put code here:

STRANGEWAY_2005__USE_ESA_INTERVALS,/BATCH_MODE,/PLOT_NORTH,/SAVE_PS,/SAVE_1S_DATA

;*****************************
;End routine:

exit
end
