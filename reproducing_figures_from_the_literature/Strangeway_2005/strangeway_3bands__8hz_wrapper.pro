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

STRANGEWAY_3BANDS__8HZ,/SAVE_PS,/DECIMATE_E_AND_B__THEN_CALC_PFLUX,/USE_JE_TBOUNDS,/SKIPDSP,/BATCH_MODE

;*****************************
;End routine:

exit
end
