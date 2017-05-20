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

STRANGEWAY_2005__APPENDIX_A,/SAVE_PS,/INTERP_4HZ_RES_TO_1S_TIMESERIES,/USE_EFIELD_FIT_VARIABLES

;*****************************
;End routine:

exit
end
