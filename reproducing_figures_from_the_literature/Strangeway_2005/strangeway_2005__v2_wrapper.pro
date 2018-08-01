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

;; Pre-20180801
;; STRANGEWAY_2005__V2,/SAVE_PS, $
;;                     IONSPECS_UPDOWNMINRATIO=2, $
;;                     IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
;;                     /INTERP_4HZ_RES_TO_1S_TIMESERIES,  $
;;                     /STRANGEWAY_2005_FIG3_PLOT ;; , $
;;                     ;; /ONLY_LEEWARD_IONS
                    

;; POST 20180801
;; for  bonusSuff = '-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s'

thresh_eFlux = 5e5
STRANGEWAY_2005__V2,/SAVE_PS, $
                    IONSPECS_UPDOWNMINRATIO=1, $
                    IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
                    IONSPECS_THRESH_EFLUX=thresh_eFlux, $
                    /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
                    /STRANGEWAY_2005_FIG3_PLOT


;*****************************
;End routine:

exit
end
