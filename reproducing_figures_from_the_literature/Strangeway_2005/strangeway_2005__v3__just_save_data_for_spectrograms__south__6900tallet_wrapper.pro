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

;; PRE 20180801
;; STRANGEWAY_2005__V2,/SAVE_PS, $
;;                     IONSPECS_UPDOWNMINRATIO=1, $
;;                     IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
;;                     /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
;;                     /STRANGEWAY_2005_FIG3_PLOT; , $
;;                     ;; /ONLY_LEEWARD_IONS

;; POST 20180801
;; for  bonusSuff = '-threshEFlux5e5-upDownRatio_1-minNQualECh_3-interp4Hz_to_1s-SOUTH'

thresh_eFlux = 5e5
enforce_this_sample_rate = 1.25
force_SH_tBounds_for_je = 1
energy_electrons_lb = 50

upDownMinRatio = 5

uDMRStr = STRING(FORMAT='(I0)',upDownMinRatio)

;; fracBelowThatMustBeUpward = 0.5D
;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat5_thresh5e5_fracBelow0_5.sav'

fracBelowThatMustBeUpward = 0.75D

;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat' + uDMRStr + '_thresh5e5_fracBelow0_75.sav'

save_individual_data_products__only_db_and_ions = 1
indivSuff = '-rawProds__dB_and_ions__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav'

STRANGEWAY_2005__V3,/SAVE_PS, $
                    IONSPECS_UPDOWNMINRATIO=upDownMinRatio, $
                    IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
                    IONSPECS_THRESH_EFLUX=thresh_eFlux, $
                    IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
                    /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
                    /STRANGEWAY_2005_FIG3_PLOT, $
                    ;; /REMAKE_DIFF_EFLUX, $
                    /SAVE_INDIVIDUAL_DATA_PRODUCTS_AND_QUIT, $
                    SAVE_INDIVIDUAL_DATA_PRODUCTS__ONLY_DB_AND_IONS=save_individual_data_products__only_db_and_ions, $
                    SAVE_INDIVIDUAL_DATA_PRODUCTS__FSUFF=indivSuff, $
                    FORCE_SH_TBOUNDS_FOR_JE=force_SH_tBounds_for_je, $
                    ENERGY_ELECTRONS_LB=energy_electrons_lb


;*****************************
;End routine:

exit
end
