; Test wrapper for IDL access from "sdt_batch"

; The following commented out lines are probably
; not needed for calling "alfven_stats_5":
;device,decomposed=0
@startup
;loadct2,43
;cols=get_colors()
;time_stamp,off=1

!PATH='/home/spencerh/software/sdt/batch_jobs/ISSI_Team_438:'+!PATH
      

;*****************************
;Put code here:

thresh_eFlux = 5e5
enforce_this_sample_rate = 1.25
energy_electrons_lb = 50

;; indivSuff = '-rawProds_med_dB_sc_and_spinplane_E.sav'

fracBelowThatMustBeUpward = 0.75D

upDownMinRatio = 5
uDMRStr = STRING(FORMAT='(I0)',upDownMinRatio)

;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav'
;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav'

;; save_individual_data_products__only_db_and_ions = 1
indivSuff = '-ion_beams_and_conics__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav'

;; TRY LEEWARD??
tryLeeward = 0
IF KEYWORD_SET(tryLeeward) THEN BEGIN & $
indivSuff = '-rawProds__dB_and_ions__leeward__upDownRat'+uDMRStr+'_thresh5e5_fracBelow0_75.sav' & $
ENDIF   


GET_ION_BEAMS_AND_CONICS,/SAVE_PS, $
                    IONSPECS_UPDOWNMINRATIO=upDownMinRatio, $
                    IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
                    IONSPECS_THRESH_EFLUX=thresh_eFlux, $
                    IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
                    ;; ONLY_LEEWARD_IONS=tryLeeward, $
                    ;; /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
                    ;; /STRANGEWAY_2005_FIG3_PLOT, $
                    ;; /REMAKE_DIFF_EFLUX, $
                    MAKE_IONS_OXYGEN=make_ions_oxygen;;, $


;*****************************
;End routine:

exit
end
