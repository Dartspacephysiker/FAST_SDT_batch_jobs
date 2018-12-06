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

thresh_eFlux = 5e5
enforce_this_sample_rate = 1.25
energy_electrons_lb = 50

;; indivSuff = '-rawProds_med_dB_sc_and_spinplane_E.sav'

fracBelowThatMustBeUpward = 0.75D
indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat10_thresh5e5_fracBelow0_75.sav'

;; fracBelowThatMustBeUpward = 0.5D
;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat2_thresh5e5_fracBelow0_5.sav'

;; fracBelowThatMustBeUpward = 0.5D
;; indivSuff = '-rawProds_med_dB_sc_and_sp_E__upDownRat5_thresh5e5_fracBelow0_5.sav'

IF GET_TODAY_STRING(/DO_YYYYMMDD_FMT) EQ '20181204' THEN BEGIN
   indivSuff = '-rawProds_med_dB_sc_and_sp_E__SNOTCH_INTERP__upDownRat10_thresh5e5_fracBelow0_75.sav'
   shadow_notch = 1
   sInterp = 1
ENDIF

STRANGEWAY_2005__V3,/SAVE_PS, $
                    IONSPECS_UPDOWNMINRATIO=10, $
                    IONSPECS_MINNUMQUALIFYINGECHANNELS=3, $
                    IONSPECS_THRESH_EFLUX=thresh_eFlux, $
                    IONSPECS_FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
                    /INTERP_4HZ_RES_TO_1S_TIMESERIES, $
                    /STRANGEWAY_2005_FIG3_PLOT, $
                    /REMAKE_DIFF_EFLUX, $
                    /SAVE_INDIVIDUAL_DATA_PRODUCTS_AND_QUIT, $
                    SAVE_INDIVIDUAL_DATA_PRODUCTS__FSUFF=indivSuff, $
                    EFIELD_SHADOW_NOTCH=shadow_notch, $
                    EFIELD_SINTERP=sInterp, $
                    ENERGY_ELECTRONS_LB=energy_electrons_lb


;*****************************
;End routine:

exit
end
