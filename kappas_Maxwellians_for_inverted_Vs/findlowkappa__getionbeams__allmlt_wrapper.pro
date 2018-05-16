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

; To get "alfven_stats_5.pro" in the PATH, we need this line:
!PATH='/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs:'+!PATH

;*****************************
;Put code here:

min_T_streakLen = 30
mltRange = [0,24]

;; skippersDate = '20180507'
date = '20180516'
checkForSkippers = 1

;; 2018/05/08 redoers
;; skippersDate = '20180507'
;; date = '20180516'
;; checkForSkippers = 1

JOURNAL__20180416__AUTOMATION__SRATE_TO_1_25_OR_2_5,orbit, $
   /NO1DPLOTSPLEASE, $
   /NOCURPOTPLOTSPLEASE, $
   /BATCH_MODE, $
   ;; /BOTH_RESTORE_FITFILE_AND_NO_REMAKE_JV_MASTERFILE, $
   CHECKFORSKIPPERS=checkForSkippers, $
   SKIPPERSDATE=skippersDate, $
   BATCH_SETUP__DATE_OF_GENERATION=date, $
   BATCH_SETUP__MLTRANGE=mltRange, $
   BATCH_SETUP__MIN_T_STREAKLEN=min_T_streakLen, $
   BATCH_SETUP__MAX_T_STREAKLEN=max_T_streakLen, $
   BATCH_SETUP__READ_NTOSKIP_FROM_MANUAL_INPUT=read_nToSkip_from_manual_input, $
   BATCH_SETUP__READ_NTOSKIP_FROM_DAILY_FILE=read_nToSkip_from_daily_file, $
   NTOSKIP_FROM_DAILY_FILE__SKIPSTART_FROM_THIS_FILE=nToSkip_from_daily_file__skipStart_from_this_file

IF N_ELEMENTS(orbit GT 0) THEN SPAWN,"cat outIDL.findlowkappa__nowautomated__getionbeams__min30secstreak_wrapper.pro errIDL.findlowkappa__nowautomated__getionbeams__min30secstreak_wrapper.pro > /SPENCEdata/software/sdt/batch_jobs/txtOutput/findlowkappa/" + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + "-Orbit_" + STRING(FORMAT='(I0)',orbit) + "-ionbeams.txt"

;*****************************
;End routine:

exit

end
