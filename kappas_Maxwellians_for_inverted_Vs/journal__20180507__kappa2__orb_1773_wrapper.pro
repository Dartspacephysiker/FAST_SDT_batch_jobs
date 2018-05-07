;2018/05/07
PRO JOURNAL__20180507__KAPPA2__ORB_1773_WRAPPER

  COMPILE_OPT IDL2,STRICTARRSUBS

  skippersDate = '20180507'
  date = '20180507'
  min_T_streakLen = 30
  max_T_streakLen = 60
  mltRange = [-3.5,1.5]

  spoofDate = 'Spence1773_20180507'

  JOURNAL__20180416__AUTOMATION__SRATE_TO_1_25_OR_2_5,orbit, $
     /NO1DPLOTSPLEASE, $
     /NOCURPOTPLOTSPLEASE, $
     /BOTH_RESTORE_FITFILE_AND_NO_REMAKE_JV_MASTERFILE, $
     CHECKFORSKIPPERS=1, $
     SKIPPERSDATE=skippersDate, $
     BATCH_SETUP__DATE_OF_GENERATION=date, $
     BATCH_SETUP__MLTRANGE=mltRange, $
     BATCH_SETUP__MIN_T_STREAKLEN=min_T_streakLen, $
     BATCH_SETUP__MAX_T_STREAKLEN=max_T_streakLen, $
     BATCH_SETUP__READ_NTOSKIP_FROM_MANUAL_INPUT=read_nToSkip_from_manual_input, $
     BATCH_SETUP__READ_NTOSKIP_FROM_DAILY_FILE=read_nToSkip_from_daily_file, $
     NTOSKIP_FROM_DAILY_FILE__SKIPSTART_FROM_THIS_FILE=nToSkip_from_daily_file__skipStart_from_this_file, $
     SPOOFDATE=spoofDate

END
