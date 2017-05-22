;2017/05/22
PRO STRANGEWAY_5BANDS__CHUNK_OFF_EPHEM_AND_EPHEM_EXTENDED

  COMPILE_OPT IDL2,STRICTARRSUBS

  DBDir             = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/Strangeway_5bands/'
  DBFile            = 'Strangeway_5bands__orbs_1436-5382_EESAItvl.sav'
  DBEphemFile       = 'Strangeway_5bands__orbs_1436-5382_EESAItvl__ephem.sav'
  DBEphemExtFile    = 'Strangeway_5bands__orbs_1436-5382_EESAItvl__ephem_extended.sav'

  RESTORE,DBDir + DBFile

  ;;Flatten ephem thing
  ephem  = {time     : leMaitre.ephem.time  , $
            orbit    : leMaitre.ephem.orbit , $
            alt      : leMaitre.ephem.alt   , $
            mlt      : leMaitre.ephem.mlt   , $
            ilat     : leMaitre.ephem.ilat  , $
            magRatio : leMaitre.ephem.magRatio}

  ephemExt = {fa_pos : leMaitre.ephem.fa_pos, $
              fa_vel : leMaitre.ephem.fa_vel, $
              ilng   : leMaitre.ephem.ilng  , $
              lat    : leMaitre.ephem.lat   , $
              lng    : leMaitre.ephem.lng   , $
              flat   : leMaitre.ephem.flat  , $
              flng   : leMaitre.ephem.flng  , $
              b_model: leMaitre.ephem.b_model}

  IF (N_ELEMENTS(TAG_NAMES(ephem)) + N_ELEMENTS(TAG_NAMES(ephemExt))) NE N_ELEMENTS(TAG_NAMES(leMaitre.ephem)) THEN BEGIN
     HELP,leMaitre
     HELP,ephem
     HELP,ephemExt
     
     PRINT,''
     PRINT,"Someone is getting skipped. Better check it out."
     STOP
  ENDIF

  PRINT,dbdir+dbEphemExtFile
  PRINT,dbdir+dbEphemFile
  PRINT,"Check and make sure you're OK with what's happening"

  STOP

  STR_ELEMENT,leMaitre,'ephem',/DELETE

  SAVE,ephem   ,FILENAME=dbdir+dbEphemFile
  SAVE,ephemExt,FILENAME=dbdir+dbEphemExtFile
  SAVE,leMaitre,FILENAME=dbdir+dbFile

END
