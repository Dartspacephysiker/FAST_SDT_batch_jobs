;;05/30/16
;;The original implementation of the Newell et al. [2009] algorithm was off, so I'm here to try to repair the damage.
;;In specific, I was mistakenly requiring that THREE or more bins fall above the 140 eV threshold
PRO JOURNAL__20160530__GET_DIAGS_ON_MISCHARACTERIZED_ESPECS, $
   SAVE_OUTPUT=save_output, $
   DIAGNOSTIC_MODE=diagnostic_mode

  COMPILE_OPT IDL2

  firstOrb           = 500
  lastOrb            = 11100

  Newell_DB_dir    = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/'
  outNewellDir     = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/fixed/'
  Newell_filePref  = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'
  Newell_updatedFP = 'Newell_et_al_identification_of_electron_spectra--ions_included--with_failCodes--Orbit_'

  diagnosticFile   = STRING(FORMAT='(A0,I0,"-",I0,"--",A0,A0)',$
                            'Newell_et_al_identification__diagnostics_of_existing_files--Orbs_', $
                            firstOrb, $
                            lastOrb, $
                            GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                            '.txt')

  IF KEYWORD_SET(diagnostic_mode) THEN BEGIN
     got_first_ions             = 0
     got_first_sc_pot           = 0
     got_first_sc_pot_i         = 0
     keepChecking               = 1

     PRINT,'DIAGNOSTIC MODE: Opening ' + diagnosticFile + '!'

     OPENW,diagLun,outNewellDir+diagnosticFile,/GET_LUN
     PRINTF,diagLun,FORMAT='("Orb/Intvl",T10,"N Energies",T25,"Has SC Pot",T40,"Has ions",T50,"Has SC Pot/i")'
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Loop over all files

  FOR curOrb=firstOrb,lastOrb DO BEGIN
     
     doneski         = 0
     curInterval     = 0
     tempFile        = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
     newFile         = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',outNewellDir,Newell_updatedFP,curOrb,curInterval)
     IF FILE_TEST(tempFile) THEN BEGIN
        WHILE ~doneski DO BEGIN
           
           ;;Clear these so they don't confound us
           jei_up             = !NULL
           out_sc_pot_i       = !NULL
           out_sc_pot         = !NULL
           tmpeSpec_lc        = !NULL
           eSpecs_parsed      = !NULL
           tmpeSpec_lc        = !NULL
           tmpjee_lc          = !NULL
           tmpje_lc           = !NULL
           failCodes          = !NULL
           jei_up             = !NULL
           ji_up              = !NULL 
           iSpec_up           = !NULL
           out_sc_pot         = !NULL
           out_sc_time        = !NULL
           out_sc_min_energy_ind = !NULL
           out_sc_pot_i       = !NULL
           out_sc_time_i      = !NULL
           out_sc_min_energy_ind_i = !NULL

           RESTORE,tempFile
           IF N_ELEMENTS(eSpecs_parsed) EQ 0 THEN STOP

           IF KEYWORD_SET(diagnostic_mode) THEN BEGIN
              has_ions        = N_ELEMENTS(jei_up) GT 0
              has_sc_pot_i    = N_ELEMENTS(out_sc_pot_i) GT 0
              has_sc_pot      = N_ELEMENTS(out_sc_pot) GT 0
              n_energies      = N_ELEMENTS(tmpeSpec_lc.v[0,*])
              
              IF keepChecking THEN BEGIN
                 IF has_ions AND ~got_first_ions THEN BEGIN
                    got_first_ions = curOrb
                 ENDIF
                 IF has_sc_pot AND ~got_first_sc_pot THEN BEGIN
                    got_first_sc_pot = curOrb
                 ENDIF
                 IF has_sc_pot_i AND ~got_first_sc_pot_i THEN BEGIN
                    got_first_sc_pot_i = curOrb
                 ENDIF

                 IF got_first_ions AND got_first_sc_pot AND got_first_sc_pot_i THEN keepChecking = 0
              ENDIF

              PRINTF,diagLun,FORMAT='(I0,"/",I0,T10,I0,T25,I0,T40,I0,T50,I0)',curOrb,curInterval,n_energies,has_sc_pot,has_ions,has_sc_pot_i
              
           ENDIF ELSE BEGIN

              ;;If we don't have s/c pot, assume that the s/c is charged to 50 eV

              IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                                                      mlt,ilat, $
                                                      eSpecs_parsed, $
                                                      SC_POT=out_sc_pot, $
                                                      IND_SC_POT=ind_sc_pot, $
                                                      /QUIET, $
                                                      BATCH_MODE=batch_mode, $
                                                      ORBSTR=orbStr, $
                                                      /PRODUCE_FAILCODE_OUTPUT, $
                                                      OUT_FAILCODES=failCodes, $
                                                      ERRORLOGFILE=badFile



              ;;Save stuff?
              IF KEYWORD_SET(save_output) THEN BEGIN
                 IF N_ELEMENTS(jei_up) GT 0 THEN BEGIN
                    ;;Save the electron stuff
                    PRINT,'Saving Newell file with ions: ' + newFile
                    SAVE,eSpecs_parsed,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                         failCodes, $
                         jei_up,ji_up,iSpec_up, $
                         out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
                         out_sc_pot_i,out_sc_time_i,out_sc_min_energy_ind_i, $
                         FILENAME=newFile
                 ENDIF ELSE BEGIN
                    ;;Save the electron stuff
                    PRINT,'Saving Newell file: ' + newFile
                    SAVE,eSpecs_parsed,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                         failCodes, $
                         out_sc_pot,out_sc_time,out_sc_min_energy_ind, $
                         FILENAME=newFile
                 ENDELSE
              ENDIF

           ENDELSE

           ;;Check for next interval
           curInterval++
           tempFile     = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
           IF ~FILE_TEST(tempFile) THEN doneski  = 1
           newFile      = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_updatedFP,curOrb,curInterval)
        ENDWHILE

     ENDIF

  ENDFOR

  IF KEYWORD_SET(diagnostic_mode) THEN BEGIN
     PRINTF,diagLun,''
     IF got_first_ions THEN BEGIN
        PRINTF,diagLun,FORMAT='("First orbit with ions",T30,": ",I0)',got_first_ions
     ENDIF
     IF got_first_sc_pot THEN BEGIN
        PRINTF,diagLun,FORMAT='("First orbit with SC Pot",T30,": ",I0)',got_first_sc_pot
     ENDIF
     IF got_first_sc_pot_i THEN BEGIN
        PRINTF,diagLun,FORMAT='("First orbit with SC Pot/i",T30,": ",I0)',got_first_sc_pot_i
     ENDIF

     CLOSE,diagLun
     FREE_LUN,diagLun

     PRINT,'Closed ' + diagnosticFile + '!'
  ENDIF



END
