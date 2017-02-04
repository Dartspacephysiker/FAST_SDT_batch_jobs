PRO ALFVEN_STATS_5__JUST_GET_INTERVAL_TIMES_AND_SAVE, $
   ENERGY_ELECTRONS=energy_electrons, $
   T1=t1,T2=t2, $
   ALTERNATIVE_OUTDIR=altOutDir, $
   ALTERNATIVE_FILEPREFIX=altPrefix, $
   DUPEREPORTDIR=dupeReportDir

  COMPILE_OPT idl2

  eesa_dir             = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/eesa_time_intervals/'
  todayStr             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  defDir               = eesa_dir + 'je_time_ind_dir__noDupes/'
  ;; defFilePref       = "je_and_cleaned_time_range_indices--orbit_"
  defFilePref          = "je_and_cleaned_time_range_indices--noDupes--orbit_"

  filePref             = KEYWORD_SET(altPrefix) ? altPrefix : defFilePref

  dupeReportFile       = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--DUPE_REPORT--as5__just_get_interval_times_and_save.txt'

  IF KEYWORD_SET(altOutDir) THEN BEGIN
     IF FILE_TEST(altOutDir) THEN BEGIN
        outDir         = altOutDir
     ENDIF ELSE BEGIN
        PRINT,"Provided directory doesn't exist! Switching to default."
        outDir         = defDir
     ENDELSE
  ENDIF ELSE BEGIN
     outDir            = defDir
  ENDELSE

  IF ~KEYWORD_SET(dupeReportDir) THEN BEGIN
     dupeReportDir     = './'
  ENDIF

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons  = [0.,30000.]                           ;use 0.0 for lower bound since the sc_pot is used to set this
  ENDIF

  ;; If no data exists, return to main
  ;; t=0
  ;; dat                                 = get_fa_ees(t,/st)
  ;;Jack Vernetti's recommendation
  dat                                    = GET_FA_EES(0.0, EN=1)
  IF dat.valid EQ 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- GET_FA_EES(0.0, EN=1) returned invalid data'
    RETURN
  ENDIF ELSE BEGIN
     n_EESA_spectra                      = dat.index+1
     last_index                          = LONG(dat.index)
  
     PRINT,'There are ' + STRCOMPRESS(n_EESA_spectra,/REMOVE_ALL) + ' EESA survey spectra currently loaded in SDT...'
  ENDELSE

  t1                                     = 0.0D
  t2                                     = 0.0D
  temp                                   = GET_FA_EES(t1,INDEX=0.0D)
  temp                                   = GET_FA_EES(t2,INDEX=DOUBLE(last_index))
  GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Welp, here we go!

  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep                                   = WHERE(FINITE(tmpj.y))
  tmpj.x                                 = tmpj.x[keep]
  tmpj.y                                 = tmpj.y[keep]
  keep                                   = WHERE(ABS(tmpj.y) GT 0.0)
  tx                                     = tmpj.x[keep]
  ty                                     = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order                             = SORT(tx)
  tx                                     = tx[time_order]
  ty                                     = ty[time_order]
  
  ;;kill dupes
  dupe_i      = WHERE(ABS(tx[1:-1]-tx[0:-2]) LT 0.0001,nDupes, $
                      COMPLEMENT=keep,NCOMPLEMENT=nKeep)
  PRINT,STRCOMPRESS(nDupes,/REMOVE_ALL) + ' Je duplicates here'
  tx          = tx[keep]
  ty          = ty[keep]
  
  ;;Ensure no duplicates
  nDupes      = N_ELEMENTS(tx) - N_ELEMENTS(UNIQ(tx))
  IF nDupes GT 0 THEN BEGIN
     nOrig    = N_ELEMENTS(tx)
  ENDIF

  ;;throw away the first 10 points since they are often corrupted
  STORE_DATA,'Je',DATA={x:tx[10:N_ELEMENTS(tx)-1],y:ty[10:N_ELEMENTS(tx)-1]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je                            = MAKE_ARRAY(N_ELEMENTS(Je.x),/DOUBLE)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j]                      = ABS(Je.x[j]-Je.x[j-1])
  ENDFOR
  part_res_Je[0]                         = part_res_Je[1]
  gap                                    = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start                      = [0,where(part_res_je GT 10.0)]
     separate_stop                       = [where(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start                      = [0]
     separate_stop                       = [N_ELEMENTS(Je.x)-1]
  ENDELSE

  ;;remove esa burp when switched on
  turn_on                                = WHERE(part_res_je GT 300.0)
  IF turn_on[0] NE -1 THEN BEGIN
     turn_on_separate                    = MAKE_ARRAY(N_ELEMENTS(turn_on),/DOUBLE)
     FOR j=0,N_ELEMENTS(turn_on)-1 DO BEGIN
        turn_on_separate[j]              = WHERE(separate_start EQ turn_on[j])
     ENDFOR
     separate_start[turn_on_separate+1]  = separate_start[turn_on_separate+1]+5
  ENDIF

  ;;identify time indices for each interval
  count                                  = 0.0
  FOR j=0,N_ELEMENTS(separate_start)-1 DO BEGIN
     IF (separate_stop[j]-separate_start[j]) GT 10 THEN BEGIN
        count                            = count+1
        IF count EQ 1.0 THEN BEGIN
           time_range_indices            = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        ENDIF ELSE BEGIN
           time_range_indices            = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        ENDELSE
     ENDIF
  ENDFOR
  
  ;;identify interval times
  time_ranges                            = je.x[time_range_indices]
  number_of_intervals                    = N_ELEMENTS(time_ranges[*,0])
  
  print,'number_of_intervals',number_of_intervals
  
  
  GET_FA_ORBIT,je.x[time_range_indices[0,0]],je.x[time_range_indices[0,1]]
  ;;now get orbit quantities
  GET_DATA,'ORBIT',DATA=orb
  orbit_num                              = orb.y[0]
  orbStr                                 = STRCOMPRESS(orbit_num,/REMOVE_ALL)
  outFile                                = STRING(FORMAT='(A0,I0,"--",I0,"_intervals.sav")', $
                                                  filePref,orbit_num,number_of_intervals)

  PRINT,'Saving ' + outFile + ' ...'
  SAVE,je,orbit_num,time_range_indices,number_of_intervals,time_ranges, $
       FILENAME=outDir+outFile

  ;;...and if there were dupes, report that too
  IF nDupes GT 0 THEN BEGIN
     OPENW,dupeLun,dupeReportDir+dupeReportFile,/GET_LUN,/APPEND
     PRINTF,dupeLun,FORMAT='(I0,T10,I0,T20,I0)',orbit_num,nOrig,nDupes
     CLOSE,dupeLun
     FREE_LUN,dupeLun
  ENDIF

  RETURN 
END
