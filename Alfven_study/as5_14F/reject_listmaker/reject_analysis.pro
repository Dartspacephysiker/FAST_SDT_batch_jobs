pro reject_analysis,orbit=orbit,interval=interval,do_as3=do_as3,comparison_file=comparison_file,reject_listfile=reject_listfile,$
    tdiff=tdiff,outfile=outfile,chast_arr=chast_arr

;defaults
DEF_OUTFILE="reject_analysis.txt"
DEF_INTERVAL=0
comparison_dir="/SPENCEdata2/Research/Cusp/ACE_FAST/Compare_new_DB_with_Chastons/txtoutput/"
reject_filedir="/home/spencerh/software/sdt/batch_jobs/Alfven_study/as5_14F/reject_listmaker/rej_output/"
comp_base="dflux_"
rej_base="rejects_"
outbase= "rej_analysis_dflux_"

;Use these arrays for find out what the matter is
rej_arr=["Current too low","Delta-b too low","Delta-E too low","ESA_j/delta_bj low",$
  "Jee ionos low","E-over-B/v_Alfvèn"]
  
rej_arr_brev=["cur","db","dE","ESAj/bj","Jee","v_a"]

;handle issues if orbit num not provided
  IF NOT KEYWORD_SET(orbit) THEN BEGIN
    IF NOT KEYWORD_SET(comparison_file) THEN BEGIN
      print, "No orbit or comparison/reject file given! laterz..."
      return
    ENDIF
    
    IF NOT KEYWORD_SET(reject_listfile) THEN BEGIN
      print, "No orbit or rejection list file given! laterz..."
      return
    ENDIF

    IF NOT KEYWORD_SET(outfile) THEN BEGIN
      print, "No outfile specified! Using default: " + def_outfile
      outfile=def_outfile
    ENDIF

  ENDIF ELSE BEGIN
    ;what if no interval specified?
    IF NOT KEYWORD_SET(interval) THEN BEGIN
      PRINT, "No interval specified for orbit " + strcompress(orbit,/REMOVE_ALL) + $
        "! Using default interval: " + STRCOMPRESS(DEF_INTERVAL,/REMOVE_ALL)
      interval=DEF_INTERVAL
    ENDIF

    comp_base += STRCOMPRESS(string(orbit) + "_" + STRING(interval),/REMOVE_ALL)
    outbase += STRCOMPRESS(string(orbit) + "_" + STRING(interval),/REMOVE_ALL)
    
    IF NOT KEYWORD_SET(do_as3) THEN BEGIN
      comparison_dir += "as5/"
      comp_base += "_as5"
      rej_base += "as5_dflux_"
    ENDIF ELSE BEGIN
      comparison_dir += "as3/"
      comp_base += "_as3"
      rej_base += "as3_dflux_"
      outbase += "as3_"
    ENDELSE

    ;slap on suffixes, handle tdiff
    comparison_file = comparison_dir + comp_base
    outfile = STRCOMPRESS(reject_filedir + outbase,/REMOVE_ALL)
    IF KEYWORD_SET(tdiff) THEN BEGIN
      comparison_file += "_tdiff_" + STRCOMPRESS(string(tdiff,format='(D-0.3)'),/REMOVE_ALL)
      outfile += "_tdiff_" + STRCOMPRESS(string(tdiff,format='(D-0.3)'),/REMOVE_ALL)
    ENDIF
    comparison_file += "--time.txt"
    outfile += ".txt"
    

    reject_listfile=reject_filedir+rej_base+STRCOMPRESS(string(orbit) + "_" + STRING(interval),/REMOVE_ALL) + ".txt"


  ENDELSE ;end we DO have orbit

  ;tell the peopl
  PRINT,"Comparison file  : " + comparison_file
  PRINT,"Reject file      : " + reject_listfile
  PRINT,"Outfile          : " + outfile

  IF NOT FILE_TEST(comparison_file) THEN BEGIN
    PRINT, "Unable to open comparison file!! Laterz..."
    RETURN
  ENDIF

  IF NOT FILE_TEST(reject_listfile) THEN BEGIN
    PRINT, "Unable to locate reject file!! Laterz..."
    RETURN
  ENDIF
  
  ;open up the relevant files, now that orbit and interval are specified
  OPENR, comp_lun, comparison_file, /GET_LUN
  OPENW, out_lun, outfile, /GET_LUN
  
  PRINTF,out_lun,SYSTIME()
  PRINTF, out_lun,""
  IF KEYWORD_SET(orbit) THEN BEGIN PRINTF,out_lun,"ORBIT:"+STRCOMPRESS(orbit,/REMOVE_ALL) & PRINTF,out_lun,"" & ENDIF
  PRINTF,out_lun,"Legend:
  PRINTF,out_lun,"**=direct match in our reject file"
  PRINTF,out_lun,"!!=close match (within +/- ~20ms)"
  PRINTF,out_lun,"??=Not a close match. Where is this event?"
  PRINTF,out_lun,""
  PRINTF,out_lun,"Chaston event #             Closest                 Do we have it? Sup?"
  
  ; Read one line at a time, saving the result into chast_arr
  chast_arr = ''
  chast_ind = ''
  line = ''
  
  skip = 7
  ;skip first seven lines
  WHILE skip NE 0 DO BEGIN & $
    READF, comp_lun, line & $
    ;PRINT,line
    skip--
  ENDWHILE
  
  ;Read comparison file to get all Chaston events we don't have
  WHILE NOT EOF(comp_lun) DO BEGIN & $
    READF, comp_lun, line & $
    IF STRMID(line,6,4) EQ '1999' THEN BEGIN
      cur_chast = STRMID(line,6,23)
      cur_ind = STRMID(line,59,3)
      chast_arr = [chast_arr, cur_chast] & $
      chast_ind = [chast_ind, cur_ind]
;      PRINTF,out_lun,format='(A-3," ",A-23)',cur_ind,cur_chast
    ENDIF
  ENDWHILE
  
  FREE_LUN, comp_lun

  ;Now we have times and indices for all Chaston files without events
  ;Let's figure out why they were rejected
  OPENR, rej_lun, reject_listfile, /GET_LUN
  
  skip = 9
  ;skip first seven lines
  WHILE skip NE 0 DO BEGIN & $
    READF, comp_lun, line & $
    PRINT,line
    skip--
  ENDWHILE

  i = 0
  chast_i = 0
  max_close = 10.0 ;seconds
  num_repeat = 0 ;for handling multiple rejection reasons
  cur_reasons=[]
  cur_vals=[]
  WHILE NOT EOF(rej_lun) DO BEGIN & $
    READF, rej_lun, line & $
      
    cur_time = STRMID(line,9,23)
    ;Find closest chast event
    cur_closest = ABS(str_to_time(cur_time) - str_to_time(chast_arr[i]))

    IF cur_closest LE max_close THEN BEGIN ;last index was closest
      IF cur_closest EQ max_close THEN BEGIN ;multiple reasons for rejection
        num_repeat++
        cur_reason = STRMID(line,37,10)
        cur_val = STRMID(line,53,6)
        CASE cur_reason OF
          cur_reason EQ STRMID(rej_arr[0],0,9): cur_reason = rej_arr_brev[0]
          cur_reason EQ STRMID(rej_arr[1],0,9): cur_reason = rej_arr_brev[1]
          cur_reason EQ STRMID(rej_arr[2],0,9): cur_reason = rej_arr_brev[2]
          cur_reason EQ STRMID(rej_arr[3],0,9): cur_reason = rej_arr_brev[3]
          cur_reason EQ STRMID(rej_arr[4],0,9): cur_reason = rej_arr_brev[4]
          cur_reason EQ STRMID(rej_arr[5],0,9): cur_reason = rej_arr_brev[5]  
        ENDCASE
        cur_reasons=[cur_reasons, cur_reason]
        cur_vals=[cur_vals,cur_val]
      ENDIF ELSE max_close = cur_closest ;this one moves us closer 
    
    
    ENDIF ELSE BEGIN ; end current time is LE max_close 
      IF cur_closest GT max_closest THEN BEGIN ;last batch was closest, so figure it out
        WHILE num_repeat GT 0 DO BEGIN
          PRINTF,out_lun,FORMAT='(A-3," ",A-23,)',cur_ind,cur_chast
          num_repeat--
      ENDIF
    ENDELSE
    last_time = cur_time
    
  ENDWHILE

  FREE_LUN, rej_lun

  ; Close the files and free the file units
  FREE_LUN, out_lun
  
  return
end