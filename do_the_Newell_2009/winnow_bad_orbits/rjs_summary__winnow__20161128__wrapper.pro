@startup

!path = !path+':'+expand_path('+$FASTHOME/idl')

SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/eSpec_winnowing/20161128/'

;.run /disks/gpc2/home/sdt/sdt/strangeway/batch_summary.pro
.run /home/spencerh/software/sdt/batch_jobs/jobs_for_summaries/summary_pros/batch_rjs_summary.pro

batch_rjs_summary,tPlt_vars=tPlt_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all,/FORCE_PAST_9936,/SKIP_EXISTING,GOT_SKIPPED=got_skipped,PLOTDIR=plotDir

loadct2,40

hemisph = getenv('FAST_ORBIT_HEMISPHERE')

get_data,'ORBIT',data=orb_tmp
;; orbit_num=strcompress(string(orb_tmp.y(0)),/remove_all)
orbit_num=STRING(FORMAT='(I05)',orb_tmp.y(0))
; print,'orbit_num= ', orbit_num
out_fname='esummary_orbit_'+orbit_num+'_'+hemisph+'.plt'

if (n_elements(tPlt_vars) gt 0) AND (~FILE_TEST(plotDir+out_fname+'.ps')) then BEGIN & $
   IF KEYWORD_SET(got_skipped) THEN BEGIN & $ 
      PRINT,"Orb " + orbit_num + ": Got skipped by RJS summary!" & $
   ENDIF ELSE BEGIN & $
      print,'out_fname= ', out_fname & $
      popen, plotDir + out_fname & $
      loadct2,40 & $
      print_options,/port & $
      tplot,tPlt_vars,var=['ALT','ILAT','MLT'] & $
      pclose & $
   ENDELSE & $
   ENDIF 

; if (n_elements(tlimit_north) gt 0) then tlimit,tlimit_north  ;

; if (n_elements(tlimit_south) gt 0) then tlimit,tlimit_south  ;

exit
