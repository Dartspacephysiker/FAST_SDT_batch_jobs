@startup
@info__orbits_with_big_northward_imf__20161129.pro

!path = !path+':'+expand_path('+$FASTHOME/idl')

SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/big_Poynting_flux_for_given_IMF_conds/' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '/'

;.run /disks/gpc2/home/sdt/sdt/strangeway/batch_summary.pro
.run /home/spencerh/software/sdt/batch_jobs/jobs_for_summaries/summary_pros/batch_rjs_summary.pro

batch_rjs_summary, $
   tPlt_vars=tPlt_vars, $
   tlimit_north=tlimit_north, $
   tlimit_south=tlimit_south,tlimit_all=tlimit_all, $
   /FORCE_PAST_9936, $
   /SKIP_EXISTING, $
   GOT_SKIPPED=got_skipped, $
   PLOTDIR=plotDir

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

tlimlist = LIST()
tlimName = !NULL
IF (N_ELEMENTS(tlimit_north) GT 0) THEN BEGIN & tLimList.Add,tlimit_north & tlimName = [tlimName,'NORTH'] & ENDIF

IF (N_ELEMENTS(tlimit_south) GT 0) THEN BEGIN & tLimList.Add,tlimit_south & tlimName = [tlimName,'SOUTH'] & ENDIF

FOR k=0,N_ELEMENTS(tlimName)-1 DO BEGIN & $

       hemisph = tlimName[k] & $

       out_fname='esummary_orbit_'+orbit_num+'_'+hemisph+'.plt' & $
       PRINT,'out_fname= ', out_fname & $
       POPEN, plotDir + out_fname & $
       LOADCT2,40 & $
       PRINT_OPTIONS,/port & $
       TLIMIT,tLimList[k] &  $
;;       TPLOT,tPlt_vars,var=['ALT','ILAT','MLT'] & $
       PCLOSE & $
ENDFOR
exit
