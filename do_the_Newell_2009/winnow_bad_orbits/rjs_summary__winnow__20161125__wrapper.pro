@startup

!path = !path+':'+expand_path('+$FASTHOME/idl')

;.run /disks/gpc2/home/sdt/sdt/strangeway/batch_summary.pro
.run /home/spencerh/software/sdt/batch_jobs/jobs_for_summaries/summary_pros/batch_rjs_summary.pro

batch_rjs_summary,tplot_vars=tplot_vars,tlimit_north=tlimit_north,tlimit_south=tlimit_south,tlimit_all=tlimit_all

loadct2,40

hemisph = getenv('FAST_ORBIT_HEMISPHERE')

get_data,'ORBIT',data=orb_tmp
orbit_num=strcompress(string(orb_tmp.y(0)),/remove_all)
; print,'orbit_num= ', orbit_num
out_fname='esummary_orbit_'+orbit_num+'_'+hemisph+'.plt'
print,'out_fname= ', out_fname

SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/eSpec_winnowing/20161125/'

if n_elements(tplot_vars) gt 0 then begin
    popen, plotDir + out_fname
    loadct2,40
    print_options,/port
    tplot,tplot_vars,var=['ALT','ILAT','MLT']
    pclose
endif

; if (n_elements(tlimit_north) gt 0) then tlimit,tlimit_north  ;

; if (n_elements(tlimit_south) gt 0) then tlimit,tlimit_south  ;

exit
