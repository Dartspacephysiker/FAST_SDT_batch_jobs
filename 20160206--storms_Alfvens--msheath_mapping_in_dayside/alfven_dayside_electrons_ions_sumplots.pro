PRO ALFVEN_DAYSIDE_ELECTRONS_IONS_SUMPLOTS
   

  ;;what are we up to?
  hemi                = KEYWORD_SET(hemi) ? hemi : 'north'
  inDir               = '/SPENCEdata/Research/Satellites/FAST/storms_Alfvens/journals/'
  inFile              = 'journal__20160206__times_for_orbs_with_more_than_50_events_on_dayside_during_mainphase__check_for_msheath_electrons__'+hemi+'.sav'
  RESTORE,inDir+inFile

  SET_PLOT_DIR,plotDir,/FOR_SDT, $
               ADD_SUFF='/20160206--storms_Alfvens--msheath_mapping_in_dayside'
  ;; plotDir             = '/SPENCEdata/software/sdt/batch_jobs/20160206--storms_Alfvens--msheath_mapping_in_dayside/plots/'
  plotPref            = 'alfven_dayside_electrons_during_mainphase--'+hemi+'--'
  plotSuff            = '.ps'

  maxAbsI             = 85
  minAbsI             = 55

  threshDiff          = 600        ;If time between plots is less than 5 min, space em out!

  ;; t=0
  ;; dat = get_fa_ees(t,/st)
  ;; IF dat.valid EQ 0 THEN BEGIN
  ;;    print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
  ;;    RETURN
  ;; ENDIF

  ;; ;; Electron current - line plot
  ;; get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons

  ;; ;;remove spurious crap
  ;; get_data,'Je',data=tmpj
  
  ;; keep=where(finite(tmpj.y) NE 0)
  ;; tmpj.x=tmpj.x(keep)
  ;; tmpj.y=tmpj.y(keep)
  
  ;; keep=where(abs(tmpj.y) GT 0.0)
  ;; tx=tmpj.x(keep)
  ;; ty=tmpj.y(keep)
  
  ;; ;;get timescale monotonic
  ;; time_order=sort(tx)
  ;; tx=tx(time_order)
  ;; ty=ty(time_order)

  ;; get_data,'Je',data=je
  ;; get_fa_orbit,/time_array,je.x

  ;; ;;Junk the junk
  ;; get_data,'ILAT',data=ilat
  ;; keep=where(abs(ilat.y) LE maxAbsI AND abs(ilat.y) GE minAbsI )
  ;; store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  ;; ;;now the good stuff
  ;; get_data,'Je',data=je
  ;; get_fa_orbit,/time_array,je.x
  ;; get_data,'ORBIT',data=tmp

  ;;get the orb number
  orbit  = 1535
  ;; orbit=tmp.y[0]
  orbit_num = STRCOMPRESS(orbit,/REMOVE_ALL)
  plotName = plotPref+orbit_num

  ;;Now get the times
  thisInd = WHERE(orbit EQ orbArr)
  IF thisInd[0] EQ -1 THEN BEGIN
     PRINT,'ERROR! No m-sheath Alfven timedata available for orbit ' + orbit + '!!'
     PRINT,'Out ...'
     RETURN
  ENDIF ELSE BEGIN
     t1     = FIX(ORB_STARTSTOPARR[0,thisInd],TYPE=3)
     t2     = FIX(ORB_STARTSTOPARR[1,thisInd],TYPE=3)

     IF (t2-t1) LE threshDiff THEN BEGIN
        PRINT,'Increasing sep. twixt t1 and t2; it is only ' + STRCOMPRESS(t2-t1,/REMOVE_ALL) + ' seconds, after all.'
        ;; maxTime    = MAX(tmp.x,MIN=minTime)
        ;; t1  = (t1 -threshDiff/2) > minTime
        ;; t2  = (t2 + threshDiff/2)  < maxTime
        t1  = t1 -threshDiff/2
        t2  = t2 + threshDiff/2
     ENDIF

     PRINT,'Orbit: ' + orbit_num
     PRINT,'Start: ' + TIME_TO_STR(t1,/MSEC)
     PRINT,'Stop : ' + TIME_TO_STR(t2,/MSEC)
  ENDELSE

  ;; Electron spectrogram - survey data, remove retrace, downgoing electrons
  get_en_spec,"fa_ees",units='eflux',name='el_0',angle=[-22.5,22.5],retrace=1,t1=t1,t2=t2,/calib
  get_data,'el_0', data=tmp                                          ; get data structure
  tmp.y = tmp.y>1.e1                                                 ; Remove zeros
  tmp.y = alog10(tmp.y)                                              ; Pre-log
  store_data,'el_0', data=tmp                                        ; store data structure
  options,'el_0','spec',1                                            ; set for spectrogram
  zlim,'el_0',6,9,0                                                  ; set z limits
  ylim,'el_0',4,40000,1                                              ; set y limits
  options,'el_0','ytitle','e- downgoing !C!CEnergy (eV)'             ; y title
  options,'el_0','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'          ; z title
  options,'el_0','x_no_interp',1                                     ; don't interpolate
  options,'el_0','y_no_interp',1                                     ; don't interpolate
  options,'el_0','yticks',3                                          ; set y-axis labels
  options,'el_0','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  options,'el_0','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  options,'el_0','panel_size',2                                        ; set panel size

  ;; Electron pitch angle spectrogram - survey data, remove retrace, >100 electrons   
  get_pa_spec,"fa_ees_c",units='eflux',name='el_pa',energy=[100,40000],retrace=1,/shift90,t1=t1,t2=t2,/calib
  get_data,'el_pa', data=tmp                               ; get data structure
  tmp.y = tmp.y>1.e1                                       ; Remove zeros
  tmp.y = alog10(tmp.y)                                    ; Pre-log
  store_data,'el_pa', data=tmp                             ; store data structure
  options,'el_pa','spec',1                                 ; set for spectrogram
  zlim,'el_pa',6,9,0                                       ; set z limits
  ylim,'el_pa',-100,280,0                                  ; set y limits
  options,'el_pa','ytitle','e- >100 eV!C!C Pitch Angle'    ; y title
  options,'el_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' ; z title
  options,'el_pa','x_no_interp',1                            ; don't interpolate
  options,'el_pa','y_no_interp',1                            ; don't interpolate
  options,'el_pa','yticks',4                                 ; set y-axis labels
  options,'el_pa','ytickname',['-90','0','90','180','270']   ; set y-axis labels
  options,'el_pa','ytickv',[-90,0,90,180,270]                ; set y-axis labels
  options,'el_pa','panel_size',2                             ; set panel size

  ;; Electron energy flux   
  get_2dt,'je_2d_fs','fa_ees_c',name='JEe',t1=t1,t2=t2,energy=[20,30000]
  ylim,'JEe',1.e-1,1.e1,1                                               ; set y limits
  options,'JEe','ytitle','Electrons!C!Cergs/(cm!U2!N-s)'                ; set y title
  options,'JEe','tplot_routine','pmplot'                                ; set 2 color plot
  options,'JEe','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] ; set color label
  options,'JEe','labflag',3                                               ; set color label
  options,'JEe','labpos',[4.e0,5.e-1]                                     ; set color label
  options,'JEe','panel_size',1                                            ; set panel size

  ;; Electron flux 
  get_2dt,'j_2d_fs','fa_ees_c',name='Je',t1=t1,t2=t2,energy=[20,30000]
  ylim,'Je',1.e7,1.e9,1                                                ; set y limits
  options,'Je','ytitle','Electrons!C!C1/(cm!U2!N-s)'                   ; set y title
  options,'Je','tplot_routine','pmplot'                                ; set 2 color plot
  options,'Je','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] ; set color label
  options,'Je','labflag',3                                               ; set color label
  options,'Je','labpos',[4.e8,5.e7]                                      ; set color label
  options,'Je','panel_size',1                                            ; set panel size

  ;; Ion spectrogram - survey data, remove retrace, upgoing ions
  get_en_spec,"fa_ies_c",units='eflux',name='ion_180',angle=[135,225],retrace=1,t1=t1,t2=t2
  get_data,'ion_180',data=tmp                                           ; get data structure
  tmp.y=tmp.y > 1.                                                      ; Remove zeros
  tmp.y = alog10(tmp.y)                                                 ; Pre-log
  store_data,'ion_180',data=tmp                                         ; store data structure
  options,'ion_180','spec',1                                            ; set for spectrogram
  zlim,'ion_180',5,7,0                                                  ; set z limits
  ylim,'ion_180',3,30000,1                                              ; set y limits
  options,'ion_180','ytitle','i+ 135!Uo!N-180!Uo!N!C!CEnergy (eV)'	; y title
  options,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'          ; z title
  options,'ion_180','x_no_interp',1                                     ; don't interpolate
  options,'ion_180','y_no_interp',1                                     ; don't interpolate
  options,'ion_180','yticks',3                                          ; set y-axis labels
  options,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] ; set y-axis labels
  options,'ion_180','ytickv',[10,100,1000,10000]                          ; set y-axis labels
  options,'ion_180','panel_size',2                                        ; set panel size

  ;; Ion pitch angle spectrogram - survey data, remove retrace, >30 ions 
  get_pa_spec,"fa_ies_c",units='eflux',name='ion_pa',energy=[30,30000],retrace=1,/shift90,t1=t1,t2=t2
  get_data,'ion_pa',data=tmp                              ; get data structure
  tmp.y=tmp.y > 1.                                        ; Remove zeros
  tmp.y = alog10(tmp.y)                                   ; Pre-log
  store_data,'ion_pa',data=tmp                            ; store data structure
  options,'ion_pa','spec',1                               ; set for spectrogram
  zlim,'ion_pa',5,7,0                                     ; set z limits
  ylim,'ion_pa',-100,280,0                                ; set y limits
  options,'ion_pa','ytitle','i+ >30 eV!C!C Pitch Angle'   ; y title
; options,'ion_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV'	; z title
  options,'ion_pa','x_no_interp',1                        ; don't interpolate
  options,'ion_pa','y_no_interp',1                        ; don't interpolate
  options,'ion_pa','yticks',4                             ; set y-axis labels
  options,'ion_pa','ytickname',['-90','0','90','180','270'] ; set y-axis labels
  options,'ion_pa','ytickv',[-90,0,90,180,270]              ; set y-axis labels
  options,'ion_pa','panel_size',2                           ; set panel size

  ;; Ion flux 
  get_2dt,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=[20,30000]
  ylim,'Ji',1.e5,1.e8,1                                      ; set y limits
  options,'Ji','ytitle','Ions!C!C1/(cm!U2!N-s)'              ; set y title
  options,'Ji','tplot_routine','pmplot'                      ; set 2 color plot
  options,'Ji','labels',['Downgoing!C Ions','Upgoing!C Ions '] ; set color label
  options,'Ji','labflag',3                                     ; set color label
  options,'Ji','labpos',[2.e7,1.e6]                            ; set color label
  options,'Ji','panel_size',1                                  ; set panel size

; Get the orbit data
  
  ;; orbit_file=fa_almanac_dir()+'/orbit/predicted'
  ;; get_fa_orbit,t1,t2,orbit_file=orbit_file,/all
  ;; get_data,'ORBIT',data=tmp
  ;; orbit=tmp.y(0)
  ;; orbit_num=strcompress(string(tmp.y(0)),/remove_all)
  
  ;; Plot the data
  ;; loadct2,43
  tplot,['el_0','el_pa','ion_180','ion_pa','JEe','Je','Ji'],$
        var_label=['ALT','ILAT','MLT'],title='FAST ORBIT '+orbit_num

  popen,/port,plotDir+plotName
  ;; loadct2,43
  ;; tplot,['el_0','el_pa','ion_180','ion_pa','JEe','Je','Ji'],$
  ;;       var_label=['ALT','ILAT','MLT'],title='FAST ORBIT '+orbit_num
  tplot
  pclose
  
  RETURN 
END
