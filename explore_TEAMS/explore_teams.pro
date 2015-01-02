pro explore_teams
  
  exp_teams_dir = '/SPENCEdata2/software/sdt/batch_jobs/explore_TEAMS/'

;;************************************************************************************
;;  Routines for plotting data
;;  Note that because TEAMS survey and high mass resolution data are at much lower time resolutions than ESA data, 
;;  you should set the GAP_TIME keyword to ensure that slow survey data are correctly plotted. 
;;Adequate values are 30 for H+/O+ survey data, 60 for He survey data, and 120 for high mass resolution data.
  
;;   get_en_spec.pro
;;   Plots an energy spectrogram. Can be called with any TEAMS get routine. See the ESA help page for more information. Remember to specify four angles (theta min and max, phi min and max) if you set the ANGLE keyword when calling with 3-D survey data. For example, to plot an energy spectrogram of alpha pole data in differential energy flux units, type
;;   get_en_spec, 'fa_tpa', units = 'eflux', gap_time = 60

;;   get_pa_spec.pro
;;   Plots a pitch angle spectrogram. Call only with the *_eq.pro get routines. See the ESA help page for more information. For example, to plot a pitch angle spectrogram of protons below 1 keV, type
;;   get_pa_spec, 'fa_tsp_eq', energy = [1, 1000], gap_time = 30
  
;;   get_tms_hm_spec.pro
;;   Plots a mass spectrogram. Call only with get_fa_th_3d.pro. Keywords are the same as for get_en_spec.pro and get_pa_spec.pro. The simplest calling sequence is
;;   get_tms_hm_spec, 'fa_th_3d', gap_time = 120

;;   conv_units.pro
;; Function to change the units of your data structure to something more convenient. For example, if you want differential number flux, call
;;   fluxdat = conv_units(dat, 'flux')

;;************************************************************************************
;; Moments routines

;; There are routines n_3d.pro, j_3d.pro, ... which are analogous to the 2-D moment routines for ESA data. 
;;   Call as in the following example:
;;     get_2dt, 'n_3d', 'fa_tsp'
;;   which gives the proton density. 
;; It is also possible, but not recommended for reasons given in the Pitfalls and Limitations section, to call
;;   get_2dt, 'n_2d', 'fa_tsp_eq'
;; Do not attempt to take 2-D moments of 3-D data, 3-D moments of 2-D data, or any moments of pole or burst data--
;;   this puts garbage in and you will get garbage out.

;;***********************************************************************************
;; TEAMS get routines

;; The keywords for these routines are similar to the keywords for the ESA get routines.

;; get_fa_t??.pro
;; The first ? stands for data type: s for survey, b for burst, and p for pole. The second ? stands for species: p for protons, a for alphas (He++), h for He+, and o for oxygen. These are the general-purpose get routines. For example, to get the first sample of burst oxygen data in SDT (time must be defined in advance; it can be initialized to 0.D):
;; oburstsamp = get_fa_tbo(time, /start)

;; get_fa_ts?_eq.pro
;; The ? stands for species, as above. These routines process survey data from the anodes nearest the spin plane into a 2-D format (48 energies * 16 angles) compatible with the structures returned by the ESA IDL routines. Use these routines if you want pitch angle or distribution function plots. For example, the next sample of spin-plane survey He+ data is obtained thus:
;; nexteqsamp = get_fa_tsh_eq(time, /advance)

;; get_fa_ts?_sp.pro
;; The ? stands for species, as above. These routines produce spin averaged data, which differ from the generic data only for H+/O+ at maximum resolution.

;; get_fa_ts?_eq_sp.pro
;; The ? stands for species, as above. These routines produce spin averaged data from the anodes nearest the spin plane, as with the *_eq.pro routines. They are used to produce the pitch angle panels in TEAMS summary plots and CDF's and are obsolete for all other purposes.

;; get_fa_th_3d.pro
;; This is the get routine for the HiMass data product. Note that the structure has additional components corresponding to the mass per charge. Unfortunately, the possibility of selecting a particular mass range for energy or pitch angle spectrograms or for moments was not forseen.

;; get_fa_tb_hdr.pro, get_fa_th_hdr.pro, get_fa_tpah_hdr.pro, get_fa_tpop_hdr.pro, get_fa_tsah_hdr.pro, get_fa_tsop_hdr.pro
;; These routines extract header information from the following
;;respective TEAMS data packets: burst, HiMass, pole (He), pole
;;(H+/O+), survey (He), and survey (H+/O+).
;;***********************************************************************************
;END DOCUMENTATION
;;***********************************************************************************

; Get the orbit data

  IF get_sdt_timespan(t1,t2) THEN BEGIN
     print,'timespan is from ',t1,' to ',t2
  ENDIF ELSE BEGIN
     print,' could not get timespan...'
  ENDELSE
  orbit_file=fa_almanac_dir()+'/orbit/predicted'
  get_fa_orbit,t1,t2,orbit_file=orbit_file,/all,status=orb_stat
  get_data,'ORBIT',data=tmp
  orbit=tmp.y(0)
  orbit_num=strcompress(string(tmp.y(0)),/remove_all)

  ;;Let's get an energy spectrum 
  tpa_name='tpa_en_spec'
  get_en_spec, 'fa_tpa', units = 'eflux', gap_time = 60,name=tpa_name
  get_data,tpa_name,data=tmp                                           ; get data structure
  tmp.y=tmp.y > 1.                                                        ; Remove zeros
  tmp.y = alog10(tmp.y)                                                   ; Pre-log
  store_data,tpa_name,data=tmp                                         ; store data structure
  options,tpa_name,'spec',1                                            ; set for spectrogram
  options,tpa_name,'x_no_interp',1                                     ; don't interpolate
  options,tpa_name,'y_no_interp',1                                     ; don't interpolate


  ;;I want to try getting an He survey energy spectrum
  tsh_name='tsh_en_spec'
  get_en_spec, 'fa_tsh', units = 'eflux', gap_time = 60,name=tsh_name
  get_data,tsh_name,data=tmp                                           ; get data structure
  tmp.y=tmp.y > 1.                                                        ; Remove zeros
  tmp.y = alog10(tmp.y)                                                   ; Pre-log
  store_data,tsh_name,data=tmp                                         ; store data structure
  options,tsh_name,'spec',1                                            ; set for spectrogram
  options,tsh_name,'x_no_interp',1                                     ; don't interpolate
  options,tsh_name,'y_no_interp',1                                     ; don't interpolate

  loadct2,43
  tplot,[tsh_name],$
        var_label=['ALT','ILAT','MLT'],title='FAST ORBIT '+orbit_num

  ;; Make a postscript
  popen, font=1, /port,'/home/spencerh/Desktop/'+tsh_name
  loadct2,43
  tplot
  pclose

  ;; From McFadden's examples

  ;;start times for McFadden's example, orbit 1858
  t1=str_to_time('97-2-9/06:06:40')
  t2=str_to_time('97-2-9/06:07:40')
  
; Ion spectrogram - survey data, remove retrace, upgoing ions 
  
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
; Ion pitch angle spectrogram - survey data, remove retrace, >30 ions 
  
  get_pa_spec,"fa_ies_c",units='eflux',name='ion_pa',energy=[30,30000],retrace=1,/shift90,t1=t1,t2=t2
  get_data,'ion_pa',data=tmp                                ; get data structure
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
; Ion flux 

  get_2dt,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=[20,30000]
  ylim,'Ji',1.e5,1.e8,1                                      ; set y limits
  options,'Ji','ytitle','Ions!C!C1/(cm!U2!N-s)'              ; set y title
  options,'Ji','tplot_routine','pmplot'                      ; set 2 color plot
  options,'Ji','labels',['Downgoing!C Ions','Upgoing!C Ions '] ; set color label
  options,'Ji','labflag',3                                     ; set color label
  options,'Ji','labpos',[2.e7,1.e6]                            ; set color label
  options,'Ji','panel_size',1                                  ; set panel size

; Plot the data

  loadct2,43
  tplot,['ion_180','ion_pa','Ji'],$
        var_label=['ALT','ILAT','MLT'],title='FAST ORBIT '+orbit_num

  ;; For hard copies use 
  popen, font=1, /port,'plot_name'
  loadct2,43
  tplot
  pclose

END


;Alfven_stats_5 snippets for reference

  ;;thresholds for inclusion as Alfven waves
  ;; current_threshold=1.0  ;microA/m^2
  ;; delta_b_threshold=5.0  ; nT
  ;; delta_E_threshold=10.0 ; mV/m
  ;; esa_j_delta_bj_ratio_threshold=0.02
  ;; electron_eflux_ionos_threshold=0.05 ;ergs/cm^2/s
  ;; eb_to_alfven_speed=10.0             ; factor by which the event can differ from model Alfven speed and still be called an Alfven wave 
  ;;(applies only to the lower limit for e over b the upper limit is taken care of by the requiremenst that delta_b exceed 5 nT

  ;;energy ranges
  ;; if not keyword_set(energy_electrons) then energy_electrons=[0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  ;; if not keyword_set(energy_ions) then energy_ions=[0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; If no data exists, return to main
  ;; t=0
  ;; dat = get_fa_ees(t,/st)
  ;; if dat.valid eq 0 then begin
  ;;    print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
  ;;    return
  ;; endif

  ;; Electron current - line plot
  ;; if keyword_set(burst) then begin
  ;;    get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  ;; endif else begin
  ;;    get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons
  ;; endelse
  
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
  
  ;;throw away the first 10  points since they are often corrupted
;;   if not keyword_set(burst) then begin
;;      store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
;;   endif else begin
;;      store_data,'Je',data={x:tx,y:ty}
;;   endelse
  
;;   ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
;;   get_data,'Je',data=je
;;   get_fa_orbit,/time_array,je.x
;;   get_data,'MLT',data=mlt
;;   get_data,'ILAT',data=ilat
;;   keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)
;;   store_data,'Je',data={x:je.x(keep),y:je.y(keep)}
  
;;   ;;Use the electron data to define the time ranges for this orbit	
;;   get_data,'Je',data=je
;;   part_res_je=make_array(n_elements(Je.x),/double)
;;   for j=1,n_elements(Je.x)-1 do begin
;;      part_res_je(j)=abs(Je.x(j)-Je.x(j-1))
;;   endfor
;;   part_res_Je(0)=part_res_Je(1)
;;   gap=where(part_res_je GT 10.0)
;;   if gap(0) NE -1 then begin
;;      separate_start=[0,where(part_res_je GT 10.0)]
;;      separate_stop=[where(part_res_je GT 10.0),n_elements(Je.x)-1]
;;   endif else begin
;;      separate_start=[0]
;;      separate_stop=[n_elements(Je.x)-1]
;;   endelse
  
;;   ;;remove esa burp when switched on
;;   if not keyword_set(burst) then begin
;;      turn_on=where(part_res_je GT 300.0)
;;      if turn_on(0) NE -1 then begin
;;         turn_on_separate=make_array(n_elements(turn_on),/double)
;;         for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
;;         separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
;;      endif
;;   endif

;;   ;;identify time indices for each interval
;;   count=0.0
;;   for j=0,n_elements(separate_start)-1 do begin
;;      if (separate_stop(j)-separate_start(j)) GT 10 then begin
;;         count=count+1
;;         if count EQ 1.0 then begin
;;            time_range_indices=transpose([separate_start(j)+1,separate_stop(j)-1])
;;         endif else begin
;;            time_range_indices=[time_range_indices,transpose([separate_start(j),separate_stop(j)-1])]
;;         endelse
;;      endif
;;   endfor
  
;;   ;;identify interval times
;;   time_ranges=je.x(time_range_indices)
;;   number_of_intervals=n_elements(time_ranges(*,0))
  
;;   print,'number_of_intervals',number_of_intervals
  
;;   ;;loop over each time interval
;;   ji_tot=make_array(number_of_intervals,/double)
;;   ji_up_tot=make_array(number_of_intervals,/double)
;;   jee_tot=make_array(number_of_intervals,/double)
;;   Ji_tot_alf=make_array(number_of_intervals,/double)
;;   Ji_up_tot_alf=make_array(number_of_intervals,/double)
;;   Jee_tot_alf=make_array(number_of_intervals,/double)
  
;;   ;;get despun mag data if keyword set
;;   if keyword_set(ucla_mag_despin) then ucla_mag_despin
  
;;   ;;begin looping each interval
;;   for jjj=0,number_of_intervals-1 do begin
;;      print,'time_range',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))
     
;;      je_tmp_time=je.x(time_range_indices(jjj,0):time_range_indices(jjj,1))
;;      je_tmp_data=je.y(time_range_indices(jjj,0):time_range_indices(jjj,1))
     
;;      store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}
     
;;      ;;get fields quantities
;;      data_valid=1.0
;;      dat=get_fa_fields('MagDC',t,/start)
;;      if dat.valid eq 0 then begin
;;         print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
;;         data_valid=0.0
;;      endif else begin
;;         if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_ranges(jjj,0),time_ranges(jjj,1),/store)
;;         dat=get_fa_fields('V5-V8_S',t,/start)
;;         if dat.valid eq 0 then begin
;;            print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
;;            data_valid=0.0
;;         endif else begin
;;            spacecraft_potential=get_fa_fields('V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;            efieldV58=get_fa_fields('V5-V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;            efieldV1214=get_fa_fields('V1-V2_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;            if efieldV1214.valid eq 0 then begin
;;               print,'No V1-V2 data - trying V1-V4'
;;               efieldV1214=get_fa_fields('V1-V4_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;               if efieldV1214.valid eq 0 then begin
;;                  print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
;;                  data_valid=0.0
;;               endif 
;;            endif 
;;         endelse
;;         ;;Langmuir=get_fa_fields('NE2_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         ;;dens_probe=2
;;         ;;if Langmuir.valid eq 0 then begin
;;         ;;	print,'No Ne2 data - trying Ne6'
;;         ;;	Langmuir=get_fa_fields('NE6_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         ;;	dens_probe=6
;;         ;;	if langmuir.valid eq 0 then begin
;;         ;;		print,'No Ne6 data - trying Ne9'
;;         ;;		Langmuir=get_fa_fields('NE9_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         ;;		dens_probe=9
;;         ;;		if langmuir.valid eq 0 then begin
;;         ;;			print,' ERROR: No FAST fields langmuir data - get_fa_fields returned invalid data'
;;         ;;			data_valid=0.0
;;         ;;		endif 
;;         ;;	endif 
;;         ;;
;;         ;;endif 
;;         ;;if langmuir.valid NE 0 then langmuir={x:langmuir.time,y:langmuir.comp1}
;;         Langmuir_2=get_fa_fields('NE2_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         Langmuir_6=get_fa_fields('NE6_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         Langmuir_9=get_fa_fields('NE9_S',time_ranges(jjj,0),time_ranges(jjj,1))
;;         Langmuir_data=[0]
;;         Langmuir_time=[0]
;;         Langmuir_prob=[0]
;;         if Langmuir_2.valid NE 0 then begin
;;            langmuir_data=[Langmuir_data,Langmuir_2.comp1]
;;            langmuir_time=[Langmuir_time,Langmuir_2.time]
;;            langmuir_prob=[Langmuir_prob,replicate(2,n_elements(Langmuir_2.time))]
;;         endif
;;         if Langmuir_6.valid NE 0 then begin
;;            langmuir_data=[Langmuir_data,Langmuir_6.comp1]
;;            langmuir_time=[Langmuir_time,Langmuir_6.time]
;;            langmuir_prob=[Langmuir_prob,replicate(6,n_elements(Langmuir_6.time))]
;;         endif
;;         if Langmuir_9.valid NE 0 then begin
;;            langmuir_data=[Langmuir_data,Langmuir_9.comp1]
;;            langmuir_time=[Langmuir_time,Langmuir_9.time]
;;            langmuir_prob=[Langmuir_prob,replicate(9,n_elements(Langmuir_9.time))]
;;         endif
;;         if n_elements(langmuir_data) GT 1 then  begin
;;            langmuir_time=langmuir_time(1:n_elements(Langmuir_time)-1)
;;            langmuir_data=langmuir_data(1:n_elements(Langmuir_time)-1)
;;            langmuir_prob=langmuir_prob(1:n_elements(Langmuir_time)-1)
;;            time_order_langmuir=sort(langmuir_time)
;;            langmuir={x:langmuir_time(time_order_langmuir),y:langmuir_data(time_order_langmuir)}
;;            dens_probe={x:langmuir_time(time_order_langmuir),y:langmuir_prob(time_order_langmuir)}
;;         endif else data_valid=0.0
;;      endelse	
     
     
;;      if data_valid NE 0.0 then begin
        
;;         ;;get E field and B field on same time scale
;; ;;			efields_combine=combinets({x:efieldV1214.time,y:efieldV1214.comp1},{x:efieldV58.time,y:efieldV58.comp1})
;;         FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
        
;;         ;;get magnitude of electric and magnetic field
;;         ;; for k=0,10,1 do begin
;;         ;;    print, "This is efieldV1214.comp1["+string(k)+"]: " + string(efieldV1214.comp1[k])
;;         ;;    print, "This is efieldV58.comp1["+string(k)+"]: " + string(efieldV58.comp1[k])
;;         ;;    print, "This is efields_combine["+string(k)+"]: " + string(efields_combine[k])
;;         ;; endfor
;;         ;; help, efieldV1214,/str
;;         ;; help, efieldV58,/str
;;         ;; help,efields_combine
;;         efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}
;;         if not keyword_set(ucla_mag_despin) then begin
;;            get_data,'MagDCcomp1',data=magx
;;            get_data,'MagDCcomp2',data=magy
;;            get_data,'MagDCcomp3',data=magz
;;         endif else begin
;;            get_data,'dB_fac_v',data=db_fac
;;            mintime=min(abs(time_ranges(jjj,0)-db_fac.x),ind1)
;;            mintime=min(abs(time_ranges(jjj,1)-db_fac.x),ind2)
           
;;            magx={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,0)}
;;            magy={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,2)}
;;            magz={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,1)}
;;         endelse
        
;;         store_data,'MagZ',data=magz
;;         ;;magz.y=smooth(magz.y,40)
;;         store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
;;         if keyword_set(filterfreq) then begin
           
;;            magz=filter(magz,filterfreq,'magfilt','l')
;;            ;;remove end effects of the filter by cutting off the first/last 2s
;;            sf=magz.x(1)-magz.x(0)
;;            np=n_elements(magz.x)
;;            padding=round(2.0/sf)
;;            magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
;;            store_data,'MagZ',data=magz
;;         endif
        
        
;;         ;;get mag and efield data on same time scale
;;         ;;SMH Try this to make fa_fields_combine stop crying                        
;;         magz={time:magz.x,comp1:magz.y,ncomp:1}
;;         efield={time:efield.x,comp1:efield.y}
        
        
;;         ;; fields=combinets(magz,efield)
;;         FA_FIELDS_COMBINE,magz,efield,result=fields,/interp,delt_t=50.,/talk
;;         fields={time:magz.time,comp1:magz.comp1,comp2:fields,ncomp:2}

;;         ;;I'm hoping this means magz is pared down somewhere else

;;         ;; dens=combinets(magz,langmuir)
;;         langmuir={time:langmuir.x,comp1:langmuir.y,ncomp:1}
;;         FA_FIELDS_COMBINE,magz,langmuir,result=dens,/talk
;;         dens={time:magz.time,comp1:magz.comp1,comp2:dens,ncomp:2}

;;         magz={x:magz.time,y:magz.comp1}
;;         langmuir={x:langmuir.time,y:langmuir.comp1}

;;         ;;get the prootn cyc frequency for smoothing the e field data later
;;         proton_cyc_freq=1.6e-19*sqrt(magx.y^2+magy.y^2+magz.y^2)*1.0e-9/1.67e-27/(2.*!DPI) ; in Hz
        
;;         ;;get_orbit data
;;         get_fa_orbit,je_tmp_time,/time_array,/all
        
;;         ;;define loss cone angle
;;         get_data,'ALT',data=alt
;;         loss_cone_alt=alt.y(0)*1000.0
;;         lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI
;;         get_data,'ILAT',data=ilat
;;         north_south=abs(ilat.y(0))/ilat.y(0)
        
;;         if north_south EQ -1 then begin
;;            e_angle=[180.-lcw,180+lcw] ; for Southern Hemis.
;;            ;;i_angle=[270.0,90.0]	
;;            ;;elimnate ram from data
;;            i_angle=[180.0,360.0]
;;            i_angle_up=[270.0,360.0]
           
;;         endif else begin
;;            e_angle=[360.-lcw,lcw] ;	for Northern Hemis.
;;            ;;i_angle=[90.,270.0]
;;            ;;eliminate ram from data
;;            i_angle=[0.0,180.0]
;;            i_angle_up=[90.0,180.0]
           
;;         endelse

;;         ;;get fields mode
;;         fields_mode=get_fa_fields('DataHdr_1032',time_ranges(jjj,0),time_ranges(jjj,1))
        
;;         ;;get the spacecraft potential per spin
;;         spin_period=4.946 ; seconds
        
;;         ;;get_sample_rate
;;         v8={x:spacecraft_potential.time,y:spacecraft_potential.comp1}
        
;;         v8_dt=abs(v8.x-shift(v8.x,-1))
;;         v8_dt(0)=v8_dt(1)
;;         v8_dt(n_elements(v8.x)-1)=v8_dt(n_elements(v8.x)-2)
        
;;         ;;get maxima within a 1 spin window
;;         j_range=where(v8.x LT v8.x(n_elements(v8.x)-1)-spin_period)
;;         index_max=max(j_range)
;;         print,index_max
;;         pot=make_array(n_elements(v8.x),/double)
;;         for j=0L,index_max do begin
;;            ;;spin_range=where(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
;;            spin_range=j+findgen(ceil(spin_period/V8_dt(j)))
;;            pot(j)=max(abs(v8.y(spin_range)),ind)
;;            sign=v8.y(spin_range(ind))/abs(v8.y(spin_range(ind)))
;;            pot(j)=sign*pot(j)
;;            ;;print,j,pot(j)
;;         endfor
;;         pot(index_max+1:n_elements(v8.x)-1)=pot(j_range(index_max))
;;         sc_pot={x:v8.x,y:pot}
;;         store_data,'S_Pot',data=sc_pot ;note this is actualy the negative of the sp. potential this corrected in the file output

;;         ;;get moments/integrals of various fluxes
;;         if keyword_set(burst) then begin

;;            get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='JEe_tot',energy=energy_electrons
;;            get_2dt_ts,'je_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='JEe',angle=e_angle,energy=energy_electrons
;;            get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='Je',energy=energy_electrons
;;            get_2dt_ts,'j_2d_b','fa_eeb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='Je_lc',energy=energy_electrons,angle=e_angle
           
;;            get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='JEi',energy=energy_ions
;;            get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='Ji',energy=energy_ions
;;            get_2dt_ts,'je_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='JEi_up',energy=energy_ions,angle=i_angle
;;            get_2dt_ts,'j_2d_b','fa_ieb',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                       name='Ji_up',energy=energy_ions,angle=i_angle
           
;;         endif else begin
           
;;            get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEe_tot',energy=energy_electrons,sc_pot=sc_pot
;;            get_2dt_ts_pot,'je_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEe',angle=e_angle,energy=energy_electrons,sc_pot=sc_pot
;;            get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Je',energy=energy_electrons,sc_pot=sc_pot
;;            get_2dt_ts_pot,'j_2d_b','fa_ees',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Je_lc',energy=energy_electrons,angle=e_angle,sc_pot=sc_pot
           
;;            get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEi',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
;;            get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Ji',energy=energy_ions,angle=i_angle,sc_pot=sc_pot
;;            get_2dt_ts_pot,'je_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEi_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
;;            get_2dt_ts_pot,'j_2d_b','fa_ies',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Ji_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
           
;;            if keyword_set(heavy) then begin
              
;;               get_2dt_pot,'je_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
;;               get_2dt_pot,'je_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
;;               get_2dt_pot,'je_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='JEh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
;;               get_2dt_pot,'j_2d','fa_tsp_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Jp_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
;;               get_2dt_pot,'j_2d','fa_tso_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Jo_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
;;               get_2dt_pot,'j_2d','fa_tsh_eq',t1=time_ranges(jjj,0),t2=time_ranges(jjj,1), $
;;                           name='Jh_up',energy=energy_ions,angle=i_angle_up,sc_pot=sc_pot
              
;;            endif
           
;;         endelse
        
;;         get_data,'Je',data=tmp
;;         get_data,'Ji',data=tmpi
;;         ;;remove crap
;;         keep1=where(finite(tmp.y) NE 0 and finite(tmpi.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         keep2=where(abs(tmp.y) GT 0.0 and abs(tmpi.y) GT 0.0)
;;         je_tmp_time=tmp.x(keep2)
;;         je_tmp_data=tmp.y(keep2)
;;         store_data,'Je',data={x:je_tmp_time,y:je_tmp_data}
        
;;         get_data,'JEe',data=tmp
;;         ;;remove crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         jee_tmp_time=tmp.x(keep2)
;;         jee_tmp_data=tmp.y(keep2)
;;         store_data,'JEe',data={x:jee_tmp_time,y:jee_tmp_data}
        
;;         get_data,'JEe_tot',data=tmp
;;         ;;remove crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         jee_tot_tmp_time=tmp.x(keep2)
;;         jee_tot_tmp_data=tmp.y(keep2)
;;         store_data,'JEe_tot',data={x:jee_tot_tmp_time,y:jee_tot_tmp_data}
        
;;         get_data,'Je_lc',data=tmp
;;         ;;remove_crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         je_lc_tmp_time=tmp.x(keep2)
;;         je_lc_tmp_data=tmp.y(keep2)
;;         store_data,'Je_lc',data={x:je_lc_tmp_time,y:je_lc_tmp_data}
        
        
        
;;         get_data,'Ji',data=tmp
;;         ;;remove crap	
;;         ;;keep1=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep2=where(abs(tmp.y) GT 0.0)
;;         ji_tmp_time=tmp.x(keep2)
;;         ji_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
;;         store_data,'Ji',data={x:ji_tmp_time,y:ji_tmp_data}
        
;;         get_data,'JEi',data=tmp
;;         ;;remove crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         jEi_tmp_time=tmp.x(keep2)
;;         jEi_tmp_data=tmp.y(keep2)
;;         store_data,'JEi',data={x:jEi_tmp_time,y:jEi_tmp_data}
        
;;         get_data,'JEi_up',data=tmp
;;         ;;remove crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         jEi_up_tmp_time=tmp.x(keep2)
;;         jEi_up_tmp_data=tmp.y(keep2)
;;         store_data,'JEi_up',data={x:jEi_up_tmp_time,y:jEi_up_tmp_data}
        
;;         get_data,'Ji_up',data=tmp
;;         ;;remove crap
;;         ;;keep=where(finite(tmp.y) NE 0)
;;         tmp.x=tmp.x(keep1)
;;         tmp.y=tmp.y(keep1)
;;         ;;keep=where(abs(tmp.y) GT 0.0)
;;         ji_up_tmp_time=tmp.x(keep2)
;;         ji_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
;;         store_data,'Ji_up',data={x:ji_up_tmp_time,y:ji_up_tmp_data}
        
        
;;         if keyword_set(heavy) then begin
           
;;            get_data,'JEp_up',data=tmp
;;            ;;remove crap
;;            keep1=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            keep2=where(abs(tmp.y) GT 0.0)
;;            jEp_up_tmp_time=tmp.x(keep2)
;;            jEp_up_tmp_data=tmp.y(keep2)
;;            store_data,'JEp_up',data={x:jEp_up_tmp_time,y:jEp_up_tmp_data}
           
;;            get_data,'Jp_up',data=tmp
;;            ;;remove crap
;;            ;;keep=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            ;;keep=where(abs(tmp.y) GT 0.0)
;;            jp_up_tmp_time=tmp.x(keep2)
;;            jp_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
;;            store_data,'Jp_up',data={x:jp_up_tmp_time,y:jp_up_tmp_data}
           
           
;;            get_data,'JEo_up',data=tmp
;;            ;;remove crap
;;            ;;keep=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            ;;keep=where(abs(tmp.y) GT 0.0)
;;            jEo_up_tmp_time=tmp.x(keep2)
;;            jEo_up_tmp_data=tmp.y(keep2)
;;            store_data,'JEo_up',data={x:jEo_up_tmp_time,y:jEo_up_tmp_data}
           
;;            get_data,'Jo_up',data=tmp
;;            ;;remove crap
;;            ;;keep=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            ;;keep=where(abs(tmp.y) GT 0.0)
;;            jo_up_tmp_time=tmp.x(keep2)
;;            jo_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
;;            store_data,'Jo_up',data={x:jo_up_tmp_time,y:jo_up_tmp_data}
           
           
;;            get_data,'JEh_up',data=tmp
;;            ;;remove crap
;;            keep1=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            keep2=where(abs(tmp.y) GT 0.0)
;;            jEh_up_tmp_time=tmp.x(keep2)
;;            jEh_up_tmp_data=tmp.y(keep2)
;;            store_data,'JEh_up',data={x:jEh_up_tmp_time,y:jEh_up_tmp_data}
           
;;            get_data,'Jh_up',data=tmp
;;            ;;remove crap
;;            ;;keep=where(finite(tmp.y) NE 0)
;;            tmp.x=tmp.x(keep1)
;;            tmp.y=tmp.y(keep1)
;;            ;;keep=where(abs(tmp.y) GT 0.0)
;;            jh_up_tmp_time=tmp.x(keep2)
;;            jh_up_tmp_data=2.0*tmp.y(keep2) ;the 2.0 here is because of the 1/2 angular range I use to exclude ram ions
;;            store_data,'Jh_up',data={x:jh_up_tmp_time,y:jh_up_tmp_data}
           
;;         endif
;;         ;;get ion end electron characteristic energies
        
;;         chare=(jee_tmp_data/je_lc_tmp_data)*6.242*1.0e11
;;         chare_tot=(jee_tot_tmp_data/je_tmp_data)*6.242*1.0e11
;;         charei=(JEi_up_tmp_data/ji_up_tmp_data)*6.242*1.0e11
;;         store_data,'CharE',data={x:jee_tmp_time,y:chare}
;;         store_data,'CharE_tot',data={x:jee_tot_tmp_time,y:chare_tot}
;;         store_data,'CharEi',data={x:jei_up_tmp_time,y:charei}
        
;;         ;;get orbit number for filenames		
;;         get_data,'ORBIT',data=tmp
;;         orbit=tmp.y(0)
;;         orbit_num=strcompress(string(tmp.y(0)),/remove_all)

;;         ;;Scale electron energy flux to 100km, pos flux earthward
;;         get_data,'ILAT',data=tmp
;;         sgn_flx = tmp.y/abs(tmp.y)
;;         get_data,'B_model',data=tmp1
;;         get_data,'BFOOT',data=tmp2
;;         mag1 = (tmp1.y(*,0)*tmp1.y(*,0)+tmp1.y(*,1)*tmp1.y(*,1)+tmp1.y(*,2)*tmp1.y(*,2))^0.5
;;         mag2 = (tmp2.y(*,0)*tmp2.y(*,0)+tmp2.y(*,1)*tmp2.y(*,1)+tmp2.y(*,2)*tmp2.y(*,2))^0.5
;;         ratio = (mag2/mag1)
;;         jee_ionos_tmp_data = sgn_flx*jee_tmp_data*ratio
;;         store_data,'JEei',data={x:jee_tmp_time,y:jee_ionos_tmp_data}
        
;;         jee_tot_ionos_tmp_data=sgn_flx*jee_tot_tmp_data*ratio
;;         store_data,'JEei_tot',data={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}
        
;;         get_data,'fa_vel',data=vel
;;         speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0
        
;;         ;;get position of each mag point
;;         ;;samplingperiod=magz.x(300)-magz.x(299)
;;         ;;position=make_array(n_elements(magz.x),/double)
;;         ;;position=speed(300)*samplingperiod*findgen(n_elements(magz.x))
;;         ;;speed_mag_point=speed(300)
        
;;         old_pos=0.
;;         position=make_array(n_elements(magz.x),/double)
;;         speed_mag_point=make_array(n_elements(magz.x),/double)
;;         for j=0L,n_elements(magz.x)-2 do begin
;;            speed_point_ind=min(abs(vel.x-magz.x(j)),ind)
;;            ;;print,ind
;;            speed_mag_point(j)=speed(ind)
;;            samplingperiod=magz.x(j+1)-magz.x(j)
;;            ;;position=make_array(n_elements(magz.x),/double)
;;            position(j)=old_pos+speed_mag_point(j)*samplingperiod
;;            old_pos=position(j)
;;         endfor
        
        
;;         ;;calculate the total ion outflow for this interval

;;         part_res_ji=make_array(n_elements(ji_up_tmp_time),/double)
;;         position_ji=make_array(n_elements(Ji_up_tmp_time),/double)
;;         position_ji(0)=0.0
;;         for j=1,n_elements(ji_tmp_time)-1 do begin
;;            part_res_ji(j)=abs(ji_up_tmp_time(j-1)-ji_up_tmp_time(j))
;;            if part_res_ji(j) EQ 0.0 then part_res_ji(j)=part_res_ji(j-1)
;;            position_ji(j)=position_ji(j-1)+speed(j)*part_res_Ji(j)
;;         endfor
;;         part_res_Ji(0)=part_res_Ji(1)
;;         ji_tot(jjj)=int_tabulated(position_ji,ji_tmp_data*sqrt(ratio))       ;mapped to ionosphere sqrt due to intergration in x 
;;         ji_up_tot(jjj)=int_tabulated(position_ji,ji_up_tmp_data*sqrt(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
        
;;         print,'ji_tot',ji_tot(jjj)
        
;;         ;;calculate the total electron downflux at the spacecraft altitude over this interval
;;         part_res_je=make_array(n_elements(jee_tmp_data),/double)
;;         position_je=make_array(n_elements(jee_tmp_time),/double)
;;         position_je(0)=0.0
;;         for j=1,n_elements(je_tmp_time)-1 do begin
;;            part_res_je(j)=abs(jee_tmp_time(j-1)-jee_tmp_time(j))
;;            if part_res_je(j) EQ 0.0 then part_res_je(j)=part_res_je(j-1)
;;            position_je(j)=position_je(j-1)+speed(j)*part_res_Je(j)
;;         endfor
;;         part_res_Je(0)=part_res_Je(1)
;;         jee_tot(jjj)=int_tabulated(position_je,jee_tmp_data*sqrt(ratio)) ;mapped to ionosphere sqrt due to intergration in x 
        
;;         ;;calculate the current from mag
;;         deltaBX=deriv(position,magz.y)
;;         jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
;;         sign_jtemp=abs(deltaBx)/deltaBx
;;         store_data,'jtemp',data={x:magz.x,y:jtemp}
        
;;         ;;terminate the intervals before the last point
;;         if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)
        
;;         start_points=[0]
;;         stop_points=[0]
        
;;         ;;get current intervals
;;         for j=1L,n_elements(sign_jtemp)-2 do begin
           
;;            if sign_jtemp(j)+sign_jtemp(j-1) EQ 0.0 then begin
;;               start_points=[start_points,j]
;;            endif
;;            if sign_jtemp(j)+sign_jtemp(j+1) EQ 0.0 then begin
;;               stop_points=[stop_points,j]
;;            endif
           
;;         endfor
        
;;         if sign_jtemp(0)+sign_jtemp(1) NE 0.0 then stop_points=stop_points(1:n_elements(stop_points)-1)
        
;;         ;;eliminate single points
;;         non_single_points=where(stop_points NE start_points)
        
;;         start_points=start_points(non_single_points)
;;         stop_points=stop_points(non_single_points)
        
;;         current_intervals=make_array(n_elements(start_points),42,/double)
;;         current_intervals(*,0)=start_points
;;         current_intervals(*,1)=stop_points
;;         current_intervals(*,2)=sign_jtemp(start_points)
;;         current_intervals(*,3)=1
        
;;         intervalparts_electrons_old=-1
;;         intervalparts_ions_old=-1
;;         valid_old=0.0
;;         for j=0L,n_elements(start_points)-1 do begin
           
;;            ;;define the interval points 
;;            intervalfields=(current_intervals(j,0))+findgen(current_intervals(j,1)+1-current_intervals(j,0))
;;            tempz=magz.y(intervalfields)
;;            fields_res_interval=magz.x(intervalfields)-magz.x(intervalfields-1)
;;            ;;help,magz,/st
;;            ;;print,'current_indices ',current_intervals(j,0),current_intervals(j,1)
;;            intervalparts_electrons=where(je_tmp_time GE magz.x(current_intervals(j,0)) and je_tmp_time LE magz.x(current_intervals(j,1)))
;;            intervalparts_ions=where(ji_up_tmp_time GE magz.x(current_intervals(j,0)) and ji_up_tmp_time LE magz.x(current_intervals(j,1)))
;;            if intervalparts_electrons(0) EQ -1 then begin
;;               minitime=min(abs(je_tmp_time-magz.x(current_intervals(j,0))),intervalparts_electrons)
;;            endif
;;            if intervalparts_ions(0) EQ -1 then begin
;;               minitime=min(abs(ji_up_tmp_time-magz.x(current_intervals(j,0))),intervalparts_ions)
;;            endif

;;            ;;get the current from b and determine if to keep this event
;;            jmax=max(jtemp(intervalfields),indjmax)
;;            current_intervals(j,4)=jmax*sign_jtemp(start_points(j))
;;            if jmax LE current_threshold then begin
;;               current_intervals(j,3)=0.0
;;            endif
           
;;            ;;define the time of the max current
;;            current_intervals(j,20)=magz.x(intervalfields(indjmax))
           
           
;;            ;;get the electron current and determine if to keep this event
;;            sign=-1.*je_tmp_data(intervalparts_electrons)/abs(je_tmp_data(intervalparts_electrons))
;;            maxJe=max(abs(je_tmp_data(intervalparts_electrons)),ind)
;;            maxJe=maxJe*sign(ind)*1.6e-9 ;in microA/m2
;;            current_intervals(j,5)=maxJe
;;            if abs(maxJe)/abs(jmax) LE esa_j_delta_bj_ratio_threshold then begin
;;               current_intervals(j,3)=0.0
;;            endif
           
;;            ;;get the electron energy flux and dtermine if to keep this event
;;            ;;print,'intervalparts_electrons',intervalparts_electrons
;;            ;;help,jee_tmp_time
;;            ;;help,je_tmp_time
;;            ;;print,'jee start stop ',time_to_str(jee_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
;;            ;;print,'je start stop ',time_to_str(je_tmp_time(0),/ms),time_to_str(jee_tmp_time(n_elements(jee_tmp_time)-1),/ms)
           
;;            sign=jee_ionos_tmp_data(intervalparts_electrons)/abs(jee_ionos_tmp_data(intervalparts_electrons)) ;note corrected direction (i.e.-1) from explore_teams_4-positive is now really downwards
;;            maxJEe_ionos=max(abs(jee_ionos_tmp_data(intervalparts_electrons)),ind)
;;            maxJEe_ionos=maxJEe_ionos*sign(ind)
           
;;            sign=jee_tot_ionos_tmp_data(intervalparts_electrons)/abs(jee_tot_ionos_tmp_data(intervalparts_electrons))
;;            maxJEe_tot_ionos=max(abs(jee_tot_ionos_tmp_data(intervalparts_electrons)),ind)
;;            maxJEe_tot_ionos=maxJEe_tot_ionos*sign(ind)
           
           
           
;;            current_intervals(j,6)=maxJEe_ionos
;;            current_intervals(j,40)=maxJEe_tot_ionos
;;            if abs(maxJEe_ionos) LE electron_eflux_ionos_threshold and abs(maxJEe_tot_ionos-maxJEe_ionos) LE electron_eflux_ionos_threshold then begin ;note change from previously when only downgoing fluxes where considered.
;;               current_intervals(j,3)=0.0				      
;;            endif
           
;;            if keyword_set(heavy) then begin
              
;;               minitime=min(abs(Jp_up_tmp_time-current_intervals(j,20)),ind_OH)
;;               current_intervals(j,28)=Jp_up_tmp_data(ind_OH)
;;               C_Ep=JEp_up_tmp_data(ind_OH)/Jp_up_tmp_data(ind_OH)*6.242*1.0e11
;;               current_intervals(j,29)=C_Ep
              
;;               current_intervals(j,30)=Jo_up_tmp_data(ind_OH)
;;               C_Eo=JEo_up_tmp_data(ind_OH)/Jo_up_tmp_data(ind_OH)*6.242*1.0e11
;;               current_intervals(j,31)=C_Eo
              
;;               minitime=min(abs(Jh_up_tmp_time-current_intervals(j,20)),ind_h)
;;               current_intervals(j,32)=Jh_up_tmp_data(ind_h)
;;               C_Eh=JEh_up_tmp_data(ind_h)/Jh_up_tmp_data(ind_h)*6.242*1.0e11
;;               current_intervals(j,33)=C_Eh
              
;;            endif
           
;;            ;;get width of current filament in time (s)
;;            time_width=magz.x(current_intervals(j,1))-magz.x(current_intervals(j,0))
           
;;            current_intervals(j,15)=time_width
;;            ;;get width of the current filament at this altitude
           
;;            width=speed_mag_point(current_intervals(j,0))*abs(magz.x(current_intervals(j,0))-magz.x(current_intervals(j,1)))
;;            ;;print,'speed',speed_mag_point(current_intervals(j,0))
;;            current_intervals(j,16)=width
           
;;            ;;get the integrated electron dflux in ionosphere over this interval
;;            if intervalparts_electrons(0) NE -1 then begin
;;               if n_elements(intervalparts_electrons) EQ 1 then begin 
                 
;;                  current_intervals(j,7)=width*jee_tmp_data(intervalparts_electrons)
;;                  current_intervals(j,41)=width*jee_tot_tmp_data(intervalparts_electrons)
;;               endif else begin
;;                  ;;interpolate particle data to same resolution as the fields data
;;                  jee_tmp_data_fields_res_interval=interpol(jee_tmp_data(intervalparts_electrons),jee_tmp_time(intervalparts_electrons),magz.x(intervalfields))
;;                  jee_tot_tmp_data_fields_res_interval=interpol(jee_tot_tmp_data(intervalparts_electrons),jee_tot_tmp_time(intervalparts_electrons),magz.x(intervalfields))
;;                  current_intervals(j,7)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,jee_tmp_data_fields_res_interval,/double)
;;                  current_intervals(j,41)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,jee_tot_tmp_data_fields_res_interval,/double)
                 
;;               endelse
              
;;               ;;map result to ionosphere (sqrt of B since have integrated in x)
;;               current_intervals(j,7)=current_intervals(j,7)*sqrt(ratio(intervalparts_electrons(0)))
;;               current_intervals(j,41)=current_intervals(j,41)*sqrt(ratio(intervalparts_electrons(0)))
;;            endif
           
           
           
;;            ;;get integrated ion outflow mapped to ionosphere over this interval
;;            if intervalparts_ions(0) NE -1 then begin
;;               if n_elements(intervalparts_ions) EQ 1 then begin 
;;                  ;;if intervalparts_ions(0) NE intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) or valid_old EQ 0.0 then begin
                 
;;                  current_intervals(j,12)=width*ji_tmp_data(intervalparts_ions)
;;                  current_intervals(j,13)=width*ji_up_tmp_data(intervalparts_ions)
;;                  ;;endif
;;               endif else begin
;;                  ;;if  intervalparts_ions(0) EQ intervalparts_ions_old(n_elements(intervalparts_ions_old)-1) and valid_old EQ 1.0 then intervalparts_ions=intervalparts_ions(1:n_elements(intervalparts_ions)-1)
;;                  ;;if n_elements(intervalparts_ions) EQ 1 then begin 
;;                  ;;current_intervals(j,12)=speed(intervalparts_ions)*part_res_ji(intervalparts_ions)*ji_up_tmp_data(intervalparts_ions)/2.0
                 
;;                  ;;endif else begin
                 
                 
;;                  ;;interpolate particle data to same resolaution as the fields data
;;                  ji_tmp_data_fields_res_interval=interpol(ji_tmp_data(intervalparts_ions),ji_tmp_time(intervalparts_ions),magz.x(intervalfields))
;;                  ji_up_tmp_data_fields_res_interval=interpol(ji_up_tmp_data(intervalparts_ions),ji_up_tmp_time(intervalparts_ions),magz.x(intervalfields))
                 
;;                  current_intervals(j,12)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,ji_tmp_data_fields_res_interval,/double)
;;                  current_intervals(j,13)=int_tabulated(findgen(n_elements(intervalfields))*speed_mag_point(intervalfields)*fields_res_interval,ji_up_tmp_data_fields_res_interval,/double)
;;                  ;;print,'ji_tot_alf',Ji_tot_alf(jjj)
                 
                 
;;                  ;;endelse
                 
;;               endelse
              
;;               ;;map result to ionosphere (sqrt of B since have integrated in x)
;;               current_intervals(j,12)=current_intervals(j,12)*sqrt(ratio(intervalparts_ions(0)))
;;               current_intervals(j,13)=current_intervals(j,13)*sqrt(ratio(intervalparts_ions(0)))
;;            endif
           
;;            ;;get max electron characteristic energy over this interval
;;            C_E=max(charE(intervalparts_electrons))
;;            C_E_tot=max(charE_tot(intervalparts_electrons))
           
;;            current_intervals(j,8)=C_E
;;            current_intervals(j,39)=C_E_tot
           
;;            ;;get max upgoing ion energy flux over this interval
;;            maxJEi=max(abs(jei_up_tmp_data(intervalparts_ions)),ind)
;;            current_intervals(j,9)=maxJEi
           
;;            ;;get max ion flux over this interval
;;            sign_ion=-1.*ji_tmp_data(intervalparts_ions)/abs(ji_tmp_data(intervalparts_ions))
;;            maxJi=max(abs(ji_tmp_data(intervalparts_ions)),ind)
;;            maxJi=maxJi*sign_ion(ind)
;;            current_intervals(j,10)=maxJi
           
;;            ;;get max upgoing ion flux over this interval
;;            maxJi_up=max(abs(ji_up_tmp_data(intervalparts_ions)),ind)
;;            current_intervals(j,11)=maxJi_up
           
;;            ;;get max characteristic ion energy over this interval
;;            C_Ei=max(charEi(intervalparts_ions))
;;            current_intervals(j,14)=C_Ei

;;            ;;fields sample period
;;            current_intervals(j,26)=magz.x(intervalfields(indjmax)+1)-magz.x(intervalfields(indjmax))
           
;;            ;;get mag field amplitude
;;            db=max(magz.y(intervalfields))-min(magz.y(intervalfields))
;;            median_db=median(magz.y(intervalfields))
;;            current_intervals(j,17)=db
;;            current_intervals(j,24)=median_db
;;            if db LT delta_b_threshold then current_intervals(j,3)=0.0 ;threshold for reliablity of identification
           
;;            ;;get elec field amplitude
;;            ;;smooth to below proton gyro freq.
;;            smooth_int=ceil((1./proton_cyc_freq(intervalfields(indjmax)))/current_intervals(j,26))
;;            if smooth_int GT 1.0 and smooth_int LE n_elements(intervalfields)/4.0 then efield_smooth=smooth(fields.comp2(intervalfields),smooth_int) else efield_smooth=fields.comp2(intervalfields)
           
;;            de=max(efield_smooth)-min(efield_smooth)
;;            median_de=median(fields.comp2(intervalfields))
;;            current_intervals(j,18)=de
;;            current_intervals(j,25)=median_de
;;            if de LT delta_E_threshold then current_intervals(j,3)=0.0 ;;threshold for reliablity of identification
           
;;            ;;get max and min L. probe currents
;;            smooth_int=ceil((1./proton_cyc_freq(intervalfields(indjmax)))/current_intervals(j,26))
;;            if smooth_int GT 1.0 and smooth_int LE n_elements(intervalfields)/4.0 then dens_smooth=smooth(dens.comp2(intervalfields),smooth_int) else dens_smooth=dens.comp2(intervalfields)
           
;;            dens_max=max(dens_smooth)
;;            dens_min=min(dens_smooth)
;;            probe_time=min(abs(dens_probe.x-magz.x(intervalfields(indjmax))),probe_ind)
           
;;            median_dens=median(dens.comp2(intervalfields))
;;            current_intervals(j,35)=dens_probe.y(probe_ind)
;;            current_intervals(j,36)=dens_max
;;            current_intervals(j,37)=dens_min
;;            current_intervals(j,38)=median_dens

;;            ;;now get orbit quantities
;;            get_data,'ORBIT',data=orb
;;            get_data,'MLT',data=mlt
;;            get_data,'ALT',data=alt
;;            get_data,'ILAT',data=ilat

;;            mintime=min(abs(mlt.x-magz.x(intervalfields(indjmax))),ind)
           
;;            current_intervals(j,19)=orb.y(ind)
;;            current_intervals(j,21)=alt.y(ind)	
;;            current_intervals(j,22)=mlt.y(ind)	
;;            current_intervals(j,23)=ilat.y(ind)           

;;            ;;fields_mode
;;            mintime=min(abs(fields_mode.time-magz.x(intervalfields(indjmax))),ind)
;;            current_intervals(j,27)=fields_mode.comp1(13,ind)

;;            ;;sc potential
;;            mintime=min(abs(sc_pot.x-magz.x(intervalfields(indjmax))),ind)
;;            current_intervals(j,34)=-1*sc_pot.y(ind)

;;            ;;e over b test
;;            va=1000.0*alfven_speed_mlt(current_intervals(j,21),current_intervals(j,22))
;;            e_over_b=(1.0e-3*current_intervals(j,18))/(current_intervals(j,17)*1.0e-9)
;;            if e_over_b/va LT 1.0/eb_to_alfven_speed then current_intervals(j,3)=0.0

;;            intervalparts_electrons_old=intervalparts_electrons
;;            intervalparts_ions_old=intervalparts_ions	
;;            valid_old=current_intervals(j,3)
;;         endfor
        
;;      endif

;;   endfor

;;   return 
;; end
