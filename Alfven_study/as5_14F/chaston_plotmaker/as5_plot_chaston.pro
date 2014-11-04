pro as5_plot_chaston,filename=filename,energy_electrons=energy_electrons,energy_ions=energy_ions,analyse_noise=analyse_noise,$
  t1=t1,t2=t2,filterfreq=filterfreq,$
  burst=burst,ucla_mag_despin=ucla_mag_despin,do_ch=do_ch,ch_int=ch_int,$
  do_overview=do_o,jmag_thres=jmag_thres

  @startup
  
;; ; if doing Chaston plots, get prepped 
;;   IF KEYWORD_SET(do_ch) THEN BEGIN
;;      IF CH_INT EQ !NULL THEN BEGIN
;;       PRINT,"No current interval specified for Chaston plots! Doing interval 0..."
;;       CH_INT = 0
;;      ENDIF
;;      chastondbdir='/SPENCEdata2/Research/Cusp/database/current_db/'
;;      outdir='/SPENCEdata2/Research/Cusp/ACE_FAST/Compare_new_DB_with_Chastons/txtoutput/'
;;      !PATH = '/SPENCEdata2/Research/Cusp/ACE_FAST/Compare_new_DB_with_Chastons/:' + !PATH

;;      orbit=10000
;;      savsuf=".sav"
;;      basename='dflux_'+strcompress(str(orbit)+"_"+str(ch_int),/remove_all)
;;      chastonfname=chastondbdir+basename
;;      chastonoutname=outdir+'chast_'+basename+savsuf

;;      print, "Chaston db file: " + chastonfname

;;      ;get Chaston db file in memory
;;      combine_dflux_dartchast,orbit, 0, in_name=chastonfname,outname=chastonoutname
;;      restore, chastonoutname

;;      n_chast = N_ELEMENTS(dat.time)
;;      data_chast = dat
     
;;      IF NOT KEYWORD_SET(jmag_thres) THEN jmag_thres = 5.0
;;      PRINT,"Current threshold set to " + string(jmag_thres)
;;   ENDIF



  t=0
  dat = get_fa_ees(t,/st)
  if dat.valid eq 0 then begin
    print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
    return
  endif

  ; Electron current - line plot

  if keyword_set(burst) then begin
    get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endif else begin
    get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons
  endelse

  ;remove spurious crap

  get_data,'Je',data=tmpj

  keep=where(finite(tmpj.y) NE 0)
  tmpj.x=tmpj.x(keep)
  tmpj.y=tmpj.y(keep)

  keep=where(abs(tmpj.y) GT 0.0)
  tx=tmpj.x(keep)
  ty=tmpj.y(keep)

  ;get timescale monotonic

  time_order=sort(tx)
  tx=tx(time_order)
  ty=ty(time_order)


  ;throw away the first 10  points since they are often corrupted
  if not keyword_set(burst) then begin
    store_data,'Je',data={x:tx(10:n_elements(tx)-1),y:ty(10:n_elements(tx)-1)}
  endif else begin
    store_data,'Je',data={x:tx,y:ty}
  endelse

  ;eliminate data from latitudes below the Holzworth/Meng auroral oval

  get_data,'Je',data=je
  get_fa_orbit,/time_array,je.x

  get_data,'MLT',data=mlt
  get_data,'ILAT',data=ilat
  keep=where(abs(ilat.y) GT auroral_zone(mlt.y,7,/lat)/(!DPI)*180.)

  store_data,'Je',data={x:je.x(keep),y:je.y(keep)}

  ;Use the electron data to define the time ranges for this orbit

  get_data,'Je',data=je
  part_res_je=make_array(n_elements(Je.x),/double)
  for j=1,n_elements(Je.x)-1 do begin
    part_res_je(j)=abs(Je.x(j)-Je.x(j-1))
  endfor
  part_res_Je(0)=part_res_Je(1)
  gap=where(part_res_je GT 10.0)
  if gap(0) NE -1 then begin
    separate_start=[0,where(part_res_je GT 10.0)]
    separate_stop=[where(part_res_je GT 10.0),n_elements(Je.x)-1]
  endif else begin
    separate_start=[0]
    separate_stop=[n_elements(Je.x)-1]
  endelse


  ;remove esa burp when switched on
  if not keyword_set(burst) then begin
    turn_on=where(part_res_je GT 300.0)
    if turn_on(0) NE -1 then begin
      turn_on_separate=make_array(n_elements(turn_on),/double)
      for j=0,n_elements(turn_on)-1 do turn_on_separate(j)=where(separate_start EQ turn_on(j))
      separate_start(turn_on_separate+1)=separate_start(turn_on_separate+1)+5
    endif
  endif
  ;identify time indices for each interval

  count=0.0
  for j=0,n_elements(separate_start)-1 do begin
    if (separate_stop(j)-separate_start(j)) GT 10 then begin
      count=count+1
      if count EQ 1.0 then begin
        time_range_indices=transpose([separate_start(j)+1,separate_stop(j)-1])
      endif else begin
        time_range_indices=[time_range_indices,transpose([separate_start(j),separate_stop(j)-1])]
      endelse
    endif
  endfor


  ;identify interval times

  time_ranges=je.x(time_range_indices)
  number_of_intervals=n_elements(time_ranges(*,0))

  print,'number_of_intervals',number_of_intervals

  ;get despun mag data if keyword set
  if keyword_set(ucla_mag_despin) then ucla_mag_despin


  ;begin looping each interval

  for jjj=0,number_of_intervals-1 do begin

    je_tmp_time=je.x(time_range_indices(jjj,0):time_range_indices(jjj,1))
    je_tmp_data=je.y(time_range_indices(jjj,0):time_range_indices(jjj,1))

    store_data,'Je_tmp',data={x:je_tmp_time,y:je_tmp_data}

    ;get fields quantities
    data_valid=1.0
    dat=get_fa_fields('MagDC',t,/start)
    if dat.valid eq 0 then begin
      print,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
      data_valid=0.0
    endif else begin
      if not keyword_set(ucla_mag_despin) then field=get_fa_fields('MagDC',time_ranges(jjj,0),time_ranges(jjj,1),/store)
      dat=get_fa_fields('V5-V8_S',t,/start)
      if dat.valid eq 0 then begin
        print,' ERROR: No FAST V5-V8 data-get_fa_fields returned invalid data'
        data_valid=0.0
      endif else begin
        spacecraft_potential=get_fa_fields('V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
        efieldV58=get_fa_fields('V5-V8_S',time_ranges(jjj,0),time_ranges(jjj,1))
        efieldV1214=get_fa_fields('V1-V2_S',time_ranges(jjj,0),time_ranges(jjj,1))
        if efieldV1214.valid eq 0 then begin
          print,'No V1-V2 data - trying V1-V4'
          efieldV1214=get_fa_fields('V1-V4_S',time_ranges(jjj,0),time_ranges(jjj,1))
          if efieldV1214.valid eq 0 then begin
            print,' ERROR: No FAST fields data - get_fa_fields returned invalid data'
            data_valid=0.0
          endif
        endif
      endelse

      Langmuir_2=get_fa_fields('NE2_S',time_ranges(jjj,0),time_ranges(jjj,1))
      Langmuir_6=get_fa_fields('NE6_S',time_ranges(jjj,0),time_ranges(jjj,1))
      Langmuir_9=get_fa_fields('NE9_S',time_ranges(jjj,0),time_ranges(jjj,1))
      Langmuir_data=[0]
      Langmuir_time=[0]
      Langmuir_prob=[0]
      if Langmuir_2.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_2.comp1]
        langmuir_time=[Langmuir_time,Langmuir_2.time]
        langmuir_prob=[Langmuir_prob,replicate(2,n_elements(Langmuir_2.time))]
      endif
      if Langmuir_6.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_6.comp1]
        langmuir_time=[Langmuir_time,Langmuir_6.time]
        langmuir_prob=[Langmuir_prob,replicate(6,n_elements(Langmuir_6.time))]
      endif
      if Langmuir_9.valid NE 0 then begin
        langmuir_data=[Langmuir_data,Langmuir_9.comp1]
        langmuir_time=[Langmuir_time,Langmuir_9.time]
        langmuir_prob=[Langmuir_prob,replicate(9,n_elements(Langmuir_9.time))]
      endif
      if n_elements(langmuir_data) GT 1 then  begin
        langmuir_time=langmuir_time(1:n_elements(Langmuir_time)-1)
        langmuir_data=langmuir_data(1:n_elements(Langmuir_time)-1)
        langmuir_prob=langmuir_prob(1:n_elements(Langmuir_time)-1)
        time_order_langmuir=sort(langmuir_time)
        langmuir={x:langmuir_time(time_order_langmuir),y:langmuir_data(time_order_langmuir)}
        dens_probe={x:langmuir_time(time_order_langmuir),y:langmuir_prob(time_order_langmuir)}
      endif else data_valid=0.0
    endelse

    if data_valid NE 0.0 then begin

      if not keyword_set(ucla_mag_despin) then begin
        get_data,'MagDCcomp1',data=magx
        get_data,'MagDCcomp2',data=magy
        get_data,'MagDCcomp3',data=magz
      endif else begin
        get_data,'dB_fac_v',data=db_fac
        mintime=min(abs(time_ranges(jjj,0)-db_fac.x),ind1)
        mintime=min(abs(time_ranges(jjj,1)-db_fac.x),ind2)

        magx={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,0)}
        magy={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,2)}
        magz={x:db_fac.x(ind1:ind2),y:db_fac.y(ind1:ind2,1)}
      endelse

      store_data,'MagZ',data=magz
      ;magz.y=smooth(magz.y,40)
      store_data,'Magz_smooth',data={x:magz.x,y:magz.y}
      if keyword_set(filterfreq) then begin

        magz=filter(magz,filterfreq,'magfilt','l')
        ;remove end effects of the filter by cutting off the first/last 2s
        sf=magz.x(1)-magz.x(0)
        np=n_elements(magz.x)
        padding=round(2.0/sf)
        magz={x:magz.x(padding:np-padding),y:magz.y(padding:np-padding)}
        store_data,'MagZ',data=magz
      endif

      ;get_orbit data

      get_fa_orbit,je_tmp_time,/time_array,/all

      ;define loss cone angle

      get_data,'ALT',data=alt
      loss_cone_alt=alt.y(0)*1000.0
      lcw=loss_cone_width(loss_cone_alt)*180.0/!DPI
      get_data,'ILAT',data=ilat
      north_south=abs(ilat.y(0))/ilat.y(0)

      ;get orbit number for filenames
      get_data,'ORBIT',data=tmp
      orbit=tmp.y(0)
      orbit_num=strcompress(string(tmp.y(0)),/remove_all)

; if doing Chaston plots, get prepped 
  IF KEYWORD_SET(do_ch) THEN BEGIN
     IF CH_INT EQ !NULL THEN BEGIN
      PRINT,"No current interval specified for Chaston plots! Doing interval 0..."
      CH_INT = 0
     ENDIF
     chastondbdir='/SPENCEdata2/Research/Cusp/database/current_db/'
     outdir='/SPENCEdata2/Research/Cusp/ACE_FAST/Compare_new_DB_with_Chastons/txtoutput/'
     !PATH = '/SPENCEdata2/Research/Cusp/ACE_FAST/Compare_new_DB_with_Chastons/:' + !PATH

     orbit=orbit_num[0]
     savsuf=".sav"
     basename='dflux_'+strcompress(str(orbit)+"_"+str(ch_int),/remove_all)
     chastonfname=chastondbdir+basename
     chastonoutname=outdir+'chast_'+basename+savsuf

     print, "Chaston db file: " + chastonfname

     ;get Chaston db file in memory
     combine_dflux_dartchast,orbit, 0, in_name=chastonfname,outname=chastonoutname
     restore, chastonoutname

     n_chast = N_ELEMENTS(dat.time)
     data_chast = dat
     
     IF NOT KEYWORD_SET(jmag_thres) THEN jmag_thres = 5.0
     PRINT,"Current threshold set to " + string(jmag_thres)
  ENDIF



      get_data,'fa_vel',data=vel
      speed=sqrt(vel.y(*,0)^2+vel.y(*,1)^2+vel.y(*,2)^2)*1000.0

      old_pos=0.
      position=make_array(n_elements(magz.x),/double)
      speed_mag_point=make_array(n_elements(magz.x),/double)
      for j=0L,n_elements(magz.x)-2 do begin
        speed_point_ind=min(abs(vel.x-magz.x(j)),ind)
        ;print,ind
        speed_mag_point(j)=speed(ind)
        samplingperiod=magz.x(j+1)-magz.x(j)
        ;position=make_array(n_elements(magz.x),/double)
        position(j)=old_pos+speed_mag_point(j)*samplingperiod
        old_pos=position(j)
      endfor

      ;calculate the current from mag

      deltaBX=deriv(position,magz.y)
      jtemp=abs(1.0e-3*(deltaBx)/1.26e-6)
      sign_jtemp=abs(deltaBx)/deltaBx
      store_data,'j_mag',data={x:magz.x,y:jtemp}
      ;terminate the intervals before the last point

      if sign_jtemp(n_elements(jtemp)-1)*sign_jtemp(n_elements(jtemp)-2) NE -1 then sign_jtemp(n_elements(jtemp)-1)=-1*sign_jtemp(n_elements(jtemp)-1)

      print,'time_range ',time_to_str(time_ranges(jjj,0)),time_to_str(time_ranges(jjj,1))

      IF KEYWORD_SET(do_o) THEN BEGIN
        cgPS_Open, 'magz_orbit' + strcompress(orbit_num+'_'+string(jjj),/remove_all) + '.ps'
        loadct,39
        !p.charsize=1.3
        tplot,['MagZ','j_mag'] ,var_label=['ALT','MLT','ILAT'],trange=[time_ranges(jjj,0),time_ranges(jjj,1)]
        cgPS_Close, /PNG, /Delete_PS, Width=1000
      ENDIF   
      
      IF KEYWORD_SET(do_ch) AND jjj EQ ch_int THEN BEGIN
         PRINT, "Doing Chaston plots..."
         FOR jj=0L,n_chast-1 DO BEGIN
           cur_time = str_to_time(data_chast.time[jj])
           IF cur_time GT time_ranges(jjj,0) AND cur_time LT time_ranges(jjj,1) THEN BEGIN
;            IF ABS(data_chast.mag_current[jj]) GE jmag_thres THEN BEGIN
              fname='plots/chastplots_' + strcompress(orbit_num+'_'+string(jjj)+'_'+string(jj),/remove_all) + '.ps'
              plotstr = "B!Dz!N and J!Dmag!N for Chaston event " + str(jj)
              tplot_options,'title',plotstr
              cgPS_Open,fname
              loadct,39
              !p.charsize = 1.3
              tfirst = cur_time-0.25
              tlast =  cur_time+0.25
              tplot,['MagZ','j_mag'] ,var_label=['ALT','MLT','ILAT'],trange=[tfirst,tlast]
              cgPS_Close, /PNG,/delete_ps, WIDTH=1000
;            ENDIF
           ENDIF ELSE PRINT,$
            FORMAT='("Chaston event[",I-0,"]: ",A-0," outside range (for jjj=",I-0,")")',$
            jj,data_chast.time[jj],jjj
         ENDFOR
         PRINT, "Done with Chaston plots for interval " + STRCOMPRESS(str(jjj),/REMOVE_ALL)
      ENDIF


    endif else print,"Data for current interval " + string(jjj) + " is INVALID!"; end of data_valid if statement

endfor ;end of number_of_intervals (indexed by jjj) loop

return
end

  
