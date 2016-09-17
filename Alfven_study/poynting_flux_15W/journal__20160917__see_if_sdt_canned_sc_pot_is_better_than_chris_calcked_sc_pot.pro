;;09/17/16
;;And from this we conclude that it's wiser to use GET_FA_POTENTIAL than whatever
;;else was tempting us
PRO JOURNAL__20160917__SEE_IF_SDT_CANNED_SC_POT_IS_BETTER_THAN_CHRIS_CALCKED_SC_POT, $
   BELOW_AURORAL_OVAL=below_auroral_oval, $
   ENERGY_ELECTRONS=energy_electrons, $
   T1=t1, $
   T2=t2, $
   BURST=burst, $
   UCLA_MAG_DESPIN=ucla_mag_despin

  IF N_ELEMENTS(ucla_mag_despin)     EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval)  EQ 0 THEN below_auroral_oval  = 1

  ;;energy ranges
  IF NOT KEYWORD_SET(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; IF no data exists, return to main
  t   = 0
  dat = GET_FA_EES(t,/ST)
  IF dat.valid eq 0 THEN BEGIN
     PRINT,' ERROR: No FAST electron survey data -- get_fa_ees(t,/ST) returned invalid data'
;     RETURN
  ENDIF

  ;; Electron current - line plot
  IF KEYWORD_SET(burst) THEN BEGIN
     GET_2DT_TS,'j_2d_b','fa_eeb',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  ENDIF ELSE BEGIN
     GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2,NAME='Je',ENERGY=energy_electrons
  endelse
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep        = WHERE(FINITE(tmpj.y) NE 0)
  tmpj.x      = tmpj.x[keep]
  tmpj.y      = tmpj.y[keep]
  
  keep        = WHERE(ABS(tmpj.y) GT 0.0)
  tx          = tmpj.x[keep]
  ty          = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order  = SORT(tx)
  tx          = tx[time_order]
  ty          = ty[time_order]
  
  
  ;;throw away the first 10  points since they are often corrupted
  IF not KEYWORD_SET(burst) THEN BEGIN
     STORE_DATA,'Je',DATA={x:tx[10:N_ELEMENTS(tx)-1],y:ty[10:N_ELEMENTS(tx)-1]}
  ENDIF ELSE BEGIN
     STORE_DATA,'Je',DATA={x:tx,y:ty}
  endelse
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  GET_DATA,'Je',DATA=je
  GET_FA_ORBIT,/TIME_ARRAY,je.x
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  IF KEYWORD_SET(below_auroral_oval) THEN BEGIN
     keep             = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr  = '--below_aur_oval'
  ENDIF ELSE BEGIN
     keep             = WHERE(ABS(ilat.y) GT AURORAL_ZONE(mlt.y,7,/LAT)/(!DPI)*180.)
     belowAurOvalStr  = ''
  ENDELSE

  STORE_DATA,'Je',DATA={x:je.x[keep],y:je.y[keep]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je         = MAKE_ARRAY(N_ELEMENTS(Je.x),/DOUBLE)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j]   = ABS(Je.x[j]-Je.x[j-1])
  endfor
  part_res_Je[0]      = part_res_Je[1]
  gap                 = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start   = [0,WHERE(part_res_je GT 10.0)]
     separate_stop    = [WHERE(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start   = [0]
     separate_stop    = [N_ELEMENTS(Je.x)-1]
  endelse
  
  ;;remove esa burp when switched on
  IF not KEYWORD_SET(burst) THEN BEGIN
     turn_on = WHERE(part_res_je GT 300.0)
     IF turn_on[0] NE -1 THEN BEGIN
        turn_on_separate = MAKE_ARRAY(N_ELEMENTS(turn_on),/DOUBLE)
        FOR j=0,N_ELEMENTS(turn_on)-1 DO turn_on_separate[j] = WHERE(separate_start EQ turn_on[j])
        separate_start[turn_on_separate+1] = separate_start[turn_on_separate+1]+5
     ENDIF
  ENDIF

  ;;identify time indices for each interval
  count = 0.0
  FOR j=0,N_ELEMENTS(separate_start)-1 DO BEGIN
     IF (separate_stop[j]-separate_start[j]) GT 10 THEN BEGIN
        count = count+1
        IF count EQ 1.0 THEN BEGIN
           time_range_indices = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        ENDIF ELSE BEGIN
           time_range_indices = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        endelse
     ENDIF
  endfor
  
  ;;identify interval times
  time_ranges          = je.x[time_range_indices]
  number_of_intervals  = N_ELEMENTS(time_ranges[*,0])
  
  PRINT,'number_of_intervals',number_of_intervals
  
  ji_tot               = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  ji_up_tot            = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  jee_tot              = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Ji_tot_alf           = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Ji_up_tot_alf        = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  Jee_tot_alf          = MAKE_ARRAY(number_of_intervals,/DOUBLE)
  
  ;;get despun mag data IF keyword set
  IF KEYWORD_SET(ucla_mag_despin) THEN ucla_mag_despin
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN
     PRINT,'time_range',TIME_TO_STR(time_ranges[jjj,0]),TIME_TO_STR(time_ranges[jjj,1])
     
     ;;get orbit number for filenames		
     GET_DATA,'ORBIT',DATA=tmp
     orbit      = tmp.y[0]
     orbit_num  = STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)

     je_tmp_time = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     
     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}
     
     magDC = GET_FA_FIELDS('MagDC',t,/START,/CALIBRATE,/REPAIR)
     ;; dat = get_fa_fields('MagDC',t,/START)
     IF magDC.valid EQ 0 THEN BEGIN
        PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
        RETURN
     ENDIF 
     
     ;;Get two types of potential. Which DO we like better?
     spacecraft_potential = GET_FA_FIELDS('V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     yunk                 = GET_FA_POTENTIAL(time_ranges[jjj,0],time_ranges[jjj,1], $
                                             ;; /SPIN, $
                                             /STORE, $
                                             /REPAIR, $
                                             STRUCTURE=sc_pot2)

     ;;get the spacecraft potential per spin
     spin_period = 4.946        ; seconds
     
     ;;get_sample_rate
     v8                         = {x:spacecraft_potential.time,y:spacecraft_potential.comp1}
     
     v8_dt                      = ABS(v8.x-shift(v8.x,-1))
     v8_dt[0]                   = v8_dt[1]
     v8_dt[N_ELEMENTS(v8.x)-1]  = v8_dt[N_ELEMENTS(v8.x)-2]

     ;;get maxima within a 1 spin window
     j_range                    = WHERE(v8.x LT v8.x(N_ELEMENTS(v8.x)-1)-spin_period)
     index_max                  = MAX(j_range)
     PRINT,index_max
     pot                        = MAKE_ARRAY(N_ELEMENTS(v8.x),/DOUBLE)
     FOR j=0L,index_max DO BEGIN
        ;;spin_range            = WHERE(v8.x GE v8.x(j) and v8.x LE v8.x(j)+spin_period)
        spin_range              = j+FINDGEN(CEIL(spin_period/V8_dt(j)))
        pot[j]                  = MAX(ABS(v8.y[spin_range]),ind)
        sign                    = v8.y[spin_range[ind]]/ABS(v8.y[spin_range[ind]])
        pot[j]                  = sign*pot[j]
                                ;print,j,pot[j]
     ENDFOR
     pot[index_max+1:N_ELEMENTS(v8.x)-1] = pot[j_range[index_max]]
     sc_pot = {x:v8.x,y:pot}
     ;; sc_pot    = {x:sc_pot2,y:pot}
     STORE_DATA,'S_Pot',DATA=sc_pot, $ ;note this is actualy the negative of the sp. potential this corrected in the file output
                DLIM={xstyle:1,ytitle:'Chris pot!C!C(Volts)'}

     TPLOT,['S_Pot','s/c potential'], $
           TRANGE=(KEYWORD_SET(t1) AND KEYWORD_SET(t2)) ? [t1,t2] : !NULL
     STOP
  ENDFOR
END
