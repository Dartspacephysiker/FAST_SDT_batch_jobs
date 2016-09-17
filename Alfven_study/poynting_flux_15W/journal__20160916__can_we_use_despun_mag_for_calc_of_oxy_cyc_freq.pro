;;09/16/16
;;We here learn that we CANNOT use despun mag data for the oxy cyc freq. It's not sufficiently awesome.
;;See the section with this label:
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;************************************;;
        ;;EXPERIMENT
        ;;************************************;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO JOURNAL__20160916__CAN_WE_USE_DESPUN_MAG_FOR_CALC_OF_OXY_CYC_FREQ, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   T1=t1, $
   T2=t2, $
   BURST=burst, $
   UCLA_MAG_DESPIN=ucla_mag_despin

  COMPILE_OPT IDL2

  IF N_ELEMENTS(ucla_mag_despin)     EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval)  EQ 0 THEN below_auroral_oval  = 1

  ;;For FFTs of Mag and E data
  FFTLen            = 1024
  nFFTAvg           = 1

  maxPeriod         = 1/100.
  minPeriod         = 1/132.
  eFSampFact        = 4         ;The fact is, E-field data gets sampled a million times faster!
  FFTFreqRange      = [0.1,19.0]
  FFTSlide          = 1.0

  ;;energy ranges
  IF not keyword_set(energy_electrons) THEN energy_electrons = [0.,30000.] ;use 0.0 for lower bound since the sc_pot is used to set this
  IF not keyword_set(energy_ions) THEN energy_ions = [0.,500.]             ;use 0.0 for lower bound since the sc_pot is used to set this

  ;; IF no data exists, return to main
  t = 0
  dat = get_fa_ees(t,/st)
  IF dat.valid eq 0 THEN BEGIN
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
;     return
  ENDIF

  ;; Electron current - line plot
  IF keyword_set(burst) THEN BEGIN
     get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,NAME='Je',ENERGY=energy_electrons
  ENDIF ELSE BEGIN
     get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,NAME='Je',ENERGY=energy_electrons
  endelse
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep = WHERE(FINITE(tmpj.y) NE 0)
  tmpj.x = tmpj.x[keep]
  tmpj.y = tmpj.y[keep]
  
  keep = WHERE(ABS(tmpj.y) GT 0.0)
  tx = tmpj.x[keep]
  ty = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order = sort(tx)
  tx = tx[time_order]
  ty = ty[time_order]
  
  
  ;;throw away the first 10  points since they are often corrupted
  IF not keyword_set(burst) THEN BEGIN
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
     keep = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr='--below_aur_oval'
  ENDIF ELSE BEGIN
     keep = WHERE(ABS(ilat.y) GT AURORAL_ZONE(mlt.y,7,/lat)/(!DPI)*180.)
     belowAurOvalStr=''
  ENDELSE

  STORE_DATA,'Je',DATA={x:je.x[keep],y:je.y[keep]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je = MAKE_ARRAY(N_ELEMENTS(Je.x),/double)
  FOR j=1,N_ELEMENTS(Je.x)-1 DO BEGIN
     part_res_je[j] = ABS(Je.x[j]-Je.x[j-1])
  endfor
  part_res_Je[0] = part_res_Je[1]
  gap = WHERE(part_res_je GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start = [0,WHERE(part_res_je GT 10.0)]
     separate_stop = [WHERE(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  ENDIF ELSE BEGIN
     separate_start = [0]
     separate_stop = [N_ELEMENTS(Je.x)-1]
  endelse
  
  ;;remove esa burp when switched on
  IF not keyword_set(burst) THEN BEGIN
     turn_on = WHERE(part_res_je GT 300.0)
     IF turn_on[0] NE -1 THEN BEGIN
        turn_on_separate = MAKE_ARRAY(N_ELEMENTS(turn_on),/double)
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
  
  print,'number_of_intervals',number_of_intervals
  
  ji_tot               = MAKE_ARRAY(number_of_intervals,/double)
  ji_up_tot            = MAKE_ARRAY(number_of_intervals,/double)
  jee_tot              = MAKE_ARRAY(number_of_intervals,/double)
  Ji_tot_alf           = MAKE_ARRAY(number_of_intervals,/double)
  Ji_up_tot_alf        = MAKE_ARRAY(number_of_intervals,/double)
  Jee_tot_alf          = MAKE_ARRAY(number_of_intervals,/double)
  
  ;;get despun mag data IF keyword set
  IF KEYWORD_SET(ucla_mag_despin) THEN ucla_mag_despin
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN
     print,'time_range',TIME_TO_STR(time_ranges[jjj,0]),TIME_TO_STR(time_ranges[jjj,1])
     
     ;;get orbit number for filenames		
     GET_DATA,'ORBIT',DATA=tmp
     orbit = tmp.y[0]
     orbit_num = STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)


     je_tmp_time = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     
     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}
     
     
     magDC = GET_FA_FIELDS('MagDC',t,/START,/CALIBRATE,/REPAIR)
     IF magDC.valid EQ 0 THEN BEGIN
        PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
        RETURN
     ENDIF 
     
     ;;Get two types of potential. Which DO we like better?
     spacecraft_potential = GET_FA_FIELDS('V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     sc_pot2              = GET_FA_POTENTIAL(time_ranges[jjj,0],time_ranges[jjj,1])

     IF (sc_pot2.valid) AND (dat.valid) THEN BEGIN
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get Mag and E field data
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;	E component: “E_along_V”
        ;;	B components: Two components perpendicular to E_along_V
        ;;		a. Use Strangeway despinning, find coords that complement E_along_V
        ;;		b. Need to use Bob model to subtract the background field

        IF not keyword_set(ucla_mag_despin) THEN BEGIN
           GET_DATA,'MagDCcomp1',DATA=magx
           GET_DATA,'MagDCcomp2',DATA=magy
           GET_DATA,'MagDCcomp3',DATA=magz
        ENDIF ELSE BEGIN
           GET_DATA,'dB_fac_v',DATA=db_fac
           mintime = MIN(ABS(time_ranges[jjj,0]-db_fac.x),ind1)
           mintime = MIN(ABS(time_ranges[jjj,1]-db_fac.x),ind2)
           ;;   From UCLA_MAG_DESPIN: "Field-aligned velocity-based coordinates defined as:    "
           ;;x (ind 0)-along track ((BxV)xB),
           ;;y (ind 1)-cross track (BxV), 
           ;;z (ind 2)-along B" (I added "ind" marks)
           magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} 
           magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} 
           magz = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}
        endelse
        
        ;;E field
        FA_FIELDS_DESPIN,efieldV58,efieldV1214,/SLOW
        ;; GET_DATA,'E_NEAR_B',DATA=eNearB
        GET_DATA,'E_ALONG_V',DATA=eAlongV
        IF SIZE(eAlongV,/TYPE) NE 8 THEN BEGIN
           PRINT,"Couldn't get E_ALONG_V!" 
           STOP
        ENDIF

        ;;Now check sorted/dupes
        CHECK_DUPES,magz.x,HAS_DUPES=magHasDupes, $
                    IS_SORTED=magIsSort,OUT_UNIQ_I=magUniq_i,/QUIET
        IF magHasDupes OR ~magIsSort THEN BEGIN
           PRINT,'Mag has dupes/is not sorted! Sorting ...'
           magx = {x:magx.x[magUniq_i],y:magx.y[magUniq_i]}
           magy = {x:magy.x[magUniq_i],y:magy.y[magUniq_i]}
           magz = {x:magz.x[magUniq_i],y:magz.y[magUniq_i]}
        ENDIF

        CHECK_DUPES,eAlongV.x,HAS_DUPES=eAVHasDupes, $
                    IS_SORTED=eAVIsSort,OUT_UNIQ_I=eAVUniq_i,/QUIET
        IF eAVHasDupes OR ~eAVIsSort THEN BEGIN
           PRINT,'EAV has dupes/is not sorted! Sorting ...'
           eAlongV = {x:eAlongV.x[eAVUniq_i],y:eAlongV.y[eAVUniq_i]}
        ENDIF

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;ii. Restrict to periods with 128 S/s 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;NOTE, we just use FA_FIELDS_BUFS because it does a great job.
        ;;See JOURNAL__20160916__MAKE_SURE … addressing this question, and try
        ;;feeding it orbit 6127. You'll see.
        FA_FIELDS_BUFS,{time:magz.x},FFTLen, $
                       BUF_STARTS=strtM_i, $
                       BUF_ENDS=stopM_i, $
                       DELTA_T=1.0e-5 ;Allowable error
        FA_FIELDS_BUFS,{time:eAlongV.x},FFTLen, $
                       BUF_STARTS=strtE_i, $
                       BUF_ENDS=stopE_i, $
                       DELTA_T=1.0e-5 ;Allowable error

        ntot = long(nFFTAvg) * long(FFTLen)
        ;; BREAK THE DATA INTO BUFFERS
        FA_FIELDS_BUFS,{time:magz.x}, ntot, $
                       BUF_STARTS=strtM_i, $
                       BUF_ENDS=stopM_i, $
                       DELTA_T=1.0e-5 ;Allowable error
                       
        
        IF strtM_i[0] EQ stopM_i[0] THEN BEGIN
           PRINT, 'FA_FIELDS_SPEC: Unable to extract continuous buffers from'
           PRINT, 'time series data - try changing NFFTAVG and FFTLEN to get a '
           PRINT, 'smaller buffer size.'
           RETURN
        endif
        
        nbufs = N_ELEMENTS(strtM_i)
        
        ;; ESTIMATE THE SIZE OF THE RESULT
        all        = TOTAL( stopM_i-strtM_i ) + nbufs    
        num_ffts   = LONG(all/(ntot *  FFTSlide))
        num_freqs  = FFTLen/2+1.
        timeFFT    = MAKE_ARRAY(num_ffts,VALUE=0.D,/DOUBLE)
        FFT_i      = 0l
        dt         = FLTARR(nbufs)
        
        fftBin_i   = MAKE_ARRAY(2,num_ffts,/LONG)
        FOR i=0,nbufs-1 DO BEGIN
           n_start           = LONG(strtM_i[i])
           n_stop            = n_start + ntot - 1l

           dt[i]             = magz.x[n_start+1] - magz.x[n_start]
           
           WHILE (n_stop LE stopM_i[i]) DO BEGIN            
              timeFFT[FFT_i]    = magz.x[(n_start+n_stop)/2]
              fftBin_i[*,FFT_i] = [n_start,n_stop]
              FFT_i++
              n_start           = n_start + LONG(ntot*FFTSlide)            
              n_stop            = n_start + ntot - 1l
           ENDWHILE
           
           ;;Humdiddle
           norm_dt           = MEDIAN(dt)
           abnorm            = WHERE(dt NE norm_dt)    
           
        ENDFOR
        
        discardMe            = WHERE(timeFFT LE 1.0,nDiscard)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Get density estimates for all good times
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;now get orbit quantities
        GET_DATA,'ORBIT',DATA=orb
        GET_DATA,'MLT',DATA=mlt
        GET_DATA,'ALT',DATA=alt
        GET_DATA,'ILAT',DATA=ilat
        GET_DATA,'fa_vel',DATA=vel
        

        ephemI_magSpec = VALUE_LOCATE(mlt.x,timeFFT)
        
        magOrb   = orb.y[ephemI_magSpec]
        magAlt   = alt.y[ephemI_magSpec]	
        magMLT   = mlt.y[ephemI_magSpec]	
        magILAT  = ilat.y[ephemI_magSpec]
        magSpd   = SQRT(vel.y[ephemI_magSpec,0]^2 + $
                        vel.y[ephemI_magSpec,1]^2 + $
                        vel.y[ephemI_magSpec,2]^2)*1000.0                        ;Speed in m/s
        magDens  = DENS_MLT(magAlt,magMLT,OUT_VA_KM_PER_SEC=va_mlt) ;in cm^-3
        magDens *= 1.0e6 ;Dens est in m^-3
        va_mlt  *= 1000. ;Get estimate of v_A in meters

        ;;What's the lower bound on frequency?
        ;; const    = 0.0029949602 ;in meter^0.5
        const    = 2.9949602e-8 ;in meter^0.5
        lowFreq  = const * SQRT(magDens) * magSpd
                                
        ;; PRINT,lowFreq

        freqRes     = 0.1D ;in Hz
        roundedFreq = ROUND(lowFreq/freqRes)*freqRes
        fHist       = HISTOGRAM(roundedFreq, $
                                BINSIZE=freqRes/2.D,LOCATIONS=Freqs, $
                                REVERSE_INDICES=fH_revI, $
                                MIN=MIN(roundedFreq)-freqRes/4.D)
        omegaP_i = WHERE(fHist GT 0,nOmegaPEsts)
        ;; omegaPEsts = Freqs[omegaP_i]


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;************************************;;
        ;;EXPERIMENT
        ;;************************************;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;Now stuff below gyro freq
        ;;Get DC mag field for this
        ;; GET_DATA,'MagDCcomp1',DATA=magDCx
        ;; GET_DATA,'MagDCcomp2',DATA=magDCy
        ;; GET_DATA,'MagDCcomp3',DATA=magDCz
        magDC_i = VALUE_LOCATE(magDC.time,timeFFT)
        ;;Oxygen cyclotron frequency is the upper bound
        oxy_cycDC = 1.6e-19*SQRT(magDC.comp1[magDC_i]^2+$
                                 magDC.comp2[magDC_i]^2+$
                                 magDC.comp3[magDC_i]^2)* $
                  1.0e-9/2.6567e-26/(2.*!DPI) ; in Hz

        mag_i = VALUE_LOCATE(magz.x,timeFFT)
        ;;Oxygen cyclotron frequency is the upper bound
        oxy_cyc = 1.6e-19*SQRT(magx.y[mag_i]^2+magy.y[mag_i]^2+magz.y[mag_i]^2)* $
                  1.0e-9/2.6567e-26/(2.*!DPI) ; in Hz

        
        STOP

     ENDIF
  ENDFOR

END
