;;09/16/16
;;See the section with this text block below:
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;************************************;;
        ;;EXPERIMENT
        ;;************************************;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO JOURNAL__20160916__MAKE_SURE_FA_FIELDS_BUFS_DOESNT_INCLUDE_MULTIPLE_SAMPLE_RATES, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   T1=t1, $
   T2=t2, $
   BURST=burst, $
   UCLA_MAG_DESPIN=ucla_mag_despin

  IF N_ELEMENTS(ucla_mag_despin)     EQ 0 THEN ucla_mag_despin     = 1
  IF N_ELEMENTS(below_auroral_oval)  EQ 0 THEN below_auroral_oval  = 1

  ;;For FFTs of Mag and E data
  FFTLen            = 1024

  maxPeriod         = 1/100.
  minPeriod         = 1/132.

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
  IF keyword_set(ucla_mag_despin) THEN ucla_mag_despin
  
  ;;begin looping each interval
  FOR jjj=0,number_of_intervals-1 DO BEGIN
     print,'time_range',TIME_TO_STR(time_ranges[jjj,0]),TIME_TO_STR(time_ranges[jjj,1])
     
     ;;get orbit number for filenames		
     GET_DATA,'ORBIT',DATA=tmp
     orbit = tmp.y[0]
     orbit_num = STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)

                                ;filename for output file
     
     je_tmp_time = je.x[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     je_tmp_data = je.y[time_range_indices[jjj,0]:time_range_indices[jjj,1]]
     
     STORE_DATA,'Je_tmp',DATA={x:je_tmp_time,y:je_tmp_data}
     
     
     dat = get_fa_fields('MagDC',t,/start)
     IF dat.valid EQ 0 THEN BEGIN
        PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     ENDIF 
     
     ;;Get two types of potential. Which DO we like better?
     spacecraft_potential = GET_FA_FIELDS('V8_S',time_ranges[jjj,0],time_ranges[jjj,1])
     sc_pot2              = GET_FA_POTENTIAL(time_ranges[jjj,0],time_ranges[jjj,1])

     IF (sc_pot2.valid) AND (dat.valid) THEN BEGIN
        
        IF not keyword_set(ucla_mag_despin) THEN BEGIN
           GET_DATA,'MagDCcomp1',DATA=magx
           GET_DATA,'MagDCcomp2',DATA=magy
           GET_DATA,'MagDCcomp3',DATA=magz
        ENDIF ELSE BEGIN
           GET_DATA,'dB_fac_v',DATA=db_fac
           mintime = MIN(ABS(time_ranges[jjj,0]-db_fac.x),ind1)
           mintime = MIN(ABS(time_ranges[jjj,1]-db_fac.x),ind2)
                                ;   From UCLA_MAG_DESPIN:
           magx = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]} ;   "Field-aligned velocity-based coordinates defined as:    "
           magy = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]} ;   "z (ind 2)-along B, y (ind 1)-cross track (BxV), x (ind 0)-along track ((BxV)xB)." (I added "ind" marks)
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
        magPeriod        = magz.x
        magPeriod[*]     = 0
        magPeriod[0:-2]  = (SHIFT(magz.x,-1)-magz.x)[0:-2]
        magPeriod[-1]    = magPeriod[-2]
        bigGaps          = WHERE(magPeriod GT 10,nBigGaps)
        IF nBigGaps GT 0 THEN BEGIN
           PRINT,'Big gaps magz! Decide!'
        ENDIF
        CASE 1 OF
           KEYWORD_SET(only_128Ss_data): BEGIN
              keepMag_i  = WHERE(magPeriod LE maxPeriod AND $
                                       magPeriod GE minPeriod,nBigGaps)
           END
           ELSE: BEGIN
              keepMag_i  = WHERE(magPeriod LE maxPeriod,nBigGaps)

              ;;Get data with appropriate sample rates
              CHECK_DUPES,magPeriod,HAS_DUPES=magPeriodDupes, $
                          IS_SORTED=magPeriodIsSort,OUT_UNIQ_I=magPeriodUniq_i,/QUIET
              
              PRINT,'You still need to handle this ... someday'

           END
        ENDCASE
        GET_STREAKS,keepMag_i,START_I=strtMag_ii,STOP_I=stopMag_ii, $
                    OUT_STREAKLENS=magStreakLens, $
                    MIN_STREAK_TO_KEEP=FFTLen
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;************************************;;
        ;;EXPERIMENT
        ;;************************************;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;Little experiment: Compare my GET_STREAKS method for identifying segments when
        ;;sample rate is 128 S/s with what comes out of FA_FIELDS_BUF
        guy     = {time:magz.x,comp1:magz.y,ncomp:1}
        guyPare = {time:magz.x[keepMag_i],comp1:magz.y[keepMag_i],ncomp:1}
        magParePeriod = magPeriod[keepMag_i]
        FA_FIELDS_BUFS,guy,FFTLen, $
                       BUF_STARTS=strtM_i, $
                       BUF_ENDS=stopM_i, $
                       DELTA_T=1.0e-5 ;Allowable error
        FA_FIELDS_BUFS,guyPare,FFTLen, $
                       BUF_STARTS=strtMP_i, $
                       BUF_ENDS=stopMP_i, $
                       DELTA_T=1.0e-5 ;Allowable error

        nBufs   = N_ELEMENTS(strtM_i)
        nPBufs  = N_ELEMENTS(strtMP_i)
        nRatesInBuf = MAKE_ARRAY(nBufs)
        nRatesInPBuf = MAKE_ARRAY(nBufs)
        FOR k=0,nBufs-1 DO BEGIN

           ;;See if sample rate is the same for these guys
           tmp_i  = [strtM_i[k]:stopM_i[k]]
           CHECK_DUPES,magPeriod[tmp_i], $
                       HAS_DUPES=tmpDupes, $
                       IS_SORTED=tmpSort, $
                       OUT_UNIQ_I=tmpUniq_ii, $
                       /QUIET
           
           PRINT,''
           PRINT,'**********'
           PRINT,''
           nRatesInBuf[k]  = N_ELEMENTS(tmpUniq_ii)
           PRINT,FORMAT='("BufStart, BufStop",T30,I0,",",T40,I0)',tmp_i[0],tmp_i[-1]
           PRINT,"Sample rates in this buff: " 
           FOR kk=0,nRatesInBuf[k]-1 DO BEGIN
              PRINT,FORMAT='(F12.7," S/s",T22,"(",I0,")")', $
                    magPeriod[tmp_i[tmpUniq_ii[kk]]], $
                    N_ELEMENTS(WHERE(ABS(magPeriod[tmp_i]- $
                                         magPeriod[tmp_i[tmpUniq_ii[kk]]]) LT 1.0e-5))
                    
           ENDFOR
           ;; IF tmpDupes THEN PRINT,"Dupes in buffer " + STRCOMPRESS(k,/REMOVE_ALL) + '!'
           IF nRatesInBuf[k]  GT 1 THEN PRINT,"Multiple sRates in buff " + $
                                        STRCOMPRESS(k,/REMOVE_ALL) + '!'

           IF ~tmpSort THEN PRINT,"Buffer " + STRCOMPRESS(k,/REMOVE_ALL) + ' not sorted!'

           PRINT,''
           ;;Now pared buff,if mulig
           IF k GE nPBufs THEN CONTINUE

           tmpP_i = [strtMP_i[k]:stopMP_i[k]]
           CHECK_DUPES,magParePeriod[tmpP_i], $
                       HAS_DUPES=tmpPDupes, $
                       IS_SORTED=tmpPSort, $
                       OUT_UNIQ_I=tmpPUniq_ii, $
                       /QUIET
           nRatesInPBuf[k] = N_ELEMENTS(tmpPUniq_ii)
           PRINT,FORMAT='("Pared BufStart, BufStop",T30,I0,",",T40,I0)', $
                 keepMag_i[tmpP_i[0]], $
                 keepMag_i[tmpP_i[-1]]
           PRINT,"Sample rates in pared buff: " 
           FOR kk=0,nRatesInPBuf[k]-1 DO BEGIN
              PRINT,FORMAT='(F12.7," S/s",T22,"(",I0,")")', $
                    magParePeriod[tmpP_i[tmpPUniq_ii[kk]]], $
                    N_ELEMENTS(WHERE(ABS(magParePeriod[tmpP_i]- $
                                         magParePeriod[tmpP_i[tmpPUniq_ii[kk]]]) LT 1.0e-5))
           ENDFOR

           IF nRatesInPBuf[k] GT 1 THEN PRINT,"Multiple sRates in pared buff " + $
                                              STRCOMPRESS(k,/REMOVE_ALL) + '!'

           IF ~tmpPSort THEN PRINT,"ParedBuffer " + STRCOMPRESS(k,/REMOVE_ALL) + ' not sorted!'
           PRINT,''
        ENDFOR

     ENDIF

  ENDFOR
END
