;;2016/05/19
;;Let's see how Newell's algorithm does for picking out the clearly visible inverted V from orbit 17773 treated by Elphic et al. [1998]
PRO JOURNAL__20160519__PRELIMINARIES_WITH_ORBIT_1773,EVENTS=events

  energy_electrons        = [50.,30000.]
  t1Str                   = '97-2-1/09:25:30'
  t2Str                   = '97-2-1/09:28:00'
  ;; t1Str                   = '97-2-1/09:26:12'
  ;; t2Str                   = '97-2-1/09:28:00'

  t1                      = STR_TO_TIME(t1Str)
  t2                      = STR_TO_TIME(t2Str)

  use_sc_pot_for_min      = 1

  ;;Get orbit stuff
  GET_FA_ORBIT,t1,t2

  ;;Define loss cone angle
  GET_DATA,'ALT',DATA=alt
  loss_cone_alt           = alt.y[0]*1000.0
  lcw                     = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
  GET_DATA,'ILAT',DATA=ilat
  north_south             = ABS(ilat.y[0])/ilat.y[0]
  
  if north_south EQ -1 then begin
     e_angle              = [180.-lcw,180+lcw] ; for Southern Hemis.

     ;;Eliminate ram from data
     i_angle              = [180.0,360.0]
     i_angle_up=[270.0,360.0]
     
  endif else begin
     e_angle              = [360.-lcw,lcw] ;	for Northern Hemis.

     ;;Eliminate ram from data
     i_angle              = [0.0,180.0]
     i_angle_up           = [90.0,180.0]
     
  endelse

  ;;Calculate current from ESAs
  IF KEYWORD_SET(use_sc_pot_for_min) THEN BEGIN
     GET_SC_POTENTIAL,T1=t1,T2=t2,DATA=sc_pot

     GET_2DT_TS_POT,'j_2d_b','fa_ees',T1=t1,T2=t2, $
                          NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle,SC_POT=sc_pot, $
                    OUT_SC_POT=out_sc_pot, $
                    OUT_SC_TIME=out_sc_time, $
                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind
     ;;A little check thing
     ;; FOR i=0,N_ELEMENTS(je.x)-1 DO PRINT,TIME_TO_STR([out_sc_time[i+1],je.x[i]],/MSEC),out_sc_time[i+1]-je.x[i]
  ENDIF ELSE BEGIN

     GET_2DT_TS,'j_2d_b','fa_ees',T1=t1,T2=t2, $
                NAME='Je_lc',ENERGY=energy_electrons,ANGLE=e_angle
  ENDELSE

  ;;Remove_crap
  GET_DATA,'Je_lc',DATA=tmp
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  je_lc_tmp_time          = tmp.x[keep2]
  je_lc_tmp_data          = tmp.y[keep2]
  STORE_DATA,'Je_lc',DATA={x:je_lc_tmp_time,y:(-1.)*je_lc_tmp_data}
  YLIM,'Je_lc',-1.e9,2.e9
  OPTIONS,'Je_lc','yticks',4                           ; set y-axis labels
  OPTIONS,'Je_lc','ytickname',['-1e9','0','1e9','2e9'] ; set y-axis labels
  OPTIONS,'Je_lc','ytickv',[-1e9,0,1e9,2e9]            ; set y-axis labels
  OPTIONS,'Je_lc','ytitle','Electron!CFlux!C(cm!U2!Ns!U-1!N'

  ;;Get electron energy flux in loss cone
  GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
             NAME='JEe',ENERGY=energy_electrons,ANGLE=e_angle
  GET_DATA,'JEe',DATA=tmp
  ;;remove crap
  keep1                   = WHERE(FINITE(tmp.y) NE 0)
  tmp.x                   = tmp.x[keep1]
  tmp.y                   = tmp.y[keep1]
  keep2                   = WHERE(ABS(tmp.y) GT 0.0)
  jee_tmp_time            = tmp.x[keep2]
  jee_tmp_data            = tmp.y[keep2]
  STORE_DATA,'JEe',DATA={x:jee_tmp_time,y:jee_tmp_data}

  ;;All around the world
  ;; GET_2DT_TS,'je_2d_b','fa_ees',T1=t1,T2=t2, $
  ;;            NAME='JEe_tot',ENERGY=energy_electrons
  ;; GET_DATA,'JEe_tot',DATA=tmp
  ;; ;;remove crap
  ;; keep1                   = WHERE(FINITE(tmp.y) NE 0)
  ;; tmp.x                   = tmp.x[keep1]
  ;; tmp.y                   = tmp.y[keep1]
  ;; keep2                   = WHERE(ABS(tmp.y) GT 0.0)

  ;; jee_tot_tmp_time        = tmp.x[keep2]
  ;; jee_tot_tmp_data        = tmp.y[keep2]
  ;; STORE_DATA,'JEe_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_tmp_data}

  ;; ;;Get ratios for mapping to 100 km as well as signs for ensuring downward is positive
  ;; GET_FA_ORBIT,jee_tmp_time,/TIME_ARRAY,/ALL
  ;; GET_DATA,'ILAT',DATA=tmp
  ;; sgn_flx                 = tmp.y/ABS(tmp.y)
  ;; GET_DATA,'B_model',DATA=tmp1
  ;; GET_DATA,'BFOOT',DATA=tmp2
  ;; mag1                    = (tmp1.y[*,0]*tmp1.y[*,0]+tmp1.y[*,1]*tmp1.y[*,1]+tmp1.y[*,2]*tmp1.y[*,2])^0.5
  ;; mag2                    = (tmp2.y[*,0]*tmp2.y[*,0]+tmp2.y[*,1]*tmp2.y[*,1]+tmp2.y[*,2]*tmp2.y[*,2])^0.5
  ;; ratio                   = (mag2/mag1)

  ;; ;;Scale electron energy flux to 100km, pos flux earthward
  ;; jee_ionos_tmp_data      = sgn_flx*jee_tmp_data*ratio
  ;; STORE_DATA,'JEei',DATA={x:jee_tmp_time,y:jee_ionos_tmp_data}
  
  ;; jee_tot_ionos_tmp_data  = sgn_flx*jee_tot_tmp_data*ratio
  ;; STORE_DATA,'JEei_tot',DATA={x:jee_tot_tmp_time,y:jee_tot_ionos_tmp_data}


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now the easy ones, ripped right from the crib sheet
  GET_EN_SPEC,"fa_ees_c",UNITS='eflux',NAME='el_0',ANGLE=e_angle,RETRACE=1,T1=t1,T2=t2,/CALIB


  ;;Now get 'em all, see what we gots
  GET_DATA,'el_0', DATA=eSpec                                            ; get data structure
  GET_DATA,'JEe',DATA=Jee
  GET_DATA,'Je_lc',DATA=Je
  ;; GET_DATA,'JEe_tot',DATA=tmp

  ;;Because we need MLT
  GET_FA_ORBIT,eSpec.x,/TIME_ARRAY
  GET_DATA,'MLT',DATA=mlt
  mlt       = mlt.y

  events       = !NULL
  nEvents      = N_ELEMENTS(eSpec.x)
  energies     = REFORM(eSpec.v[0,*])
  nEnergies    = N_ELEMENTS(energies)
  max_en_ind   = MAKE_ARRAY(nEvents,/INTEGER,VALUE=-2)
  IF KEYWORD_SET(out_sc_pot) THEN BEGIN
     FOR i=0,nEvents-1 DO BEGIN
        tempInd      = MAX(WHERE(energies GT out_sc_pot[i]))
        ;; tempMax      = MAX(WHERE(
       max_en_ind[i] = tempInd
     ENDFOR
  ENDIF
  ;; IF KEYWORD_SET(out_sc_min_energy_ind) THEN BEGIN
  ;;    max_en_ind = out_sc_min_energy_ind
  ;; ENDIF

  FOR i=0,N_ELEMENTS(eSpec.x)-1 DO BEGIN

     ;; tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:-2])),v:REVERSE(REFORM(eSpec.v[i,0:-2]))}
     tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:max_en_ind[i]])),v:REVERSE(REFORM(eSpec.v[i,0:max_en_ind[i]]))}
     ;; tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__NEWELL_ET_AL_2009(tempeSpec,Je.y[0],Jee.y[i],MLT[i])
     tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ(tempeSpec,Je.y[0],Jee.y[i],MLT[i])
     events    = [events,tempEvent]
  ENDFOR

  ;;Show which have been identified as which
  ;; PLOT_SPECTRAL_TYPE__NEWELL_ET_AL_2009,events, $
  ;;                                       OUTNAME='Orbit_1773--Newell_et_al_2009_algorithm--adjusted_monoenergetic_lims--otherwise_identical.png', $
  ;;                                       TITLE='Elphic et al. [1998] Inverted V!C(Newell et al. [2009] algorithm, monoenergetic lims adjusted)'
  PLOT_SPECTRAL_TYPE__NEWELL_ET_AL_2009,events,OUTNAME='Orbit_1773--Newell_et_al_2009_algorithm--adjusted_for_FAST.png', $
                                        TITLE='Orbit 1773: Inverted V from Elphic et al. [1998]!C(Newell et al. [2009] algorithm, adjusted for FAST)'
                                        
END