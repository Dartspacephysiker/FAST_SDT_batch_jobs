;;12/14/16
PRO JOURNAL__20161214__KH__GET_PARTICLE_FLUX_DATA_FOR_ORBIT_9113

  COMPILE_OPT IDL2

  makeFile         = 0

  energy_electrons = [30,3e4]

  ;; save_diff_eFlux_to_file = 1

  orb              = 9113
  eeb_or_ees       = 'ees'
  plotNamePref     = '--checkout_KH_AGU'

  orbStr           = STRCOMPRESS(orb,/REMOVE_ALL)

  loadDir          = '~/software/sdt/batch_jobs/saves_output_etc/'
  plotDir          = '~/Desktop/KH_plots/'
  outName          = 'orb_' + orbStr + '--KH_checkitout--20161214.sav'

  ;; diff_eFlux_file  = 'orb_' + orbStr + $
  ;;                    '--diff_eflux--' + eeb_or_ees + $
  ;;                        plotNamePref + '.sav'

  IF KEYWORD_SET(makeFile) THEN BEGIN

     this             = GET_ESA_TIMERANGES()

     GET_DATA,'MLT',DATA=mlt

     mlt_i            = GET_MLT_INDS(!NULL,2,10, $
                                     ;; /DAWNSECTOR, $
                                     DIRECT_MLTS=mlt.y)

     t1               = mlt.x[mlt_i[0]]
     t2               = mlt.x[mlt_i[-1]]

     PRINT,TIME_TO_STR([t1,t2])

     t2old = t2
     GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2Old, $
                                 LOAD_DAT_FROM_FILE=diff_eFlux_file, $
                                 LOAD_DIR=loadDir, $
                                 EEB_OR_EES=eeb_or_ees, $
                                 DIFF_EFLUX=diff_eFlux, $
                                 SPECTRA_AVERAGE_INTERVAL=spec_avg_intvl, $
                                 OUT_ORB=orb, $
                                 OUT_ANGLERANGE=e_angle, $
                                 /FIT_EACH_ANGLE, $ ;Perma-set because we need all angles here
                                 CUSTOM_E_ANGLERANGE=electron_angleRange, $
                                 ANGLESTR=angleStr, $
                                 ESPECUNITS=eSpecUnits, $
                                 ELECTRON_ENERGY_LIMS=energy_electrons, $
                                 SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file

     GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c', $
             NAME='Jee', $
             T1=t1, $
             T2=t2, $
             ENERGY=energy_electrons, $
             ANGLE=e_angle, $
             /CALIB

     GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c', $
             NAME='Je', $
             T1=t1, $
             T2=t2, $
             ENERGY=energy_electrons, $
             ANGLE=e_angle, $
             /CALIB

     GET_DATA,'Jee',DATA=Jee
     GET_DATA,'Je',DATA=Je


     IF N_ELEMENTS(Jee.y) NE N_ELEMENTS(Je.y) THEN STOP

     keep = WHERE(FINITE(je.y) AND FINITE(jee.y))
     je.x = je.x[keep]
     je.y = je.y[keep]
     je.x = je.x[keep]
     je.y = je.y[keep]
     
     time_order = SORT(je.x)
     je.x  = je.x[time_order]
     je.y  = je.y[time_order]
     jee.x = jee.x[time_order]
     jee.y = jee.y[time_order]

     GET_FA_ORBIT,Je.x,/TIME_ARRAY
     GET_DATA,'MLT',DATA=mlt
     GET_DATA,'ILAT',DATA=ilat
     GET_DATA,'ALT',DATA=alt

     chare = (jee.y / je.y ) * 6.242 * 1.0e11

     ;;Now some bonus stuff--angles, position, you know
     GET_DATA,'fa_vel',DATA=vel
     speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
     
     ;;get position of each mag point
     ;;samplingperiod=magz.x(300)-magz.x(299)
     ;;position=make_array(n_elements(magz.x),/double)
     ;;position=speed(300)*samplingperiod*findgen(n_elements(magz.x))
     ;;speed_mag_point=speed(300)
     
     old_pos        = 0.
     pos            = MAKE_ARRAY(N_ELEMENTS(je.x),/DOUBLE)
     speed_je_point = MAKE_ARRAY(N_ELEMENTS(je.x),/DOUBLE)
     FOR j=0L,N_ELEMENTS(je.x)-2 DO BEGIN

        speed_point_ind   = MIN(ABS(vel.x-je.x[j]),ind)

        speed_je_point[j] = speed[ind]
        samplingperiod    = je.x[j+1]-je.x[j]

        pos[j]            = old_pos+speed_je_point[j]*samplingperiod
        old_pos           = pos[j]
     ENDFOR

     width_angle = SPHDIST(DOUBLE(mlt.y[0:-2])*15.D,DOUBLE(ilat.y[0:-2]), $
                           DOUBLE(mlt.y[1:-1])*15.D,DOUBLE(ilat.y[1:-1]), $
                           /DEGREES)

     angle       = [0,TOTAL(width_angle,/CUMULATIVE)]

     ;;derivatives
     derivs = {charE : {mlt  : DERIV(mlt.y ,charE), $
                        ilat : DERIV(ilat.y,charE), $
                        pos  : DERIV(pos   ,charE), $
                        angle: DERIV(angle ,charE)},$
               je    : {mlt  : DERIV(mlt.y ,je.y ), $
                        ilat : DERIV(ilat.y,je.y ), $
                        pos  : DERIV(pos   ,je.y ), $
                        angle: DERIV(angle ,je.y )},$
               jee   : {mlt  : DERIV(mlt.y ,jee.y), $
                        ilat : DERIV(ilat.y,jee.y), $
                        pos  : DERIV(pos   ,jee.y ),$
                        angle: DERIV(angle ,jee.y)}}

     struct = {x      : mlt.x , $
               mlt    : mlt.y , $
               ilat   : ilat.y, $
               je     : je.y  , $
               jee    : jee.y , $
               charE  : chare , $
               derivs : derivs  }
     


     SAVE,struct,FILENAME=loadDir+outName

  ENDIF ELSE BEGIN
     RESTORE,loadDir+outName
  ENDELSE

  window = WINDOW(DIMENSION=[1000,800])

  ;; xVar     = struct.ilat
  ;; latRange = [70,73]
  ;; layout1 = [2,2,1]
  ;; layout2 = [2,2,2]
  ;; layout3 = [2,2,3]
  xVar = struct.mlt
  xRange = [6.0,7.0]

  ;; this = PLOT(xVar,ABS(struct.chare),XRANGE=xRange, $
  ;;             LAYOUT=layout1,CURRENT=N_ELEMENTS(layout1) GT 0)
  ;; this = PLOT(xVar,ABS(struct.derivs.jee.mlt),XRANGE=xRange, $
  ;;             LAYOUT=layout2,CURRENT=N_ELEMENTS(layout1) GT 0)
  ;; this = PLOT(xVar,struct.jee,XRANGE=xRange, $
  ;;             LAYOUT=layout2,CURRENT=N_ELEMENTS(layout1) GT 0)
  ;; this = PLOT(xVar,ABS(struct.derivs.chare.mlt),XRANGE=xRange, $
  ;;             LAYOUT=layout3,CURRENT=N_ELEMENTS(layout1) GT 0)

  yVar    = LIST(struct.jee, $
                 struct.je*1.6e-9, $
                 struct.charE, $
                 struct.derivs.jee.mlt, $
                 struct.derivs.je.mlt*1.6e-9, $
                 ABS(struct.derivs.jee.mlt), $
                 ABS(struct.derivs.je.mlt)*1.6e-9, $
                 ABS(struct.derivs.chare.mlt))

  yTitles = ['Energy flux (mW/m^2)', $
             'Number flux ($\mu$A/m$^2$', $
             'Char. Energy (eV)', $
             'dJ$_E$/dMLT', $
             'dJ/dMLT', $
             'd|J$_E$|/dMLT', $
             'd|J|/dMLT', $
             'd|<E>|/dMLT']
  navn    = ['eFlux', $
             'eNumFlux', $
             'charE', $
             'deF_dMLT', $
             'deNumF_dMLT', $
             'deF_ABS_dMLT', $
             'deNumF_ABS_dMLT', $
             'dCharE_dMLT']
            
  outNavn  = 'orbit_' + orbStr + '-' + navn + '.png'

  FOR k=0,N_ELEMENTS(yVar)-1 DO BEGIN
     window = WINDOW(DIMENSION=[1000,800])

     this = PLOT(xVar,yVar[k], $
                 XRANGE=xRange, $
                 TITLE="Orbit " + orbStr, $
                 XTITLE='MLT', $
                 YTITLE=yTitles[k], $
                 FONT_SIZE=16, $
                 LAYOUT=layout3, $
                 /CURRENT)

     this1 = PLOT(xVar,SMOOTH(yVar[k],7), $
                  XRANGE=xRange, $
                  LAYOUT=layout3, $
                  COLOR='Red', $
                  THICK=3, $
                  /CURRENT, $
                  /OVERPLOT)

     window.Save,plotDir+outNavn[k]
     window.Close
     window = !NULL

  ENDFOR

  STOP
  ;; this = PLOT(xVar,ABS(struct.derivs.chare.mlt), $
  ;;             XRANGE=xRange, $
  ;;             TITLE="Orbit " + orbStr, $
  ;;             XTITLE='MLT', $
  ;;             YTITLE='d|<E>|/dMLT', $
  ;;             FONT_SIZE=16, $
  ;;             LAYOUT=layout3, $
  ;;             /CURRENT)

  ;; this1 = PLOT(xVar,SMOOTH(ABS(struct.derivs.chare.mlt),7), $
  ;;              XRANGE=xRange, $
  ;;              LAYOUT=layout3, $
  ;;              COLOR='Red', $
  ;;              THICK=3, $
  ;;              /CURRENT, $
  ;;              /OVERPLOT)

  STOP


END


