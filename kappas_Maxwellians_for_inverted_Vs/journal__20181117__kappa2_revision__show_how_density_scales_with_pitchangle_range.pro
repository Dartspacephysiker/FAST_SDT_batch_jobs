;2018/11/17
PRO JOURNAL__20181117__KAPPA2_REVISION__SHOW_HOW_DENSITY_SCALES_WITH_PITCHANGLE_RANGE

  COMPILE_OPT IDL2,STRICTARRSUBS

  t1 = '1997/02/01-09:27:01.261'
  t2 = '1997/02/01-09:27:01.893'

  GET_DIFF_EFLUX,T1=t1,T2=t2,EEB_OR_EES='ees',/ARRAY_OF_STRUCTS_INSTEAD,OUT_DIFF_EFLUX=diff_eFlux

  ;; See?
  ;; WINDOW,0,XSIZE=800,YSIZE=800
  ;; CONTOUR2D,diff_eflux[0],/POLAR

  ;; Pitch angle spacing?
  PRINT,SHIFT(REFORM(diff_eflux.theta[0,*]),1)-REFORM(diff_eflux.theta[0,*])

  ;; It's 5.625 degrees at this time

  ;; Now density


  eRange = [1D2,2D4]
  startA = 5.
  dPA = 6
  nPA = LONG((180.-startA)/dPA)
  pitchAngles=startA+FINDGEN(nPA)*dPA

  densities = MAKE_ARRAY(nPA,VALUE=0.)
  FOR k=0,nPA-1 DO BEGIN
     tmpAngles = [-pitchAngles[k],pitchAngles[k]]
     densities[k] = N_2D_FS(diff_eFlux,ENERGY=eRange,ANGLE=tmpAngles)
  ENDFOR

  plot = PLOT(pitchAngles,densities, $
              XTITLE="Pitch angle (deg)", $
              YTITLE="Density (cm!U-3!N)", $
              TITLE="Orbit 1773: " +T2S(diff_eFlux.time))

  STOP

  xSize       = 9.5
  ySize       = 9.5
  land        = 1

  xWinSize    = 700
  yWinSize    = 700

outdir = '/SPENCEdata/software/sdt/batch_jobs/kappas_Maxwellians_for_inverted_Vs/'
outPS = 'Orb1773-contour'
     POPEN,outDir+outPS, $
           XSIZE=xSize, $
           YSIZE=ySize, $
           LAND=land, $
           CTABLE=43, $
           ENCAPSULATED=eps, $
           OPTIONS={charsize:1.5}

        CONTOUR2D,diff_eFlux, $
                  ;; ANGLE=angle, $
                  /POLAR, $
                  /FILL, $
                  ;; /OVERPLOT, $
                  /MSEC, $
                  LIMITS=limits, $
                  /LABEL, $
                  THICK=thick

        PCLOSE

END
