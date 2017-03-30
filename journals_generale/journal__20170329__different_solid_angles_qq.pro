;2017/03/29
;;Proof positive that no two calculations of omega are made alike
PRO PICKITUP,tvd,vdrift,IN_MOMS=moms,T1=t1,T2=t2

  COMPILE_OPT idl2

  me = 9.11e-31         ;;; electron mass [kg]
  mi = 16*1.67e-27      ;;; for Oxygen ion

;'1997-01-30/06:40:55','1997-01-30/06:41:27'    (1750)  => southward  down
;;;;;;; v_drift, ion by computing the 1st moment of ion distribution function

  vdrift = FLTARR(1)
  tvd = DBLARR(1)

  ivdrift = 0
  time = GETTIME(t1)

  WHILE time LT GETTIME(t2) DO BEGIN
     ies = GET_FA_IES_C(time,/ADVANCE)
     ies = CONV_UNITS(ies,'COUNTS')
     ies.data = ies.data > 1
     IF (MIN(ies.data) EQ 1) THEN ies.data[WHERE(ies.data EQ 1)] = !VALUES.F_NaN
     ies = CONV_UNITS(ies,'df')
     iesvprp = ies.data*0.0
     iesvpar = ies.data*0.0
     iesvvv = SQRT(ies.energy*1.6e-19*2/mi)     
     iesvprp = iesvvv*SIN(ies.theta*!DTOR)
     iesvpar = iesvvv*COS(ies.theta*!DTOR)

     d3v = SQRT(ies.energy)*ies.denergy*ABS(SIN(ies.theta/57.3))*ies.dtheta
     vdriftok = WHERE(ies.data eq ies.data and ies.denergy ne 0)
     newvdrift =                         $
        TOTAL(iesvprp[vdriftok]*ies.data[vdriftok]*d3v[vdriftok])/   $
        TOTAL(ies.data[vdriftok]*d3v[vdriftok]   )                     
     newtvd = time

     if (ivdrift eq 0) then begin
        vdrift = [newvdrift] 
        tvd = [newtvd]
     endif else begin
        vdrift = [vdrift, [newvdrift]]
        tvd = [tvd, [newtvd]]
     endelse

     ivdrift = ivdrift+1
  endwhile


  ;;;;;;; E_ion = -(v_drift+v_sc) X b, including subtraction of sc velocity contribution

  ;; GET_FA_ORBIT,tvd,N_ELEMENTS(tvd),/TIME_ARRAY,/NO_STORE,STRUC=orb,/ALL,/DEFINITIVE
  ;; bbbalt = SQRT(TOTAL(orb.B_model^2,2))    ;;;;; in nT
  ;; v_sc = SQRT(TOTAL(orb.fa_vel^2,2))       ;;;;; in km/s

  ;;;;;;; Coordinate transformation

  ;; unitB = NORMN3(orb.B_model)                                 ;;; z-axis
  ;; unitQ = NORMN3(CROSSN3(orb.fa_vel,orb.B_model))             ;;; y-axis 
  ;; unitv_fperp = NORMN3(crossn3(orb.B_model,unitQ))            ;;; x-axis

  ;; GET_FA_ATTITUDE,orb.time,/TIME_ARRAY,STRUCT=att

  GET_FA_FAC_VECTORS,tvd,t2, $
                     ORBSTRUCT=orbStruct, $
                     /TIME_ARRAY, $
                     FAC=fac, $
                     VEL_FAC=facv, $
                     DESPUN_SAXIS_SPLANE=spin
  
;;; v_drift = (v_drift dot unitQ)*unitQ + (v_drift dot unitv_fperp)*unitv_fperp
;;;                  (1)                           (2)
;;;         (1)  <== from ealongv/B,i.e.,eavmean [mV/m]  
;;;         (2)  <== from v_drift from ion distribution fn.  therefore you already get these (1) and (2).
;;; v_drift = (East_comp)*unitE + (North_comp)*unitN
;;;
;;; E_ion = - v_drift X B = B*(v_drift dot unitQ)*(unitB X unitQ) + B*(v_drift dot unitv_fperp)*(unitB X unitv_fperp)
;;;                                               -------------- unitv_fperp                     ------------ - unitQ
;;; East_comp = unitE (dot) E_ion
;;;           = B*(v_drift dot unitQ)*(unitE dot unitv_fperp) - B*(v_drift dot unitv_fperp)*(unitE dot unitQ)
;;; North_comp = unitN (dot) E_ion
;;;            = B*(v_drift dot unitQ)*(unitN dot unitv_fperp) - B*(v_drift dot unitv_fperp)*(unitN dot unitQ)

;;   unitSpin = [[SIN(att.angles[*].spin_ra*!DTOR)], $
;;               [COS(att.angles[*].spin_ra*!DTOR)], $
;;               [SIN(att.angles[*].spin_dec*!DTOR)]]

;;   unitR = NORMN3(orb.fa_pos)
;;   z = [0.,0.,1.]
;;   unitz = MAKE_ARRAY(N_ELEMENTS(orb.time),VALUE=1.0D,/DOUBLE) # TRANSPOSE(z)
;; ;; unitz = repvec(z,n_elements(orb.time))                
;;   unitE = NORMN3(CROSSN3(unitz,unitR))             ;;; East unit vector
;;   unitN = NORMN3(CROSSN3(unitR,unitE))             ;;; North unit vector 

;;   fa_pos_gei = orb.fa_pos

;;   b_unit = NORMN3(orb.B_model)
;;   r_unit = NORMN3(fa_pos_gei)

;;   e_unit = NORMN3(CROSSN3(b_unit,r_unit))
;;   n_unit = CROSSN3(e_unit,b_unit)

;;   ;;See how aligned the spin axis is with geographic east?
;;   alignedness_GEO = TOTAL(unitE*unitSpin,2)

;;   ;;See how aligned the spin axis is with geographic east?
;;   alignedness_MAG = TOTAL(e_unit*unitSpin,2)


;;   unit_spinPlaneB = CROSSN3(b_unit,unitSpin)

;;   unit2D_spPl_N   = TOTAL(n_unit*unitSpin,2)
;;   unit2D_spPl_E   = TOTAL(e_unit*unitSpin,2)

;;   norm_spPl       = SQRT(unit2D_spPl_N^2+unit2D_spPl_E^2)
;;   unit2D_spPl_N  /= norm_spPl
;;   unit2D_spPl_E  /= norm_spPl

  ;; cur_n           = unit2D_spPl_N * moms.y[*].perp.j * 1.6D-9
  ;; cur_e           = unit2D_spPl_E * moms.y[*].perp.j * 1.6D-9

;   Field-aligned coordinates defined as: 
;   z-along B, y-east (BxR), x-nominally out

  ;; tmp   = DBLARR(N_ELEMENTS(b_unit[*,0]),3,3)
  ;; tmp[*,*,0] = n_unit
  ;; tmp[*,*,1] = e_unit
  ;; tmp[*,*,2] = b_unit

  ;; gei_to_fac = {x:orb.time,y:tmp}

  cur_n           = spin.plane.nMAG * moms.y[*].perp.j * 1.6D-9
  cur_e           = spin.plane.eMAG * moms.y[*].perp.j * 1.6D-9

  eavmean = CONGRID(eavmean,N_ELEMENTS(tvd))
  viondrift = vdrift+v_sc*1000.0

  Enorth = eavmean*TOTAL(unitN*unitv_fperp,2) - bbbalt*viondrift*TOTAL(unitN*unitQ,2)*1.D-6     ;;; in mV/m
  Eeast = eavmean*TOTAL(unitE*unitv_fperp,2) - bbbalt*viondrift*TOTAL(unitE*unitQ,2)*1.D-6      ;;; in mV/m

END
PRO JOURNAL__20170329__DIFFERENT_SOLID_ANGLES_QQ

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; GET_2DT_TS,,'fa_ees',T1=t1,T2=t2,$
  ;;            NAME='JEe',ENERGY=energy_electrons,ANGLE=e_angle

  rout_name   = 'JOURNAL__20170329__DIFFERENT_SOLID_ANGLES_QQ'

  orbit       = 1773
  orbit       = 9627

  b_or_s      = 'b'
  e_or_i      = 'i'

  funct       = 'j_2d_' + b_or_s
  eeb_or_ees  = e_or_i + 'e' + b_or_s
  get_dat     = 'fa_' + eeb_or_ees
  ;; routine  = 'get_'+get_dat+'_ts'
  routine     = 'get_'+get_dat
  calib       = 1

  save_diff_eFlux_to_file = 1
  loaddir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/diff_eFlux/'
  loadFile = 1

  orbStr      = STRING(FORMAT='(I0)',orbit)
  diff_eFlux_file = rout_name + '_' + eeb_or_ees + '_' + orbStr + '.sav'

  ;;Get times
  
  ;; dqd      = 'Eesa Survey'
  ;; dqd      = 'Eesa Burst'
  ;; this     = GET_SDT_TIMESPAN(t1,t2,DQD=dqd)
  ;; t1Str    = '1997-02-01/09:26:53'

  CASE orbit OF
     1773: BEGIN
        t1Str = '1997-02-01/09:25:40'
        t2Str = '1997-02-01/09:27:30'
     END
     9627: BEGIN
        t1Str = '1999-01-27/11:32:36'
        t2Str = '1999-01-27/11:33:24.5'
     END
  ENDCASE



  t1          = STR_TO_TIME(t1Str)
  t2          = STR_TO_TIME(t2Str)

  t1a         = t1
  t2a         = t2
  CASE STRMATCH(routine,'*TS',/FOLD_CASE) OF
     0: BEGIN
        dat   = CALL_FUNCTION(routine,t1a,CALIB=calib)
     END
     1: BEGIN
        dat   = CALL_FUNCTION(routine,t1a,t2a,CALIB=calib,NPTS=n_get_pts)
     END
  ENDCASE


  ;; omega       = OMEGA_2D_B(dat[0])

  ;; jNew        = J_2D_NEW(dat[0],OUT_DOMEGA=domega_jNew)
  ;; J           = J_2D_B(dat[0],OUT_DOMEGA=domega_j,OUT_1DOMEGA=domega1_J,OUT_2DOMEGA=domega2_J)
  ;; JFS         = J_2D_FS(dat[0])
  ;; jReal       = J_2D_B__REAL2D(dat[0],ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
  ;; jfsReal     = J_2D_FS__REAL2D(dat[0],ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)

  ;; jeNew       = JE_2D_NEW(dat)
  ;; Je          = JE_2D_B(dat[0])
  ;; JEFS        = Je_2D_FS(dat[0])

  ;; N           = N_2D_B(dat[0],OUT_DOMEGA=domega_n,OUT_1DOMEGA=domega1_N,OUT_2DOMEGA=domega2_N)
  ;; NReal       = N_2D_B__REAL2D(dat[0],OUT_DOMEGA=domega_nReal)
  ;; NNew        = N_2D_NEW(dat[0],OUT_DOMEGA=domega_nNew)
  ;; nFSReal     = N_2D_FS__REAL2D(dat[0])

  ;; v           = V_2D_FS(dat[0])
  ;; vFSReal     = V_2D_FS__REAL2D(dat[0])

  ;; P           = P_2D_B(dat[0], $
  ;;               OUT_DOMEGA_XX=domega_xx,OUT_1DOMEGA_XX=domega1_xx,OUT_2DOMEGA_XX=domega2_xx, $
  ;;               OUT_DOMEGA_ZZ=domega_zz,OUT_1DOMEGA_ZZ=domega1_zz,OUT_2DOMEGA_ZZ=domega2_zz)
  ;; PNew        = P_2D_NEW(dat[0])

  ;; T           = T_2D_B(dat[0])
  ;; TNew        = T_2D_New(dat[0])

  GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   CALC_GEOM_FACTORS=calc_geom_factors, $
                   UNITS=units, $          
                   ;; ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                   FIT_EACH_ANGLE=fit_each_angle, $
                   TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                   OUT_DIFF_EFLUX=diff_eflux, $
                   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                   DIFF_EFLUX_FILE=diff_eFlux_file, $
                   LOAD_DAT_FROM_FILE=loadFile, $
                   LOAD_DIR=loadDir

  MOMENT_SUITE_2D,diff_eFlux, $
                  ENERGY=energy, $
                  ARANGE__MOMENTS=aRange__moments, $
                  ARANGE__CHARE=aRange__charE, $
                  SC_POT=sc_pot, $
                  EEB_OR_EES=eeb_or_ees, $
                  /ERROR_ESTIMATES, $
                  MAP_TO_100KM=map_to_100km, $ 
                  ORBIT=orbit, $
                  /NEW_MOMENT_ROUTINE, $
                  QUIET=quiet, $
                  OUT_STRUCT=moms

  ;; moms = MOMENTS_2D_NEW__FROM_DIFF_EFLUX(diff_eFlux, $
  ;;                                        ENERGY=en, $
  ;;                                        ERANGE=er, $
  ;;                                        EBINS=ebins, $
  ;;                                        ANGLE=an, $
  ;;                                        ARANGE=ar, $
  ;;                                        BINS=bins, $
  ;;                                        SC_POT=sc_pot, $
  ;;                                        EEB_OR_EES=eeb_or_ees, $
  ;;                                        QUIET=quiet)

  PICKITUP,tvd,vdrift,IN_MOMS=moms,T1=t1,T2=t2
  vdrift /= 1000.

  these = VALUE_CLOSEST2(moms.x,tvd,/CONSTRAINED)
  PRINT,moms.x[these]-tvd

  diff = moms.y[these].perp.v-vdrift
  dRelVD = ABS(diff)/vdrift
  dRelVP = ABS(diff)/moms.y[these].perp.v

  PRINT,'Rel vdrift','Rel to vP'
  FOR k=0,N_ELEMENTS(these)-1 DO BEGIN
     PRINT,dRelVD[k],dRelVP[k]
  ENDFOR

  startT = MIN([tvd[0],moms.x[0]])

  colvd = 'red'
  colvp = 'blue' 
  colz  = 'black'

  navnvd = 'Kyoung-Joo'
  navnvp = 'hammertime'

  ;; lsvp   = ':'
  lsvp   = '-'
  lsvd   = '--'
  lsz    = '-'

  transpz = 80

  pTitle = 'Time since ' + TIME_TO_STR(startT,/MSEC)

  window = WINDOW(DIMENSIONS=[1000,800])

  

  plotvd = PLOT(tvd-startT,vdrift, $
                TITLE=pTitle, $
                NAME=navnvd, $
                COLOR=colvd, $
                LINESTYLE=lsvd, $
                /CURRENT)

  plotvp = PLOT(moms.x-startT,moms.y[*].perp.v, $
                NAME=navnvp, $
                COLOR=colvp, $
               LINESTYLE=lsvp, $
               /OVERPLOT)

  zero  = moms.x*0.D
  plotz = PLOT(moms.x-startT,zero, $
                NAME=navnz, $
                COLOR=colz, $
               LINESTYLE=lsz, $
               TRANSPARENCY=transpz, $
               /OVERPLOT)

  leg    = LEGEND(TARGET=[plotvd,plotvp])

  STOP

END
