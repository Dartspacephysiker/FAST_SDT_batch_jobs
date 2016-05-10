;2016/04/21 at EGU meeting
;First step is to see if I can get a dist. function to show, and then attemp to fit it
;Other work to be done includes identifying monoenergetic, inverted-V structures

;;fire up
@startup

;;For orbit 10000, pick 99-03-02/17:57:25 through 99-3-2/17:57:33

;From the FAST ESA IDL demo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Examples of 3D plots data distribution at peak

;;some good times for Orb 10000
;; timeStr = '99-3-2/17:58:39'
timeStr = '99-3-2/18:08:42'
;; timeStr = '99-3-2/18:12:54'

;;Spectrum stuff
t=str_to_time(timeStr)
dat = get_fa_ees(t) ; get electron esa survey
spec2d,dat,/label ; plot spectra
pitch2d,dat,/label,energy=[2000,10000] ; plot pitch angle
contour2d,dat,/label,ncont=20 ; plot contour plot
dat = get_fa_ies(t); get ion esa survey data
contour2d,dat,/label,ncont=20 ; plot contour plot
fu_spec2d,'n_2d_fs',dat,/integ_f,/integ_r ; plot partial density, partial integral densities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example functions
t=str_to_time(timeStr)
dat = get_fa_ees(t) ; get electron survey data
print,n_2d_fs(dat,energy=[100,30000]) ; print density >100 eV, #/cm3
print,j_2d_fs(dat,energy=[100,30000]) ; print flux >100 eV, #/cm2-s
print,je_2d_fs(dat,energy=[100,30000]) ; print energy flux >100 eV, ergs/cm2-s
print,v_2d_fs(dat,energy=[100,30000]) ; print Vx,Vy,Vz, km/s
print,p_2d_fs(dat,energy=[100,30000]) ; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
print,t_2d_fs(dat,energy=[100,30000]) ; print Tx,Ty,Tz,Tavg, eV
print,vth_2d_fs(dat,energy=[100,30000]) ; print Vthx,Vthy,Vthz,Vthavg, km/s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fitting data to an accelerated Maxwellian
t=str_to_time(timeStr)
dat = get_fa_ees(t) ; get data at t
funct_fit2d,dat,angle=[-45,45] ; fit the data
; click left button on the peak energy (6keV)
; click left button on the lower limit to the energy range fit ( 6 keV)
; click left button on the upper limit to the energy range fit (15 keV)
; click the right button to end the selection
; plot will show a maxwellian fit to data over the energy range
; text on the screen will show the source temperature and density 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;OR, for Orbit 1858 ...

timeStr = '97-2-9/06:05:06'

;;Spectrum stuff
t=str_to_time(timeStr) ; pick a time
dat = get_fa_ees(t) ; get electron esa survey
spec2d,dat,/label ; plot spectra
pitch2d,dat,/label,energy=[2000,10000] ; plot pitch angle
contour2d,dat,/label,ncont=20 ; plot contour plot
dat = get_fa_ies(t); get ion esa survey data
contour2d,dat,/label,ncont=20 ; plot contour plot
fu_spec2d,'n_2d_fs',dat,/integ_f,/integ_r ; plot partial density, partial integral densities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Example functions
t=str_to_time(timeStr)
dat = get_fa_ees(t) ; get electron survey data
print,n_2d_fs(dat,energy=[100,30000]) ; print density >100 eV, #/cm3
print,j_2d_fs(dat,energy=[100,30000]) ; print flux >100 eV, #/cm2-s
print,je_2d_fs(dat,energy=[100,30000]) ; print energy flux >100 eV, ergs/cm2-s
print,v_2d_fs(dat,energy=[100,30000]) ; print Vx,Vy,Vz, km/s
print,p_2d_fs(dat,energy=[100,30000]) ; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
print,t_2d_fs(dat,energy=[100,30000]) ; print Tx,Ty,Tz,Tavg, eV
print,vth_2d_fs(dat,energy=[100,30000]) ; print Vthx,Vthy,Vthz,Vthavg, km/s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fitting data to an accelerated Maxwellian
t=str_to_time(timeStr)
dat = get_fa_ees(t) ; get data at t
funct_fit2d,dat,angle=[-45,45] ; fit the data
; click left button on the peak energy (6keV)
; click left button on the lower limit to the energy range fit ( 6 keV)
; click left button on the upper limit to the energy range fit (15 keV)
; click the right button to end the selection
; plot will show a maxwellian fit to data over the energy range
; text on the screen will show the source temperature and density 
