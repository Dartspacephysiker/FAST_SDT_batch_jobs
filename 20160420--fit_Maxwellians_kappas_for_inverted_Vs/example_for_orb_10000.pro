;2106/04/21 at EGU meeting
;First step is to see if I can get a dist. function to show, and then attemp to fit it
;Other work to be done includes identifying monoenergetic, inverted-V structures

;From the FAST ESA IDL demo

dat = get_fa_seb_c(t) ; ses burst data
; Examples of 3D plots data distribution at peak
t=str_to_time('97-2-9/06:06:45') ; pick a time
dat = get_fa_ees(t) ; get electron esa survey
spec2d,dat,/label ; plot spectra
pitch2d,dat,/label,energy=[2000,10000] ; plot pitch angle
contour2d,dat,/label,ncont=20 ; plot contour plot
dat = get_fa_ies(t); get ion esa survey data
contour2d,dat,/label,ncont=20 ; plot contour plot
fu_spec2d,'n_2d_fs',dat,/integ_f,/integ_r ; plot partial density, partial integral densities
; Example functions
t=str_to_time('97-2-9/06:06:45') ; pick a time
dat = get_fa_ees(t) ; get electron survey data
print,n_2d_fs(dat,energy=[100,30000]) ; print density >100 eV, #/cm3
print,j_2d_fs(dat,energy=[100,30000]) ; print flux >100 eV, #/cm2-s
print,je_2d_fs(dat,energy=[100,30000]) ; print energy flux >100 eV, ergs/cm2-s
print,v_2d_fs(dat,energy=[100,30000]) ; print Vx,Vy,Vz, km/s
print,p_2d_fs(dat,energy=[100,30000]) ; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
print,t_2d_fs(dat,energy=[100,30000]) ; print Tx,Ty,Tz,Tavg, eV
print,vth_2d_fs(dat,energy=[100,30000]) ; print Vthx,Vthy,Vthz,Vthavg, km/s
; Fitting data to an accelerated Maxwellian
t=str_to_time('97-2-9/06:06:45') ; pick a time
dat = get_fa_ees(t) ; get data at t
funct_fit2d,dat,angle=[-45,45] ; fit the data
; click left button on the peak energy (6keV)
; click left button on the lower limit to the energy range fit ( 6 keV)
; click left button on the upper limit to the energy range fit (15 keV)
; click the right button to end the selection
; plot will show a maxwellian fit to data over the energy range
; text on the screen will show the source temperature and density 

02/09/1997
06:06:35
02/09/1997
06:07:00
