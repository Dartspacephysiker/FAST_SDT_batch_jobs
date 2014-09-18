 ; Fast IDL Demo               created by J. Mcfadden 01-01-31
; Run the following after starting idl to set up the defaults
@startup
; Use sdt and get data for orbit 1858, sdt config should contain electron and ion spectrograms

;********************************************************************************************************
;********************************************************************************************************

; Getting particle data structures
t=str_to_time('97-2-9/06:06:50') 	; pick a time
dat = get_fa_ees(t) 	; get electron esa survey data
help,dat,/st 	; look at data structure
dat = get_fa_ees(t,/ad) 	; get next data - advance
dat = get_fa_ees(t,/re) 	; get previous data - retreat
dat = get_fa_ees(t,/st) 	; get first data - start
dat = get_fa_ees(t,/en) 	; get last data - end

; Averaging particle data
t=str_to_time('97-2-9/06:06:45') 	; pick a time
dat = get_fa_ees(t) 	; get data at t
dat = sum3d(dat,get_fa_ees(t,/ad)) 	; sum data over 2 sweeps
for a=0,9 do dat = sum3d(dat,get_fa_ees(t,/ad)) 	; sum data over 10 more sweeps

; Other ESA get routines
dat = get_fa_ies(t) 	; ion survey data
dat = get_fa_ses(t) 	; ses survey data
dat = get_fa_eeb(t) 	; electron burst data
dat = get_fa_ieb(t) 	; ion burst data
dat = get_fa_seb(t) 	; ses burst data (combined)
dat = get_fa_seb1(t) 	; ses burst data (sensor 1)
dat = get_fa_seb2(t) 	; ses burst data (sensor 2)
dat = get_fa_seb3(t) 	; ses burst data (sensor 3)
dat = get_fa_seb4(t) 	; ses burst data (sensor 4)
dat = get_fa_seb5(t) 	; ses burst data (sensor 5)
dat = get_fa_seb6(t) 	; ses burst data (sensor 6)
dat = get_fa_ees_sp(t) 	; spin average of electron survey data
dat = get_fa_ies_sp(t) 	; spin average of ion survey data

; Other ESA get routines that work best when using get_en_spec.pro, get_pa_spec.pro, get_2dt.pro
; These routines act just like the above routines in that they return a single data structure, however
; they grab about 100 data arrays from sdt at a time then buffer them in memory to increase speed.
dat = get_fa_ees_c(t) 	; electron survey data
dat = get_fa_ies_c(t) 	; ion survey data
dat = get_fa_ses_c(t) 	; ses survey data
dat = get_fa_eeb_c(t) 	; electron burst data
dat = get_fa_ieb_c(t) 	; ion burst data
dat = get_fa_seb_c(t) 	; ses burst data

; Examples of 3D plots data distribution at peak
t=str_to_time('97-2-9/06:06:45') 	; pick a time
dat = get_fa_ees(t) 	; get electron esa survey
spec2d,dat,/label 	; plot spectra
pitch2d,dat,/label,energy=[2000,10000] 	; plot pitch angle
contour2d,dat,/label,ncont=20 	; plot contour plot
dat = get_fa_ies(t)	; get ion esa survey data
contour2d,dat,/label,ncont=20 	; plot contour plot
fu_spec2d,'n_2d_fs',dat,/integ_f,/integ_r 	; plot partial density, partial integral densities

; Example functions
t=str_to_time('97-2-9/06:06:45') 	; pick a time
dat = get_fa_ees(t) 	; get electron survey data
print,n_2d_fs(dat,energy=[100,30000])	; print density >100 eV, #/cm3
print,j_2d_fs(dat,energy=[100,30000])	; print flux >100 eV, #/cm2-s
print,je_2d_fs(dat,energy=[100,30000])	; print energy flux >100 eV, ergs/cm2-s
print,v_2d_fs(dat,energy=[100,30000]) 	; print Vx,Vy,Vz, km/s
print,p_2d_fs(dat,energy=[100,30000]) 	; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
print,t_2d_fs(dat,energy=[100,30000]) 	; print Tx,Ty,Tz,Tavg, eV
print,vth_2d_fs(dat,energy=[100,30000])	; print Vthx,Vthy,Vthz,Vthavg, km/s

; Fitting data to an accelerated Maxwellian
t=str_to_time('97-2-9/06:06:45') 	; pick a time
dat = get_fa_ees(t) 	; get data at t
funct_fit2d,dat,angle=[-45,45] 	; fit the data

; click left button on the peak energy (6keV)
; click left button on the lower limit to the energy range fit ( 6 keV)
; click left button on the upper limit to the energy range fit (15 keV)
; click the right button to end the selection
; plot will show a maxwellian fit to data over the energy range
; text on the screen will show the source temperature and density

;********************************************************************************************************
;********************************************************************************************************

; Examples for time series plots - these take longer

t1=str_to_time('97-2-9/06:06:40')
t2=str_to_time('97-2-9/06:07:40')

; Electron spectrogram - survey data, remove retrace, downgoing electrons

get_en_spec,"fa_ees_c",units='eflux',name='el_0',angle=[-22.5,22.5],retrace=1,t1=t1,t2=t2,/calib
get_data,'el_0', data=tmp 	; get data structure
tmp.y = tmp.y>1.e1 	; Remove zeros
tmp.y = alog10(tmp.y) 	; Pre-log
store_data,'el_0', data=tmp 	; store data structure
options,'el_0','spec',1 	; set for spectrogram
zlim,'el_0',6,9,0 	; set z limits
ylim,'el_0',4,40000,1 	; set y limits
options,'el_0','ytitle','e- downgoing !C!CEnergy (eV)' 	; y title
options,'el_0','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' 	; z title
options,'el_0','x_no_interp',1 	; don't interpolate
options,'el_0','y_no_interp',1 	; don't interpolate
options,'el_0','yticks',3 	; set y-axis labels
options,'el_0','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] 	; set y-axis labels
options,'el_0','ytickv',[10,100,1000,10000] 	; set y-axis labels
options,'el_0','panel_size',2 	; set panel size

; Electron pitch angle spectrogram - survey data, remove retrace, >100 electrons

get_pa_spec,"fa_ees_c",units='eflux',name='el_pa',energy=[100,40000],retrace=1,/shift90,t1=t1,t2=t2,/calib
get_data,'el_pa', data=tmp 	; get data structure
tmp.y = tmp.y>1.e1 	; Remove zeros
tmp.y = alog10(tmp.y) 	; Pre-log
store_data,'el_pa', data=tmp 	; store data structure
options,'el_pa','spec',1 	; set for spectrogram
zlim,'el_pa',6,9,0 	; set z limits
ylim,'el_pa',-100,280,0 	; set y limits
options,'el_pa','ytitle','e- >100 eV!C!C Pitch Angle' 	; y title
options,'el_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' 	; z title
options,'el_pa','x_no_interp',1 	; don't interpolate
options,'el_pa','y_no_interp',1 	; don't interpolate
options,'el_pa','yticks',4 	; set y-axis labels
options,'el_pa','ytickname',['-90','0','90','180','270']	; set y-axis labels
options,'el_pa','ytickv',[-90,0,90,180,270] 	; set y-axis labels
options,'el_pa','panel_size',2 	; set panel size

; Electron energy flux

get_2dt,'je_2d_fs','fa_ees_c',name='JEe',t1=t1,t2=t2,energy=[20,30000]
ylim,'JEe',1.e-1,1.e1,1 	; set y limits
options,'JEe','ytitle','Electrons!C!Cergs/(cm!U2!N-s)' 	; set y title
options,'JEe','tplot_routine','pmplot' 	; set 2 color plot
options,'JEe','labels',['Downgoing!C Electrons','Upgoing!C Electrons '] 	; set color label
options,'JEe','labflag',3 	; set color label
options,'JEe','labpos',[4.e0,5.e-1] 	; set color label
options,'JEe','panel_size',1 	; set panel size

; Electron flux

get_2dt,'j_2d_fs','fa_ees_c',name='Je',t1=t1,t2=t2,energy=[20,30000]
ylim,'Je',1.e7,1.e9,1 	; set y limits
options,'Je','ytitle','Electrons!C!C1/(cm!U2!N-s)' 	; set y title
options,'Je','tplot_routine','pmplot'	; set 2 color plot
options,'Je','labels',['Downgoing!C Electrons','Upgoing!C Electrons ']	; set color label
options,'Je','labflag',3 	; set color label
options,'Je','labpos',[4.e8,5.e7] 	; set color label
options,'Je','panel_size',1	; set panel size

; Ion spectrogram - survey data, remove retrace, upgoing ions

get_en_spec,"fa_ies_c",units='eflux',name='ion_180',angle=[135,225],retrace=1,t1=t1,t2=t2
get_data,'ion_180',data=tmp 	; get data structure
tmp.y=tmp.y > 1. 	; Remove zeros
tmp.y = alog10(tmp.y) 	; Pre-log
store_data,'ion_180',data=tmp 	; store data structure
options,'ion_180','spec',1	; set for spectrogram
zlim,'ion_180',5,7,0 	; set z limits
ylim,'ion_180',3,30000,1 	; set y limits
options,'ion_180','ytitle','i+ 135!Uo!N-180!Uo!N!C!CEnergy (eV)' 	; y title
options,'ion_180','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' 	; z title
options,'ion_180','x_no_interp',1 	; don't interpolate
options,'ion_180','y_no_interp',1 	; don't interpolate
options,'ion_180','yticks',3 	; set y-axis labels
options,'ion_180','ytickname',['10!A1!N','10!A2!N','10!A3!N','10!A4!N'] 	; set y-axis labels
options,'ion_180','ytickv',[10,100,1000,10000] 	; set y-axis labels
options,'ion_180','panel_size',2 	; set panel size

; Ion pitch angle spectrogram - survey data, remove retrace, >30 ions

get_pa_spec,"fa_ies_c",units='eflux',name='ion_pa',energy=[30,30000],retrace=1,/shift90,t1=t1,t2=t2
get_data,'ion_pa',data=tmp 	; get data structure
tmp.y=tmp.y > 1. 	; Remove zeros
tmp.y = alog10(tmp.y)	; Pre-log
store_data,'ion_pa',data=tmp 	; store data structure
options,'ion_pa','spec',1 	; set for spectrogram
zlim,'ion_pa',5,7,0 	; set z limits
ylim,'ion_pa',-100,280,0 	; set y limits
options,'ion_pa','ytitle','i+ >30 eV!C!C Pitch Angle'	; y title
; options,'ion_pa','ztitle','Log Eflux!C!CeV/cm!U2!N-s-sr-eV' 	; z title
options,'ion_pa','x_no_interp',1 	; don't interpolate
options,'ion_pa','y_no_interp',1 	; don't interpolate
options,'ion_pa','yticks',4 	; set y-axis labels
options,'ion_pa','ytickname',['-90','0','90','180','270'] 	; set y-axis labels
options,'ion_pa','ytickv',[-90,0,90,180,270] 	; set y-axis labels
options,'ion_pa','panel_size',2 	; set panel size

; Ion flux

get_2dt,'j_2d_fs','fa_ies_c',name='Ji',t1=t1,t2=t2,energy=[20,30000]
ylim,'Ji',1.e5,1.e8,1 	; set y limits
options,'Ji','ytitle','Ions!C!C1/(cm!U2!N-s)' 	; set y title
options,'Ji','tplot_routine','pmplot' 	; set 2 color plot
options,'Ji','labels',['Downgoing!C Ions','Upgoing!C Ions '] 	; set color label
options,'Ji','labflag',3 	; set color label
options,'Ji','labpos',[2.e7,1.e6] 	; set color label
options,'Ji','panel_size',1 	; set panel size

; Get the orbit data

orbit_file=fa_almanac_dir()+'/orbit/predicted'
get_fa_orbit,t1,t2,orbit_file=orbit_file,/all
get_data,'ORBIT',data=tmp
orbit=tmp.y(0)
orbit_num=strcompress(string(tmp.y(0)),/remove_all)

; Plot the data

loadct2,43
tplot,['el_0','el_pa','ion_180','ion_pa','JEe','Je','Ji'],$
          var_label=['ALT','ILAT','MLT'],title='FAST ORBIT '+orbit_num

; For viewing FAST Key Parameter Files (summary plots)

load_fa_k0_ees,orbit=1858 	; load electron k0 data
plot_fa_k0_ees 	; plot electron k0 data
load_fa_k0_ies,orbit=1858 	; load ion k0 data
plot_fa_k0_ies 	; plot ion k0 data

; For hard copies use

popen,/port,'plot_name'
loadct2,43
tplot
pclose

; For making and reading cdf files use

makecdf.pro
loadcdf.pro
loadcdfstr.pro

; For FAST orbit plots

plot_fa_crossing,orbit=1858

; For FAST attitude plots

plot_fa_att,'97-2-9/06:06:50'

; End crib 