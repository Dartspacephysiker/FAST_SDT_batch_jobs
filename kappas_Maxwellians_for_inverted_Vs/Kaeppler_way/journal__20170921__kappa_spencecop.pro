@ 'kappa_functions_03062014.pro'
; @ '../../distribution/test_funct_create_dist_04182013.pro'
 @ 'time_stamp.pro'

@'mpfit.pro'
@'mpfitfun.pro'

function funct,X,P


; print,'X', X
; print,'P', P
n_0 = P[0]
V_0 = P[1]
E_0 = P[2]
kappa = P[3]
beta = P[4]
; beta = A[3]
energy_cal =X

; print, P
; stop

;;;;;;;;;;; setting model parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create_F_e_mal, n_0, E_0, V_0, beta, E_in
E_min = 10.
E_max = 2e4
E_inc = 10.
; create_E_grid,E_max, E_min, E_inc, E_0
E_model = create_E_grid(E_max, E_min, E_inc, E_min)
; create_F_e_mal, n_0, E_0, V_0, beta, E_in

m_e = double(0.510998928e6/(2.9979e10^2)) ; in eV

; model = create_F_e_mal(n_0, E_0, V_0, beta, E_model)

; plot, E, diff_e_flux, /xlog, /ylog, xrange=[100,1e4], yrange=[1e5,1e10]
; oplot, E,diff_e_flux, psym=4
; oplot, E_model, E_model*model.F_E, color =230
 

; Velocity Grid
; do not set the parallel velocity to zero
V_inc =1e7 ;SQRT(2.*E_inc/m_e);1e7
V_0_perp = -1e10  ;-SQRT(2.*E_max/m_e);-5e9
V_f_perp = 1e10;SQRT(2.*E_max/m_e); 5e9
V_0_par = 0.5e8;SQRT(2.*E_inc/m_e);0.5e8 ;
v_f_par = 1e10;

; 05 27 2014
; accidently left this commented in for previous runs
;beta = 6.



v_grid = create_v_arr(m_e, V_0_perp,V_f_perp, V_0_par, V_f_par, V_inc) ; velocity grid
;   stop

    f_v = create_f_v_kappa(n_0, E_0,V_0, beta,kappa, v_grid.v_perp, v_grid.v_par)
    d_theta = 0.5

    pa_15 = pa_slice_E(P[4], v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
;      pa_30 = pa_slice_E(30., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
 
; window, 0
;  plot, E_model, pa_15.J_grid, /xlog, /ylog, yrange=[1e3,1e7]


; i0 = where(abs(E_model-X[0]) eq min(abs(E_model-X[0])) )
; 
; if((pa_15.j_grid[i0] lt 0.1)) then begin
; print, 'shifting V0'
; ; shift V0 slightly
; f_v = create_f_v_kappa(n_0, E_0,V_0-50., beta,kappa, v_grid.v_perp, v_grid.v_par)
;     d_theta = 0.5
; 
;     pa_15 = pa_slice_E(15., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
; endif
; stop
J_out = spline(E_model, pa_15.J_grid, X)
; J_out = X*J_out


; J_out = pa_15.J_grid;+;(r.J_upflux_E/(!pi))

return, J_out
end

function funct15_30,X,P

X_in = X

; added on 03 06 2014
; assumption, I am creating these arrays in sequetial order:
; 1,2,3..,2,3,..,1,2,3..
; sort the energy array

; test;;;;;;;;;;;
; x1 = findgen(9);+1
; x2 = findgen(10)+1
; 
; x = [x1,x2]
;;;;;;;;;;;;;;;;;;;;;;
q = sort(X)

; if energies are 1,2,3..,1,2,3, the first two indices are the same
cond0 = where( (x ge X[q[0]]-0.1) and (x le X[q[0]]+0.1))

; in the event that cond0 has one elements, where x = q[0] only
cond1 = where((x ge X[q[1]]-0.1) and (x le X[q[1]]+0.1))

; exclude the possiblity of this case if cond1 =1, and other conditions filled
if((cond0[0] ne -1) and (n_elements(cond0) gt 1) and (n_elements(cond1) gt 1)) then begin
X15 = X[cond0[0]:(cond0[1]-1)]
X30 = X[cond0[1]:*]
endif

if((cond1[0] ne -1) and (n_elements(cond1) gt 1) and (cond0[0] ne -1)) then begin
X15 = X[cond0[0]:(cond1[1]-1)]
X30 = X[cond1[1]:*]
endif

; there is the case that I may have: 1,2,3...,0,1,2,3
; that case is not included as it seems unlikely.

; print, X15
; print, X30


; stop

; print,'X', X
; print,'P', P
n_0 = P[0]
V_0 = P[1]
E_0 = P[2]
kappa = P[3]
 beta = P[4]
; energy_cal =X

; print, P
; stop

;;;;;;;;;;; setting model parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create_F_e_mal, n_0, E_0, V_0, beta, E_in
E_min = 10.
E_max = 2e4
E_inc = 10.
; create_E_grid,E_max, E_min, E_inc, E_0
E_model = create_E_grid(E_max, E_min, E_inc, E_min)
; create_F_e_mal, n_0, E_0, V_0, beta, E_in

m_e = double(0.510998928e6/(2.9979e10^2)) ; in eV

; model = create_F_e_mal(n_0, E_0, V_0, beta, E_model)

; plot, E, diff_e_flux, /xlog, /ylog, xrange=[100,1e4], yrange=[1e5,1e10]
; oplot, E,diff_e_flux, psym=4
; oplot, E_model, E_model*model.F_E, color =230
 

; Velocity Grid
; do not set the parallel velocity to zero
V_inc =1e7 ;SQRT(2.*E_inc/m_e);1e7
V_0_perp = -1e10  ;-SQRT(2.*E_max/m_e);-5e9
V_f_perp = 1e10;SQRT(2.*E_max/m_e); 5e9
V_0_par = 0.5e8;SQRT(2.*E_inc/m_e);0.5e8 ;
v_f_par = 1e10;

; commented out on 
;beta = 6.



v_grid = create_v_arr(m_e, V_0_perp,V_f_perp, V_0_par, V_f_par, V_inc) ; velocity grid
;   stop
print, beta
    f_v = create_f_v_kappa(n_0, E_0,V_0, beta,kappa, v_grid.v_perp, v_grid.v_par)
    d_theta = 0.5

    pa_15 = pa_slice_E(15., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
      pa_30 = pa_slice_E(30., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
 
; window, 0
;  plot, E_model, pa_15.J_grid, /xlog, /ylog, yrange=[1e3,1e7]


; i0 = where(abs(E_model-X[0]) eq min(abs(E_model-X[0])) )
; 
; if((pa_15.j_grid[i0] lt 0.1)) then begin
; print, 'shifting V0'
; ; shift V0 slightly
; f_v = create_f_v_kappa(n_0, E_0,V_0-50., beta,kappa, v_grid.v_perp, v_grid.v_par)
;     d_theta = 0.5
; 
;     pa_15 = pa_slice_E(15., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
; endif
; stop
J_out1 = spline(E_model, pa_15.J_grid, X15)
J_out2 = spline(E_model, pa_30.J_grid, X30)



J_out = [J_out1, J_out2]

; print, J_out

; stop
; J_out = X*J_out


; J_out = pa_15.J_grid;+;(r.J_upflux_E/(!pi))

return, J_out
end

function funct00_15,X,P

X_in = X

; added on 03 06 2014
; assumption, I am creating these arrays in sequetial order:
; 1,2,3..,2,3,..,1,2,3..
; sort the energy array

; test;;;;;;;;;;;
; x1 = findgen(9);+1
; x2 = findgen(10)+1
; 
; x = [x1,x2]
;;;;;;;;;;;;;;;;;;;;;;
q = sort(X)

; if energies are 1,2,3..,1,2,3, the first two indices are the same
cond0 = where( (x ge X[q[0]]-0.1) and (x le X[q[0]]+0.1))

; in the event that cond0 has one elements, where x = q[0] only
cond1 = where((x ge X[q[1]]-0.1) and (x le X[q[1]]+0.1))

; exclude the possiblity of this case if cond1 =1, and other conditions filled
if((cond0[0] ne -1) and (n_elements(cond0) gt 1) and (n_elements(cond1) gt 1)) then begin
X00 = X[cond0[0]:(cond0[1]-1)]
X15 = X[cond0[1]:*]
endif

if((cond1[0] ne -1) and (n_elements(cond1) gt 1) and (cond0[0] ne -1)) then begin
X00 = X[cond0[0]:(cond1[1]-1)]
X15 = X[cond1[1]:*]
endif

; there is the case that I may have: 1,2,3...,0,1,2,3
; that case is not included as it seems unlikely.

; print, X00
; print, X15


; stop

; print,'X', X
; print,'P', P
n_0 = P[0]
V_0 = P[1]
E_0 = P[2]
kappa = P[3]
 beta = P[4]
; energy_cal =X

; print, P
; stop

;;;;;;;;;;; setting model parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create_F_e_mal, n_0, E_0, V_0, beta, E_in
E_min = 10.
E_max = 2e4
E_inc = 10.
; create_E_grid,E_max, E_min, E_inc, E_0
E_model = create_E_grid(E_max, E_min, E_inc, E_min)
; create_F_e_mal, n_0, E_0, V_0, beta, E_in

m_e = double(0.510998928e6/(2.9979e10^2)) ; in eV

; model = create_F_e_mal(n_0, E_0, V_0, beta, E_model)

; plot, E, diff_e_flux, /xlog, /ylog, xrange=[100,1e4], yrange=[1e5,1e10]
; oplot, E,diff_e_flux, psym=4
; oplot, E_model, E_model*model.F_E, color =230
 

; Velocity Grid
; do not set the parallel velocity to zero
V_inc =1e7 ;SQRT(2.*E_inc/m_e);1e7
V_0_perp = -1e10  ;-SQRT(2.*E_max/m_e);-5e9
V_f_perp = 1e10;SQRT(2.*E_max/m_e); 5e9
V_0_par = 0.5e8;SQRT(2.*E_inc/m_e);0.5e8 ;
v_f_par = 1e10;

; commented out on 
;beta = 6.



v_grid = create_v_arr(m_e, V_0_perp,V_f_perp, V_0_par, V_f_par, V_inc) ; velocity grid
;   stop
print, beta
    f_v = create_f_v_kappa(n_0, E_0,V_0, beta,kappa, v_grid.v_perp, v_grid.v_par)
    d_theta = 0.5

    pa_00 = pa_slice_E(00., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
      pa_15 = pa_slice_E(15., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
 
; window, 0
;  plot, E_model, pa_00.J_grid, /xlog, /ylog, yrange=[1e3,1e7]


; i0 = where(abs(E_model-X[0]) eq min(abs(E_model-X[0])) )
; 
; if((pa_00.j_grid[i0] lt 0.1)) then begin
; print, 'shifting V0'
; ; shift V0 slightly
; f_v = create_f_v_kappa(n_0, E_0,V_0-50., beta,kappa, v_grid.v_perp, v_grid.v_par)
;     d_theta = 0.5
; 
;     pa_00 = pa_slice_E(00., v_grid.pa_arr, v_grid.v_perp,v_grid.v_par, f_v.J_v, m_e, v_grid.v_perp_plot, v_grid.v_par_plot,d_theta,E_model);
; endif
; stop
J_out1 = spline(E_model, pa_00.J_grid, X00)
J_out2 = spline(E_model, pa_15.J_grid, X15)



J_out = [J_out1, J_out2]

; print, J_out

; stop
; J_out = X*J_out


; J_out = pa_15.J_grid;+;(r.J_upflux_E/(!pi))

return, J_out
end

function peak_finder, E, J_in
; wrote on 03 04 2014
; this routine is designed to find the peak flux and the decaying portion of the flux

; all it will do is take the derivative and find three consecutive negative slopes
; AND
; that the J[i+1]/J[i] and J[i+2]/J[i] have an exponential decrease when decaying
; it will loop through to find that location

derv = deriv(E, J_in)
n = n_elements(E)-1
i =0
foo = [0.,0.]

while( i le n-3) do begin
;  print, i, E[i],J_in[i], derv[i], J_in[i+1]/J_in[i], J_in[i+2]/J_in[i]
  if((derv[i+1] lt 0) AND (derv[i+2] lt 0) and (derv[i+3] lt 0) and (E[i] ge 500)and ((J_in[i+1]/J_in[i]) lt 0.95) and ((J_in[i+2]/J_in[i]) lt 0.75) ) then begin
  print, i, E[i],J_in[i], derv[i]
 E_out = E[i]
break
;    stop
  endif else begin
  i++
  E_out = -1000.
  endelse
endwhile

if (E_out eq -1000.) then begin
print, 'could not find decreasing slope'
print, 'E_out being set to 500'
E_out = 500.
endif
 
return, E_out
end










function reduce_spectra, E, J_in,err_in,E_peak 

; copying just so I don't F-up the data.
J_15 = J_in
err_15 = err_in
energy_cal = E
E_peak15 = E_peak


q_select15 = where((J_15 ne 0.) and (energy_cal ge E_peak15[0]) AND FINITE(J_15) AND FINITE(err_in) AND FINITE(err_in)) ; and (deriv(energy_cal, J_15) lt 0.) )

if(q_select15[0] ne -1) then begin
X = double(energy_cal[q_select15]); or (energy_cal ge max(energy_cal))))])
Y = double(J_15[q_select15]); or (energy_cal ge max(energy_cal))))])
err = double(err_15[q_select15])
endif else begin
print, 'issue finding data points on J15'
endelse

out = {X:X, $
Y:Y, $
err:err}

return, out
end




function plot_spectra, x,y,err,energy_cal, J_15, P,E_peak15,win,ytitle
pl = funct(X,P)
chi = total((pl-y)^2/err^2)/(n_elements(y)-4.)


;  window, win
plot, x, y, /xlog, /ylog, yrange=[1e3,1e7],xrange=[1e2,1e4],ytitle=ytitle, $
title =strcompress(string(P[4]), /remove_all)+'PA, i:'+strcompress(string(win)), $
xtitle = 'n0: '+strcompress(string(P[0]), /remove_all)+ $
	 'V0: '+strcompress(string(P[1]), /remove_all)+ $
	 'E0: '+strcompress(string(P[2]), /remove_all)+ $
	 'kap: '+strcompress(string(P[3]), /remove_all)+ $
	 'Chi: '+strcompress(string(chi), /remove_all)

errplot, x, y-err, y+err
; f_j = linfit(x,alog(y))
; y1 = f_j[0]+(f_j[1]*x)
; oplot, x, exp(y1)
oplot, x, pl, color =230
oplot, energy_cal, J_15
oplot, energy_cal[where(energy_cal eq E_peak15)], J_15[where(energy_cal eq E_peak15)], psym=4
time_stamp
return, 0
end




function int_eepaa_flux, V_0,d_theta,E, diff_n_0, diff_n_15, diff_n_30,diff_n_45,diff_n_60,diff_n_75,diff_n_90,pa_arr
; integrate the eepaa flux directly
; first step, convert from differential energy flux into f(E,alpha)

;; pa_arr = [0.,15.,30.,45.,60.,75.,90.]
n = n_elements(E)

IF N_ELEMENTS(d_theta) EQ 1 THEN dTh = REPLICATE(d_theta,N_ELEMENTS(pa_arr)) ELSE dTh = d_theta

par = [reform(diff_n_0*sin(!dtor*pa_arr[0])*cos(!dtor*pa_arr[0])*dTh[0]*!dtor, [1,n]), $
		  reform(diff_n_15*sin(!dtor*pa_arr[1])*cos(!dtor*pa_arr[1])*dTh[1]*!dtor, [1,n]), $
		  reform(diff_n_30*sin(!dtor*pa_arr[2])*cos(!dtor*pa_arr[2])*dTh[2]*!dtor, [1,n]), $
		  reform(diff_n_45*sin(!dtor*pa_arr[3])*cos(!dtor*pa_arr[3])*dTh[3]*!dtor, [1,n]), $
		  reform(diff_n_60*sin(!dtor*pa_arr[4])*cos(!dtor*pa_arr[4])*dTh[4]*!dtor, [1,n]), $
		  reform(diff_n_75*sin(!dtor*pa_arr[5])*cos(!dtor*pa_arr[5])*dTh[5]*!dtor, [1,n]), $
		  reform(diff_n_90*sin(!dtor*pa_arr[6])*cos(!dtor*pa_arr[6])*dTh[6]*!dtor, [1,n])]



; test the omni directional using Evans parameters
omni = [reform(diff_n_0*sin(!dtor*pa_arr[0])*dTh[0]*!dtor, [1,n]), $
		  reform(diff_n_15*sin(!dtor*pa_arr[1])*dTh[1]*!dtor, [1,n]), $
		  reform(diff_n_30*sin(!dtor*pa_arr[2])*dTh[2]*!dtor, [1,n]), $
		  reform(diff_n_45*sin(!dtor*pa_arr[3])*dTh[3]*!dtor, [1,n]), $
		  reform(diff_n_60*sin(!dtor*pa_arr[4])*dTh[4]*!dtor, [1,n]), $
		  reform(diff_n_75*sin(!dtor*pa_arr[5])*dTh[5]*!dtor, [1,n]), $
		  reform(diff_n_90*sin(!dtor*pa_arr[6])*dTh[6]*!dtor, [1,n])]

i =0
omni_arr = [0.,0.]
par_arr = [0.,0.]
q_E = where(E ge V_0[0])

while(i le (n-1)) do begin

tmp_par = int_tabulated(pa_arr, par[*,i])
par_arr = [par_arr, tmp_par]

tmp_omni = int_tabulated(pa_arr, omni[*,i])
omni_arr = [omni_arr, tmp_omni]

i++
endwhile

par_arr = par_arr[2:*]
omni_arr = omni_arr[2:*]

print, 'omni flux - my function', int_tabulated(E[Q_E], omni_arr[Q_E])
print, 'par flux - my function', int_tabulated(E[Q_E], par_arr[Q_E])

out = {par_num_flux:int_tabulated(E[Q_E], par_arr[Q_E]), $
      omni_num_flux: int_tabulated(E[Q_E], omni_arr[Q_E])}

return, out
end
 

function calc_n0_max, par_num_flux, V_0, E_0, beta  
; look in test_integrate_eepaa_03182014.pro that tests this function

A_exp = (-V_0)/(E_0*(beta-1.)) 
K_cor = beta*(1.-(1.-(1./beta))*exp(A_exp))


; using the knight relation from Bruning and Goertz, 1986
n_0 = par_num_flux/((1.67425e7)*SQRT(E_0)*K_cor)

return, n_0
end

function calc_n0_kappa, par_num_flux, V_0, E_0, beta, kappa 
; look in test_integrate_eepaa_03182014.pro that tests this function

; dors forumla 11, 12, and 10

W_temp = 0.5*E_0*(2.*kappa-3.) ; in units of eV

m_e = double(0.510998928e6/(2.9979e10^2)) ; in eV

w = SQRT((E_0*(2.*kappa-3.))/(m_e*kappa))

A_k = gamma(kappa+1)/((kappa-1.)*SQRT(kappa)*gamma(kappa-0.5))

Exp_kappa = (1.-(1./beta))*((1.+(V_0/(W_temp*(beta-1.))))^(-kappa+1.))

; par_num_flux = (n_0*w*A_k*beta*(1.-exp_kappa ))/(2.*SQRT(!pi) )

n_0 = (par_num_flux*2.*SQRT(!pi))/(w*A_k*beta*(1.-exp_kappa))

; stop

return, n_0
end


function find_n0, V_0, E_0,beta, d_theta,E,diff_n_0, diff_n_15, diff_n_30,diff_n_45,diff_n_60,diff_n_75,diff_n_90,pa_arr
; this is an updated function from find n0, which I will likely delete after I have raided it

; I will integrate the rocket distribution function, and from that get guesses on n0

; first integrate the distribution function
; should be the same for both maxwellian and kappa
; stop
par_num_eepaa = int_eepaa_flux(V_0,d_theta,E, diff_n_0, diff_n_15, diff_n_30,diff_n_45,diff_n_60,diff_n_75,diff_n_90,pa_arr)

;  stop
par_num_flux = par_num_eepaa.par_num_flux
; from the 
;P_in[0] = V0, P_in[1] =E0, P_in[2] = beta
n0_max = calc_n0_max(par_num_flux, V_0, E_0, beta)
n0_kap_25 = calc_n0_kappa(par_num_flux, V_0, E_0, beta, 25.)
n0_kap_8 = calc_n0_kappa(par_num_flux, V_0, E_0, beta, 10.)
n0_kap_3 = calc_n0_kappa(par_num_flux, V_0, E_0, beta, 3.)
; now comes the test, calculate reduced chi squared for the kappa of 25 or kappa of 10.
print, '------------------------'
print, V_0, E_0, beta
print, 'n0_maxwellian', n0_max
print, 'n0_kappa_25', n0_kap_25
print, 'n0_kappa_10', n0_kap_8
print, 'n0_kappa_3', n0_kap_3
print, '------------------------'
out = {n0_max:n0_max, $
	n0_kap_25:n0_kap_25, $
 	n0_kap_3:n0_kap_3, $
 	n0_kap_8:n0_kap_8}

 return, out
 end






















;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





pro kappa_converge10,KAEPPLER_WAY=Kaeppler_way

;  03 05 2014
; this is going to be a significant branch cut from the previous version included cleaned up code
; I will also be creating functions for doing the LM procedure and having it produce and output
; because I am coping the same pieces of code again and again

; 03 07 2014
; this branch came from initial_guess_mike file
; these files are more self contained

; 03 18 2014
; another branch today on the code
; worked up a grid search routine and a band where the solutions appear to produce nearly the same chi-squared
; SO a grid search alone won't nessesarily produce a good solution
; to work around, determined a way using the knight relation for kappas (dors) and maxwellian to 'back out' an estimate for n0
; note that the peak electron flux corresponds to a guess of V0 and is an upper limit on this parameter
; if V0 is too large, when you take a slice you won't see the peak
; so the for max V0 we can have, it will give us the lowest bound on n0.  
; so maybe do a routine that starts V0 at the peak and n0 at whatever it comes out to, then let LM determine the best parameter estimates

; one thing to watch with is to see how n0 changes.  I may want to write a constraint that n0 effectively fixes n0
; for now leave totally unconstrained

; also may still want to do 15/30/ 15&30 together, although I think the combined 15&30 is probably the best for our purposes.  


; 03 19 2014
; this program is designed to get the inital guesses for n0 and v0 from the peak finding program and the 
; I will probably just print these out

; the strategy in this program will be two steps before calculating LM
; step 1, determine the peak and 'guess' n0
; calculate an initial chi square with those parameters
; do a very limited grid search to see if a better chi square can be obtained
; by limited, I mean +/- 0.1 in n0 and V0, go a bit below peak V0, and up to E_peak, at 100 eV increments
; may also want to test kappa = 8, 25 (maxwellian)
; with these parameters in hand, then do a LM search to obtain the best solution with lowest chi
; strategy is to run initial searches around that guess and do the LM last and only once to reduce computation time

; 04 09 2014
; I made a pretty significant mistake in the 03212014 and 03312014 03312014_runs;
; the inital estimates I made were done with beta = 25, rather than beta =6
;I think I made a simple error about which elements were which in the array of parameters entering
; so that as disappointing, but I think it is correct for me to go back through and rerun this

; 05 27 2014
; updated to now include beta in the parameter and also writing a save file in with the parameters
; I am doing this mainly to keep things consistent between various runs 
; Craig pointed out while I was back in IA that technically the temperature integral does not close if
; kappa = 2.  So he suggested setting that as a limit in the fitting routine
; all of the results for beta = 6, kappa limit = 1.5 still stand 
; I am going to re run the results for various betas and kappa limit = 2 this week

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; initialization parameters/files
out_ps= './05272014/BRO.ps'
out_sav = './05272014/BRO.sav'
in_data = '../spectra_combine/21139_av_spectra_03302014.h5'; 21139_av_spectra_nosqrt_04012014.h5';21139_av_spectra_03302014.h5'

;;Spencefile
inDir = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/'
inFile = '20170922_kappaGaussFits_orig__for_kappaManuscript_Fig2.sav'

; kappa = 8.
cmd = 'cp '+out_sav+' ~/Desktop/'
; restore the parameter constraint
;k15 = kappa limit set to 1.5
; k20 = kappa limit set to 2.0
restore, 'parameter_constraints_k20_05272014.sav'

; I will now HARDCODE IN what beta, E_0 will be
E_0 = 800. 
beta = 500.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

t_start =systime(1)

!p.font=0
!p.multi=0
t = 5
!x.thick=t
!y.thick =t
!z.thick =t 
!p.thick =t
set_plot, 'ps'
 device, filename=out_ps, /landscape,/color, bits =8
loadct, 39

; using h5 files to do data placement - easier to deal with and more transferrable

IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
j_start = 0.
j_end = 404.

d_theta = 7.5

in = H5_PARSE(in_data, /READ_DATA)

; data management

energy_cal = in.j_eepaa._data.energy_cal
i = in.j_eepaa._data.i
J_15_mean = in.j_eepaa._data.J_15_mean
J_15_stdev = in.j_eepaa._data.J_15_stdev
J_30_mean = in.j_eepaa._data.J_30_mean
J_30_stdev = in.j_eepaa._data.J_30_stdev
J_45_mean = in.j_eepaa._data.J_45_mean
J_45_stdev = in.j_eepaa._data.J_45_stdev
J_60_mean = in.j_eepaa._data.J_60_mean
J_60_stdev = in.j_eepaa._data.J_60_stdev
J_75_mean = in.j_eepaa._data.J_75_mean
J_75_stdev = in.j_eepaa._data.J_75_stdev
J_90_mean = in.j_eepaa._data.J_90_mean
J_90_stdev = in.j_eepaa._data.J_90_stdev

; ran into an issue where the err was = 0
; to elminate that problem, will set the error value to 0.5*Y at that point
J_15_stdev[where(J_15_stdev eq 0.)] = 0.5*J_15_mean[where(J_15_stdev eq 0.)]
J_30_stdev[where(J_30_stdev eq 0.)] = 0.5*J_30_mean[where(J_30_stdev eq 0.)]
J_45_stdev[where(J_45_stdev eq 0.)] = 0.5*J_45_mean[where(J_45_stdev eq 0.)]

thetas = [0.,15.,30.,45.,60.,75.,90.]
nThetas = N_ELEMENTS(thetas)
thetaInds = INDGEN(nThetas)

ENDIF ELSE BEGIN

i = [0,1,2]
j_start = 0.
j_end = 4.

RESTORE,inDir+inFile

dat = conv_units(dat,'flux')

fullTheta = REVERSE(dat.theta,1)
JSp = REVERSE(dat.data,1)
errSp = REVERSE(dat.ddata,1)
energy_cal = REFORM((REVERSE(dat.energy,1))[*,0])
;; thetas = REFORM((REVERSE(dat.theta,1))[0,*])
thetas = MEAN(REVERSE(dat.theta,1),DIMENSION=1)
dthetas = N_ELEMENTS(SIZE(dat.dtheta,/DIM)) GT 1 ? MEAN(REVERSE(dat.dtheta,1),DIMENSION=1) : REVERSE(dat.dtheta,1)
nThetas = N_ELEMENTS(thetas)

ENDELSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; starting the loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this will be for the V0 = E_peak-1
best_guess0_arr = fltarr([2,6]) ; after coming out of the 
P_chi0 = [0.,0.]
P_arr0 = fltarr([2,5])
P_err_arr0 = fltarr([2,5])
P_bestnorm0 = [0.,0.]
status0 = [0.,0.]

; V0 = E_peak-delta/2
best_guess1_arr = fltarr([2,6]) ; after coming out of the 
P_chi1 = [0.,0.]
P_arr1 = fltarr([2,5])
P_err_arr1 = fltarr([2,5])
P_bestnorm1 = [0.,0.]

time_exe_arr = [0.,0.]

flag_chi0 = [0.,0.]
flag_chi1 = [0.,0.]

i_arr = [0.,0.]
; selecting the index of the spectra of interest
j=j_start
 print, i[j]
 
t_start = systime(1)
 while(j le j_end) do begin

 i_arr = [i_arr, j]

IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
;;Kaeppler way
J_00 = J_15_MEAN[j,*]
J_15 = J_15_MEAN[j,*]
J_30 = J_30_mean[j,*]
J_45 = J_45_mean[j,*]
J_60 = J_60_MEAN[j,*]
J_75 = J_75_mean[j,*]
J_90 = J_90_mean[j,*]

err_00 = J_15_stdev[j,*]
err_15 = J_15_stdev[j,*]
err_30 = j_30_stdev[j,*]
err_45 = j_45_stdev[j,*]

ENDIF ELSE BEGIN
;;Spence way
theseThetas = [0,15.,30.,45.,60.,75.,90.]
thetaInds = !NULL
FOR k=0,N_ELEMENTS(theseThetas)-1 DO BEGIN
   junk = MIN(ABS(theseThetas[k]-thetas),ind)
   ;; VALUE_CLOSEST2(thetas[SORT(thetas)],theseThetas,/CONSTRAIN) ;;can't use because theta not sorted
   thetaInds = [thetaInds,ind]
ENDFOR

d_theta = dthetas[thetaInds]

J_00 = JSp[*,thetaInds[0]]
J_15 = JSp[*,thetaInds[1]]
J_30 = JSp[*,thetaInds[2]] 
J_45 = JSp[*,thetaInds[3]] 
J_60 = JSp[*,thetaInds[4]] 
J_75 = JSp[*,thetaInds[5]] 
J_90 = JSp[*,thetaInds[6]] 

err_00 = errSp[*,thetaInds[0]]
err_15 = errSp[*,thetaInds[1]]
err_30 = errSp[*,thetaInds[2]]
err_45 = errSp[*,thetaInds[3]]

ENDELSE

;;Reverse thangs
;; kappaFit.orig.x = REVERSE(kappaFit.orig.x)
;; kappaFit.orig.y = REVERSE(kappaFit.orig.y)
;; kappaFit.orig.yError = REVERSE(kappaFit.orig.yError)

;; energy_calSp = kappaFit.orig.x
;; J_Sp        = kappaFit.orig.y
;; err_Sp      = kappaFit.orig.yError

; stop
E_peak00 = peak_finder(energy_cal, REFORM(J_00))
E_peak15 = peak_finder(energy_cal, REFORM(J_15))
E_peak30 = peak_finder(energy_cal, REFORM(J_30))

;; E_peak   = MAKE_ARRAY(nThetas,VALUE=0.D,/DOUBLE)
;; FOR k=0,nThetas-1 DO BEGIN
;;    E_peak[k] = PEAK_FINDER(energy_calSP, REFORM(JSp[*,k]))
;; ENDFOR
;; E_peakSp = peak_finder(energy_calsp, J_Sp)

; 21 March 2014
; take the peak E_peak between 30 and 15 PA

if(E_peak00 gt E_peak15) then begin
E_peak15 = E_peak00
endif

if(E_peak15 gt E_peak00) then begin
E_peak00 = E_peak15
endif

if(E_peak15 gt E_peak30) then begin
E_peak30 = E_peak15
endif

if(E_peak30 gt E_peak15) then begin
E_peak15 = E_peak30
endif

; the peak energy at one index less
; this will form the initial guess for V0
; read notebook for explaination
V0_peak_less1_00 = energy_cal[where(energy_cal eq E_peak00)-1]
V0_peak_less1_15 = energy_cal[where(energy_cal eq E_peak15)-1]
; V0_guess30 = energy_cal[where(energy_cal eq E_peak30)-1]

;; V0_peak_less1Sp = energy_calSp[WHERE(energy_calSp EQ E_peakSp)-1]

; take a 'middle ground' position for peak energy
V0_guess00 = double(E_peak00 - ((E_peak00-V0_peak_less1_00)/2.))
V0_guess15 = double(E_peak15 - ((E_peak15-V0_peak_less1_15)/2.))
;; V0_guessSp = double(E_peakSp - ((E_peakSp-V0_peak_less1Sp)/2.))
; get the reduced spectra - this is the spectra from the peak energy onward
; this corresponds to the portion of the distribution that has been accelerated
; by the potential drop
pa00 = reduce_spectra(energy_cal, J_00,err_00,E_peak00) 
pa15 = reduce_spectra(energy_cal, J_15,err_15,E_peak15) 
pa30 = reduce_spectra(energy_cal, J_30,err_30,E_peak30) 
;; paSp = REDUCE_SPECTRA(energy_calSp, J_Sp,err_Sp,E_peakSp) 
; pa45 = reduce_spectra(energy_cal, J_45,err_45,E_peak45) 


; 03 18 2014
; so now that I have determined the peak energy for some pitch angle
; I want to take the guess for V0 and determine the guess for n0
; I want to do this over the test intervals first and then see over a larger range
; one other thing I will do as a quick test - I will use kappa = 25 as being effectively maxwellian
; but I want to write a robust way that will use a kappa guess it the distribution appears to be better fit with a kappa distribution
; to do this and from now on, I will assume two sets of parameters
; for the maxwellian P = [n_0, V_0(guess), 1000, 26]
; for the kappa P = 

X_15_30 = [pa15.X,pa30.X]
Y_15_30 = [pa15.y, pa30.Y]
err_15_30 = [pa15.err, pa30.err]

X_00_15 = [pa00.X,pa15.X]
Y_00_15 = [pa00.y, pa15.Y]
err_00_15 = [pa00.err, pa15.err]


IF KEYWORD_SET(showPlots) THEN BEGIN
   IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
      plot = ERRORPLOT(X_15_30,Y_15_30,err_15_30,LINESTYLE='',SYMBOL='+',/XLOG,/YLOG)
   ENDIF ELSE BEGIN
      plot = ERRORPLOT(X_00_15,Y_00_15,err_00_15,LINESTYLE='',SYMBOL='+',/XLOG,/YLOG)
   ENDELSE
ENDIF
;; STOP
; 03 20 2014
; method 1 will be when I use V_0-delta/2
; method 2 will use V0 based on peak energy -1

 
; now determines the n0 which is consistent, based on the flux, with what we would expect
n0_best0 = find_n0(V0_peak_less1_00, E_0,beta, d_theta,energy_cal,J_00, J_15, J_30,J_45,J_60,J_75,J_90,thetas[thetaInds])

P_guess0 = [n0_best0.n0_kap_8, V0_peak_less1_00, E_0, 10.,beta]
IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
   pl = funct15_30(X_15_30,P_guess0)
   tmp_chi_kappa = total((pl-y_15_30)^2/err_15_30^2)/(n_elements(y_15_30)-4)
ENDIF ELSE BEGIN
   pl = funct00_15(X_00_15,P_guess0)
   tmp_chi_kappa = TOTAL((pl-y_00_15)^2/err_00_15^2)/(N_ELEMENTS(y_00_15)-4)
ENDELSE

tmp_P_out = [tmp_chi_kappa, P_guess0]
best_guess0_arr = [best_guess0_arr, reform(tmp_P_out, [1,6])]

;  stop
; so now I have an inital guess for n0
; now time to do a small order grid search to see if there are any better parameters
; note as well that I will do this for 15 degree PA for testing, but when I run this for real will do 15/30 together



; 
; 
n0_best1 = find_n0(V0_guess00,E_0,beta,d_theta,energy_cal,J_00, J_15, J_30,J_45,J_60,J_75,J_90,thetas[thetaInds])
P_guess1 = [n0_best1.n0_kap_8, V0_guess00, E_0, 10.,beta]

IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
pl = funct15_30(X_15_30,P_guess1)
tmp_chi_kappa = total((pl-y_15_30)^2/err_15_30^2)/(n_elements(y_15_30)-4)
ENDIF ELSE BEGIN
pl = funct00_15(X_00_15,P_guess1)
tmp_chi_kappa = total((pl-y_00_15)^2/err_00_15^2)/(n_elements(y_00_15)-4)
ENDELSE
tmp_P_out1 = [tmp_chi_kappa, P_guess1]
best_guess1_arr = [best_guess1_arr, reform(tmp_P_out1, [1,6])]



; 


		  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; putting data into LM routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; updated on 05272014
; putting in save file so that way parameter estimates are consistent





; ; parameter info - to constrain parameters
; pi = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D], mpside:0, relstep:0},4)
; 
; ; updated n0 lower bound
; ;n_0
; pi[0].limited[0] = 1 ; turn on lower boundary
; pi[0].limits[0] = 0.1 ; set lower boundary
; pi[0].limited[1] = 1 ; turn on upper boundary
; pi[0].limits[1] = 2.5 ; set upper boundary
; 
; ;V_0
; pi[1].limited[0] = 1 ; turn on lower boundary
; pi[1].limits[0] = 0. ; set lower boundary
; pi[1].limited[1] = 1 ; turn on upper boundary
; pi[1].limits[1] =  4000. ; set upper boundary
; 
; ;E_0
; pi[2].limited[0] = 1 ; turn on lower boundary
; pi[2].limits[0] = 0. ; set lower boundary
; pi[2].limited[1] = 1 ; turn on upper boundary
; pi[2].limits[1] = 1800. ; set upper boundary
; 
; 
; ;kappa
; ; note if kappa < 1.5 then temperature goes to zero
; ; kappa < 1.5 would correspond to a 'negative temperature' which doesn't make sense to me
; ; check that the equation I am using is correct.
; pi[3].limited[0] = 1 ; turn on lower boundary
; pi[3].limits[0] = 1.5 ; set lower boundary
; pi[3].limited[1] = 1 ; turn on upper boundary
; pi[3].limits[1] = 50. ; set upper boundary
; 
; pi[1].MPSIDE =2
; pi[0].mpside=2
; pi[2].mpside=2
; pi[3].mpside=2


;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ; flags
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  
; ; I want to set a couple of flags that I will return as well
; ; one flag will be a chi square flag and another will be for 'edges' on the grid search
; ; I am going to set three chi square levels
; ; chi sq <= 5, chi sq <= 10, chi_sq > 10
; ; if chi sq <=5, that is probably a good enough fit

;
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ; Run LM
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  


IF KEYWORD_SET(Kaeppler_way) THEN BEGIN
   tmp_result15_30 = MPFITFUN('funct15_30', X_15_30, Y_15_30,err_15_30, P_guess0, parinfo=pi, covar=covar,perror=perror, status=status, bestnorm=bestnorm,dof=dof,xtol=1D-3)
P_arr0 = [P_arr0, reform(tmp_result15_30, [1,5])]
ENDIF ELSE BEGIN
   tmp_result00_15 = MPFITFUN('funct00_15', X_00_15, Y_00_15,err_00_15, P_guess0, parinfo=pi, covar=covar,perror=perror, status=status, bestnorm=bestnorm,dof=dof,xtol=1D-3)
P_arr0 = [P_arr0, reform(tmp_result00_15, [1,5])]
ENDELSE

; ; testing
; tmp_result15_30 = fltarr(4)
; perror = fltarr(4)
; dof = 1
; bestnorm =1


P_err_arr0 = [P_err_arr0, reform(perror, [1,5])]
P_chi0 = [P_chi0, bestnorm/dof]
status0 = [status0, status]
; these flags

if((bestnorm/dof) le 10) then begin
tmp_chi = 1
endif

if(((bestnorm/dof) gt 10) and ((bestnorm/dof) le 25)) then begin
tmp_chi = 2
endif

if((bestnorm/dof) gt 25) then begin
tmp_chi = -1
endif

flag_chi0 = [flag_chi0, tmp_chi]


P15 = [tmp_result15_30, 15.]
P30 = [tmp_result15_30, 30.]
!p.multi = [0,2,2]
plot15 = plot_spectra(pa15.x,pa15.y,pa15.err,energy_cal, J_15, P15,E_peak15,j, '0: V0_peak_less1')
 plot30 = plot_spectra(pa30.x,pa30.y,pa30.err,energy_cal, J_30, P30,E_peak15,j, '0: V0_peak_less1')




; ; second guess
; 
; ; run LM routine
 tmp_result15_30 = MPFITFUN('funct15_30', X_15_30, Y_15_30,err_15_30, P_guess1, parinfo=pi, covar=covar,perror=perror, status=status, bestnorm=bestnorm,dof=dof,xtol=1D-3)


P_arr1 = [P_arr1, reform(tmp_result15_30, [1,5])]

P_err_arr1 = [P_err_arr1, reform(perror, [1,5])]
P_chi1 = [P_chi1, bestnorm/dof]

if((bestnorm/dof) le 10) then begin
tmp_chi = 1
endif

if(((bestnorm/dof) gt 10) and ((bestnorm/dof) le 25)) then begin
tmp_chi = 2
endif

if((bestnorm/dof) gt 25) then begin
tmp_chi = -1
endif

flag_chi1 = [flag_chi1, tmp_chi]


tmp_time = systime(1)-t_start

time_exe_arr = [time_exe_arr, tmp_time]
; stop
; ; plot the spectra for the initial Guess
; ; last variable in the call is for which idl window you want the plot to appear in

P15 = [tmp_result15_30, 15.]
P30 = [tmp_result15_30, 30.]
!p.multi =[0,1,2]
plot15 = plot_spectra(pa15.x,pa15.y,pa15.err,energy_cal, J_15, P15,E_peak15,j, '1: Epeak-delta/2')
plot30 = plot_spectra(pa30.x,pa30.y,pa30.err,energy_cal, J_30, P30,E_peak15,j, '1: Epeak- delta/2')
; ; stop

; do the 'insurance thing'

save, filename = out_sav, best_guess0_arr, P_chi0, P_arr0,P_err_arr0,P_bestnorm0, $
time_exe_arr,flag_chi0,i_arr, best_guess1_arr, P_chi1, P_arr1,P_err_arr1,P_bestnorm1,in_data

spawn, cmd


j++
endwhile

device, /close
set_plot, 'x'

end


; P_arr = P_arr[2:*,*]
; P_err_arr = P_err_arr[2:*,*]
; P_chi = p_chi[2:*]
; initial_guess_arr = initial_guess_arr[2:*,*]
; i_arr = i_arr[2:*]
; n_iarr = n_elements(i_arr)
; g_search_arr = g_search_arr[2:*]
; g_search_arr = reform(g_search_arr, [n_iarr, 50,5])
; best_guess_arr = best_guess_arr[2:*,*]
; flag_chi = flag_chi[2:*]
; flag_edge = flag_edge[2:*]
; time_exe_arr = time_exe_arr[2:*]
; 
; 
; 
; stop
; 
; end
