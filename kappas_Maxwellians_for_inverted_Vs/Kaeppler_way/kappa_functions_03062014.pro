

; create energy grid
function create_E_grid,E_max, E_min, E_inc, E_0
E_in_grid = (findgen(((E_max-E_min)/E_inc)+1)*E_inc)+E_0
return, E_in_grid
end



;;;;;;;;;;;;;;;;;;;;;;;;;; create v array;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function create_v_arr,m_e, V_0_perp,V_f_perp, V_0_par, V_f_par, V_inc 
i = long(0)
j = long(0)
n_perp = long(abs(V_f_perp - V_0_perp)/V_inc)+1
n_par = long(abs(V_f_par - V_0_par)/V_inc)+1

seed = reform(dblarr(n_perp*n_par), [n_perp, n_par])
 V = seed
E = seed;
v_par = seed
v_perp = seed
dE = seed
while(i lt n_perp) do begin


 V_perp_I = double(V_0_perp+(i*V_inc))

if(V_perp_I lt 0.) then begin
E_perp = -0.5*m_e*(V_perp_I^2)
; print, v_perp
endif else begin
E_perp = 0.5*m_e*(V_perp_I^2)
; print, v_perp
endelse
; V_perp = V_0_perp+(V_inc_i)
; print, E_perp

while(j lt n_par) do begin
if(j eq 0) then begin
; V_par = V_0_par
; print, SQRT(2.*V_0_par/m_e)
 V_par_I = V_0_par
endif
  V_par_I = double(V_0_par+j*V_inc)

; print, V_par_I, V_perp_I, 0.5*m_e*((V_par_I^2)+(V_perp_I^2))

V[i,j] = SQRT((V_par_I^2)+(V_perp_I^2))
; E[i,j] = 0.5*m_e*((V_par_I^2)+(V_perp_I^2))
; dE[i,j] = 0.5*m_e*((V_par_I[i,j-1]-V_par_I)^2 + (V_perp_I^2))
v_perp[i,j] = V_perp_I
v_par[i,j] = V_par_I

j++
endwhile
j =0

i++

endwhile

V_perp_plot = double((findgen(n_perp)*((V_f_perp-V_0_perp)/(n_perp))) + (V_0_perp))
 V_par_plot = double(findgen(n_par)*((V_f_par-V_0_par)/(n_par)) + (V_0_par))
;  V_0_plot = dblarr(n_perp)+sqrt(2.*V_0/m_e)
; stop
pa_arr = acos(v_par/SQRT((v_perp^2)+(v_par^2)))*!radeg

out = {E:E, $
V:V, $
pa_arr:pa_arr, $
; dE:dE, $
v_perp:v_perp, $
v_par:v_par, $
V_perp_plot:V_perp_plot, $
V_par_plot:V_par_plot}
return, out
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function create_f_v,n_0, E_0,V_0, beta, v_perp, v_par

m_e = double(0.510998928e6/(2.9979e10^2)) ; defining the constant in eV
A_1 = n_0*((m_e/(2.*!pi*E_0))^1.5) 


; if((type eq 'B') and (beta eq 1.)) then begin
; v_par_b = (v_par^2) - ((2.*V_0)/m_e)
; E_b_exp = 0.5*m_e*((v_perp^2)+(v_par_b))
; tmp_f_e_b = A_1*exp(-E_b_exp/E_0))*v_par
; 
; ; logic that no flux at energies below the potential
; q = where(v_par_b lt SQRT((2.*V_0)/m_e))
; tmp_f_e_b[q] = 0.
; f_v = tmp_f_e_b
; endif


; now at the ionosphere
v_par_I = (v_par^2) - ((2.*V_0)/m_e) +(1.-(1./beta))*v_perp^2
v_perp_I = (v_perp^2)/beta
E_I_exp = 0.5*m_e*(v_perp_I+v_par_I)
tmp_f_e_I = A_1*exp(-E_I_exp/E_0)

tmp_J_v = ((v_par^2 + v_perp^2)/m_e)*A_1*exp(-E_I_exp/E_0)
q_I = where(v_par_I lt SQRT((2.*V_0)/m_e))


;;;;;;;; additions on 24 September 2012;;;;;;;;;;;;;;
; I want to calculate the energy grid and pitch angle grid



if (q_I[0] ne -1) then begin
tmp_f_e_I[q_I] = 0.
f_v = tmp_f_e_I

tmp_J_v[q_I] = 0.
j_v = tmp_J_v 

endif else begin
f_v = tmp_f_e_I
j_v = tmp_J_v
endelse
;  stop

out = {f_v:f_v, $
j_v:j_v}

return, out
end


function create_f_v_kappa,n_0, E_0,V_0, beta,kappa, v_perp, v_par

; equation 6.45, p 120 in baumjohann and trueman
W_0 = E_0(1.-(3./(2.*kappa)))
m_e = double(0.510998928e6/(2.9979e10^2)) ; defining the constant in eV
A_1 = n_0*((m_e/(2.*!pi*W_0*kappa))^1.5) 
 print, n_0,E_0,V_0,W_0, kappa

B_1 = double(gamma(kappa+1.)/gamma(kappa-0.5))
; print, B_1
; if((type eq 'B') and (beta eq 1.)) then begin
; v_par_b = (v_par^2) - ((2.*V_0)/m_e)
; E_b_exp = 0.5*m_e*((v_perp^2)+(v_par_b))
; tmp_f_e_b = A_1*exp(-E_b_exp/E_0))*v_par
; 
; ; logic that no flux at energies below the potential
; q = where(v_par_b lt SQRT((2.*V_0)/m_e))
; tmp_f_e_b[q] = 0.
; f_v = tmp_f_e_b
; endif


; now at the ionosphere
v_par_I = (v_par^2) - ((2.*V_0)/m_e) +(1.-(1./beta))*v_perp^2
v_perp_I = (v_perp^2)/beta
E_I_exp = 0.5*m_e*(v_perp_I+v_par_I)
tmp_f_e_I = A_1*B_1*((1.+(E_I_exp/(kappa*W_0)))^(-kappa-1.))
; print, tmp_f_e_I
tmp_J_v = ((v_par^2 + v_perp^2)/m_e)*A_1*B_1*((1.+(E_I_exp/(kappa*W_0)))^(-kappa-1.))
q_I = where(v_par_I lt SQRT((2.*V_0)/m_e))


;;;;;;;; additions on 24 September 2012;;;;;;;;;;;;;;
; I want to calculate the energy grid and pitch angle grid



if (q_I[0] ne -1) then begin
tmp_f_e_I[q_I] = 0.
f_v = tmp_f_e_I

tmp_J_v[q_I] = 0.
j_v = tmp_J_v 

endif else begin
f_v = tmp_f_e_I
j_v = tmp_J_v
endelse
;  stop

out = {f_v:f_v, $
j_v:j_v}

; stops
return, out
end

;;;;;;;;;;;;;;;;;;;;;;;;;; PA Slice Function;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this funct


function pa_slice_E,pa_select, pa, v_perp,v_par, J, m_e, v_perp_plot, v_par_plot,d_theta,E_grid;  E_max, E_min, dE, E_start
; stop
;  pa = acos(v_par/SQRT((v_perp^2)+(v_par^2)))*!radeg

if (pa_select eq 0.) then begin
pa_minus =0.
pa_plus = pa_select+d_theta
i = where( (pa ge pa_minus) and (pa le pa_plus) and (J ne 0.))
endif

if (pa_select ne 0.) then begin

pa_minus = float(pa_select-d_theta)
; if(pa_minus lt 0.) then begin
; pa_minus =0.
; endif

pa_plus = float(pa_select+d_theta)
; print, pa_minus, pa_plus
;(pa gt pa_minus) and
 i = where( (pa gt pa_minus) and (pa lt pa_plus) and (J ne 0.) and (v_perp ge 0.))

endif


if (pa_select eq 90.) then begin

pa_minus = float(pa_select-d_theta)
; if(pa_minus lt 0.) then begin
; pa_minus =0.
; endif

pa_plus = float(pa_select)
; print, pa_minus, pa_plus
;(pa gt pa_minus) and
 i = where( (pa gt pa_minus) and (pa lt pa_plus) and (J ne 0.) and (v_perp ge 0.))

endif


; print, n_elements(i)

; logic check, if we have no pa points in the selection, we have a problem
if(i[0] eq -1) then begin
print, 'no points match pa selection'
; return, 0
; stop
endif

; this is just a warning that there are a low number of points, which could make things tricky
if(n_elements(i) lt 5.) then begin
print, 'WARNING: less than 10 points in pitch angle selection'
; break
endif

; print, n_elements(i), n_elements(k)
E = (0.5*m_e*(v_par[i]^2))+(0.5*m_e*(v_perp[i]^2))
; print, E
J_select = J[i]
;  print,E, J_select
; a couple of checks
; finding the maximum energy out of the selected criterion
e_max = where(J_select eq max(J_select))
xr = [10,10000] ; consistent with evans, 1974
yr = [1e4,1e7] ; consistent with evans, 1974
title = 'J(# cm^-2 s ster eV)/E at PA '+strcompress(string(pa_select))




; n_levels = max(J)*(findgen(21.)*0.05)
; c_labels = fltarr(n_elements(n_levels))+1.
; cthick = c_labels
; cthick[4] = 5
;  title = var+' V_0: '+strcompress(string(V_0))+ ' Beta: '+strcompress(string(beta))
; stop


;  contour, J, v_perp_plot, v_par_plot, levels=n_levels,c_labels=c_labels,thick=t, /isotropic, xtitle = 'V_perp (cm/s)', ytitle='V_parallel (cm/s)'
; oplot, /polar, SQRT((2.*E)/m_e) ,fltarr(n_elements(E))+pa_select;, /xlog,/ylog, xrange=xr,yrange=yr, title = title, psym=4


; A=[1.,1.,1.]
; gauss = GAUSSFIT( E, J_select , A, nterms=3) 

;  E_grid = create_E_grid(E_max, E_min, dE, E_start)
; print, pa_select
; help, E, J_select, E_grid, /st

; this condition is due to a situation where such as falling directly through the potential drop
; there is not much flux above the edges of the distribtuion
; this condition makes it so that way the spline interpolation doesn't go crazy
; if the lowest energy in the selected energy is above the gridded energies, then we can skip it.
; i have checked and there is little flux anyway
cond0 = where((E_grid[n_elements(E_grid)-1] le E[0]) )
if(cond0 eq -1) then begin
 J_grid = fast_interpol(E, J_select, E_grid)
endif else begin
J_grid = fltarr(n_elements(E_grid))
endelse
; print, '------------------------------------'
;     stop


; plot, E, J_select, /xlog,/ylog, xrange=xr,yrange=yr, title = title, psym=4
;  oplot, E_grid, J_grid,color=230

; numflux_tot = int_tabulated(E, J_select)

; xyouts, 0.25,0.25, 'Peak E: '+strcompress(string(E[e_max])), /normal
; time_stamp
; xyouts, 0.25, 0.22, strcompress(string(J_select[e_max]/1.e6)), /normal
; xyouts, 0.25, 0.20, J_select[e_max_2]/1.e6, /normal
; xyouts, 0.25, 0.18, E[e_max_2], /normal

; print, int_tabulated(E, J_select)
;    stop
out = {J_select:J_select, $
E:E, $
E_grid:E_grid, $
 J_grid:J_grid}
return, out
end


; quick spline fitting program
function fast_interpol, E_in, J_in, E_grid
; I need to keep this on a grid of energies
; 

; strategy
; spline fit over defined energy ranges
; then fill the output vector for E_grid
i_first = where(abs(E_grid-E_in[0]) eq min(abs(E_grid-E_in[0])))
i_last = where(abs(E_grid-E_in[n_elements(E_in)-1]) eq min(abs(E_grid-E_in[n_elements(E_in)-1])))

E_spline = E_grid[i_first[0]:i_last[0]]

; saw that sometimes the E_in array is not sorted at the higher pitch angles
; so this will eliminate that problem
E_sort_in = E_in[UNIQ(E_in, SORT(E_in))]
J_sort_in = J_in[UNIQ(E_in, SORT(E_in))]
; stop

n = n_elements(E_grid)
j = long(0)

J_grid = fltarr(n)

cond0 = where(n_elements(J_sort_in) le 3.)
; print, 'condition 0', cond0[0]


 if(cond0[0] eq -1) then begin
; print, 'starting loop'

; ; if this condition produces 0, 1 it means that E_sort_in is out of the range of E_spline
; ; cond2 = n_elements(where(E_spline le E_sort_in))
; ; if(cond2 le 1) then begin
; ; print, 'Error'
; ; stop
; ; endif

tmp_J_spline = spline(E_sort_in, J_sort_in, E_spline)

; stop
cond1 = where( finite(tmp_J_spline, /nan) eq 1)
if(cond1[0] ne -1) then begin
print, 'ERROR:'
print, 'spline fitting issues in fast interpol'
stop
endif

; 
;  stop
; print, '--------------------------------------------'
; ; print, E_in[i_last[0]]
i = 0
while(j lt n) do begin

if((E_grid[j] lt E_spline[0]) or (E_grid[j] gt E_spline[n_elements(E_spline)-1]) ) then begin
J_grid[j] =0.
endif else begin
; print, j, E_grid[j], E_spline[i], tmp_J_spline[i]
J_grid[j] = tmp_J_spline[i]
i++
endelse
j++
endwhile

 endif

;  help, E_spline, J_grid, /st
; stop
return, J_grid
end


; quick spline fitting program
function fast_interpol, E_in, J_in, E_grid
; I need to keep this on a grid of energies
; 

; strategy
; spline fit over defined energy ranges
; then fill the output vector for E_grid
i_first = where(abs(E_grid-E_in[0]) eq min(abs(E_grid-E_in[0])))
i_last = where(abs(E_grid-E_in[n_elements(E_in)-1]) eq min(abs(E_grid-E_in[n_elements(E_in)-1])))

E_spline = E_grid[i_first[0]:i_last[0]]

; saw that sometimes the E_in array is not sorted at the higher pitch angles
; so this will eliminate that problem
E_sort_in = E_in[UNIQ(E_in, SORT(E_in))]
J_sort_in = J_in[UNIQ(E_in, SORT(E_in))]
; stop

n = n_elements(E_grid)
j = long(0)

J_grid = fltarr(n)

cond0 = where(n_elements(J_sort_in) le 3.)
; print, 'condition 0', cond0[0]


 if(cond0[0] eq -1) then begin
; print, 'starting loop'

; ; if this condition produces 0, 1 it means that E_sort_in is out of the range of E_spline
; ; cond2 = n_elements(where(E_spline le E_sort_in))
; ; if(cond2 le 1) then begin
; ; print, 'Error'
; ; stop
; ; endif

tmp_J_spline = spline(E_sort_in, J_sort_in, E_spline)

; stop
cond1 = where( finite(tmp_J_spline, /nan) eq 1)
if(cond1[0] ne -1) then begin
print, 'ERROR:'
print, 'spline fitting issues in fast interpol'
stop
endif

; 
;  stop
; print, '--------------------------------------------'
; ; print, E_in[i_last[0]]
i = 0
while(j lt n) do begin

if((E_grid[j] lt E_spline[0]) or (E_grid[j] gt E_spline[n_elements(E_spline)-1]) ) then begin
J_grid[j] =0.
endif else begin
; print, j, E_grid[j], E_spline[i], tmp_J_spline[i]
J_grid[j] = tmp_J_spline[i]
i++
endelse
j++
endwhile

 endif

;  help, E_spline, J_grid, /st
; stop
return, J_grid
end
