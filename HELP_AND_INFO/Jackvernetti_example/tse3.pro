;For creating L1 CDFs.

pro tse3

QTY_NAME = string('Sesa 1 Burst')
; QTY_NAME = string('Sesa Survey')

; diags
diag_flg=1
dnt=-1
; end diags

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, 'tse3:  QTY_NAME= ', QTY_NAME
endif else begin
endelse
; end diags

stime= 0.D
etime= 0.D

stime1= 0.D
etime1= 0.D

I1 = 0L
LIDX = 0L

; Get the time stamp before we start so that we can see
; how long the extraction took for all 7 Sesa qtys.
stime = systime(1)

arr_dsc1 = 0L
arr_dsc2 = 0L

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory before get_fast_esa_mdata_bundled:'
endif else begin
endelse

help,/mem

; Get "QTY_NAME" for the entire orbit:
stime1 = systime(1)
dat=get_fast_esa_mdata_bundled(QTY_NAME, ADESC1=arr_dsc1, $
	ADESC2=arr_dsc2, ALL=all)
etime1 = systime(1)

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory after get_fast_esa_mdata_bundled:'
endif else begin
endelse

help,/mem

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'after get_fast_esa_mdata_bundled():'
   flush, dnt
   printf, dnt, format='("   get_fast_bnd:  EndTime=   ", F17.6)', etime1
   flush, dnt
   printf, dnt, format='("   get_fast_bnd:  StartTime= ", F17.6)', stime1
   flush, dnt
   printf, dnt, ' '
   flush, dnt

   MCNT = 0L

   if (N_Elements(arr_dsc1) GT 0) then begin
      LIDX = long(N_Elements(arr_dsc1)) - 1L

      printf, dnt, 'START PRINT arr_dsc1:   NElts= ', LIDX + 1
      flush, dnt

      for J1 = 0L, LIDX do begin
	 printf, dnt, ' '
	 flush, dnt
	 printf, dnt, format='("   arr_dsc1[", I6, "]:")', J1
	 flush, dnt
	 printf, dnt, format='("      nbins= ", I4, "   nsubdims= ", I2)', $
	     arr_dsc1[J1].nbins, arr_dsc1[J1].nsubdims
         printf, dnt, '      delta,midp:'
	 flush, dnt
         KIDX = long(arr_dsc1[J1].nbins * arr_dsc1[J1].nsubdims) - 1L
         for J2 = 0L, KIDX do begin
	     printf, dnt, $
	        format='("        [", I4, "]:   ", F16.5, "  ", F16.5)', $
	        J2, arr_dsc1[J1].delta[J2], arr_dsc1[J1].midp[J2]
	     flush, dnt
         endfor

;  Check that this description is unique:
	 if (J1 GT 0) then begin
	     J3 = 0L
	     J4 = long(J1 - 1L)
	     ACNT = 0L
	     for J3 = 0, J4 do begin
	         if (arr_dsc1[J3].nbins EQ arr_dsc1[J1].nbins) then begin
	             J5 = 0L
	             J6 = long(arr_dsc1[J1].nbins - 1L)
		     BCNT = 0L
	             for J5 = 0, J6 do begin
		         MN1 = double(arr_dsc1[J1].midp[J5])
		         MN2 = double(arr_dsc1[J3].midp[J5])
		         MX1 = double(arr_dsc1[J1].delta[J5])
		         MX2 = double(arr_dsc1[J3].delta[J5])
			 if ((abs(MN1 - MN2) LT 0.00001) AND $
			     (abs(MX1 - MX2) LT 0.00001)) then begin
			     BCNT = BCNT + 1L
                         endif else begin
                         endelse
	             endfor
		     if (BCNT GT 0L) then begin
		         ACNT = ACNT + 1L
	                 printf, dnt, $
    format='("      REPEAT:  J1= ", I5, "  J3= ", I5, "  BCNT= ", I5)', $
			    J1, J3, BCNT
	                 flush, dnt
                     endif else begin
                     endelse
                 endif else begin
                 endelse
	     endfor

             ; If ACNT is GT 0, then we found this is a repeat.
	     if (ACNT GT 0L) then begin
	         MCNT = MCNT + 1L
	         printf, dnt, '      REPEAT:  MCNT= ', MCNT
	         flush, dnt
             endif else begin
	         printf, dnt, '      UNIQUE'
	         flush, dnt
             endelse

         endif else begin
	     printf, dnt, '      UNIQUE'
	     flush, dnt
         endelse

      endfor

      printf, dnt, '    Number of repeats:  ', MCNT
      flush, dnt

   endif else begin
   endelse

   printf, dnt, 'END PRINT arr_dsc1:'
   printf, dnt, ' '

   MCNT = 0L

   if (N_Elements(arr_dsc2) GT 0) then begin
      LIDX = long(N_Elements(arr_dsc2)) - 1L

      printf, dnt, 'START PRINT arr_dsc2:   NElts= ', LIDX + 1
      flush, dnt

      for J1 = 0L, LIDX do begin
	 printf, dnt, ' '
	 flush, dnt
	 printf, dnt, format='("   arr_dsc2[", I6, "]:")', J1
	 flush, dnt
	 printf, dnt, format='("      nbins= ", I4, "   nsubdims= ", I2)', $
	     arr_dsc2[J1].nbins, arr_dsc2[J1].nsubdims
         printf, dnt, '      delta,midp:'
	 flush, dnt
         KIDX = long(arr_dsc2[J1].nbins * arr_dsc2[J1].nsubdims) - 1L
         for J2 = 0L, KIDX do begin
	     printf, dnt, $
	        format='("        [", I4, "]:   ", F16.5, "  ", F16.5)', $
	        J2, arr_dsc2[J1].delta[J2], arr_dsc2[J1].midp[J2]
	     flush, dnt
         endfor

;  Check that this description is unique:
	 if (J1 GT 0) then begin
	     J3 = 0L
	     J4 = long(J1 - 1L)
	     ACNT = 0L
	     for J3 = 0, J4 do begin
	         if (arr_dsc2[J3].nbins EQ arr_dsc2[J1].nbins) then begin
	             J5 = 0L
	             J6 = long(arr_dsc2[J1].nbins - 1L)
		     BCNT = 0L
	             for J5 = 0, J6 do begin
		         MN1 = double(arr_dsc2[J1].midp[J5])
		         MN2 = double(arr_dsc2[J3].midp[J5])
		         MX1 = double(arr_dsc2[J1].delta[J5])
		         MX2 = double(arr_dsc2[J3].delta[J5])
			 if ((abs(MN1 - MN2) LT 0.00001) AND $
			     (abs(MX1 - MX2) LT 0.00001)) then begin
			     BCNT = BCNT + 1L
                         endif else begin
                         endelse
	             endfor
		     if (BCNT GT 0L) then begin
		         ACNT = ACNT + 1L
	                 printf, dnt, $
    format='("      REPEAT:  J1= ", I5, "  J3= ", I5, "  BCNT= ", I5)', $
			    J1, J3, BCNT
	                 flush, dnt
                     endif else begin
                     endelse
                 endif else begin
                 endelse
	     endfor

             ; If ACNT is GT 0, then we found this is a repeat.
	     if (ACNT GT 0L) then begin
	         MCNT = MCNT + 1L
	         printf, dnt, '      REPEAT:  MCNT= ', MCNT
	         flush, dnt
             endif else begin
	         printf, dnt, '      UNIQUE'
	         flush, dnt
             endelse

         endif else begin
	     printf, dnt, '      UNIQUE'
	     flush, dnt
         endelse

      endfor

      printf, dnt, '    Number of repeats:  ', MCNT
      flush, dnt

   endif else begin
   endelse

   printf, dnt, 'END PRINT arr_dsc2:'
   printf, dnt, ' '

endif else begin
endelse
; end diags

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, format='(A, ":")', QTY_NAME
   printf, dnt, '   Number Array elements:  ', N_Elements(dat)
   flush, dnt
   old_nenergy = 0
   old_nbins = 0

   bad_compares = 0L
   good_compares = 0L

   if (N_Elements(dat) GT 1) then begin
      LIDX = long(N_Elements(dat)) - 1L
      for I1 = 0L, LIDX do begin
	 printf, dnt, format='("   dat[", I6, "]:  T=", F17.6)', I1, $
	     dat(I1).time
	 flush, dnt
         printf, dnt, '      integ_t= ', dat(I1).integ_t
	 flush, dnt
	 printf, dnt, format='("      end_time= ", F17.6)', $
	     dat(I1).end_time
         printf, dnt, '      index= ', dat(I1).index
	 flush, dnt
         printf, dnt, '      adesc_idx1= ', dat(I1).adesc_idx1
	 flush, dnt
         printf, dnt, '      adesc_idx2= ', dat(I1).adesc_idx2
	 flush, dnt
         printf, dnt, '      nenergy= ', dat(I1).nenergy
         printf, dnt, '      nenergy= ', dat(I1).nenergy
	 flush, dnt
         if (dat(I1).nenergy EQ old_nenergy) then begin
             printf, dnt, '        no change energies'
	     flush, dnt
         endif else begin
             printf, dnt, '        CHANGE energies'
	     flush, dnt
         endelse
         if (dat(I1).nbins EQ old_nbins) then begin
             printf, dnt, '        no change angles'
	     flush, dnt
         endif else begin
             printf, dnt, '        CHANGE angles'
	     flush, dnt
         endelse
         printf, dnt, '      nbins= ', dat(I1).nbins
	 flush, dnt
         printf, dnt, '      mass= ', dat(I1).mass
	 flush, dnt
         printf, dnt, '      geomfactor= ', dat(I1).geomfactor
	 flush, dnt
         printf, dnt, '      eff(0)= ', dat(I1).eff(0)
	 flush, dnt
         printf, dnt, '      header_bytes:'
	 flush, dnt
	 printf, dnt, format='(10(" ",I4))', dat[I1].header_bytes
	 flush, dnt
         printf, dnt, '      spin_phase= ', dat(I1).spin_phase
	 flush, dnt
         printf, dnt, '      spin_num= ', dat(I1).spin_num
	 flush, dnt
         printf, dnt, '      spin_ph_hdr= ', dat(I1).spin_ph_hdr
	 flush, dnt
         printf, dnt, '      sweep_num= ', dat(I1).sweep_num
	 flush, dnt
         printf, dnt, '      swps_per_set= ', dat(I1).swps_per_set
	 flush, dnt

	 ;
	 ; Compare array descriptions for "adesc_idx1", "adesc_idx2",
	 ; with those in nenergy, nangle.
	 ;
	 diff_flg = 0
	 vdiff = 0.0
	 wdiff = 0.0
         EIDX = long(dat[I1].nenergy) - 1L
	 DIDX = dat[I1].adesc_idx1

	 if (arr_dsc1[DIDX].nbins NE dat[I1].nenergy) then begin
             printf, dnt, '      DIFF ERROR  size:  nenergy not the same'
	     flush, dnt
             printf, dnt, '       I1= ', I1
	     flush, dnt
             printf, dnt, '       dat[I1].nenergy= ', dat[I1].nenergy
	     flush, dnt
             printf, dnt, '       data[I1].adesc_idx1== DIDX= ', DIDX
	     flush, dnt
             printf, dnt, '       arr_dsc[DIDX].nbins= ', $
	         arr_dsc1[DIDX].nbins
	     flush, dnt
	     diff_flg = 1
         endif else begin
         endelse

         for E1 = 0L, EIDX do begin
	     vdiff = dat[I1].energy[E1,0] - arr_dsc1[DIDX].midp[E1]
	     if (abs(vdiff) GE 0.0001) then begin
                 printf, dnt, '      DIFF ERROR  energy not the same'
	         flush, dnt
                 printf, dnt, '       I1= ', I1
	         flush, dnt
                 printf, dnt, '       E1= ', E1
	         flush, dnt
	         printf, dnt, format='("       vdiff= ", F17.8)', $
		     vdiff
	         flush, dnt
	         printf, dnt, $
		     format='("       dat[I1].energy[E1,0]= ", F17.8)', $
		     dat[I1].energy[E1,0]
	         flush, dnt
	         printf, dnt, $
		     format='("       arr_dsc1[DIDX].midp[E1]= ", F17.8)', $
		     arr_dsc1[DIDX].midp[E1]
	         flush, dnt
	         diff_flg = 1
             endif else begin
             endelse

	     wdiff = dat[I1].denergy[E1,0] - arr_dsc1[DIDX].delta[E1]
	     if (abs(wdiff) GE 0.0001) then begin
                 printf, dnt, '      DIFF ERROR  denergy not the same'
	         flush, dnt
                 printf, dnt, '       I1= ', I1
	         flush, dnt
                 printf, dnt, '       E1= ', E1
	         flush, dnt
                 printf, dnt, '       dat[I1].adesc_idx1= DIDX= ', DIDX
	         flush, dnt
	         printf, dnt, format='("       wdiff= ", F17.8)', $
		     wdiff
	         flush, dnt
	         printf, dnt, $
		     format='("       dat[I1].denergy[E1,0]= ", F17.8)', $
		     dat[I1].denergy[E1,0]
	         flush, dnt
	         printf, dnt, $
		     format='("       arr_dsc1[DIDX].delta[E1]= ", F17.8)', $
		     arr_dsc1[DIDX].delta[E1]
	         flush, dnt
	         diff_flg = 1
             endif else begin
             endelse
	 endfor

	 vdiff = 0.0
	 wdiff = 0.0
         EIDX = long(dat[I1].nbins) - 1L
	 DIDX = dat[I1].adesc_idx2

	 if (arr_dsc2[DIDX].nbins NE dat[I1].nbins) then begin
             printf, dnt, '      DIFF ERROR  size:  nangles not the same'
	     flush, dnt
             printf, dnt, '       I1= ', I1
	     flush, dnt
             printf, dnt, '       dat[I1].nbins= ', dat[I1].nbins
	     flush, dnt
             printf, dnt, '       data[I1].adesc_idx2== DIDX= ', DIDX
	     flush, dnt
             printf, dnt, '       arr_dsc[DIDX].nbins= ', $
	         arr_dsc1[DIDX].nbins
	     flush, dnt
	     diff_flg = 1
         endif else begin
         endelse

         for E1 = 0L, EIDX do begin
	     vdiff = dat[I1].theta[0,E1] - arr_dsc2[DIDX].midp[E1]
	     if (abs(vdiff) GE 0.0001) then begin
                 printf, dnt, '      DIFF ERROR  theta not the same'
	         flush, dnt
                 printf, dnt, '       I1= ', I1
	         flush, dnt
                 printf, dnt, '       E1= ', E1
	         flush, dnt
                 printf, dnt, '       dat[I1].adesc_idx2= DIDX= ', DIDX
	         flush, dnt
	         printf, dnt, format='("       vdiff= ", F17.8)', $
		     vdiff
	         flush, dnt
	         printf, dnt, $
		     format='("       dat[I1].theta[0,E1]= ", F17.8)', $
		     dat[I1].theta[0,E1]
	         flush, dnt
	         printf, dnt, $
		     format='("       arr_dsc2[DIDX].midp[E1]= ", F17.8)', $
		     arr_dsc2[DIDX].midp[E1]
	         flush, dnt
	         diff_flg = 1
             endif else begin
             endelse

	     wdiff = dat[I1].dtheta[E1] - arr_dsc2[DIDX].delta[E1]
	     if (abs(wdiff) GE 0.0001) then begin
                 printf, dnt, '      DIFF ERROR  dtheta not the same'
	         flush, dnt
                 printf, dnt, '       I1= ', I1
	         flush, dnt
                 printf, dnt, '       E1= ', E1
	         flush, dnt
                 printf, dnt, '       dat[I1].adesc_idx2= DIDX= ', DIDX
	         flush, dnt
	         printf, dnt, format='("       wdiff= ", F17.8)', $
		     wdiff
	         flush, dnt
	         printf, dnt, $
		     format='("       dat[I1].dtheta[E1]= ", F17.8)', $
		     dat[I1].dtheta[E1]
	         flush, dnt
	         printf, dnt, $
		     format='("       arr_dsc2[DIDX].delta[E1]= ", F17.8)', $
		     arr_dsc2[DIDX].delta[E1]
	         flush, dnt
	         diff_flg = 1
             endif else begin
             endelse
	 endfor

	 if (diff_flg GT 0) then begin
	     bad_compares = bad_compares + 1L
         endif else begin
	     good_compares = good_compares + 1L
         endelse

	 old_nenergy = dat(I1).nenergy
	 old_nbins = dat(I1).nbins
      endfor

   printf, dnt, ' '
   printf, dnt, 'dat arrays with equal energy,theta compares= ', $
       good_compares
   flush, dnt
   printf, dnt, ' '
   printf, dnt, 'dat arrays with different energy,theta compares= ', $
       bad_compares
   flush, dnt

   endif else begin
   endelse
endif else begin
endelse
; end diags

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory before delete dat:'
endif else begin
endelse

help,/mem

dat=0L

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory before delete arr_dsc1:'
endif else begin
endelse

help,/mem

arr_dsc1=0L

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory before delete arr_dsc2:'
endif else begin
endelse

help,/mem

arr_dsc2=0L

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, 'memory at exit:'
endif else begin
endelse

help,/mem

etime = systime(1)

; diags
if (diag_flg EQ 1) then begin
   printf, dnt, ' '
   printf, dnt, format='("EndTime=   ", F17.6)', etime
   printf, dnt, format='("StartTime= ", F17.6)', stime
endif else begin
endelse
; end diags

return
end
