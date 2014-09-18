; IDL Version 8.3 (linux x86_64 m64)
; Journal File for spencerh@Merry2
; Working directory: /SPENCEdata/software/sdt/batch_jobs/playground1
; Date: Tue Sep 16 09:02:57 2014

;I'M USING ORBIT 20000
 
@startup
;ON_ERROR set to 1...
;    16777216 colors available.

field=get_fa_fields('MagDC',/all,/store)

print,field
;MagDCcomp1 MagDCcomp2 MagDCcomp3

get_data,'MagDCcomp3',data=magz
store_data,'MagZ',data=magz
store_data,'Magz_smooth',data={x:magz.x,y:magz.y}

efieldV1214=get_fa_fields('V1-V2_S',/all)
efieldV58=get_fa_fields('V5-V8_S',/all)

FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/interp,delt_t=50.,/talk
;npts1 =        80256npts2 =        80256matched =        80256

efield={time:efieldV1214.time,comp1:sqrt(efieldV1214.comp1^2+efields_combine^2)}
magz={time:magz.x,comp1:magz.y,ncomp:1}
help,efield,/str

FA_FIELDS_COMBINE,magz,efield,result=efield_aligned,/interp,delt_t=50.,/talk
;npts1 =        20028npts2 =        80256matched =        20028

plot,efield_aligned
print,efield_aligned(0:100)
;      352.953      349.236      336.532      337.687      346.822      346.790      352.008      363.573      402.664
;      478.002      580.649      646.394      669.454      671.932      658.875      592.902      487.240      413.179
;...Until dah yeah
print,where(FINITE(efield_aligned) ne 0)
;           0           1           2           3           4           5           6           7           8           9
;          10          11          12          13          14          15          16          17          18          19
;............until element 20027!
