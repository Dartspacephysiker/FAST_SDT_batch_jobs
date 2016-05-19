;Start the show by turning on SDT and loading chaston_survey as the config
CD, '/home/spencerh/Desktop'
@startup

field=get_fa_fields('MagDC',/all,/store)

print,field
;MagDCcomp1 MagDCcomp2 MagDCcomp3

get_data,'MagDCcomp3',data=magz
; Compiled module: NDIMEN.

store_data,'MagZ',data=magz
store_data,'Magz_smooth',data={x:magz.x,y:magz.y}

efieldV1214=get_fa_fields('V1-V2_S',/all)
efieldV58=get_fa_fields('V5-V8_S',/all)
;Now combine these guys
FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine

efield={time:efieldV1214.time,comp1:sqrt(efieldV1214.comp1^2+efields_combine^2)}

;Set up structures so that FA_FIELDS_COMBINE knows what to do with them
magz={time:magz.x,comp1:magz.y,ncomp:1}

help,efield,/str
;** Structure <1405cb38>, 2 tags, length=540672, data length=540672, refs=1:
;   TIME            DOUBLE    Array[45056]
;   COMP1           FLOAT     Array[45056]

FA_FIELDS_COMBINE,magz,efield,result=efield_aligned,/interp,delt_t=50.,/talk
;npts1 =       189672npts2 =        45056matched =            0

plot,efield_aligned
; PLOT: Not enough valid and unique points specified.
; Error occurred at: $MAIN$          
; Execution halted at: $MAIN$          

print,efield_aligned(0:100)
;NaN city

print,where(FINITE(efield_aligned) ne 0)
;          -1
 
