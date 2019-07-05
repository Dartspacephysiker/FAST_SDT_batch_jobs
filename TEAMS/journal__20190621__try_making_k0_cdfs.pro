;2019/06/21
;; Det siste jeg gjorde var å prøve å bruke makecdf2 make skeleton-fil som nevnes under.
;; Om du så bestemmer deg for å ta det opp igjen må du skrive ferdig en versjon av data-strukturen "data" under som er tilpasset TEAMS
;; data. Det har eg begynte.
;; Altså:
;;
;; UNFINISHED
;; Need to (i) finish writing data structure below; (ii) make sure it works with makecdf2
;;
;; WHY
;; Because the cdf files available through NASA CDAWeb don't include NH winter 1996/1997, so I've had to come up with an egen løsning
;;
PRO JOURNAL__20190621__TRY_MAKING_K0_CDFS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; fast_t_summary,BW=bw,k0=k0

;     Then to make the CDF file named 'fa_k0_ees.cdf'containing the variables 'unix_time',
;     'el_0', 'el_90', 'el_180', 'el_en', 'el_low', 'el_low_pa', 'el_high',
;     'el_high_pa', 'JEe', and 'Je', you could give the following IDL commands:
;
;         > get_data, 'el_0',     data=el_0
;         > get_data, 'el_90',    data=el_90
;         > get_data, 'el_180',   data=el_180
;         > get_data, 'el_low',   data=el_low
;         > get_data, 'el_high',  data=el_high
;         > get_data, 'JEe',      data=JEe
;         > get_data, 'Je',       data=Je
;         >
;         > data = {unix_time:  {name:'unix_time',  value:el_0.x,    recvary:1, fill:0}, $
;         >         el_0:       {name:'el_0',       value:el_0.y,    recvary:1, fill:1}, $
;         >         el_90:      {name:'el_90',      value:el_90.y,   recvary:1, fill:1}, $
;         >         el_180:     {name:'el_180',     value:el_180.y,  recvary:1, fill:1}, $
;         >         el_en:      {name:'el_en',      value:el_0.v,    recvary:1, fill:1}, $
;         >         el_low:     {name:'el_low',     value:el_low.y,  recvary:1, fill:1}, $
;         >         el_low_pa:  {name:'el_low_pa',  value:el_low.v,  recvary:1, fill:1}, $
;         >         el_high:    {name:'el_high',    value:el_high.y, recvary:1, fill:1}, $
;         >         el_high_pa: {name:'el_high_pa', value:el_high.v, recvary:1, fill:1}, $
;         >         JEe:        {name:'JEe',        value:JEe.y,     recvary:1, fill:1}, $
;         >         Je:         {name:'Je',         value:Je.y,      recvary:1, fill:1}}
;         >
;         > makecdf2, data, sktfile='fa_k0_ees_template', $
;               cdffile='fa_k0_ees', status=status, /overwrite
;         > if status ne 0 then begin
;         >     message, /info, 'makecdf2 failed.'
;         >     return
;         > endif

;; vars from cdfs
;; ['H+','He+','O+','H+_low','H+_high','He+_low',$
;; 'He+_high','O+_low','O+_high']

  make_tms_svy_cdf,SPECIES='o',/ALL, $
                   /RETURN_STRUCT,struct=structo

  ;; protons
  make_tms_svy_cdf,SPECIES='p',/ALL, $
                   /RETURN_STRUCT,struct=structh

  ;; helium
  make_tms_svy_cdf,SPECIES='h',/ALL, $
                   /RETURN_STRUCT,struct=structhe

  outDir = '/SPENCEdata/Research/database/FAST/TEAMS/'
  filenameo=STR(FORMAT='("orb",I0,"_teams_O.sav")',structo[0].orbit)
  filenameh=STR(FORMAT='("orb",I0,"_teams_H.sav")',structh[0].orbit)
  filenamehe=STR(FORMAT='("orb",I0,"_teams_He.sav")',structhe[0].orbit)

  print,"Saving it all for orbit "+STRING(FORMAT='(I0)',structo[0].orbit)+ ' ...'
  save,structo,filename=outDir+filenameo
  save,structh,filename=outDir+filenameh
  save,structhe,filename=outDir+filenamehe

END  

;; sktfile = '/SPENCEdata/software/sdt/Linux.2.6/lib/cdf_templates/fa_k0_tms_day_template.skt'
;; makecdf2,data,sktfile=sktfile


;; UTKAST AV 
;;

;; Needed days (orbits 1421-1503 for FAST outflow artikkel 201906)

;; tRanges = [['1996-12-30/00:00:00','1996-12-31/00:00:00'], $
;;            ['1996-12-31/00:00:00','1997-01-01/00:00:00'], $
;;            ['1997-01-01/00:00:00','1997-01-02/00:00:00'], $
;;            ['1997-01-02/00:00:00','1997-01-03/00:00:00'], $
;;            ['1997-01-03/00:00:00','1997-01-04/00:00:00'], $
;;            ['1997-01-04/00:00:00','1997-01-05/00:00:00'], $
;;            ['1997-01-05/00:00:00','1997-01-06/00:00:00'], $
;;            ['1997-01-06/00:00:00','1997-01-07/00:00:00'], $
;;            ['1997-01-07/00:00:00','1997-01-08/00:00:00']]



;;             var=['H+','He+','O+','H+_low','H+_high','He+_low','He+_high',$
;;                  'O+_low','O+_high']
;;             dvar=['H+_en','He+_en','O+_en','H+_low_pa','H+_high_pa',$
;;                   'He+_low_pa','He+_high_pa','O+_low_pa','O+_high_pa']

;; Hplus: {name:'H+', value:H+.Y, recvary:1, fill:1}, $
;; Heplus: {Name:'He+', Value:He+.Y, Recvary:1, Fill:1}, $
;; Oplus: {name:'O+', value:O+.Y, recvary:1, fill:1}, $
;; Hplus_low: {name:'H+_low', value:H+_low.y, recvary:1, fill:1}, $
;; Hplus_high: {name:'H+_high', value:H+_high.y, recvary:1, fill:1}, $
;; Heplus_low: {name:'He+_low', value:He+_low.y, recvary:1, fill:1}, $
;; Heplus_high: {name:'He+_high', value:He+_high.y, recvary:1, fill:1}, $
;; Oplus_low: {name:'O+_low', value:O+_low.y, recvary:1, fill:1}, $
;; Oplus_high: {name:'O+_high', value:O+_high.y, recvary:1, fill:1}, $


;; data = {unix_time: {

;; END

