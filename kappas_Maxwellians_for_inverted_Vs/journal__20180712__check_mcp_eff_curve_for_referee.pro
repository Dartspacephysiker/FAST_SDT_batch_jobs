;2018/07/12
PRO JOURNAL__20180712__CHECK_MCP_EFF_CURVE_FOR_REFEREE

  COMPILE_OPT IDL2,STRICTARRSUBS

  tid='1997-01-17/12:01:12'
  t1=S2T(tid)
  data=GET_FA_EES(t1,/CALIB)

  efStr=data
  CONVERT_ESA_UNITS2,efstr,"eflux"

  thunk=min(data.theta[0,*],ind)
  dats={energy:data.energy[*,ind], $
        counts:data.data[*,ind], $
        ef:efstr.data[*,ind], $
        efToCnt:efstr.data[*,ind]/data.data[*,ind]}

  dats.eftocnt[WHERE(~FINITE(dats.eftocnt),/NULL)] = !VALUES.F_NAN

  win = WINDOW(DIMENSIONS=[800,800])
  p1 = PLOT(dats.energy,dats.counts, $
            /XLOG, $
            XRANGE=[5,3E4], $
            XTITLE="Energy (eV)", $
            YTITLE="Counts", $
            /CURRENT, $
            LAYOUT=[1,3,1])
  p2 = PLOT(dats.energy,dats.ef, $
            /XLOG, $
            /YLOG, $
            XRANGE=[5,3E4], $
            XTITLE="Energy (eV)", $
            YTITLE="EFlux", $
            /CURRENT, $
            LAYOUT=[1,3,2])
  p3 = PLOT(dats.energy,dats.efToCnt/MAX(dats.efToCnt), $
            /XLOG, $
            XRANGE=[5,3E4], $
            XTITLE="Energy (eV)", $
            YTITLE="Ratio (Normalized to 1)", $
            /CURRENT, $
            LAYOUT=[1,3,3])

  ;; Want to make sure there's no dead-time strangeness happening

  dt = efStr.integ_t		; integration time (sec)	scalar or (nenergy,nbins)
  gf = efStr.geomfactor		; geometric factor of smallest bin, scaler (cm^2-sr)
  mass = efStr.mass		; scaler eV/(km/s)^2
  dead = .11e-6			; dead time, (sec) FAST AMPTEK A121
  geom = efStr.geom[*,ind]       ; relative geometric factor of bins (nenergy,nbins)
  scale = (gf * geom)

  tmp = scale * efstr.data[*,ind]

 ;Remove dead time correction
  tmp = round(dt*tmp/(1.+tmp*dead))


END
