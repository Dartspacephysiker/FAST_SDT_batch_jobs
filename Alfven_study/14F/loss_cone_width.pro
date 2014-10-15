FUNCTION loss_cone_width, alt, ilat
  

;This pro calculates the loss cone width, assuming 
;1) a dipole magnetic field geometry, 
;2) invariance of magnetic moment, mu = m + v_perp^2 / ( B * 2 ),
;3) particles are confined to a given field line, i.e., r = L * cos^2 (lambda)
;4) These particles have a mirror point at the surface of the earth

;B(m, r, lambda) = mu_0 * m / ( 4 * PI * (alt + R_E )^3 ) * $
; SQRT( 1 + 3 * sin^2 (lambda ) )

;B_equator = B(m, r, lambda = 0) = mu_0 * m / ( 4 * PI * (alt + R_E)^3 )
;
;

  R_E = 6371000.                ;mean earth radius in meters

  R = R_E + alt
  
  print, "R: " + str(R)

  guts= ( 1 + 3 * ( SIN(ACOS(SQRT(R * (COS(ilat * !DPI / 180))^2 ) ) ) )^2 ) / (1 + 3 * ( SIN(ilat* !DPI / 180) )^2 )

  print, 'guts: ' + str(guts)
;  sin_sq_alpha = SQRT( (1 + 3 * ( SIN(ACOS(SQRT(R * (COS(ilat * !DPI / 180))^2 ) ) ) )^2 ) / (1 + 3 * ( SIN(ilat* !DPI / 180) )^2 ) ) / R^3
  sin_sq_alpha = SQRT(guts) / R^3

  print, 'sin_sq_alpha: ' + str(sin_sq_alpha)

  guts2=SQRT(sin_sq_alpha)

  print, "guts2: " + str(guts2)
;  alpha = ASIN( SQRT(sin_sq_alpha) )
  alpha = ASIN(guts2)

  RETURN, alpha

END
