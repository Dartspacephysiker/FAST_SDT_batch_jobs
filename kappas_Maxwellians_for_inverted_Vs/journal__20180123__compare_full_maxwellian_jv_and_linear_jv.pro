mRat          = 72
dens_m        = 1
T_m           = 110
pot           = 2024.

speedOfLight  = 299792458D                 ;m / s
inMass        = 5.1099891D5/speedOfLight^2 ;eV/c^2
eCharge       = DOUBLE(1.6D-19)            ;Coulombs

;;Convert input params
mRatS         = mRat
n             = DOUBLE(Dens_m * 1000000.D) ;dens_m in m^-3
potBar        = DOUBLE(pot/T_m)            ;potential normalized by temperature

;; Linearized
JVLin         = eCharge * n / SQRT(2.D * !DPI * inMass * T_m) * pot

JVinv         = (1.D) * eCharge * n
JV1           = SQRT( T_m / ( 2.D * !PI * inMass ) )
;; JV2sub     = (-1.D) * potBar / ( mRat - 1.D + helpMeNotBeZero )
JV2sub        = (-1.D) * potBar / ( mRatS - 1.D )
JV2           = mRat * (1.D - (1.D - 1.D/mRat) * EXP(JV2sub) )

Jpar          = JVinv * JV1 * JV2

PRINT,(jvlin-jpar)/jpar