function loss_cone_width, altitude
;this pro given an altitude in m returns the half angle of the loss cone
;assuming a width of 90 deg at 100km
ratio=dipolefield(100.0*1000.0)/dipolefield(altitude)
halfwidth=atan(sqrt(1/(ratio-1)))
return,halfwidth
end