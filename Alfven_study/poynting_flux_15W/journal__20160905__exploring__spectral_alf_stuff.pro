;09/05/16
;I'm using notes from a telecon with Chris and Jim as a guide
;i. Identify orbits (easy—the big Poynting flux orbits)
;-->    Orbits to check out: 6127, 9859, 12539
;ii. Restrict to periods with 128 S/s
;iii. Fourier transform/Hanning window for E and B-field data
;	1. Fourier details
;		a. Decide length of Fourier transform
;		b. High frequency set by roll-off of fluxgate mag.
;		c. 2pi lambda_e sets low-frequency end
;			i. Use density model already in place for calculating omega_p
;	2. E component: “E_along_V”
;	3. B components: all
;		a. Use Strangeway despinning, find coordinate system that complements E_along_V
;		b. Need to use Bob model to subtract the background field
;iv. Calculate Poynting flux
;v. Screenings
;	1. Keep all data, but let screenings happen on the fly
;		a. Change in B (instrument threshold)
;		b. Change in E (instrument threshold)
;		c. E-over-B ratio
;	2. Two ideas from Chris. Could use:
;		a. Requirement that Pflux be > 1 mW/m2, corresponding to visible aurora
;		b. Requirement that E and B be above noise level (“but maybe it’s all noise!”)

PRO JOURNAL__20160905__EXPLORING__SPECTRAL_ALF_STUFF

  COMPILE_OPT IDL2

END
