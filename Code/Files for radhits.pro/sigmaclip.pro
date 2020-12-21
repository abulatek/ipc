PRO sigmaclip, darkcurrentmap, welldepthmap, mask,$
	maxiter=maxiter, clipsig=clipsig


;+
; NAME:
;	SIGMACLIP
;
; PURPOSE:
;	Sigma clip high dark current pixels in order to create a mask. 
;
; Inputs:
;	darkcurrentmap - dark current map
;	welldepthmap - well depth map
;
; Outputs:
;	dark - dark current map with flagged pixels
;	well - well depth map with flagged pixels
;	mask - mask of 1s and NANs for good and flagged pixels
;
; Keywords:
;	maxiter=maxiter - number of sigma clip iterations
;	clipsig=clipsig - 
;
;-

; Masks (don't over write in memory)
dark = darkcurrentmap
well = welldepthmap

; Set reference pixels to NAN
dark[0:3,*] = !values.f_nan
dark[1020:1023,*] = !values.f_nan
dark[*,0:3] = !values.f_nan
dark[*,1020:1023] = !values.f_nan

well[0:3,*] = !values.f_nan
well[1020:1023,*] = !values.f_nan
well[*,0:3] = !values.f_nan
well[*,1020:1023] = !values.f_nan

; Set default values
IF n_elements(maxiter) LT 1 THEN maxiter = 5
IF n_elements(clipsig) LT 1 THEN clipsig = 3

; Iterate
subsdark = where(finite(dark),darkct)
subswell = where(finite(well),wellct)
iter=0
REPEAT BEGIN
    darkpix = dark[subsdark]
    wellpix = well[subswell]
    iter = iter + 1
    
    ; median
    medvdark = median(darkpix)
    medvwell = median(wellpix)
    
    ; stdev
    momdark = moment(darkpix,max=2,double=double)
    sigdark = sqrt(momdark[1])
    momwell = moment(wellpix,max=2,double=double)
    sigwell = sqrt(momwell[1])  
    
    ; Find indidces where not outliers
    wsmd = where(abs(darkpix-medvdark) LT clipsig*sigdark,darkct)
    wsmw = where(abs(wellpix-medvwell) LT clipsig*sigwell,wellct)

    IF darkct GT 0 THEN subsdark = subsdark[wsmd] 
    IF wellct GT 0 THEN subswell = subswell[wsmw] 
ENDREP UNTIL (iter GT maxiter)

; Find the intersection of both index arrays of good points
values = intersect(subsdark, subswell)

; Create mask
mask = fltarr(1024,1024)

; Good pixels = 1
mask[values] = 1

; Where not a good pixel (basically not a 1, so a 0) set to NAN
nan_values = where(mask EQ 0, count)
mask[nan_values] = !values.f_nan

END
