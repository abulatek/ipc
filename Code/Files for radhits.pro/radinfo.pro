PRO radinfo, det, energy, bias, picnum, path=path, data=data, eperV=eperV, $
	itime=itime, outstr=outstr, darkmap=darkmap, mask=mask, itarr=itarr

IF N_PARAMS() eq 0 AND ~keyword_set(more) THEN BEGIN
	print, "SYNTAX: "
	print, "	RADINFO, det, energy, bias, picnum, [...]
	return
ENDIF

;+
; Name:
;	RADINFO
;
; Purpose:
;	Look up table to return path to low dose radiation data.
;
; Inputs:
;	det - which detector? 346 or 354
;	energy - which energy? 63, 32, or 12 MeV
;	bias - which applied bias? 150 or 250mV
;	picnum - 'picture number' which sample up the ramp ie. 1-10 of f-8 or 
;		1-2 of f-32
;
; Optional Outputs:
;	path=path - path to data
;	data=data - data (I am always removing the first frame because it has 
;		little physical meaning.)
;	eperV=eperV - conversion factor
;	itime=itime - integration time between fowler reads. Be careful if 
;		there is a frame delay between sig and ped. 
;	itarr=itarr - integration time array ASSUMING NO FRAME DELAY! ALSO,
;		this is removing the first frame!!!
;	darkmap=darkmap - dark current map BEFORE radiation
;	mask=mask - mask from dark and well before radiation (sigmaclipped)
;	outstr=outstr - where to save data. also where to find map and mask
;-

; Some strings
detinfo, det, detname=detname
IF det EQ 354 OR det EQ 346 THEN BEGIN
	;mediastr = '~/io/'+detname+'/nasaames_radtest/'
	mediastr = '/media/data/'+detname+'/nasaames_radtest/'
ENDIF  
IF det EQ 86 THEN BEGIN
	;mediastr = '/media/data/H1RG-16886-rad/2015_SEP_22_rad/'
	mediastr = '/home/abulatek/Nix/H1RG-16886-rad/2015_SEP_22_rad/'
ENDIF

outloc = !H1RG+'radiation_testing/'+detname+'/'

CASE bias OF

250: BEGIN
CASE det OF

	; 48 micron substrate device data path
	354: BEGIN
		eperV = 30256.7
		CASE energy OF
			63: BEGIN
				path = mediastr+'firstrun/rad/Beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 11, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'firstrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			32: BEGIN
				path = mediastr+'secondrun/2014_oct_02_davis_beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 22, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			12: BEGIN
				path = mediastr+'secondrun/2014_oct_02_davis_beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 5, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
		ENDCASE	
		END
	
	; 0 micron substrate device data path
	346: BEGIN
		eperV = 37274.771 ; stripe III
		CASE energy OF
			63: BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-64MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 4, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			32: BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-32MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 4, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			12:BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-12MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 8, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
		ENDCASE
		END

	; Full on substrate (800 micron) data path
	86: BEGIN
		eperV = 37105.6
		CASE energy OF
			18: BEGIN
				path = mediastr
				; 2 x F-64
				;fowler_number = 64
				;readinames, path, 35, 11, picnum, 1, 1, fowler_number, data
				; 4 x F-8 (saturates by end)
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 20, picnum, 1, 1, fowler_number, data
				outstr = outloc 
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			34: BEGIN
				path = mediastr
				; 2 x F-64
				;fowler_number = 64
				;readinames, path, 35, 26, picnum, 1, 1, fowler_number, data
				; 4 x F-8 (saturates by end)
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 34, picnum, 1, 1, fowler_number, data
				outstr = outloc
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			0: BEGIN
				path = '/media/data/H1RG-16886-rad/2015_SEP_22_darks/'
				fowler_number = 64
				IF arg_present(data) THEN $
				readinames, path, 35, 2, picnum, 1, 1, fowler_number, data
				outstr = outloc
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
		ENDCASE
		END
	
ENDCASE
END

150: BEGIN
CASE det OF

	; 48 micron substrate device data path
	354: BEGIN
		eperV = 42339.9
		CASE energy OF
			63: BEGIN
				path = mediastr+'firstrun/rad/Beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 14, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'firstrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
			32: BEGIN
				path = mediastr+'secondrun/2014_oct_02_davis_beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 18, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
			12: BEGIN
				path = mediastr+'secondrun/2014_oct_02_davis_beam/'
				fowler_number = 32
				IF arg_present(data) THEN $
				readinames, path, 35, 6, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
		ENDCASE	
		END
	
	; 0 micron substrate device data path
	346: BEGIN
		eperV = 38520.303 ; stripe III
		CASE energy OF
			63: BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-64MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 2, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
			32: BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-32MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 6, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
			12:BEGIN
				path = mediastr+'2014_Sep_04_Rad/Beam-12MeV/'
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 10, picnum, 1, 1, fowler_number, data
				outstr = outloc + 'secondrun/'
				darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent)
				END
		ENDCASE
		END

	; Full on substrate (800 micron) data path
	86: BEGIN
		eperV = 36968.6
		CASE energy OF
			18: BEGIN
				path = mediastr
				; 2 x F-64
				;fowler_number = 64
				;readinames, path, 35, 10, picnum, 1, 1, fowler_number, data
				; 4 x F-8 (saturates by end)
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 19, picnum, 1, 1, fowler_number, data
				outstr = outloc 
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
			34: BEGIN
				path = mediastr
				; 2 x F-64
				;fowler_number = 64
				;readinames, path, 35, 25, picnum, 1, 1, fowler_number, data
				; 4 x F-8 (saturates by end)
				fowler_number = 8
				IF arg_present(data) THEN $
				readinames, path, 35, 33, picnum, 1, 1, fowler_number, data
				outstr = outloc
				darkmap = readfits(outstr+'35K_250mV_darkbefore.fits',/silent)
				wellmap = readfits(outstr+'35K_250mV_wellbefore.fits',/silent)
				END
		ENDCASE
		END
	
ENDCASE
END
ENDCASE

IF arg_present(data) THEN BEGIN
	; Get dimensions of data
	sz = size(data, /dimensions)

	; Remove first frame
	IF sz[0] NE 0 THEN data = data[*,*,1:sz[2]-1] 
ENDIF

; itegration time
itime = 5.278 ;sec

; integration time array
itarr = (findgen(fowler_number*2.-1)+1)*itime

; Get mask
sigmaclip, darkmap, wellmap, mask

END
