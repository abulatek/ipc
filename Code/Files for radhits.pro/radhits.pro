PRO radhits, det, energy, bias, nramps, x, hist, histerr, bkgmode, bkgmed, $
	bkg=bkg, npixhit=npixhit, totalenergyhit=totalenergyhit, $
	filename=filename, cutoffval=cutoffval, maxsig=maxsig, $ 
	rowplot=rowplot, nosave=nosave, stopfit=stopfit, outenergy=outenergy, $
	outpix=outpix, dark1=dark1, dark2=dark2, flux=flux, more=more, $
	ipcdata=ipcdata

IF N_PARAMS() eq 0 AND ~keyword_set(more) THEN BEGIN
	print, "SYNTAX: "
	print, "	RADHITS, det, energy, bias, nramps, [...]
	return
ENDIF

IF keyword_set(more) THEN BEGIN
	doc_library, 'radhits'
	return
ENDIF
;+
; NAME:
;	RADHITS
;
; PURPOSE:
;	Investigate high energy radiation exposure from crocker lab. First 
;	look at background current (non-hit pixels) by masking out possible 
;	hits. Then look at hits by finding regions of interest containing pixels
;	5 sigma above background.
;
; INPUTS:
;	det - detector array (346, 354, or 86)
; 	energy - the beam energy (63, 32, 12 MeV) for 354 or 346, (18, 34 MeV)
;		for 86.
;	bias - applied bias at which data are obtained (150 or 250)
;	nramps - how many ramps in data set (10 for f-8, 2 for f-32). You can 
;		also to choose to read in only 2 of 10 F-8 ramps if you so 
;		choose.
;
; OUTPUTS:
;
;
;
; KEYWORDS:
;	/bkg - set this keyword to look at the non-hit pixels
;	/flux - only print proton flux and return
;	cutoffval=cutoffval - cutoff value for which hits to select
;	cutoffenergy=cutoffenergy - cutoff energy if there are two peaks in 
;		energy deposited in one hit
;	npixhit=npixhit - select to stop after number of pix per hit plot
;	totalenergyhit=totalenergyhit - select to stop after total signal in one
;		hit plot
;	maxsig=maxsig - select to stop after max signal in one hit plot
;	rowplot=rowplot - select to stop after plotting a row plot.
;
;
; Some Notes:
; 354, 12 MeV used cutoffval 2e5. For 32, 63 MeV used 1e5 
; 346 all energies used cutoffval 1e5
; 86
;
;
;
;-


; ************************* this is to do cumhist
IF det EQ 354 AND bias EQ 250 AND energy EQ 12 THEN temp_slope = fltarr(134222,nramps) 
IF det EQ 346 AND bias EQ 250 AND energy EQ 12 THEN temp_slope = fltarr(144546,nramps) 
IF det EQ 86  AND bias EQ 150 AND energy EQ 18 THEN temp_slope = fltarr(95732, nramps) 
IF det EQ 86  AND bias EQ 150 AND energy EQ 34 THEN temp_slope = fltarr(410051, nramps)
; *************************

; For 346, read in a mask for only stripe III
IF det EQ 346 THEN BEGIN
	; Mask
	stripe3mask = readfits(!H1RG+'H1RG-17346/masks/region3.fits',/silent)
	; Indices 
	stripe3 = where(stripe3mask EQ 1)
ENDIF

; Set up empty arrays for bkg stats. Determined by number of points in hist we
; make later. change the points in the saved hist, change these values
numpoints = 121
IF det EQ 86 AND energy EQ 18 THEN numpoints=101 
IF det EQ 86 AND energy EQ 34 THEN numpoints=1002
IF det EQ 86 AND energy EQ 0 THEN numpoints=20021
histograms = fltarr(nramps, numpoints)
meddata = fltarr(nramps)
modedata = fltarr(nramps)
stdevdata = fltarr(nramps)

; Set up empty arrays for proton hit stats
npix = 0. ;set initial value for number of pix in each hit. will concatinate
	; on the number as we calculate. we dont have knowledge right now of
	; how large to make this array.
totalsignalhit = 0.
maxsignalhit = 0.
nhits = double(0) ; number of hits we are using in calculations total
errs = double(0)
; array to sum up vertical line through hit
sumlinplot = fltarr(7) 

; Iterate through each ramp
FOR picnum = 1, nramps DO BEGIN

; Current ramp as string
ramp = trim(picnum)

IF keyword_set(ipcdata) THEN BEGIN
	; UNCOMMENT THE BELOW FOUR LINES TO RUN ON CORRECTED DATA FOR 354	
	;data = readfits('/home/abulatek/Code/Alyssa/H1RG-17354_corrected.fits')
	;eperV = 42339.9
	;outstr = '/home/meghan/det/data/H1RG_data/radiation_testing/H1RG-17354/secondrun/'
	;fowler_number = 32		

	; UNCOMMENT THE BELOW FOUR LINES TO RUN ON CORRECTED DATA FOR 346
	data = readfits('/home/abulatek/Code/Alyssa/H1RG-17346_corrected.fits')
	eperV = 38520.303
	outstr = '/home/meghan/det/data/H1RG_data/radiation_testing/H1RG-17346/secondrun/' 
	fowler_number = 8
	
	darkmap = readfits(outstr+'35K_150mV_darkbefore.fits',/silent) 
	wellmap = readfits(outstr+'35K_150mV_wellbefore.fits',/silent) 

	; Get dimensions of data
	sz = size(data, /dimensions)

	; Remove first frame
	IF sz[0] NE 0 THEN data = data[*,*,1:sz[2]-1] 

	; itegration time
	itime = 5.278 ;sec

	; integration time array
	itarr = (findgen(fowler_number*2.-1)+1)*itime

	; Get mask
	sigmaclip, darkmap, wellmap, mask

	goto, skipradinfo
ENDIF

; Read in data - will give dark current before rad testing, mask from 
; 		before testing, rad test data, and conversion factor. 
radinfo, det, energy, bias, picnum, data=data, eperV=eperV, itime=itime, $
	outstr=outstr, darkmap=darkmap, mask=mask, itarr=itarr

skipradinfo:

; Get eperADU to convert DC map to electrons (used for temporal portion)

IF det EQ 346 THEN BEGIN
detinfo, det, acqbox='black4gain11', eperADU=eperADU, bias=bias , $
	detname=detname, region=3
ENDIF ELSE BEGIN
detinfo, det, acqbox='black4gain11', eperADU=eperADU, bias=bias , $
	detname=detname
ENDELSE

; Some output filenames
filename_energy = outstr+'totalcharge_'+trim(det)+'_'+trim(energy)+'MeV_'+trim(bias)+'mV.png'
filename_npix = outstr+'numpixperhit_'+trim(det)+'_'+trim(energy)+'MeV_'+trim(bias)+'mV.png'

; Reference pixel subtract
refmap, data, '', '', det=det, /sutr, /replace

; Get array dimensions
sz = size(data, /dimensions)

; Here we are changing the number of samples up the ramp we are going to look 
; at for 86, because for the data sets we dont have to use as many (F-64 takes 
; a long time) and for the F-8, data saturates by the end of the ramp. 
IF (det EQ 86) AND (energy NE 0) THEN BEGIN
; F-64 data for 86
;sz[2] = 64
; F-8 data for 86
sz[2] = 14
ENDIF

; Empty array for difference images
diff_array = fltarr(sz[0], sz[1], sz[2]-1)

; Difference consecutive frames
FOR i=0, sz[2]-2 DO BEGIN
	diff_array[*,*,i] = (data[*,*,i+1] - data[*,*,i])*mask*eperV
ENDFOR


; Create a new mask and set ref pix to NAN - we are going to set ref. pix
;	to NAN, bad pix to NAN, bkg pix to 0 and possible hits to 1. Setting 
;	the bkg pix to an actual value allows us to find them and do 
;	statistits (where()), but we will later set those to NAN as well to 
;	only focus in on the hits. 
newmask = fltarr(sz[0], sz[1], sz[2]-1)
newmask[0:3,*,*] = !values.f_nan
newmask[1020:1023,*,*] = !values.f_nan
newmask[*,0:3,*] = !values.f_nan
newmask[*,1020:1023,*] = !values.f_nan

; Find instances in SUTR (differences) where there is a jump to a higher 
;	level. This will differentiate the background pixels from POSSIBLE
;	rad hits. Some possible hits will not really be hits and will be dealt
;	with later. 
itarr2 = itarr[0:sz[2]-2]

; First loop set pix hit multiple times to NAN
FOR x=4, sz[0]-5 DO BEGIN 
	FOR y=4, sz[1]-5 DO BEGIN
		pixel = reform(diff_array[x,y,*], sz[2]-1)
		; If NAN (bad pixel) set new mask to NAN and go to next pixel
		IF finite(pixel[0], /NAN) THEN GOTO, endloop
		; We will set a stipulation that there is a jump if the amount of 
		; current is greater than 3 times the noise. Where returns the 
		; index of where the jump occurred (temporal diff dev)
		jumpup = where(pixel GT (abs(mean(pixel)) + 3*stdev(pixel))) 
		; If there are multiple jumps, then one pixel sees multiple hits. 
		; Lets set this to NAN (excluded pixels),
		; we want to be exclusionary
		IF n_elements(jumpup) GT 1 THEN BEGIN
			;for k=0, n_elements(jumpup)-1 do begin
			; Consider a 5x5 box around that index. 
			; Truncate if at edge
			IF (x-2) GE 4 THEN lowx=x-2 ELSE lowx=4
			IF (x+2) LE 1019 THEN highx=x+2 ELSE highx = 1019
			IF (y-2) GE 4 THEN lowy=y-2 ELSE lowy=4
			IF (y+2) LE 1019 THEN highy=y+2 ELSE highy=1019
			newmask[lowx:highx,lowy:highy,*] = !values.f_nan
			;endfor
			;newmask[x,y,*] = !values.f_nan
		ENDIF
	endloop:
	ENDFOR
ENDFOR

; Second loop repeat and set pixels hit to 2 and 5x5 surround to 1
FOR x=4, sz[0]-5 DO BEGIN 
	FOR y=4, sz[1]-5 DO BEGIN
		pixel = reform(diff_array[x,y,*], sz[2]-1)
		; If NAN (bad pixel) go to next pixel
		IF finite(pixel[0], /NAN) THEN GOTO, endloop2
		jumpup = where(pixel GT (abs(mean(pixel)) + 3*stdev(pixel))) 
		; Where() will either return an index or -1
		; Also, set a dark current limit because high dark current pix
		; may be mistaken for a jump. (probably not necessary with mask)
		;darkcurrent = darkmap[x,y]*eperADU
		IF n_elements(jumpup) EQ 1 THEN BEGIN 
			;IF jumpup EQ -1 THEN newmask[x,y,*]=0 ELSE BEGIN
			IF jumpup NE -1 THEN BEGIN
				; Consider a 5x5 box around that index. 
				; Truncate if at edge
				IF (x-2) GE 4 THEN lowx=x-2 ELSE lowx=4
				IF (x+2) LE 1019 THEN highx=x+2 ELSE highx = 1019
				IF (y-2) GE 4 THEN lowy=y-2 ELSE lowy=4
				IF (y+2) LE 1019 THEN highy=y+2 ELSE highy=1019
				region = newmask[lowx:highx,lowy:highy,jumpup] 
				; if there is already a hit in the region, only
				; set zeros to 1 and center to 2
				IF total(region) GT 0 THEN BEGIN
					sz_r = size(region, /dimensions)
					ncol_r = sz_r[0]
					ind_r = where(region EQ 0)
					WHATCOORDINATE, ncol_r, ind_r, out_r
					for r=0, n_elements(ind_r)-1 do begin
						newmask[lowx+out_r[0,r],lowy+out_r[1,r],jumpup] = 1
					endfor
					newmask[x,y,jumpup]=2
				; else set all values to 1 and center to 2
				ENDIF ELSE BEGIN
					; All 5x5 surround set to 1
					newmask[lowx:highx,lowy:highy,jumpup]=1
					; where jump occured = 2
					newmask[x,y,jumpup]=2
				ENDELSE
			ENDIF
			;IF pixel[jumpup] LT (10*darkcurrent) THEN $
				;newmask[x,y,jumpup]=0
		ENDIF
	endloop2:
	ENDFOR
ENDFOR


; Finally set bad pixels to NAN again in case they were set to 1 in the 
; loop above (near a hit)
FOR x=4, sz[0]-5 DO BEGIN 
	FOR y=4, sz[1]-5 DO BEGIN
		pixel = reform(diff_array[x,y,*], sz[2]-1)
		; If NAN (bad pixel) go to next pixel
		IF finite(pixel[0], /NAN) THEN newmask[x,y] = !values.f_nan
	ENDFOR
ENDFOR


; Sum the mask array along the z-direction. the result is all pix with a jump
;	will be GE 1, pixels excluded should be NAN, and background pixels will
;	remain 0.
sumarray = total(newmask, 3)

; Where mask is 0 are background pixels - we are going to loop through all of
;	the indices. If statement to exclude all but stripe 3 for 346.
IF det EQ 346 THEN BEGIN
	bkgpix = where(sumarray EQ 0)
	bkgind = intersect(bkgpix, stripe3)
ENDIF ELSE bkgind = where(sumarray EQ 0)

; Fit a line to find the dark current in bkg pix
numbkgpix = n_elements(bkgind)
slopes = fltarr(numbkgpix)
chisqarr = fltarr(numbkgpix)
print, 'number of non-hit pix found ' + trim(numbkgpix)
print, ''

FOR i=0, numbkgpix-1 DO BEGIN
	; Current index from list of indices
	currentind = bkgind[i]

	; Get coordinate from index
	whatcoordinate, sz[0], currentind, xy

	; x and y data for pixel
	x = itarr
	y = data[xy[0],xy[1],*]*eperV

	; Linear fit to data
	params = linfit(x, y, chisq=chisq)

	a = params[0]
	b = params[1]

	; Calculate error
	N = size(x, /dimensions)
	sigma2 = (1./(N-2))*total((y-a-(b*x))^2)

	delta = (N*total(x^2.))-(total(x)^2.)
	err2 = abs((N/delta)*sigma2)
	errs = [errs,sqrt(err2)]

	;ENDIF
	slopes[i] = params[1]
	chisqarr[i] = chisq
	; ************************
	IF  total(size(temp_slope, /dimensions)) GT 0 THEN $
		 temp_slope[i,picnum-1] = params[1]
	; ************************
ENDFOR

; Saving Histogram data for bkg pix for each ramp 
slopesNE0 = where(slopes NE 0)
bin = 0.05
IF det EQ 86 AND energy EQ 18 THEN bin=10 
IF det EQ 86 AND energy EQ 34 THEN bin=1
IF det EQ 86 THEN maxh=1000 ELSE maxh=5

minh = -1
print, 'Number of pixels in bkg calc: ' + trim(n_elements(slopes[slopesNE0]))
histograms[picnum-1,*]=histogram(slopes[slopesNE0],binsize=bin,min=minh, max=maxh)
modedata[picnum-1] = mode(slopes[slopesNE0], bin)
meddata[picnum-1] = median(slopes[slopesNE0])
stdevdata[picnum-1] = stdev(slopes[slopesNE0])
print, 'max (non-hit) dark current found: ' + trim(max(slopes[slopesNE0]))
print, ''

IF keyword_set(bkg) THEN goto, endramps
; ************************** Looking hits here **************************

; Set some empty arrays for stats later
nblobs = fltarr(sz[2]-1)
images2D = fltarr(11,11)

; Iterate through each sample up the ramp image and examine ROI.

FOR i=1, sz[2]-2 DO BEGIN
	; Current mask 
	hitmask = newmask[*,*,i] 
	current_mask = fltarr(sz[0],sz[1])
	; Set jump values (2) to 1 in current mask
	current_mask[where(hitmask EQ 2)] = 1
	
	IF det EQ 346 THEN current_mask = current_mask*stripe3mask

	; Extract regions of interest from mask
	hits = Obj_New('Blob_Analyzer', current_mask)

	; Extract voltage info for hits from current mask
	IF det EQ 346 THEN BEGIN
		voltagedata = current_mask*diff_array[*,*,i]*stripe3mask
	ENDIF ELSE voltagedata = current_mask*diff_array[*,*,i]

	; Set NAN in voltagedata to 0 because filtering will not work otherwise
	voltagedata[where(finite(voltagedata) EQ 0)] = 0
	hitmask[where(finite(hitmask) EQ 0)] = 0

	; Use filtering to extract hits. Will have to manually choose a
	; threshold, but once chosen can be added as a keyword
	b= bpass(voltagedata, 1, 11)

	IF ~KEYWORD_SET(cutoffval) THEN BEGIN
		f = feature(b, 11)
		cgplot, f(2,*), f(3,*), psym=6
		print, 'Choose cutoff value'
		RETURN
	ENDIF
	f = feature(b,7,masscut=cutoffval, /quiet)

	centroids = round(f(0:1, *))

	; For some data from 346 there are lots of hits so we want to exclude
	; the ones too close together.
	IF det EQ 346 THEN BEGIN
		IF energy EQ 32 or energy EQ 12 THEN BEGIN
		; Exclude centroids too close together
		maxdist = 50 ; pixels
		centroid_distance, centroids, maxdist, centroids_iso

		; Convert centroid [x,y] to index
		whatindex, sz[0], centroids_iso, centroid_index
		ENDIF ELSE whatindex, sz[0], centroids, centroid_index
	ENDIF ELSE whatindex, sz[0], centroids, centroid_index

	; Get number of hits detected
	nblobs[i] = hits -> NumberOfBlobs()

	; Look at each ROI individually
	FOR n=0, nblobs[i]-1 DO BEGIN
		; Get indices of hit pixels
		indices = hits -> GetIndices(n)

		; Find regions that contain centroids
		centerblob = intersect(indices, centroid_index)

		; If current region contains a centroid, continue and save data
		IF total(centerblob) GT 0 THEN BEGIN

			; Pixels in ROI with voltage > median bkg + 5*stdev
			hitind = where(voltagedata[indices] GT $
				(meddata[picnum-1] + 5*stdevdata[picnum-1]))

			; Go to next ROI if no pixels are greater than mean bkg
			IF n_elements(hitind) EQ 1 THEN BEGIN
				IF hitind EQ -1 THEN goto, NextROI
			ENDIF

			; Make sure that there are not multiple peaks in a hit
			; ie. 2's (jumps) are adjacent in mask
			singleblob = fltarr(sz[0],sz[1])
			singleblob[indices[hitind]] = 1
			twos = Obj_New('Blob_Analyzer', singleblob)
			numinoneblob = twos -> NumberOfBlobs()
			IF numinoneblob GT 1 THEN goto, NextROI

			; Add one to number of hits
			nhits += 1

			; Number of pixels in hit
			npix = [npix,n_elements(hitind)]

			; ROI in electrons
			electronshit = voltagedata[indices[hitind]]

			; row through center of hit
			maxind = indices[where(electronshit EQ max(electronshit))]
			whatcoordinate, sz[0], maxind, maxcoord
			sumlinplot += $
			voltagedata[maxcoord[0]-3:maxcoord[0]+3, maxcoord[1]]

			; Make a 2D image of hit
			;image2D = fltarr(7,7)
			images2D = [[[images2D]], $
			[[voltagedata[maxcoord[0]-5:maxcoord[0]+5, maxcoord[1]-5:maxcoord[1]+5]]]]
			
			; How many electrons deposited per hit
			totalsignalhit = [totalsignalhit, total(electronshit)]

			; Maximum signal per hit
			maxsignalhit = [maxsignalhit, max(electronshit)]

			; Go to next ROI if no pixels are greater than mean bkg
			NextROI:
		ENDIF
	 ENDFOR

ENDFOR

print, 'Number of hits for ramp '+trim(picnum)+': '+trim(nhits)
endramps:
ENDFOR ;end for all ramps

cgHistoplot, npix, BINSIZE=1.0, /FILLPOLYGON, TITLE='Hit sizes for H1RG-17346 after correction', /Window, XTITLE = 'Pixels per hit', YTITLE='Frequency'
print, 'The mean of the distribution is:'+MEAN(npix)
print, 'The median of the distribution is:'+MEDIAN(npix)

stop

images2D = images2D[*,*,1:*]
getdimsfarr, images2D, smallcol, smallrow, numhits
sclsize = 10
summedarray = fltarr(11*sclsize,11*sclsize)
FOR i=0, numhits-1 DO BEGIN
	region = images2D[*,*,i]
	newsize = smallcol*sclsize
	resamp = rebin(region, newsize, newsize, /sample)/(sclsize^2.0)
	gcntrd, region, 5, 5, xcen, ycen, 3
	
	; define center
	newcenter = round(((newsize)-1)/2.)

	; Now shift the array by the centroid (round to nearest tenth)
	shiftx = newcenter - round(xcen*10.)
	shifty = newcenter - round(ycen*10.)
	shiftarray = shift(resamp, shiftx, shifty)

	; Add shifted array to the sum
	summedarray += shiftarray

END

; The following was added by Alyssa.

summedarray = summedarray/max(summedarray)
xfit = findgen(110)/10.
yfit = findgen(110)/10.
fit2d = GAUSS2DFIT(summedarray, A, xfit, yfit, /TILT)

width_x = A[2]
width_y = A[3]

FWHM_x = 2 * SQRT(2 * ALOG(2)) * width_x
FWHM_y = 2 * SQRT(2 * ALOG(2)) * width_y

FWHM_avg = (FWHM_x + FWHM_y)/2.

;print, 'The average full width at half maximum for this ramp is: '
;print, FWHM_avg

stop

IF keyword_set(bkg) THEN goto, background_analysis
; ************************ Plotting for Proton Hits ************************
; *** Flux for the ramp ***
flux = nhits/(itarr[sz[2]-2]*nramps)/ ((1016.0*18.0*1e-4)^2.) ; itime per ramp times n ramps
print, 'Proton flux = ' + trim(flux) + ' protons/sec/cm^2'
print, ''
IF keyword_set(flux) THEN stop

; *** Total Energy deposited in one hit ***
xtitle = 'Signal (electrons)'
ytitle = 'Number of Hits per Bin'
title = 'Total Charge Collected in a Proton Hit'

IF det EQ 354 THEN BEGIN
	IF energy EQ 12 THEN BEGIN
	binsize = 1e4
	mininput=0
	maxinput=5e5
	ENDIF

	IF energy EQ 32 THEN BEGIN
	binsize = .5e4
	mininput=0
	maxinput=2.5e5
	ENDIF

	IF energy EQ 63 THEN BEGIN
	binsize = .5e4
	mininput=0
	maxinput=3.5e5
	ENDIF
ENDIF
IF det EQ 346 THEN BEGIN
	IF energy EQ 12 THEN BEGIN
	binsize = 1e4
	mininput=0
	maxinput=5e5
	yrange=[0,150]
	ENDIF

	IF energy EQ 32 THEN BEGIN
	binsize = .5e4
	mininput=0
	maxinput=4e5
	ENDIF

	IF energy EQ 63 THEN BEGIN
	binsize = .5e4
	mininput=0
	maxinput= 2.5e5
	ENDIF

ENDIF
IF det EQ 86 THEN BEGIN
	IF energy EQ 18 THEN BEGIN
	binsize = 1e4
	mininput=0
	maxinput=6e5
	ENDIF

	IF energy EQ 34 THEN BEGIN
	binsize = .5e4
	mininput=0
	maxinput=4e5
	ENDIF


ENDIF

cghistoplot, totalsignalhit, color='black', binsize=binsize, $
missing=0, xtitle=xtitle, ytitle=ytitle, title=title, $
mininput=mininput, maxinput=maxinput

print, "mean: " + trim(mean(totalsignalhit[1:*]))
print, "median: " + trim(median(totalsignalhit[1:*]))

B = ''
READ, B, PROMPT='Fit Two Curves? >>'

IF strmatch(B, 'yes') THEN BEGIN
	fittwocurves2, totalsignalhit[1:*], npix[1:*], filename_energy, $
	filename_npix, nosave=nosave, stopfit=stopfit, binsize=binsize, $
	mininput=mininput, maxinput=maxinput
	return
ENDIF

cghistoplot, totalsignalhit[1:*], mininput=mininput, maxinput=maxinput, $
	color='black', title=title, xtitle=xtitle, ytitle=ytitle, $
	binsize=binsize
medsignal = trim(round(median(totalsignalhit[1:*])))
addcomma, medsignal
cgText, 0.65, 0.80, /NORMAL, 'Median: ' + medsignal + ' e$\up-$', $
	COLOR='black'
modesignal = trim(round(mode(totalsignalhit[1:*], binsize)))
addcomma, modesignal
cgText, 0.65, 0.75, /NORMAL, 'Mode: ' + modesignal + ' e$\up-$', $
	COLOR='black'


IF keyword_set(totalenergyhit) THEN STOP

; Number of pixels per hit
binsize = 1
xtitle = 'Number of Pixels'
ytitle = 'Number of Hits per Bin'
title = 'Number of Pixels Affected by a Proton Hit'

delvar, yrange, xrange


IF det EQ 354 THEN BEGIN
	IF energy EQ 12 THEN BEGIN
	xrange = [0,40]
	ENDIF

	IF (energy EQ 32) or (energy EQ 63) THEN BEGIN
	xrange = [0,40]
	yrange = [0, 500]
	ENDIF
ENDIF

IF det EQ 346 THEN BEGIN
	IF energy EQ 12 THEN BEGIN
	binsize=1
	xrange = [0,90]
	yrange = [0,75]
	ENDIF
	IF energy EQ 63 THEN BEGIN
	binsize=1
	xrange = [0,30]
	ENDIF
ENDIF

IF det EQ 86 THEN BEGIN
	binsize=1
	xrange = [0,25]
ENDIF

cghistoplot, npix, color='black', missing=0, binsize=binsize, xrange=xrange, $
	yrange=yrange, xtitle=xtitle, ytitle=ytitle, title=title, $
	HISTDATA=h, LOCATIONS=loc
binCenters = loc + (binsize / 2.0)
yfit = GaussFit(binCenters, h, coeff, NTERMS=3)
cgPlot, binCenters, yfit, COLOR='black', THICK=2, /OVERPLOT
centerfit = String(coeff[1], FORMAT='(F0.1)')
fwhm = String(2 * SQRT(2 * ALOG(2)) * coeff[2], FORMAT='(F0.1)')
cgText, 0.7, 0.70, /NORMAL, 'Center: ' + centerfit, COLOR='black'
cgText, 0.7, 0.65, /NORMAL, 'FWHM: ' + fwhm, COLOR='black'


IF keyword_set(npixhit) THEN STOP

; histogram of max signal hit
title = 'Charge Deposited in Central Pixel'
xtitle = 'Charge (electrons)'
cghistoplot, maxsignalhit, missing=0, /xlog, color='black', xrange=[.5e4,1e5], $
	title=title, xtitle=xtitle
filename = outstr+'maxsig_'+trim(det)+'_'+trim(energy)+'MeV_'+trim(bias)+'mV.png'
IF KEYWORD_SET(maxsig) THEN STOP

; Average row for plot
IF KEYWORD_SET(rowplot) THEN BEGIN
avglinploty = sumlinplot/nhits
avglinplotx = indgen(7)-3
cgplot, avglinplotx, avglinploty
stop
ENDIF

; ************************ Background **************************************
background_analysis:

; Print error in slope
slope_err = errs[1:*]
ones = where(finite(slope_err) EQ 1)
print, 'Slope Error ' + trim(mean(slope_err[ones]))


; *************************
; For individual histograms: this is in debug mode
;slope1 = temp_slope[where(temp_slope[*,0] NE 0),0]
;slope2 = temp_slope[where(temp_slope[*,1] NE 0),1]
;slope9 = temp_slope[where(temp_slope[*,8] NE 0),2]
;minh = -10
;maxh = 200
;bin = 0.1
;h1 = histogram(slope1,binsize=bin,min=minh, max=maxh)
;h2 = histogram(slope2,binsize=bin,min=minh, max=maxh)
;h9 = histogram(slope9,binsize=bin,min=minh, max=maxh)
;cumh1 = double(total(h1, /cumulative, /preserve_type))
;cumh2 = double(total(h2, /cumulative, /preserve_type))
;cumh9 = double(total(h9, /cumulative, /preserve_type))
;numindex=round((maxh-minh)/bin) 
;x = findgen(numindex)*bin + minh

;IF keyword_set(dark1) THEN BEGIN
;	darkbefore1 = dark1[bkgind]
;	hbefore1 = histogram(darkbefore1,binsize=bin,min=minh, max=maxh)
;	cumhbefore1 = double(total(hbefore1, /cumulative, /preserve_type))
;ENDIF
;IF keyword_set(dark2) THEN BEGIN
;	darkbefore2 = dark2[bkgind]
;	hbefore2 = histogram(darkbefore2,binsize=bin,min=minh, max=maxh)
;	cumhbefore2 = double(total(hbefore2, /cumulative, /preserve_type))
;ENDIF

; Cumulative hist
;cgps_open, filename=filename, default_thickness=3
;title = 'Cumulative Dark Current for Non-Hit Pixels'
;ytitle = '% Non-Hit Pixels'
;xtitle = 'Dark Current (e$\up$-/sec)'
;xrange = [.1,200]
;yrange = [80.,100]
;cgplot, x, (cumh1/n_elements(slope1))*100.,psym=10, xrange=xrange, /xstyle, $
;	yrange=yrange, title=title, xtitle=xtitle, ytitle=ytitle, /xlog, /ystyle

;dc_cave_354_12MeV, 250, xbeforecave, hbeforecave
;dc_cave_346_12MeV, 8, xbeforecave, hbeforecave
;cumhcavebefore = double(total(hbeforecave, /cumulative, /preserve_type))
;cgplot, xbeforecave, (cumhcavebefore/max(cumhcavebefore))*100.,psym=10, $
;	 linestyle=5, /overplot


;IF keyword_set(dark1) THEN BEGIN
;cgplot, x, (cumhbefore1/n_elements(darkbefore1))*100.,psym=10, linestyle=4, $
;	/overplot
;ENDIF
;IF keyword_set(dark2) THEN BEGIN
;cgplot, x, (cumhbefore2/n_elements(darkbefore2))*100.,psym=10, linestyle=3, $
;	/overplot
;ENDIF
;al_legend, ['UR Lab Before','CNL Cave Before','During Irradiation'], linestyle=[4,5,0], /right, charsize=1.5, bthick=3, /bottom
;cgtext, 0.51,0.40, trim(detname), /normal  
;cgtext, 0.51,0.35, 'Applied Bias '+trim(bias)+'mV', /normal 
;cgtext, 0.51,.30, trim(energy)+'MeV Proton Irradiation', /normal

;stop
; *************************

; Mean of all histograms
hist = mean(histograms, dimension=1)
meanmode = mean(modedata)
meanmed = mean(meddata)
histerr = stddev(histograms, dimension=1)
lowErrs = hist-(histerr/2.)
highErrs = hist+(histerr/2.)

IF KEYWORD_SET(bkg) THEN BEGIN

	; Line thickness for plot
	charsize=1.8

	title = 'Current in Non-Hit Pixels'
	xtitle = 'Current (e$\up-$/s)'
	ytitle = 'Number of Pixels Per Bin'

	; cgplot, x, histograms[0,*], psym=10

	; Minimum xrange for plot
	minx = minh
	maxx = maxh
	IF det EQ 86 AND energy EQ 18 THEN minx = 500
	IF det EQ 86 AND energy EQ 34 THEN maxx = 200

	numindex=round((maxh-minh)/bin)
	x = findgen(numindex)*bin + minh

	; Plot mean histogram
	cgplot, x, hist, psym=10, xrange=[minx,maxx], /xstyle, $
	title=title, ytitle=ytitle, xtitle=xtitle, charsize=charsize

	; Plot errors
	;errplot, x, lowErrs, highErrs, color=0

	; Text for plot
	IF det EQ 354 OR det EQ 346 THEN xp = 0.5
	IF det EQ 86 THEN xp = 0.15

	 
	IF det EQ 86 THEN round_decimal, meanmed, bkgmed, /tens ELSE $
		round_decimal, meanmed, bkgmed, /thousands
	IF det EQ 86 THEN round_decimal, meanmode, bkgmode, /tens ELSE $
		round_decimal, meanmode, bkgmode, /thousands
	cgtext, xp, 0.85, 'Detector Array '+detname, /normal, charsize=charsize
	cgtext, xp, 0.75, 'Beam Energy '+ trim(energy)+'MeV', /normal, $
		charsize=charsize
	cgtext, xp, 0.8, '35K, '+trim(bias)+'mV Applied Bias', /normal, $
		charsize=charsize
	cgtext, xp, 0.7, 'Median '+bkgmed+' e$\up-$/s', /normal, $
		charsize=charsize
	cgtext, xp, 0.65, 'Mode '+bkgmode+' e$\up-$/s', /normal, charsize=charsize
stop

IF det EQ 354 THEN BEGIN
	dc_cave_354_12MeV, bias, xbefore, hbefore
	hbefore_norm = hbefore/max(hbefore) 
ENDIF
IF det EQ 346 THEN dc_cave_346_12MeV, 8, xbefore, hbefore_norm


hafter_norm = hist/max(hist) 
loerr = lowErrs/max(hist) 
hierr = highErrs/max(hist) 


	thick=3
	xthick=3
	ythick=3
	charthick=3
	linethick=3
	yrange=[0,1.1]
	cgplot, x, hafter_norm, psym=10, xrange=[-0.5,2.5], /xstyle, title=title, ytitle=ytitle, 	xtitle=xtitle, charsize=1.8, yrange=yrange, /ystyle, thick=thick, charthick=charthick, xthick=xthick, ythick=ythick
	errplot, x, loerr, hierr, color=0                                                                                              
	cgplot, xbefore, hbefore_norm, psym=10, /overplot, linestyle=4, thick=thick, xthick=xthick,ythick=ythick                                                                                                      
	al_legend, ['Before Radiation','During Radiation'], linestyle=[4,0], /right, charsize=1.5, bthick=3, charthick=charthick
	cgtext, 0.5, 0.73, 'Detector Array H1RG-17346', /normal, charsize=1.8 , charthick=charthick
	cgtext, 0.5, 0.68, '35K, 250mV Applied Bias', /normal,charsize=1.8, charthick=charthick
	cgtext, 0.5, 0.63, 'Beam Energy 12MeV', /normal, charsize=1.8 , charthick=charthick

	print, 'background median: '+bkgmed
	print, 'background mode: '+bkgmode

	xtitle='Dark Current (e$\up$-/sec)'
	ytitle='Number of Pixels Per Bin'
	title = 'Dark Current of Non-Hit Pixels'
	yrange=[.01,1e4]
	xrange=[.1,300]
stop
	;cgplot, x, h1, psym=10, /ylog,/xlog,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,title=title, ytickformat='exponent'

;	cgtext, 0.2,0.4, 'H1RG-17354', /normal  
;	cgtext, 0.2,0.35, 'Applied Bias 150mV', /normal 
;	cgtext, 0.2,.3, 'Max Dark Current: 143.7e/s', /normal
;	cgtext, 0.2,.25, '12 Pixels >100e/s', /normal
stop
ENDIF



END
