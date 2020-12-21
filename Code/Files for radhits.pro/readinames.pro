PRO readinames, path, temp, run_number, picture_number, nframes, $
	start_frame, fowler_number, data, hdr=hdr, more=more

IF N_PARAMS() LT 1 AND ~KEYWORD_SET(more) THEN BEGIN
	print, "SYNTAX: "
	print, "	readinames, path, temp, run_number, picture_number, "
	print, "	nframes, start_frame, fowler_number, [data, /more]
	return
ENDIF

IF KEYWORD_SET(more) THEN BEGIN
	doc_library, 'readinames'
	return
ENDIF

;+
; NAME:
;	readinames 
;
; PURPOSE:
;	Read in data obtained at nasa ames. 
;
; Inputs:
; 	path - Path to data directory (string).
;	temp - temp at which data obtained
;	run_number - first directory for multiple runs. This program will 
;			read in only one run at a time. 
;	picture_number - picture number (basially data set)
;	nframes - number of ramps to read in
;	fowler_number - fowler number. basically the size of the array is 
;			2x this number.
;	start_frame - frame (ramp) number to start reading in
;
; Outputs:
;	data - Data out. Can be one cube (3D) of sutr images or a huge 4D array of 
;			multiple 3D ramps. 
;
; Keywords:
;	more=more - print documentation to screen.	
;
; Notes:
;	ex path: '~/data/35K_1/Picture_1/Frame_10.fits
;	The above path is one cube of fowler data ie. perhaps a F-4 cube. 
;-

; Strings for path name
temp = trim(temp)
run = trim(run_number)
picture = trim(picture_number)
path_root = path+temp+'K_'+run+'/Picture_'+picture+'/'

; If multiple ramps to read in
IF nframes GT 1 THEN BEGIN

	; Create empty array to read in data
	data = fltarr(1024, 1024, fowler_number*2., nframes)
	
	; Iterate through ramps
	count=0
	FOR i=start_frame, start_frame+nframes-1 DO BEGIN
		frame = trim(i)
		data[*,*,*,count] = readfits(path_root+'Frame_'+frame+'.fits', $
			/silent)
		count+=1
	ENDFOR	

; If only one ramp to read in
ENDIF ELSE BEGIN
	frame = trim(start_frame)
	data = readfits(path_root+'Frame_'+frame+'.fits', hdr, /silent)
ENDELSE









END
