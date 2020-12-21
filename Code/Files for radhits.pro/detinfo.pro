pro detinfo, det, detname=detname, epermV=epermV, eperADU=eperADU, mux=mux, $
	refcols=refcols, more=more, tree=tree, fullarrdims=fullarrdims, $
	bias=bias, acqbox=acqbox, DCgain=DCgain, pixelpitch=pixelpitch, $
	linslope=linslope, ipcfrac=ipcfrac, subst=subst, ipcmatrix=ipcmatrix, $
	region=region, cap=cap, q=q, path2maps=path2maps

if n_params() eq 0 AND ~keyword_set(more) AND ~keyword_set(tree) then begin
	print, "SYNTAX: "
	print, "	DETINFO, det, [detname=detname, epermV=epermV, "
	print, "	eperADU=eperADU, cap=cap, acqbox=acqbox, mux=mux,"
	print, "	refcols=refcols, fullarrdims=fullarrdims, bias=bias,"
	print, "	DCgain=DCgain, pixelpitch=pixelpitch, ipcfrac=ipcfrac, "
	print, "	linslope=linslope, subst=subst, ipcmatrix=ipcmatrix, "
	print, "	region=region, /more, /tree]"
	return
endif

if keyword_set(more) then begin
	doc_library, 'detinfo'
	return
endif

;+
; NAME:
;	DETINFO
;
; PURPOSE:
;	Use this procedure to obtain detector specific information to be used in 
;	reduction.
;
; SYNTAX:
;	DETINFO, det, [detname=detname, epermV=epermV, eperADU=eperADU, 
;	acqbox=acqbox, mux=mux, refcols=refcols, fullarrdims=fullarrdims, 
;	bias=bias, DCgain=DCgain, pixelpitch=pixelpitch, linslope=linslope, 
;	ipcfrac=ipcfrac, subst=subst, ipcmatrix=ipcmatrix, region=region, 
;	/more, /tree]
;
; INPUTS:
;	det - Detector number.  If unknown, enter ' ' for a list of choices.
;
; OUTPUTS:
;	NONE
;
; KEYWORDS:
;	detname=detname - Outputs the full detector name.
;		epermV=epermV - Outputs the electrons per mV conversion factor
;	eperADU=eperADU - Outputs the electrons per ADU conversion factor
;		(Requires the data acquisition box.)
;	cap=cap - Outputs capcacitance. Also requires data acquisition box. 
;	acqbox=acqbox - Input the data acquisition box name for the eperADU 
;		conversion factor.
;	bias=bias - Input the bias at which data were taken to obtain bias 
;		specific epermV information, assuming that information exists.
;	mux=mux - Outputs the multiplexer name.
;	refcols=refcols - Outputs the reference columns of the specified 
;		detector.
;	fullarrdims=fullarrdims - Outputs the full array dimensions
;	DCgain=DCgain - Outputs the DC gain of the multiplexer for the specified 
;		detector.
;	linslope=linslope - non linearity
;	ipcfrac=ipcfrac - fraction interpixel capacitance
;	ipcmatrix=ipcmatrix - Interpixel capacitance deconvolution matrix.
;		Will only return if one has been recorded here.
;	subst=subst - substrate thickness
;	region=region - Set this keyword to a number 1-4 if you are using 
;		detector 346, in order to get correct e/ADU for each region
;		with different backside treatment. If not set, e/adu will be 
;		an average. 
;	path2maps=path2maps - path to dark current maps as set by meghan dorn
;	/left - set this keyword for detector 8228 for left side array
;	/right - set this keyword for detector 8228 for right side array
;	/more - Generates this list.
;	/tree - Prints a file tree to the screen.
;
; PROCEDURES CALLED:
;		DEFAULT, BOXINFO
; NOTES: 
;	If electrons / ADU for a particular detector is not yet known, set it 
;	equal to ''.
;
; HISTORY:
;	Written by C. Bacon
;	5/14/2009 - CMB - Added Fire H2RG detector information.
;	5/24/2012 - CMF - Because so many programs call this file, added 
;		user input in case the detector in question is not recognized 
;		by the file.  If additional detectors are added, be sure to not 
;		only add them to case, also add them to else.
;	6/4/2012 - CMF - Edited string inputs so that detector information is 
;		only listed once.
;	6/6/2012 - CMF - Added default if det is not given.
;	6/13/2012 - CMF - Wrote default.pro and edited detinfo so it calls 
;		default.pro if no detector is listed.
;	7/3/2012 - CMF - Added dcgain to info.  Note: 0.95 is set on all current 
;		detectors (1,2,3,110,160), though not verified.
;	7/6/2012 - CMF - Added baseline for detectors 85, 86 and 87.
;	7/16/2012 - KGA - changed instances of box to acqbox.
;	8/14/2012 - MD - Added source follower gain for H1RG-16886
;	9/14/2012 - MD - Added source follower gain for H1RG-16887, added 
;		keyword to specify e/mV for each bias
;	3/1/2013 - MD - Added H1RG-17179
;	6/23/2013 - MD - Added H1RG-17354
;	6/27/2013 - MD - Updated SFgain H1RG-17354
;	6/28/2013 - MD - 354 capacitances and statements for data sets which 
;			there is no capacitance for. 
;	6/28/2013 - CMF - Corrected error message to include detectors 179 and 354.
;	7/1/2013 - MD - Fixed error in bias call for 87 
;	7/1/2013 - MD - Updated capacitance for 354 again. 
;	7/4/2013 - MD - Added keyword linslope for linearity correction (QE) 354
;	7/15/2013 - MD - Linearity for 87
;	7/16/2013 - MD - Added keyword parameters for substrate thickness and 
;		IPC correction
;	7/16/2013 - CMF - Corrected documentation and added keyword ipcmatrix.
;		Added ipcmatrix for 354.  Note: columns inside rows
;	7/17/2013 - MD - Added new detector 346 and sfgain for that detector. 
;	7/19/2013 - MD - Updated epermV for 346
;	7/25/2013 - MD - Changed 250mV capacitance for 354 because more data. 
;		AND added the average of all four sections for capacitance for 
;		346 for now. More changes - 346 specific capacitances for 
;		region keyword and added cap=cap keyword. 
;	7/31/2013 - MD - linearity correction for 346. changed keyword ipc=ipc
;		to ipcfrac=ipcfrac because after adding another keyword that
;		starts with 'ipc' that keyword becomes ambiguous. 
;	11/12/2013 - MD - Added H1RG-17453, verified DCgain 453
;	12/13/2013 - MD - Added H1RG-17557, verified DCgain 557
;	1/6/2014 - MD - Updated info on 557: 35K, 350mV (linearity and epermV)
;	7/11/2014 - MD - Added 591
;	9/18/2015 - MD - Added 8228
;	9/28/2015 - MD - Update to 8228, region keyword
;	12/8/2015 - MD - Add 8381
;	5/23/2016 - MD - Added 8481. there are other dets ive added in the 
;		meantime.
;	6/7/16 - MD - Added 8482
;-

if keyword_set(tree) then begin
		if tree eq 1 then print, "-->detinfo"
		if tree gt 1 then begin
			for i=1, tree - 1 do print, format='($, "      ")'
			print, "'->detinfo"
		endif
		tree = tree + 1
			default, tree=tree
			boxinfo, tree=tree
		tree = tree - 1
		return
endif

if n_elements(det) eq 0 then default, det=det
if keyword_set(acqbox) then boxinfo, acqbox, mVperADU
if size(det, /type) eq 7 then begin
RETRYDETNAME:
case det of
	'H1RG-16-001': det=1
	'H1RG-16-002': det=2
	'H1RG-16-003': det=3
	'H1RG-16885': det=85
	'H1RG-16886': det=86
	'H1RG-16887': det=87
	'H1RG-17179': det=79
	'H1RG-17354': det=354
	'H1RG-17346': det=346
	'H1RG-17453': det=453
	'H1RG-17557': det=557
	'H1RG-17591': det=591
	'H2RG-18228': det=8228
	'H2RG-18381': det=8381
	'H1RG-18367': det=8367
	'H2RG-18470': det=8470
	'H2RG-18235': det=8235
	'H2RG-18481': det=8481
	'H2RG-18482': det=8482
	'H2RG-18621': det=8621
	'H2RG-18692': det=8692
	'H2RG-18693': det=8693
	'H2RG-18694': det=8694
	'H1RG-110': det=110
	'FIRE-Acq-H2RG-160': det=160
	else: det=9999
endcase
endif 

RETRYDETNUM:
case det of
	1: begin
			detname='H1RG-16-001'
			;eperADU=4.2
			epermV=663.5
			DCgain = 0.95
			mux='H1RG'
			refcols=[0,1,510,511]
			fullarrdims=[512,512]
			pixelpitch=3.6e-5 ;m
		end
	2: begin
			detname='H1RG-16-002'
			;eperADU=4.4
			epermV=695.
			DCgain = 0.95
			mux='H1RG'
			refcols=[0,1,510,511]
			fullarrdims=[512,512]
			pixelpitch=3.6e-5 ;m
		end
	3: begin
			detname='H1RG-16-003'
			;eperADU=4.3
			epermV=679.
			DCgain = 0.95
			mux='H1RG'
			refcols=[0,1,510,511]
			fullarrdims=[512,512]
			pixelpitch=3.6e-5 ;m
		end
	110: begin
			detname='H1RG-110'
			;eperADU=5.3
			;epermV=837. ;OLD!!!!
			epermV=573.4
			DCgain = 0.95
			mux='H1RG'
			refcols=[0,1,510,511]
			fullarrdims=[512,512]
			pixelpitch=3.6e-5 ;m
		end
	85: begin
			detname='H1RG-16885'
			ipcfrac = 0.0122 ; from det=86
			epermV=306.381 ;4.675 e/ADU MD 8/23/12
			path2maps = !16885
			DCgain=0.95 ;Verified. 7/9/2012 
			mux='H1RG'
			refcols=[0,1,2,3, 1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.8e-3 ;800um for 85, 86, 87 substrate
		end
	86: begin
			detname='H1RG-16886'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.0122)
			IF KEYWORD_SET(bias) THEN BEGIN 	
				case bias of
					150: begin
						epermV = 306.166*ipcreduce ;2.128 e/ADU MD 8/23/12
						linslope=-2.61e-6
						end
					; 200 mV bias average of 150 and 250
					200: begin
						epermV = ((306.166+238.794)/2)*ipcreduce
						end
					250: begin
						epermV = 238.794*ipcreduce
						linslope=-1.98e-6
						end
					350: begin ; set e/mV same as 250 mV
						print, 'Approx as 250 mV bias'
						epermV = 238.794*ipcreduce
						linslope = -1.72e-6
						end
				endcase			
			
			ENDIF ELSE BEGIN
				print, 'Set bias if getting e/adu' 
				epermV = 306.166
				linslope=-2.61e-6
			ENDELSE
			path2maps = !16886 + 'secondrun/'
			DCgain=0.953 ;Verified. 8/14/12 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.8e-3
		end
	87: begin
	; Note: linearity for 87 set to the linearity found for 354 because
	; no linearity taken for 87 and they are from same wafer
			detname='H1RG-16887'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.0122) ; from det=86
			IF keyword_set(bias) THEN BEGIN 
				case bias of
					150: begin
						epermV = 314.933*ipcreduce
						linslope = -2.97051e-6
						end
					250: begin
						eperMV = 301.197*ipcreduce
						linslope = -1.95772e-6
						end
					; The capacitance for 350 mV is AFTER
					; ETCH. This is needed to reduce the 
					; QE post etch.
					350: begin
						print, 'capacitance after etch'
						epermV = 277.376*ipcreduce
						linslope = -1.8e-6
						end
				endcase
			ENDIF ELSE epermV = 314.933 ;Default to 150mV bias

			path2maps = !16887 + 'secondrun_scale_ref/'
			DCgain=0.952 ;Verified. 9/14/12 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.8e-3
		end
	79: begin
			detname='H1RG-17179'
			; We have more capacitance data for 79, but I am not 
			; sure if we reduced it all
			IF keyword_set(bias) THEN BEGIN 
				IF bias LT 150 THEN bias=150
				IF bias GT 200 THEN bias=200
				case bias of
					150: epermV = 337.398
					200: eperMV = 376.924
				endcase
			ENDIF ELSE epermV = 337.398 ;Default to 150mV bias
			
			path2maps = !17179
			DCgain=0.952 ;Verified 3/5/13 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0. 
		end
	354: begin
			detname='H1RG-17354'
			ipcfrac = 0.0102
			; Need to set bias to get epermV, eperADU, or linslope
			; If you don't set a bias, default to 150mV
			; If you set a bias not listed, there are defaults for 
			; that too
			IF keyword_set(bias) THEN BEGIN 
				IF bias LT 150 THEN bias=150
				IF bias GT 350 THEN bias=350

				case bias of
					150: begin
						epermV = 361.642
						linslope = -2.97051e-6
						end
					250: begin
						eperMV =  258.435; most recent (39fF)
						linslope = -1.95772e-6
						end
					350: begin
						eperMV = 312.830
						linslope = -1.34163e-6
						end 
					
					else: begin
					; If bias is between known values, do 
					; an average
					IF bias GT 150 AND bias LT 250 THEN BEGIN
						epermV = (361.642+286.881)/2
						linslope = (-2.97051e-6-1.95772e-6)/2
					ENDIF
					IF bias GT 250 AND bias LT 350 THEN BEGIN
						epermV = (286.881+312.830)/2
						linslope = (-1.95772e-6-1.34163e-6)/2
					ENDIF
					end
				endcase
			ENDIF ELSE BEGIN
				IF n_elements(epermV) EQ 1 OR $
					n_elements(eperADU) EQ 1 THEN $
				print, 'Defaulting to 150mV bias'
				epermV = 361.642
			ENDELSE

			path2maps = !17354
			DCgain=0.942 ;Verified 6/27/13 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 50.e-6 ;m
			ipcmatrix=[[0,0.007,0],[0.013,0.966,0.007],[0,0.007,0]]
		end
	346: begin
			detname='H1RG-17346'
			ipcfrac = 0. ; set to zero for qe b/c cap is reduced already
			ipcreduce = 1.0-(8*0.011)
			IF keyword_set(bias) THEN BEGIN 
				IF bias LT 150 THEN bias=150
				IF bias GT 350 THEN bias=350

				IF keyword_set(region) THEN BEGIN
 
				CASE region OF
					1: begin
						case bias of 
							150:begin
								epermV = 304.646*ipcreduce
								linslope = 0.
								end
							250:begin
								epermV = 292.472 *ipcreduce
								linslope = 0.
								end
							350:begin
								epermV = 289.063*ipcreduce
								linslope = 0.
								end
						endcase
						end
					2:begin
						case bias of 
							150:begin
								epermV = 298.572*ipcreduce
								linslope = 0.
								end
							250:begin
								epermV = 301.281*ipcreduce
								linslope = 0.
								end
							350:begin
								epermV = 322.849*ipcreduce
								linslope = 0.
								end
						endcase
						end
					3:begin
						case bias of 
							150:begin
								epermV = 328.009*ipcreduce
								linslope = -4.746e-6
								end
							250:begin
								epermV = 317.403*ipcreduce
								linslope = -2.763e-6
								end
							350:begin
								epermV = 309.484*ipcreduce
								linslope = -1.874e-6
								end
						endcase
						end
					4:begin
						case bias of 
							150:begin
								epermV = 310.485*ipcreduce
								linslope = -3.237e-6
								end
							250:begin
								epermV = 305.875*ipcreduce
								linslope = -2.044e-6
								end
							350:begin
								epermV = 300.481*ipcreduce
								linslope = -1.321e-6
								end
						endcase
						end
				ENDCASE
				ENDIF ELSE BEGIN

				CASE bias OF
				;linslope is avg of known values.
					150: begin
						epermV = 314.300
						linslope = -3.992e-6
						end
					250: begin
						eperMV = 302.922
						linslope = -2.404e-6
						end
					350: begin
						eperMV = 301.560
						linslope = -1.598e-6
						end 
					else: begin
					; If bias is between known values, do 
					; an average
					IF bias GT 150 AND bias LT 250 THEN BEGIN
						epermV = (314.3+302.922)/2
						linslope = -3.198e-6
					ENDIF
					IF bias GT 250 AND bias LT 350 THEN BEGIN
						epermV = (302.922+301.56)/2
						linslope = -2.001e-6
					ENDIF
					end
				ENDCASE
				ENDELSE
			ENDIF ELSE BEGIN
				IF n_elements(epermV) EQ 1 OR $
					n_elements(eperADU) EQ 1 THEN $
				print, 'Defaulting to 150mV bias'
				epermV = 314.300
				linslope = -3.992e-6
			ENDELSE

			path2maps = !17346
			DCgain=0.945 ;Verified 7/17/13 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.
			;ipcmatrix=0.
		end

	453: begin
			detname='H1RG-17453'
			ipcfrac = 0.0
			IF keyword_set(bias) THEN BEGIN 
				IF bias LT 150 THEN bias=150
				IF bias GT 350 THEN bias=350

				IF keyword_set(region) THEN BEGIN
				; The only reduction done on the capacitance 
				; for this det is for 150mV bias
				CASE region OF
					1: begin
						case bias of 
							150:begin
								epermV = 262.336
								linslope = 0.
								end
							250:begin
								epermV = 292.472 
								linslope = 0.
								end
							350:begin
								epermV = 289.063
								linslope = 0.
								end
						endcase
						end
					2:begin
						case bias of 
							150:begin
								epermV = 288.704
								linslope = 0.
								end
							250:begin
								epermV = 301.281
								linslope = 0.
								end
							350:begin
								epermV = 322.849
								linslope = 0.
								end
						endcase
						end
					3:begin
						case bias of 
							150:begin
								epermV = 313.976
								linslope = -4.746e-6
								end
							250:begin
								epermV = 317.403
								linslope = -2.763e-6
								end
							350:begin
								epermV = 309.484
								linslope = -1.874e-6
								end
						endcase
						end
					4:begin
						case bias of 
							150:begin
								epermV = 319.384
								linslope = -3.237e-6
								end
							250:begin
								epermV = 305.875
								linslope = -2.044e-6
								end
							350:begin
								epermV = 300.481
								linslope = -1.321e-6
								end
						endcase
						end
				ENDCASE
				ENDIF ELSE BEGIN

				PRINT, 'Using average e/mV. '
				PRINT, 'Set /more for info on region=region.'

				CASE bias OF
				;linslope is avg of known values.
					150: begin
						epermV = 296.100
						linslope = -3.992e-6
						end
					250: begin
						eperMV = 302.922
						linslope = -2.404e-6
						end
					350: begin
						eperMV = 301.560
						linslope = -1.598e-6
						end 
					else: begin
					; If bias is between known values, do 
					; an average
					IF bias GT 150 AND bias LT 250 THEN BEGIN
						epermV = (296.1+302.9)/2
						linslope = -3.198e-6
					ENDIF
					IF bias GT 250 AND bias LT 350 THEN BEGIN
						epermV = (302.922+301.56)/2
						linslope = -2.001e-6
					ENDIF
					end
				ENDCASE
				ENDELSE
			ENDIF ELSE BEGIN
				IF n_elements(epermV) EQ 1 OR $
					n_elements(eperADU) EQ 1 THEN $
				print, 'Defaulting to 150mV bias'
				epermV = 296.100
				linslope = -3.992e-6
			ENDELSE

			path2maps = !17453
			DCgain=0.944 ;Verified 11/12/13 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.
			;ipcmatrix=0.
		end

	557: begin
			detname='H1RG-17557'
			ipcfrac = 0.0
				IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 286.104
							linslope = -3.6244e-6
							end
						250: begin
							epermV = 271.768
							linslope = -2.127e-6
							end
						350: begin
							epermV = 253.210
							linslope = -1.375e-6
							end
					ENDCASE
				ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 286.104
					linslope = -3.6244e-6
				ENDELSE

			path2maps = !17557
			DCgain=0.944 ;Verified 12/13/2013 MD
			mux='H1RG'
			refcols=[0,1,2,3, 1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.8e-3 ;800um
	end

	591: begin
			detname='H1RG-17591'
			ipcfrac = 0.011 ;default from 346
			IF keyword_set(bias) THEN BEGIN 
				IF bias LT 150 THEN bias=150
				IF bias GT 350 THEN bias=350

				IF keyword_set(region) THEN BEGIN
				CASE region OF
					1: begin
						case bias of 
							150:begin
								epermV = 301.803
								linslope = -5.03e-06
								end
							250:begin
								epermV = 289.562
								linslope = -3.07e-06
								end
							350:begin
								epermV = 280.951
								linslope = -1.83e-06
								end
						endcase
						end
					2:begin
						case bias of 
							150:begin
								epermV = 311.973
								linslope = -3.73e-06
								end
							250:begin
								epermV = 295.861
								linslope = -2.46e-06
								end
							350:begin
								epermV = 298.295
								linslope = -1.66e-06
								end
						endcase
						end
					3:begin
						case bias of 
							150:begin
								epermV = 270.859
								linslope =  -3.61e-06
								end
							250:begin
								epermV = 253.081
								linslope = -2.60e-06
								end
							350:begin
								epermV = 244.222
								linslope = -1.77e-06
								end
						endcase
						end
					4:begin
						case bias of 
							150:begin
								epermV = 283.445
								linslope = -3.00e-06
								end
							250:begin
								epermV = 261.213
								linslope = -1.70e-06
								end
							350:begin
								epermV = 246.200
								linslope = -9.40e-07
								end
						endcase
						end
				ENDCASE
				ENDIF ELSE BEGIN

				PRINT, 'Using average e/mV. '
				PRINT, 'Set /more for info on region=region.'

				CASE bias OF
				;linslope is avg of known values.
					150: begin
						epermV = 292.02
						linslope = 0.
						end
					250: begin
						eperMV = 273.426
						linslope = 0.
						end
					350: begin
						eperMV = 267.417
						linslope = 0.
						end 
					else: begin
					; If bias is between known values, do 
					; an average
					IF bias GT 150 AND bias LT 250 THEN BEGIN
						epermV = 282.72
						linslope = 0.
					ENDIF
					IF bias GT 250 AND bias LT 350 THEN BEGIN
						epermV = 270.42
						linslope = 0.
					ENDIF
					end
				ENDCASE
				ENDELSE
			ENDIF ELSE BEGIN
				IF n_elements(epermV) EQ 1 OR $
					n_elements(eperADU) EQ 1 THEN $
				print, 'Defaulting to 150mV bias'
				epermV = 292.02
				linslope = 0.
			ENDELSE

			path2maps = !17591
			DCgain=0.944 ; Verified 7/11/14 MD
			mux='H1RG'
			refcols=[0,1,2,3,1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.
			;ipcmatrix=0.
		end
	8228: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18228'
			ipcfrac = 0.012 ; Calculated 10/7/15 MD
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			CASE region OF
				; Left side array
				1: begin
				path2maps = !18228 + 'Leftside_scale_ref/scale-0.95'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; Corrected for IPC 
							epermV =  248.687
							linslope = -3.82e-06
							end
						250: begin
							; Corrected for IPC 
							epermV = 255.451
							linslope = -2.62e-06
							end
						350: begin
							; Corrected for IPC 
							epermV = 252.939
							linslope = -1.55e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 248.687
					linslope = -3.82e-06
					ENDELSE
				end
				; Right side array
				2: begin
				path2maps = !18228 + 'Rightside_scale_ref/scale-0.95'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; Corrected for IPC
							epermV = 257.590  
							linslope = -3.76e-06
							end
						250: begin
							; Corrected for IPC
							epermV = 259.099
							linslope = -2.96e-06
							end
						350: begin
							; Corrected for IPC
							epermV = 255.972
							linslope = -2.27e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 257.590
					linslope = -3.76e-06
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.9 ;Verified 9/18/15 MD
			mux='H2RG'
			;refcols=[0,1,2,3,952,953,954,955]
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 32e-6 ; Verified 12/8/2015 MD
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end
	8381: begin 
			IF arg_present(path2maps) THEN print, 'maps in mario directory'
			detname='H2RG-18381'
			ipcfrac = 0.0
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			CASE region OF
				; Left side array
				1: begin
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; 
							epermV =  0.
							linslope = 0.
							end
						250: begin
							;  
							epermV = 0.
							linslope = 0.
							end
						350: begin
							; 
							epermV = 0.
							linslope = 0.
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 0.
					linslope = 0.
					ENDELSE
				end
				; Right side array
				2: begin
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; 
							epermV = 0.  
							linslope = 0.
							end
						250: begin
							; 
							epermV = 0.
							linslope = 0.
							end
						350: begin
							; 
							epermV = 0.
							linslope = 0.
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 0.
					linslope = 0.
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.9 ;Verified 12/8/15 MD
			mux='H2RG'
			;refcols=[0,1,2,3,952,953,954,955]
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 32e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end
	8367: begin
			detname='H1RG-18367'
			ipcfrac = 0.0
				IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 297.4
							linslope = -1.8e-6
							end
						250: begin
							epermV = 259.1
							linslope = -1.5e-6
							end
						350: begin
							epermV = 241.8
							;linslope = 0.
							end
					ENDCASE
				ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 292.2
					linslope = 0.
				ENDELSE
			DCgain=0.897 ;Verified 2/17/2016 MD
			mux='H1RG'
			refcols=[0,1,2,3, 1020,1021,1022,1023]
			fullarrdims=[1024,1024]
			pixelpitch=1.8e-5 ;m
			subst = 0.8e-3 ;800um
	end

	8470: begin 
			detname='H2RG-18470'
			ipcfrac = 0.0 ;not needed if e/mV is scaled for ipc
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			; All e/mV ipc corrected 
			CASE region OF
				; Left side array
				1: begin
				path2maps = !18470 + 'Leftside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; set to right side b/c
							; bad region dragging 
							; down median
							epermV =  290.997
							linslope = -4.1e-6
							end
						250: begin
							;  
							epermV = 284.113
							linslope = -2.9e-6
							end
						350: begin
							; 
							epermV = 263.104
							linslope = -2.3e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 0.
					linslope = 0.
					ENDELSE
				end
				; Right side array
				2: begin
				path2maps = !18470 + 'Rightside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; 
							epermV = 290.997 
							linslope = -4.1e-6
							end
						250: begin
							; 
							epermV = 284.113
							linslope = -3.0e-6
							end
						350: begin
							; 
							epermV = 263.104
							linslope = -2.5e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 290.997
					linslope = 0.
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.897 ;Verified 3/2/16 MD
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end
	8235: begin 
			detname='H2RG-18235'
			ipcfrac = 0.0 ;not needed if e/mV is scaled for ipc
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			; All e/mV ipc corrected 
			CASE region OF
				; Left side array [IPC=1.27%]
				1: begin
				path2maps = !18235 + 'Leftside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 273.377
							linslope = -4.25e-6
							end
						250: begin
							;  
							epermV = 266.286
							linslope = -2.77e-6
							end
						350: begin
							; 
							epermV = 251.361
							linslope = -2.07e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 273.377
					linslope = -4.25e-6
					ENDELSE
				end
				; Right side array [IPC=1.25%]
				2: begin
				path2maps = !18235 + 'Rightside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							; 
							epermV = 271.203
							linslope = -3.87e-6
							end
						250: begin
							; 
							epermV = 264.903
							linslope = -2.78e-6
							end
						350: begin
							; 
							epermV = 253.461
							linslope = -1.98e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 273.377
					linslope = -4.25e-6
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.9 ;Verified 3/29/16 MD
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end

		8481: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18481'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			CASE region OF
				; Left side array
				1: begin
				path2maps = !18481 + 'Leftside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV =  317.644*ipcreduce
							linslope = -3.48e-6
							end
						250: begin
							epermV = 302.483*ipcreduce
							linslope = -2.83e-6
							end
						350: begin
							epermV = 285.281*ipcreduce
							linslope = -2.48e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 317.644*ipcreduce
					linslope = -3.48e-6
					ENDELSE
				end
				; Right side array
				2: begin
				path2maps = !18481 + 'Rightside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV =  313.905*ipcreduce
							linslope = -3.58e-6
							end
						250: begin
							epermV = 300.047*ipcreduce
							linslope = -2.95e-6
							end
						350: begin
							epermV = 283.52*ipcreduce
							linslope = -2.52e-6
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 313.905*ipcreduce 
					linslope = -3.58e-6
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.894 ;Verified 5/23/16 MD
			mux='H2RG'
			;refcols=[0,1,2,3,952,953,954,955]
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end

		8482: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18482'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			CASE region OF
				; Left side array
				1: begin
				path2maps = !18482 + 'Leftside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 340.118*ipcreduce
							linslope = -3.46e-06
							end
						250: begin
							epermV = 303.888*ipcreduce
							linslope = -2.83e-06
							end
						350: begin
							epermV = 300.492*ipcreduce
							linslope = -2.37e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 340.118*ipcreduce
					linslope = -3.46e-06
					ENDELSE
				end
				; Right side array
				2: begin
				path2maps = !18482 + 'Rightside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 328.201*ipcreduce
							linslope = -3.30e-06
							end
						250: begin
							epermV = 292.593*ipcreduce
							linslope = -2.44e-06
							end
						350: begin
							epermV = 291.884*ipcreduce
							linslope = -1.91e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 328.201*ipcreduce
					linslope = -3.30e-06
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.898 ;Verified 6/7/16 MD
			mux='H2RG'
			;refcols=[0,1,2,3,952,953,954,955]
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end

		8621: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18621'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			IF KEYWORD_SET(region) THEN BEGIN ;region specific terms
			CASE region OF
				; Left side array
				1: begin
				path2maps = !18621 + 'Leftside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 319.254*ipcreduce
							linslope = -3.68e-06
							end
						250: begin
							epermV = 322.860*ipcreduce
							linslope = -3.36e-06
							end
						350: begin
							epermV = 287.349*ipcreduce
							linslope = -2.87e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 319.254*ipcreduce
					linslope = -3.68e-06
					ENDELSE
				end
				; Right side array
				2: begin
				path2maps = !18621 + 'Rightside/'
					IF KEYWORD_SET(bias) THEN BEGIN
					CASE bias OF
						150: begin
							epermV = 323.276*ipcreduce
							linslope = -3.88e-06
							end
						250: begin
							epermV = 290.540*ipcreduce
							linslope = -3.27e-06
							end
						350: begin
							epermV = 290.540*ipcreduce
							linslope = -2.72e-06
							end

					ENDCASE
					ENDIF ELSE BEGIN
					print, 'Defaulting to 150mV bias'
					epermV = 323.276*ipcreduce
					linslope = -3.88e-06
					ENDELSE
				end
				ENDCASE
			ENDIF
			DCgain=0.9 ;Verified by M Cabrera Jul 2016
			mux='H2RG'
			;refcols=[0,1,2,3,952,953,954,955]
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(eperADU) THEN BEGIN
				print, 'Set keyword region for this array if e/ADU needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(epermV) THEN BEGIN
				print, 'Set keyword region for this array if e/mV needed.'
				return
			ENDIF
			IF ~KEYWORD_SET(region) AND ARG_PRESENT(cap) THEN BEGIN
				print, 'Set keyword region for this array if capacitance needed.'
				return
			ENDIF
		end
	8692: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18692'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			path2maps = !18692 + 'scaleref12pct/'
			IF KEYWORD_SET(bias) THEN BEGIN
				CASE bias OF
					150: begin
						epermV = 307.618*ipcreduce
						linslope = -3.4e-06

						end
					250: begin
						epermV = 275.584*ipcreduce
						linslope = -2.0e-06
						end
					350: begin
						epermV = 276.589*ipcreduce
						linslope = -9.6e-07
						end

				ENDCASE
			ENDIF 
			IF ~keyword_set(bias) AND keyword_set(epermV) THEN BEGIN
				print, 'Defaulting to 150mV bias'
				epermV = 307.618*ipcreduce 
				linslope = -3.4e-06
			ENDIF
			DCgain=0.903 ;Verified by M Dorn Oct 2016
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
		end

	8693: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18693'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			path2maps = !18693 + 'scale12pct/'
			IF KEYWORD_SET(bias) THEN BEGIN
				CASE bias OF
					150: begin
						epermV = 287.466*ipcreduce  ; Set to same as 250 mV
													; Bad 150 mV data
													;(FW slip - no signal)
						linslope = -2.6e-6

						end
					250: begin
						epermV = 287.466*ipcreduce
						linslope = -2.3e-6
						end
					350: begin
						epermV = 274.553*ipcreduce
						linslope = -1.95e-6
						end

				ENDCASE
			ENDIF 
			IF ~keyword_set(bias) AND keyword_set(epermV) THEN BEGIN
				print, 'Defaulting to 250mV bias'
				epermV = 276.677*ipcreduce 
				linslope = -2.3e-6
			ENDIF
			DCgain=0.902 ;Verified by M Dorn Oct 2016
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
		end


	8694: begin ; Converstion factors here are IPC corrected
			detname='H2RG-18694'
			ipcfrac = 0.0
			ipcreduce = 1.0-(8*0.011)
			path2maps = !18694 + 'scale12pct/'
			IF KEYWORD_SET(bias) THEN BEGIN
				CASE bias OF
					150: begin
						epermV = 279.898*ipcreduce
						linslope = -3.2e-6 
						end
					250: begin
						epermV = 299.289*ipcreduce
						linslope = -2.2e-6
						end
					350: begin
						epermV = 282.731*ipcreduce
						linslope = -1.8e-6
						end

				ENDCASE
			ENDIF 
			IF ~keyword_set(bias) AND keyword_set(epermV) THEN BEGIN
				print, 'Defaulting to 150mV bias'
				epermV = 279.898*ipcreduce 
				linslope = -3.2e-6 
			ENDIF
			DCgain=0.892 ;Verified by M Dorn Sep 2016
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
			subst = 30e-6 
		end

	160: begin
			detname='FIRE-Acq-H2RG-160'
			;eperADU=1.76
			epermV=278.0
			DCgain = 0.95
			mux='H2RG'
			refcols=[0,1,2,3,2044,2045,2046,2047]
			fullarrdims=[2048,2048]
			pixelpitch=1.8e-5 ;m
		end
	else: begin
		print, "***That is not a valid choice.  Current detector choices are:"
		print, "   1 for 'H1RG-16-001', "
		print, "   2 for 'H1RG-16-002', "
		print, "   3 for 'H1RG-16-003', "
		print, "   110 for 'H1RG-110',  "
		print, "   160 for 'FIRE-Acq-H2RG-160' "
		print, "   85 for 'H1RG-16885', "
		print, "   86 for 'H1RG-16886', "
		print, "   87 for 'H1RG-16887', "
		print, "   79 for 'H1RG-17179', "
		print, "   354 for 'H1RG-17354', "
		print, "   346 for 'H1RG-17346', "
		print, "   453 for 'H1RG-17453', "
		print, "   557 for 'H1RG-17557', "
		print, "   591 for 'H1RG-17591', "
		print, "   8228 for 'H2RG-18228', "
		print, "   8235 for 'H2RG-18235', "
		print, "   8381 for 'H2RG-18381', "
		print, "   8367 for 'H1RG-18367', "
		print, "   8470 for 'H2RG-18470',  "
		print, "   8481 for 'H2RG-18481', "
		print, "   8482 for 'H2RG-18482', "
		print, "   8621 for 'H2RG-18621', " 
		print, "   8692 for 'H2RG-18692', "
		print, "   8693 for 'H2RG-18693', or "
		print, "   8694 for 'H2RG-18694' "
		print, "   NOTE: Detector serial numbers are case sensitive."
		print, "   Is your detector amongst these choices?"
		goahead, retry, /yes1
		if retry eq 1 then begin
			print, "Please enter your detector choice."
			read, det, prompt='> '
			GOTO, RETRYDETNUM
		endif else begin
			print, "***Edit detinfo.pro under ~/idlpro/det/information/ to contain your"
			print, "   detector information and try again."
			retall
		endelse
	end
endcase

IF KEYWORD_SET(acqbox) THEN BEGIN
	IF arg_present(eperADU) EQ 1 OR arg_present(cap) EQ 1 THEN BEGIN
		eperADU=epermV*mVperADU 
		q=1.602*10^(-4.)
		mVperV=1000.
		cap = q*DCgain*eperADU*mVperV/mVperADU
	ENDIF
ENDIF ELSE BEGIN
	IF ARG_PRESENT(eperADU) OR ARG_PRESENT(cap) THEN BEGIN
		print, "Cannot return e/ADU or capacitance without knowing the "
		print, "data acquisition box."
	ENDIF
ENDELSE


END
