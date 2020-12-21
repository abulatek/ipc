pro boxinfo, acqbox, mVperADU, more=more, tree=tree

if n_params() LT 1 AND ~keyword_set(more) AND ~keyword_set(tree) then begin
	print, "	SYNTAX: "
	print, "		BOXINFO, acqbox, mVperADU, [/more, /tree]"
	return
endif

if keyword_set(more) then begin
	doc_library, 'boxinfo'
	return
endif

;+
; NAME:
;		BOXINFO
; PURPOSE:
;		This procedure provides data acquisition box specific information to be used in 
;		data reduction.
; SYNTAX:
;		BOXINFO, acqbox, mVperADU, [/more, /tree]
; INPUTS:
;		acqbox - Name of the data acquisition box.  If unknown, enter 'list' to see choices.
; OUTPUTS:
;		mVperADU - Conversion factor between mV and ADU for the set acquisition box.
; KEYWORDS:
;		/more - Generates this list.
;		/tree - Prints a file tree to the screen.
; PROCEDURES CALLED:
;		DEFAULT
; HISTORY:
;		Written by C. Bacon
;		7/6/2012 - CMF - Distinguished four channel and two channel black boxes.  Added 
;			error prompt if box not recognized.
;		7/13/2012 - MD - Added black4gain25 and black4gain5 in response to the change in 
;			gain.
;		7/16/2012 - KGA - Changed instances of box to acqbox.
;		8/8/12 - MD - added black4gain11
;		6/11/2013 - CMF - Corrected syntax error in call and added 'list' option to 
;			acqbox.
;-

if keyword_set(tree) then begin
		if tree eq 1 then print, "-->boxinfo"
		if tree gt 1 then begin
			for i=1, tree - 1 do print, format='($, "      ")'
			print, "'->boxinfo"
		endif
		tree = tree + 1
			default, tree=tree
		tree = tree - 1
		return
endif

if n_elements(acqbox) eq 0 then default, acqbox=acqbox

;mVperADU is calculated by voltage swing (in mV) / (pre-amp gain * dynamic range (ADU)) 

RETRYBOX:
case acqbox of
	"silver": begin
			mVperADU=10./2l^15*10l^3
		end
	"black2": begin
			mVperADU=5.0*1.037/25./2l^15*10l^3 ; 0.00633
		end
	"black4gain25": begin
			mVperADU=2.5/25./2l^15*10l^3
		end
	"black4gain5": begin
			mVperADU=2.5/5./2l^15*10l^3
		end
	"black4gain11": begin
			mVperADU=2.5/10.975/2l^15*10l^3
		end
	else: begin
		if acqbox eq 'list' then print, "Current data acquisition box choices are:" else $
		print, "***That is not a valid choice.  Current data acquisition box choices are:"
		print, "   'silver', 'black2', 'black4gain5', 'black4gain25', and 'black4gain11'."
		print, "   NOTE: Box codes are case sensitive."
		if acqbox eq 'list' then return
		print, "   Is your box amongst these choices?"
		goahead, retry, /yes1
		if retry eq 1 then begin
			print, "Please enter your box choice without quotes."
			read, acqbox, prompt='> '
			GOTO, RETRYBOX
		endif else begin
			print, "***Update boxinfo.pro under ~/idlpro/det/information/ to contain the"
			print, "   parameters for your new data acquisition box and try again."
			retall
		endelse
		end
endcase

end
