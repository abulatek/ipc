pro refmap, inarraywithrefs, outarrayrefsub, referencemap, filtval=filtval, plaid=plaid, $
	sutr=sutr, full=full, sepoutputs=sepoutputs,nosmooth=nosmooth, det=det, $
	replace=replace, more=more, tree=tree, left=left, right=right, refcols=refcols, $
	refrows=refrows, wherereg=wherereg, factor=factor, acqbox=acqbox
	
if n_params() LT 1 AND ~keyword_set(more) AND ~keyword_set(tree) then begin
	print, "	SYNTAX: "
	print, "		REFMAP, inarraywithrefs[, outarrayrefsub, referencemap,"
	print, "		filtval=[3,3,0,4], /plaid, /sutr, /full, /sepoutputs,"
	print, "		/nosmooth, /replace, /left, /right, refcols=refcols, det=det,"
	print, "		refrows=refrows, wherereg=wherereg, factor=factor, /more, /tree]"
	return
endif

if keyword_set(more) then begin
	doc_library, 'refmap'
	return
endif

;+
; NAME:
;		REFMAP
; PURPOSE:
;		This procedure creates a reference pixel map to be used in reference subtraction.
; SYNTAX:
;		REFMAP, inarraywithrefs[, outarrayrefsub, referencemap, filtval=[3,3,0,4], /plaid, 
;		/sutr, /full, /sepoutputs, /nosmooth, /replace, /left, /right, refcols=refcols, 
;		det=det, refrows=refrows, wherereg=wherereg, factor=factor, /more, /tree]
; INPUTS:
;		inarraywithrefs - Array from which the reference map is to be generated.  Must 
;			contain reference pixels or use keyword /full to use all pixels.
; OUTPUTS:
;		outarrayrefsub - Array reference subtracted. 
;		referencemap - Array of the same dimensions as the input array containing 
;			the reference pixel drift value.  Unless keywords are used, each row contains 
;			the same value.
; KEYWORDS:
;		filtval=[3,3,0,4] - Values used in savgol smoothing routine.  Default is [3,3,0,4].
;		/sepoutputs - Splits the separate outputs on the detector.
;			Assumes two outputs split amongst columns unless keyword plaid is set.  
;			Then assumes quad outputs.
;		/plaid - Uses both column and row reference pixels: NOT RECOMMENDED
;		/sutr - Set keyword if data are SUTR
;		/full - Uses all pixels for the reference map. If set, keywords refcols, left and 
;			right are ignored.
;		/nosmooth - Uses raw values averaged across rows and does not smooth data from 
;			row to row.
;		/replace - Replace inarraywithrefs with outarrayrefsub.
;			If this keyword is set, referencemap will not be returned.
;		/left - Uses only reference columns on the left side of the array.
;		/right - Uses only reference columns on the right side of the array.
;		refcols=refcols - Set to an array containing the reference column numbers to use 
;			a user defined set of reference columns.
;		refrows=refrows - Set to an array containing the reference row numbers to use a 
;			user defined set of reference rows.
;		det=det - Alternately (to refcols), you can specify the detector and the 
;			reference columns will be obtained from detinfo.
;		wherereg=wherereg - If set to a list of indexes where new data sets begin, 
;			procedure will truncating refmapping at those indices.
;		factor=factor - Multiplicative capacitance scale factor by which to 
;			reduce the amount you are reference subtracting. 
;		/more - Generates this list.
;		/tree - Prints a file tree to the screen.
; NOTES:
;		As of 7/18/2012 this program can only handle 8 individual SUTR groups (in keyword 
;			wherereg).
;		Keyword /nofirstcol is obsolete.  If you don't want to use the first column of
;			reference pixels, use keyword(s) refcols (& refrows)
; PROCEDURES CALLED:
;		GOAHEAD, GETDIMSFARR, DETINFO, GETREFINFO
; HISTORY:
;		Written by C. Bacon
;		7/10/2007 - CMF - Keyword /split replaced with /sepoutputs
;		5/13/2009 - CMF - Added left / right keywords to use when reading out only the 
;			left or right hand side of the array.  Plaid keyword automatically set to zero 
;			when left / right keywords are set.
;		5/23/2012 - CMF - Added keyword refcols and refrows and streamlined syntax.
;		5/15/2013 - MD - Added factor=factor keyword.  
;-

if keyword_set(tree) eq 1 then begin
		if tree eq 1 then print, "-->refmap"
		if tree gt 1 then begin
			for i=1, tree - 1 do print, format='($, "      ")'
			print, "'->refmap"
		endif
			tree=tree + 1
			goahead, tree=tree
			getdimsfarr, tree=tree
			detinfo, tree=tree
			getrefinfo, tree=tree
			tree=tree - 1
		return
endif

;another idea on this is to take the left-hand side and the right-hand side and 
;make a gradient from one side to the other, then try that with the outputs 
;split...

if n_params() eq 1 and keyword_set(replace) eq 0 then begin
	print, "You only gave an input array.  Would you like it replaced "
	print, "with the reference map subtracted array?"
	goahead, replace, /yes1
	if replace eq 0 then begin
		print, "Nothing to do... returning..."
		return
	endif
endif

IF ~keyword_set(factor) THEN factor=1.0

getdimsfarr, inarraywithrefs, ncol, nrow, narrays

if n_elements(wherereg) gt 0 then begin
	;If we are here, then we want to break the array into pieces to do this for the 
	;individual SUTRs separately.
endif

;Procedure does not use reference columns when using the full array to reference map.

if n_elements(refcols) eq 0 AND keyword_set(full) eq 0 then begin
	detinfo, det, refcols=refcols, fullarrdims=fullarrdims, acqbox=acqbox
	if keyword_set(left) then getrefinfo, refcols, fullarrdims, left=usecols
	if keyword_set(right) then getrefinfo, refcols, fullarrdims, right=usecols
	if n_elements(usecols) gt 0 then refcols=temporary(usecols)
endif

;Compare reference columns to array being refmapped to make sure we can index!
if keyword_set(full) eq 0 then begin
	if refcols[n_elements(refcols)-1]+1 gt ncol then begin
		;reference column greater than the number of columns on the array!
		print, "Cannot use the specified refcols because one or more of them is not on the array."
		print, "Redefine refcols in keyword and try again."
		return
	endif
	if keyword_set(plaid) then begin
		if n_elements(refrows) eq 0 then refrows=refcols
		if refrows[n_elements(refrows)-1]+1 gt nrow then begin
			;reference row greater than the number of rows on the array!
			print, "Cannot use the specified refrows because one or more of them is not on the array."
			print, "Redefine refrows in keyword and try again."
			return
		endif 
	endif
endif


if keyword_set(filtval) then begin
	savgolfilter=savgol(filtval(0),filtval(1),filtval(2),filtval(3))
endif else begin
	savgolfilter=savgol(3,3,0,4)
endelse
;filter coefficients, 3 to the left, 3 to the right, order 0, degree 4
;for use with convol

;SPLIT ARRAY HERE
if n_elements(wherereg) gt 1 then begin
	;Here adding an index that will correspond to the last frame in the array
	nsutrdata=intarr(n_elements(wherereg)+1)
	nsutrdata[0:n_elements(wherereg)-1]=wherereg
	nsutrdata[n_elements(wherereg)]=narrays
	
	;break array into individual arrays to referencemap
	switch n_elements(nsutrdata) of
		9: splitarr8=inarraywithrefs[*,*,nsutrdata[7]:nsutrdata[8]-1]
		8: splitarr7=inarraywithrefs[*,*,nsutrdata[6]:nsutrdata[7]-1]
		7: splitarr6=inarraywithrefs[*,*,nsutrdata[5]:nsutrdata[6]-1]
		6: splitarr5=inarraywithrefs[*,*,nsutrdata[4]:nsutrdata[5]-1]
		5: splitarr4=inarraywithrefs[*,*,nsutrdata[3]:nsutrdata[4]-1]
		4: splitarr3=inarraywithrefs[*,*,nsutrdata[2]:nsutrdata[3]-1]
		3: splitarr2=inarraywithrefs[*,*,nsutrdata[1]:nsutrdata[2]-1]
		2: begin & splitarr1=inarraywithrefs[*,*,nsutrdata[0]:nsutrdata[1]-1] & break & end
		else: begin
			help, splitarr1, splitarr2, splitarr3
			print, "Cannot accommodate that many SUTR groupings.  If more are desired, edit this program."
			return
		end
	endswitch
	delvarx, inarraywithrefs; Delete it here to save memory.  We will reconstruct it later.
	startwithgroup=0
endif
		
STARTAGAIN: 

if n_elements(wherereg) gt 1 then begin
	case startwithgroup of 
		0: inarraywithrefs=splitarr1
		1: inarraywithrefs=splitarr2
		2: inarraywithrefs=splitarr3
		3: inarraywithrefs=splitarr4
		4: inarraywithrefs=splitarr5
		5: inarraywithrefs=splitarr6
		6: inarraywithrefs=splitarr7
		7: inarraywithrefs=splitarr8
		else: begin
			print, "If you edited the number of SUTR groupings, you also need to edit the startwithgroup list."
			return
		end
	endcase
	getdimsfarr, inarraywithrefs, ncol, nrow, narrays 
	; Called this because program needs narrays and narrays is different for a SUTR group.
endif

if keyword_set(replace) eq 0 then referencemap=fltarr(ncol,nrow,narrays)
;workingarray=inarraywithrefs

if keyword_set(sutr) then begin
	zeroframe=inarraywithrefs(0:ncol-1,0:nrow-1,0)
	;this is going to make it so the noise reduction is only performed on the 
	;subtraction of images, since it wasn't working on a regular frame because of
	;the offset variation from pixel to pixel.  so instead, I'm going to subtract
	;frame #1 from all the frames and then the map will be made on the differences
	;inarraywithrefs(0:ncol-1,0:nrow-1,*)=inarraywithrefs(0:ncol-1,0:nrow-1,*)-zeroframe
	for l=0,narrays-1 do begin
		inarraywithrefs(0:ncol-1,0:nrow-1,l)=$
			inarraywithrefs(0:ncol-1,0:nrow-1,l)-zeroframe
	endfor
endif

if keyword_set(full) then begin
	fluctsrow=fltarr(nrow)
	if keyword_set(sepoutputs) then begin
		fluctsrow1=fluctsrow
		fluctsrow2=temporary(fluctsrow)
		for l=1,narrays-1 do begin
			for j=0,nrow-1 do begin
				fluctsrow1(j)=median(inarraywithrefs(0:ncol/2-1,j,l))
				fluctsrow2(j)=median(inarraywithrefs(ncol/2:ncol-1,j,l))
			endfor
			if keyword_set(nosmooth) eq 0 then begin
				smoothedrow1=convol(fluctsrow1,savgolfilter,/edge_truncate)
				smoothedrow2=convol(fluctsrow2,savgolfilter,/edge_truncate)
				if keyword_set(replace) then begin
					for k=0,ncol/2-1 do inarraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-smoothedrow1
					for k=ncol/2,ncol-1 do inarraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-smoothedrow2
				endif else begin
					for k=0,ncol/2-1 do referencemap(k,*,l)=smoothedrow1
					for k=ncol/2,ncol-1 do referencemap(k,*,l)=smoothedrow2
				endelse
			endif else begin
				if keyword_set(replace) then begin
					for k=0,ncol/2-1 do inarraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-fluctsrow1
					for k=ncol/2,ncol-1 do narraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-fluctsrow2
				endif else begin
					for k=0,ncol/2-1 do referencemap(k,*,l)=fluctsrow1
					for k=ncol/2,ncol-1 do referencemap(k,*,l)=fluctsrow2
				endelse
			endelse
		endfor ;l, narrays
	endif else begin
		for l=1,narrays-1 do begin
			for j=0,nrow-1 do fluctsrow(j)=median(inarraywithrefs(0:ncol-1,j,l))
			if keyword_set(nosmooth) eq 0 then begin
				smoothedrow=convol(fluctsrow,savgolfilter,/edge_truncate)
				if keyword_set(replace) then begin
					for k=0,ncol-1 do inarraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-smoothedrow
				endif else begin
					for k=0,ncol-1 do referencemap(k,*,l)=smoothedrow
				endelse
			endif else begin
				if keyword_set(replace) then begin
					for k=0,ncol-1 do inarraywithrefs(k,*,l)=$
						inarraywithrefs(k,*,l)-fluctsrow
				endif else begin
					for k=0,ncol-1 do referencemap(k,*,l)=fluctsrow
				endelse
			endelse
		endfor ;l, narrays
	endelse
endif else begin; keyword_set(full) 

if keyword_set(sepoutputs) then begin
	outputs=2 ; assumption of two outputs
	refcolsout1=reform(refcols[0:n_elements(refcols)/2-1])
	refcolsout2=reform(refcols[n_elements(refcols)/2:n_elements(refcols)-1])

	;reference rows all in all
	;If keyword_set(plaid) assumption is quad outputs ; bad assumption?
	if keyword_set(plaid) then begin
		refrowsout1=reform(refrows[0:n_elements(refrows)/2-1])
		refrowsout2=reform(refrows[n_elements(refrows)/2:n_elements(refrows)-1])
	endif
endif

for l=0,narrays-1 do begin
if keyword_set(plaid) then fluctsrow=fltarr(ncol); yes, redefine for each array
fluctscol=fltarr(nrow)

;these are correct.  the fluctuations in the reference columns are from row to row, so 
;smoothedcol should be indexed by a row.
if keyword_set(sepoutputs) eq 0 then begin
	;If plaid keyword set, then we need column to column fluctuations.  Otherwise we do not.
	if keyword_set(plaid) then begin
		for m=0,ncol-1 do begin	
			fluctsrow[m]=mean(inarraywithrefs[m,refrows,l])
		endfor
	endif
	for m=0,nrow-1 do begin
		fluctscol[m]=mean(inarraywithrefs[refcols,m,l])
	endfor	
	smoothedcol=convol(fluctscol,savgolfilter,/edge_truncate)
	if keyword_set(plaid) then smoothedrow=convol(fluctsrow,savgolfilter,/edge_truncate)
	for k=0,ncol-1 do begin
	for j=0,nrow-1 do begin
		if keyword_set(plaid) eq 0 then begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-(factor*smoothedcol(j))
			endif else begin
				referencemap(k,j,l)=smoothedcol(j)
			endelse
		endif else begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-$
					(((smoothedrow(k)+smoothedcol(j))/2.)*factor)
			endif else begin
				referencemap(k,j,l)=(smoothedrow(k)+smoothedcol(j))/2.
			endelse
		endelse
	endfor
	endfor


endif else begin; keyword_set(sepoutputs) eq 1 so here we are dealing with outputs separated
	if keyword_set(plaid) then begin
		fluctsrowout1=fltarr(ncol/2)
		fluctsrowout2=fltarr(ncol/2)
		for m=0,ncol/2-1 do begin	;For quad outputs...  Assume plaid = quad outputs? bad assumption?
			fluctsrowout1[m]=mean(inarraywithrefs[m,refrowsout1,l])
			fluctsrowout2[m]=mean(inarraywithrefs[m+ncol/2,refrowsout2,l])
		endfor
		smoothedrowout1=convol(fluctsrowout1,savgolfilter,/edge_truncate)
		smoothedrowout2=convol(fluctsrowout2,savgolfilter,/edge_truncate)
	endif
	fluctscolout1=fltarr(ncol)
	fluctscolout2=fltarr(ncol)
	for m=0,nrow-1 do begin
		fluctscolout1[m]=mean(inarraywithrefs[refcolsout1,m,l])
		fluctscolout2[m]=mean(inarraywithrefs[refcolsout2,m+nrow/2,l])
	endfor	
	smoothedcolout1=convol(fluctscolout1,savgolfilter,/edge_truncate)
	smoothedcolout2=convol(fluctscolout2,savgolfilter,/edge_truncate)
	
	for j=0,nrow-1 do begin
	for k=0,ncol/2-1 do begin	;output1
		if keyword_set(plaid) eq 0 then begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-$
					(factor*smoothedcolout1(j))
			endif else begin
				referencemap(k,j,l)=smoothedcolout1(j)
			endelse
		endif else begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-$
					(factor*((smoothedrowout1(k)+smoothedcolout1(j))/2.))
			endif else begin
				referencemap(k,j,l)=(smoothedrowout1(k)+smoothedcolout1(j))/2.
			endelse
		endelse
	endfor
	for k=ncol/2,ncol-1 do begin	;output2
		if keyword_set(plaid) eq 0 then begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-$
					(factor*smoothedcolout2(j))
			endif else begin
				referencemap(k,j,l)=smoothedcolout2(j)
			endelse
		endif else begin
			if keyword_set(replace) then begin
				inarraywithrefs(k,j,l)=inarraywithrefs(k,j,l)-$
					(factor*((smoothedrowout2(k-ncol/2)+smoothedcolout2(j))/2.))
			endif else begin
				referencemap(k,j,l)=(smoothedrowout2(k-ncol/2)+smoothedcolout2(j))/2.
			endelse
		endelse
	endfor
	endfor
	

endelse

endfor ;l

endelse ; keyword full
if keyword_set(sutr) then for l=0,narrays-1 do begin
	inarraywithrefs(0:ncol-1,0:nrow-1,l)=inarraywithrefs(0:ncol-1,0:nrow-1,l)+zeroframe
endfor

if n_elements(wherereg) gt 1 then begin
	;save referenced / referencemap
	;if keyword replace is set, then inarraywithrefs was overwritten, so save that.
	;Otherwise, splitarr is still defined, so no need to write it back.
	if keyword_set(replace) then begin
		case startwithgroup of 
			0: splitarr1=temporary(inarraywithrefs)
			1: splitarr2=temporary(inarraywithrefs)
			2: splitarr3=temporary(inarraywithrefs)
			3: splitarr4=temporary(inarraywithrefs)
			4: splitarr5=temporary(inarraywithrefs)
			5: splitarr6=temporary(inarraywithrefs)
			6: splitarr7=temporary(inarraywithrefs)
			7: splitarr8=temporary(inarraywithrefs)
			else: begin
				print, "If you edited the number of SUTR groupings, you also need to edit the startwithgroup list."
				return
			end
		endcase
	endif
	
	;Also save referencemap
		case startwithgroup of 
			0: refarr1=temporary(referencemap)
			1: refarr2=temporary(referencemap)
			2: refarr3=temporary(referencemap)
			3: refarr4=temporary(referencemap)
			4: refarr5=temporary(referencemap)
			5: refarr6=temporary(referencemap)
			6: refarr7=temporary(referencemap)
			7: refarr8=temporary(referencemap)
			else: begin
				print, "If you edited the number of SUTR groupings, you also need to edit the save referencemap list."
				return
			end
		endcase
		
	if startwithgroup lt n_elements(wherereg)-1 then begin
		;More to go.  Start again.
		startwithgroup=temporary(startwithgroup)+1
		GOTO, STARTAGAIN
	endif
	
	;If we got past the last statement then we didn't start again, so we must be done!
	;Put stuff back together now.
	
	inarraywithrefs=fltarr(ncol,nrow,nsutrdata[n_elements(wherereg)],/NOZERO)
	referencemap=fltarr(ncol,nrow,nsutrdata[n_elements(wherereg)],/nozero)
	switch n_elements(nsutrdata) of
		9: begin & inarraywithrefs[*,*,nsutrdata[7]:nsutrdata[8]-1]=temporary(splitarr8) & $
			referencemap[*,*,nsutrdata[7]:nsutrdata[8]-1]=temporary(refarr8) & end
		8: begin & inarraywithrefs[*,*,nsutrdata[6]:nsutrdata[7]-1]=temporary(splitarr7) & $
			referencemap[*,*,nsutrdata[6]:nsutrdata[7]-1]=temporary(refarr7) & end
		7: begin & inarraywithrefs[*,*,nsutrdata[5]:nsutrdata[6]-1]=temporary(splitarr6) & $
			referencemap[*,*,nsutrdata[5]:nsutrdata[6]-1]=temporary(refarr6) & end
		6: begin & inarraywithrefs[*,*,nsutrdata[4]:nsutrdata[5]-1]=temporary(splitarr5) & $
			referencemap[*,*,nsutrdata[4]:nsutrdata[5]-1]=temporary(refarr5) & end
		5: begin & inarraywithrefs[*,*,nsutrdata[3]:nsutrdata[4]-1]=temporary(splitarr4) & $
			referencemap[*,*,nsutrdata[3]:nsutrdata[4]-1]=temporary(refarr4) & end
		4: begin & inarraywithrefs[*,*,nsutrdata[2]:nsutrdata[3]-1]=temporary(splitarr3) & $
			referencemap[*,*,nsutrdata[2]:nsutrdata[3]-1]=temporary(refarr3) & end
		3: begin & inarraywithrefs[*,*,nsutrdata[1]:nsutrdata[2]-1]=temporary(splitarr2) & $
			referencemap[*,*,nsutrdata[1]:nsutrdata[2]-1]=temporary(refarr2) & end
		2: begin & inarraywithrefs[*,*,nsutrdata[0]:nsutrdata[1]-1]=temporary(splitarr1) & $
			referencemap[*,*,nsutrdata[0]:nsutrdata[1]-1]=temporary(refarr1) & break & end
		else: begin
			print, "If you edited the number of SUTR groupings, you also need to edit the replacement lists."
			return
		end
	endswitch
	;Now inarraywithrefs and referencemap should be back to their original size.
endif

;Replace after fully generating reference map.
if keyword_set(replace) eq 0 then outarrayrefsub=inarraywithrefs-(referencemap*factor)

end
