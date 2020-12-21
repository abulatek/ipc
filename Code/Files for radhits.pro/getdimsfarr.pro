pro getdimsfarr, array, ncol, nrow, nfiles, x1, x2, x3, more=more, tree=tree

if n_params() lt 1 AND ~keyword_set(more) AND ~keyword_set(tree) then begin
	print, "	SYNTAX: "
	print, "		GETDIMSFARR, array, [ncol, nrow, nfiles, x1, x2, x3, /more, /tree]"
	return
endif

if keyword_set(more) then begin
	doc_library, 'getdimsfarr'
	return
endif

;+
; NAME:
;		GETDIMSFARR
; PURPOSE:
;		The purpose of the procedure is to get the dimensions from a specified array.  
;		Particularly useful when called from another procedure.
; SYNTAX:
;		GETDIMSFARR, array, [ncol, nrow, nfiles, x1, x2, x3, /more, /tree]
; INPUTS:
;		array - The array from which the dimensions are needed.
; OUTPUTS:
;		ncol - The number of columns (Dimension #1).
;		nrow - The number of rows (Dimension #2).
;		nfiles - The number of files (Dimension #3).
;		x1 - Dimension #4.
;		x2 - Dimension #5.
;		x3 - Dimension #6.
; KEYWORDS:
;		
;		/more - Generates this list.
;		/tree - Prints a file tree to the screen.
; PROCEDURES CALLED:
;		
; HISTORY:
;		
;-

if keyword_set(tree) eq 1 then begin
		if tree eq 1 then print, "-->getdimsfarr"
		if tree gt 1 then begin
			for i=1, tree - 1 do print, format='($, "      ")'
			print, "'->getdimsfarr"
		endif
		return
endif

dimensions=size(array,/dimensions)

if dimensions(0) eq 0 then ncol=1 else ncol=dimensions(0)
if n_elements(dimensions) ge 2 then nrow=dimensions(1) else nrow=1
if n_elements(dimensions) ge 3 then nfiles=dimensions(2) else nfiles=1
if n_elements(dimensions) ge 4 then x1=dimensions(3) else x1=1
if n_elements(dimensions) ge 5 then x1=dimensions(4) else x2=1
if n_elements(dimensions) ge 6 then x1=dimensions(5) else x3=1


end
