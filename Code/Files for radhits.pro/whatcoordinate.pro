PRO whatcoordinate, ncol, indices, output, more=more

IF keyword_set(more) THEN BEGIN
	doc_library, 'whatcoordinate'
	return
ENDIF

;+
; Name: 
;	WHATCOORDINATE
;
; Purpose:
;	Return 2D subscripts of an index or a set of indexes. Note that the 
;	subscripts are based at [0,0].
;
; Input:
;	ncol - number of columns in 2D array
;	indexes - index or indexes of interest
;
; Output:
;	
;
;
; Example:
;	you have a 1024x1024 image and want to know the subscripts of the index
;	value 10000
;
;	IDL>whatcoordinate, 1024, 10000, output
;	IDL>print, output
;	    784       9
;-

; Print syntax if no arguments are provided 
IF n_params() LT 1 THEN BEGIN
	print, "Syntax: "
	print, "        WHATCOORDINATE, ncol, indices, output "
	return
ENDIF

; Make sure the values have enough precision
;ncol = float(ncol)
;indices = float(indices)

; Size of input 
N = n_elements(indices)

; Array to output 2D row and column *subscripts*
output = intarr(2, N)

FOR i=0, N-1 DO BEGIN
	output[*,i] = [(indices[i] MOD ncol),(indices[i] / ncol)]
ENDFOR

END
