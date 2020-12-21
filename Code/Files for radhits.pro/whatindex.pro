PRO whatindex, ncol, coordinates, output, more=more

if keyword_set(more) then begin
	doc_library, 'whatindex'
	return
endif


;+
; Name: 
;	WHATINDEX
;
; Purpose:
;	Return 1D Index corresponding to 2D coordinate from array.
;
; Input:
;	ncol - number of columns in 2D array
;	coordinates - [x,y] coordinates. May be 2xN array
;
; Output:
;	output - index or indices if multiple x and y values are input
;       
;-

; Print syntax if no arguments are provided 
IF n_params() LT 1 THEN BEGIN
	print, "Syntax: "
	print, "       WHATINDEX, ncol, coordinates, [output, /more] "
	return
ENDIF

; Make sure there is enough precision
coordinates = double(coordinates)
ncol = double(ncol)

; Get dimensions
getdimsfarr, coordinates, nc, nr

; Create array to output index (or indices)
output = dblarr(nr)

FOR i=0, nr-1 DO BEGIN
	output[i] = round(coordinates[0,i] + (coordinates[1,i]*ncol))
ENDFOR

END

