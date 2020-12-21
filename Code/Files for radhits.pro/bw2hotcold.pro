; bw2hotcold.pro,  started 6-20-98 by ERW
;
; see http://www.physics.emory.edu/~weeks/idl/mkpov.html

function bw2hotcold,grey,noscale=noscale
; grey is an array of gray scale colors
; returns a byte array, 3 x N
; /noscale indicates grey values are between 0 and 1, and don't need
; to be scaled (default rescales grey array to floats between 0 and 1)
; low values = cold, high values = hot
;
; the mapping from grey to colors seems to work, historically speaking...
; (this is a translation from an awk script)


if keyword_set(noscale) then begin
	red=128b-byte(128.0*cos(grey*3.2))
	green=bytarr(n_elements(grey))
	blue=255b-byte(204.0*grey)
endif else begin
	maxg=max(grey)
	ming=min(grey)
	if maxg ne ming then begin
		scaleg=1.0/(maxg-ming)
	endif else begin
		message,'warning: all the same color, unpredicatable results.',/inf
		scaleg=1.0
	endelse
	red=128b-byte(128.0*cos((grey-ming)*scaleg*3.2))
	green=bytarr(n_elements((grey-ming)))
	blue=255b-byte(204.0*(grey-ming)*scaleg)
endelse

result=fltarr(3,n_elements(grey))
result(0,*)=red/255.0
result(1,*)=green/255.0
result(2,*)=blue/255.0

return,result
end

