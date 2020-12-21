; bw2rgb.pro,  started 6-20-98 by ERW
;
; see http://www.physics.emory.edu/~weeks/idl/mkpov.html

function bw2rgb,grey,noscale=noscale
; grey is an array of gray scale colors
; returns a byte array, 3 x N
; /noscale indicates grey values are between 0 and 1, and don't need
; to be scaled (default rescales grey array to floats between 0 and 1)
;
; the mapping from grey to colors seems to work, historically speaking...
; (this is a translation from an awk script)


if keyword_set(noscale) then begin
	red=255b-byte(204.0*grey)
	green=byte(255.0*sin(grey*3.1))
	blue=128b-byte(128.0*cos(grey*3.2))
endif else begin
	maxg=max(grey)
	ming=min(grey)
	if maxg ne ming then begin
		scaleg=1.0/(maxg-ming)
	endif else begin
		message,'warning: all the same color, unpredicatable results.',/inf
		scaleg=1.0
	endelse
	red=255b-byte(204.0*(grey-ming)*scaleg)
	green=byte(255.0*sin((grey-ming)*scaleg*3.1))
	blue=128b-byte(128.0*cos((grey-ming)*scaleg*3.2))
endelse

; this helps solve problems if 'grey' is higher dimensional
; (although it doesn't actually do what we'd like, which is to
; generate a 3 x whatever x whatever... array.)
result=fltarr(3,n_elements(grey))
result(0,*)=red/255.0
result(1,*)=green/255.0
result(2,*)=blue/255.0

return,result
end

