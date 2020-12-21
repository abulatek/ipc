; bw2rgb.pro,  started 6-20-98 by ERW
;
; converts a greyscale value into red/green/blue colors
; see http://www.physics.emory.edu/~weeks/idl/mkpov.html


function bw2rgb2,grey,noscale=noscale
; grey is an array of gray scale colors
; returns a floating point array, 3 x N
; /noscale indicates grey values are between 0 and 1, and don't need
; to be scaled (default rescales grey array to floats between 0 and 1)
; 
;

g=bytarr(256)
g(97:162)=255b
g(97-64:96)=indgen(64)*4
g(163:163+63)=252b-indgen(64)*4
r=shift(g,66)
b=shift(g,-66)

if keyword_set(noscale) then begin
	red=interpolate(r,255.99*grey)
	green=interpolate(g,255.99*grey)
	blue=interpolate(b,255.99*grey)
endif else begin
	maxg=max(grey)
	ming=min(grey)
	if maxg ne ming then begin
		scaleg=1.0/(maxg-ming)
	endif else begin
		message,'warning: all the same color, unpredictable results.',/inf
		scaleg=1.0
	endelse
	i=(255.99*(grey-ming)*scaleg)
	red=r(i) & green=g(i) & blue=b(i)
	red=interpolate(r,i)
	green=interpolate(g,i)
	blue=interpolate(b,i)
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

