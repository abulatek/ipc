; ********** start of bpass3d.pro
;+
; NAME:
;		bpass3d
; PURPOSE:
;		Implements a real-space bandpass filter to suppress 
;		pixel noise and slow-scale image variations while 
;		retaining information of a characteristic size.
;		*Works with anisotropic 3d cube data*
;
; CATEGORY:
;		3d Image Processing
; CALLING SEQUENCE:
;		res = bpass3d( image, lnoise, lobject )
; INPUTS:
;		image:	The two-dimensional array to be filtered.
;		lnoise: Characteristic lengthscale of noise in pixels.
;			Additive noise averaged over this length should
;			vanish. May assume any positive floating value.
;			Make it a 3-vector if aspect ratio is not 1:1:1.
;		lobject: A length in pixels somewhat larger than a typical
;			object. Must be an odd valued integer.
;			Make it a 3-vector if aspect ratio is not 1:1:1.
; KEYWORDS:
;		pad:    Set this keyword to pad out the front and rear
;			planes to reduce data loss.  Fills with average
;			value of the adjoining face plane to reduce
;			ringing effects.
;		noclip: Set this keyword to disable zero thresholding.
;			The resulting images will have negative-going
;			portions.
; OUTPUTS:
;		res:	filtered image.
; PROCEDURE:
;		simple 'mexican hat' wavelet convolution yields spatial 
;		bandpass filtering.
; NOTES:
; MODIFICATION HISTORY:
;		based on bpass.pro, 
;		Written by David G. Grier, The University of Chicago, 2/93.
;		Greatly revised version DGG 5/95.
;		Added /field keyword JCC 12/95.
;		Revised & added 'stack','voxel' capability JCC 5/97.
;		made separate function JCC 6/98.
;
;	This code 'bpass3d.pro' is copyright 1998, John C. Crocker and 
;	David G. Grier.  It should be considered 'freeware'- and may be
;	distributed freely in its original form when properly attributed.
;-
function bpass3d, image, lnoise, lobject, noclip=noclip, nopad = nopad

on_error, 2				; go to caller on error

nn = n_elements(lnoise)
no = n_elements(lobject)
if (nn gt 1 and no eq 1) or (nn eq 1 and no gt 1) then $
	message,'Both length parameters must be scalars or 3-vectors!',/inf 

; do xdirection masks
bb = float( lnoise(0) )
w = round( lobject(0) > (2. * bb) )
N = 2*w + 1
r = (findgen( N ) - w)/(2. * bb)

gx = exp( -r^2 )
gx = gx / total(gx)
bx = fltarr(N) - 1./N

factor = ( total(gx^2) - 1/N )

if nn eq 1 then begin

	gy = transpose(gx)
	gz = transpose(gx)
	by = transpose(bx)
	bz = transpose(bx)

endif else begin

	; do y direction masks
	bb = float( lnoise(1) )
	w = round( lobject(1) > (2. * bb) )
	N = 2*w + 1
	r = (findgen( N ) - w)/(2. * bb)
	
	gy = exp( -r^2 )
	gy = gy / total(gy)
	gy = transpose(gy)
	by = fltarr(N) - 1./N
	by = transpose(by)
	
	; do z direction masks
	bb = float( lnoise(2) )
	w = round( lobject(2) > (2. * bb) )
	N = 2*w + 1
	r = (findgen( N ) - w)/(2. * bb)
	
	gz = exp( -r^2 )
	gz = gz / total(gz)
	gz = transpose(gz)
	bz = fltarr(N) - 1./N
	bz = transpose(bz)

endelse

nf = n_elements(image(0,0,*))
nx = n_elements(image(*,0,0))

if (N ge nf) and keyword_set(nopad)  then begin	
	; stack is too thin for any data to survive!
	message,'Warning: data cube thinner than convolution kernel!',/inf
	message,"	  Disabling 'nopad' keyword to compensate.",/inf
	pad = 1 
endif

if not keyword_set(nopad) then begin
	if n_elements(lobject) eq 1 then begin
		padxw = round( lobject(0) > (2. * bb) ) 
		padyw = round( lobject(0) > (2. * bb) ) 
		padzw = round( lobject(0) > (2. * bb) ) 
	endif else begin
		padxw = round( lobject(0) > (2. * bb) ) 
		padyw = round( lobject(1) > (2. * bb) ) 
		padzw = round( lobject(2) > (2. * bb) ) 
	endelse
	ny = n_elements(image(0,*,0))

	; pad out the array with average values of corresponding frames
	ave = fltarr(nf)
	for i = 0,nf-1 do ave(i) = total(image(*,*,i))/(nx*ny) 
	g = fltarr(nx+(2*padxw),ny+(2*padyw),nf+(2*padzw))
	g(*,*,0:padzw-1) = ave(0)
	g(*,*,nf-padzw:*) = ave(nf-1)	
	for i = 0,nf-1 do g(*,*,padzw+i) = ave(i)	
	g(padxw:padxw+nx-1,padyw:padyw+ny-1,padzw:padzw+nf-1) = float(image)

	nx = nx + (2*padxw)
	ny = ny + (2*padyw)
	nf = nf + (2*padzw)
endif else begin
	g = float(image)
	padxw = 0
	padyw = 0
	padzw = 0
endelse

b = g

; do x and y convolutions
for i = padzw,nf-1-padzw do begin
	g(*,*,i) = convol( g(*,*,i), gx )
	g(*,*,i) = convol( g(*,*,i), gy )

	b(*,*,i) = convol( b(*,*,i), bx )
	b(*,*,i) = convol( b(*,*,i), by )
endfor

; do z convolution
for i = padxw,nx-1-padxw do begin
	g(i,*,*) = convol( reform(g(i,*,*)), gz )	
	b(i,*,*) = convol( reform(b(i,*,*)), bz )
endfor

if not keyword_set(nopad) then begin
	g = g(padxw:nx-padxw-1,padyw:ny-padyw-1,padzw:nf-padzw-1)
	b = b(padxw:nx-padxw-1,padyw:ny-padyw-1,padzw:nf-padzw-1)
endif

g = g+b  ; this is the final straw on memory: 12x number of original

if keyword_set( noclip ) then $
	return,g/factor $
else $
	return,g/factor > 0

end

;*********** end of bpass.pro












