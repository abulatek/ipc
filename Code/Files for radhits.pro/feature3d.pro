;+
; NAME:
;		Feature3d
; PURPOSE:
;		Finds and measures roughly spheroidal 'features' within 
;		a 3d image. Works best with dilute or separate feature,
;		i.e. non close-packed structures.
; CATEGORY:
;		Image Processing
; CALLING SEQUENCE:
;		f = fcp3d( image, diameter [, separation=separation, 
;			masscut = masscut, threshold = threshold] )
; INPUTS:
;		image:	(nx,ny) array which presumably contains some
;			features worth finding
;		diameter: a parameter which should be a little greater than
;			the diameter of the largest features in the image.
;			May be a single float number if the image is 
;			isotropic, a 3-vector otherwise.
;		separation: an optional parameter which specifies the 
;			minimum allowable separation between feature 
;			centers. The default value is diameter-1.
;		masscut: Setting this parameter saves runtime by reducing
;			the runtime wasted on low mass 'noise' features.
;		threshold: Set this parameter to a number less than 1 to
;			threshold each particle image by 
;			(peak height)*(threshold).  Reduces pixel biasing
;			with an particle specific threshold.
; OUTPUTS:
;		f(0,*):	this contains the x centroid positions, in pixels.
;		f(1,*): this contains the y centroid positions, in pixels. 
;		f(2,*): this contains the z centroid positions, in pixels.
;		f(3,*): this contains the integrated brightness.
;		f(4,*): this contains the squared radius of gyration. 
;		f(5,*): this contains the peak height of the feature.
;		f(6,*): this contains the fraction of voxels above threshold.
;
; SIDE EFFECTS:
;		Displays the number of features found on the screen.
; RESTRICTIONS:
;		To work properly, the image must consist of bright, 
;		smooth regions on a roughly zero-valued background. 
;		To find dark features, the image should be 
;		inverted and the background subtracted. If the image
;		contains a large amount of high spatial frequency noise,
;		performance will be improved by first filtering the image.
;		'bpass3d' will remove high spatial frequency noise, and 
;		subtract the image background and thus may provides a useful 
;		complement to using this program. Individual features 
;		should NOT overlap or touch.  Furthermore, the maximum
;		value of the top of the feature must be in the top 30th
;		percentile of brightness in the entire image.
;		For images where the particles are close packed, the
;		system of bpass3d/feature3d is not ideal, but will give
;		rough coordinates.  We often find setting 'sep' to roughly
;		diameter/2 seems helpful to avoid particle loss.
; PROCEDURE:
;		First, identify the positions of all the local maxima in
;		the image ( defined in a circular neighborhood with radius
;		equal to 'separation' ). Around each of these maxima, place a 
;		circular mask, of diameter 'diameter', and calculate the x,y,z
;		centroids, the total of all the pixel values.
;		If the restrictions above are adhered to, and the features 
;		are more than about 5 pixels across, the resulting x 
;		and y values will have errors of order 0.1 pixels for 
;		reasonably noise free images.
;		If 'threshold' is set, then the image within the mask is 
;		thresholded to a value of the peak height*threshold.  This
;		is useful when sphere images are connected by faint bridges
;		which can cause pixel biasing.
;
; *********	       READ THE FOLLOWING IMPORTANT CAVEAT!        **********
;		'feature3d' is capable of finding image features with sub-pixel
;		accuracy, but only if used correctly- that is, if the 
;		background is subtracted off properly and the centroid mask 
;		is larger than the feature, so that clipping does not occur.
;		It is an EXCELLENT idea when working with new data to plot
;		a histogram of the x-positions mod 1, that is, of the
;		fractional part of x in pixels.  If the resulting histogram
;		is flat, then you're ok, if its strongly peaked, then you're
;		doing something wrong- but probably still getting 'nearest
;		pixel' accuracy.
;
;		For a more quantitative treatment of sub-pixel position 
;		resolution see: 
;		J.C. Crocker and D.G. Grier, J. Colloid Interface Sci.
;		*179*, 298 (1996).
;
; MODIFICATION HISTORY:
;		This code is inspired by feature_stats2 written by
;			David G. Grier, U of Chicago, 			 1992.
;		Generalized version of feature.pro			 1998.
;		Improved local maximum routine, from fcp3d		 1999.
;		Added 'threshold' keyword to reduce pixel biasing.	 1999.
;		
;	This code feature3d.pro is copyright 1999, John C. Crocker and 
;	David G. Grier.  It should be considered 'freeware'- and may be
;	distributed freely in its original form when properly attributed.
;-
;
;	produce a 3d, anisotropic parabolic mask
;	anisotropic masks are 'referenced' to the x-axis scale.
;	ratios less than one squash the thing relative to 'x'
;	using float ratios allows precise 'diameter' settings 
;	in an odd-sized mask.
;
function lrsqd3d,extent,yratio=yratio,zratio=zratio

if not keyword_set(yratio) then yratio = 1.
if not keyword_set(zratio) then zratio = 1.
if n_elements(extent) eq 1 then ext = intarr(3)+extent else ext = extent
x = ext(0)
y = ext(1)
z = ext(2)

r2 = fltarr(x,y,z,/nozero)
xc = float(x-1) / 2.
yc = float(y-1) / 2.
zc = float(z-1) / 2.

yi = fltarr(x) +1
xi = fltarr(y) +1

xa = (findgen(x) - xc)
xa = xa^2
ya = (findgen(y) - yc)/yratio
ya = ya^2
za = (findgen(z) - zc)/zratio
za = za^2

for k=0,z-1 do begin
	r2(*,*,k) = (xi ## xa) + (ya ## yi) + za(k) 
endfor

return,r2
end
;
;	a 3d-happy version of DGGs 'local max', which does NOT use
;	the dilation algorithm.  NB: the data MUST be padded, or it
;	can crash!! Best of all, 'sep' is a float 3-vector. 'sep' is
;	the actual minimum distance between two maxima (i.e. the bead
;	diameter or so.  If the image is 'padded', tell the code the
;	padding size to save it a little work.
;
function llmx3d, image, sep, pad

a = image
allmin = min(a)

a(0,0) = allmin			; tuck the min value into a before bytscl
a = fix(bytscl(a))		; convert image into bytscl'd integer array.
allmin = a(0,0)			; get it back-- cheese!

nx = long(n_elements(a(*,0,0)))
ny = long(n_elements(a(0,*,0)))
nz = long(n_elements(a(0,0,*)))
bignum = nx*ny*nz

; the diameter of the local max algorithm is 2*sep.
; extent is the next biggest odd integer.
extent = fix(sep*2.) + 1	; i.e. mask is diameter 2*sep
extent = extent + (extent+1) mod 2
rsq   = lrsqd3d(extent,yratio=sep(1)/sep(0),zratio=sep(2)/sep(0))
mask = rsq lt (sep(0))^2

; cast the mask into a one dimensional form-- imask!
bmask = bytarr(nx,ny,extent(2))
bmask(0:extent(0)-1,0:extent(1)-1,*) = mask
imask = where(bmask gt 0) + bignum -(nx*ny*(extent(2)/2)) $
	-(nx*(extent(1)/2)) -(extent(0)/2)

; let's try Eric's hash table concept.
; set percentile to 0. if you want ever voxel to be a potential maximum
; set it to 0.8 or so if you have lots of tiny spikes, to run faster
percentile = 0.7

hash = bytarr(nx,ny,nz)+1B
ww = where(a(*,*,pad(2):nz-pad(2)-1) gt allmin,nww) + (nx*ny*pad(2))
ss = sort(a(ww))
s = ww(ss(percentile*nww:*))
ww = 0
s = [reverse(s),0]	; so it knows how to stop!
ns = n_elements(s)

idx = 0L
rr = s(idx)
m = a(rr)
r = -1L
i = -1
erwidx=n_elements(s)-1L
repeat begin

;	get the actual local max in a small mask
	actmax = max(a((rr+imask) mod bignum))

; 	if our friend is a local max, then nuke out the big mask, update r
	if m ge actmax then begin
		r = [[r],[rr]]
		hash((rr+imask) mod bignum) = 0
	endif else begin
		w = where(a((rr+imask) mod bignum) lt m, nw)
		if nw gt 0 then hash((rr+imask(w) mod bignum)) = 0
	endelse

;	get the next non-nuked id
	; 2nd half of this added 8-31-01 / ERW:
	repeat idx = idx+1 until (hash(s(idx)) eq 1 or idx ge erwidx)
	; if statement, and else action added 8-31-01 / ERW
	if (idx lt erwidx) then begin
		rr = s(idx)
		m = a(s(idx))
	endif else begin
		m = allmin
	endelse

endrep until m le allmin

if n_elements(r) gt 1 then r = r(1:*) else return,[-1L]

x = ( r mod (nx*ny) mod nx )
y = ( r mod (nx*ny) / nx )
z = ( r  / (nx*ny) )

w = where( (x gt pad(0)) and (x lt nx-pad(0)-1) and $
	(y gt pad(1)) and (y lt ny-pad(1)-1) and $
	(z gt pad(2)) and (z lt nz-pad(2)-1), nw )
if nw gt 0 then return,[[x(w)],[y(w)],[z(w)]] else return,[-1L]

end
;
;	For anisotropy, make diameter a 3-vector, otherwise one # is ok.
;	Image should consist of smooth well separated peaks on a zero
;	or near zero background, with diameter set somewhat larger
;	than the diameter of the peak!
;
function feature3d, image, diameter, separation=separation, $
	masscut = masscut, threshold = threshold

if not keyword_set(masscut) then masscut = 0
if keyword_set(threshold) then $
	if threshold le 0 or threshold ge 0.9 then $
	  message,'Threshold value must be between 0.0 and 0.9!'

if n_params() eq 1 then message,'User must supply a feature diameter!'

; make extents be the smallest odd integers bigger than diameter
if n_elements(diameter) eq 1 then diameter = fltarr(3)+diameter
extent = fix(diameter) + 1
extent = extent + (extent+1) mod 2

sz = size( image )
nx = sz(1)
ny = sz(2)
nz = sz(3)
if not keyword_set(separation) then sep = diameter-1 $
	else sep = float(separation)
if n_elements(sep) eq 1 then sep = fltarr(3)+sep

;	Put a border around the image to prevent mask out-of-bounds
a = fltarr( nx+extent(0), ny+extent(1), nz+extent(2) )
for i = 0,nz-1 do $	; do as a loop to reduce memory piggage
a(extent(0)/2:(extent(0)/2)+nx-1,extent(1)/2:(extent(1)/2)+ny-1,$
	extent(2)/2+i) = float( image(*,*,i) )
nx = nx + extent 
ny = ny + extent

;	Find the local maxima in the filtered image
loc = llmx3d(a,sep,(extent/2))
if loc(0) eq -1 then begin
	message,'No features found!',/inf		
	return,-1
endif

; 	Set up some stuff....
nmax=n_elements(loc(*,0))
x = loc(*,0)
y = loc(*,1)
z = loc(*,2)
xl = x - fix(extent(0)/2) 
xh = xl + extent(0) -1
yl = y - fix(extent(1)/2) 
yh = yl + extent(1) -1
zl = z - fix(extent(2)/2) 
zh = zl + extent(2) -1
m  = fltarr(nmax)
pd = fltarr(nmax)
thresh = fltarr(nmax)
nthresh = fltarr(nmax)

;	Set up some masks
rsq   = lrsqd3d(extent,yratio=diameter(1)/diameter(0),$
	zratio=diameter(2)/diameter(0))
mask  = rsq lt ((diameter(0)/2.))^2 +1.
shell = mask and rsq gt ((diameter(0)/2. -1.))^2
nask  = total(mask)
rmask = (rsq*mask)+(1./6.) 

imask = make_array( extent(0), extent(1), extent(2), /float, /index ) $
	mod ( extent(0) ) + 1.
xmask = mask * imask
imask = make_array( extent(1), extent(0), extent(2), /float, /index ) $
	mod ( extent(1) ) + 1.
ymask = mask * transpose(imask,[1,0,2])
imask = make_array( extent(2), extent(1), extent(0), /float, /index ) $
	mod ( extent(2) ) + 1.
zmask = mask * transpose(imask,[2,1,0])

;	Get the 'tops', i.e. peak heights
tops = a(x,y,z)
tops = tops(*)
if keyword_set(threshold) then thresh = tops*threshold

; 	if 'threshold' then get the max shell values
if keyword_set(threshold) then for i=0L,nmax-1L do nthresh(i) = $
   total(((a(xl(i):xh(i),yl(i):yh(i), zl(i):zh(i))) * mask) gt thresh(i))/ $
   total(((a(xl(i):xh(i),yl(i):yh(i), zl(i):zh(i))) * mask) gt 0)

;	Estimate the mass	
for i=0L,nmax-1L do m(i) = total( (a(xl(i):xh(i),yl(i):yh(i),$
	zl(i):zh(i))-thresh(i) > 0) * mask )

; do a masscut, and prevent divide by zeroes in the centroid calc.
w = where( m gt masscut, nmax )
if nmax eq 0 then begin
	message,'No features found!',/inf
	return,-1
endif
xl = xl(w)
xh = xh(w)
yl = yl(w)
yh = yh(w)
zl = zl(w)
zh = zh(w)
x = x(w)
y = y(w)
z = z(w)
m = m(w)
tops = tops(w)
thresh = thresh(w)
nthresh = nthresh(w)

message, strcompress( nmax ) + ' features found.',/inf

;	Setup some result arrays
xc = fltarr(nmax)
yc = fltarr(nmax)
zc = fltarr(nmax)
rg = fltarr(nmax)

;	Calculate the radius of gyration^2
for i=0L,nmax-1L do rg(i) = total( (a(xl(i):xh(i),yl(i):yh(i),$
	zl(i):zh(i)) - thresh(i) > 0) * rmask )/m(i)

;	Calculate peak centroids
for i=0L,nmax-1L do begin
	xc(i) = total( (a(xl(i):xh(i),yl(i):yh(i),$
		zl(i):zh(i)) - thresh(i) >0) * xmask )
	yc(i) = total( (a(xl(i):xh(i),yl(i):yh(i),$
		zl(i):zh(i)) - thresh(i) >0) * ymask )
	zc(i) = total( (a(xl(i):xh(i),yl(i):yh(i),$
		zl(i):zh(i)) - thresh(i) >0) * zmask )
endfor

;	Correct for the 'offset' of the centroid masks
xc = xc / m - ((float(extent(0))+1.)/2.)
yc = yc / m - ((float(extent(1))+1.)/2.)
zc = zc / m - ((float(extent(2))+1.)/2.)

;	Update the positions and correct for the width of the 'border'
x = x + xc - extent(0)/2
y = y + yc - extent(1)/2 
z = z + zc - extent(2)/2 

if keyword_set(threshold) then $
return,[transpose(x),transpose(y),transpose(z),$
   transpose(m),transpose(rg),transpose(tops),transpose(nthresh)] else $
return,[transpose(x),transpose(y),transpose(z),$
	transpose(m),transpose(rg),transpose(tops)]

end





