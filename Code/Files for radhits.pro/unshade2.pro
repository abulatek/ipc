; unshade.pro, started 6-18-98 ERW
;
; unshades any x- and y-shading in an array of images
; unshades x and y independently (assumes  shade(x,y)=f(x)*g(y))
;
; USAGE:  newimages=unshade2(images)
;

function unshade2,images,poly=poly
; images:    array of images
; poly:      degree of polynomial fit (polynum=2 [parabola] by default)

if not keyword_set(poly) then poly=2

numx = n_elements(images(*,0,0))
numy = n_elements(images(0,*,0))
numz = n_elements(images(0,0,*))

sumay = total(images,2)
sumaz = total(sumay,2)
linfit = poly_fit(findgen(numx),sumaz,poly,xvec)
sumax = total(images,1)
sumaz2 = total(sumax,2)
linfit2 = poly_fit(findgen(numy),sumaz2,poly,yvec)

unita=fltarr(numy)+1.0
unshader=(unita ## xvec)
unitb=fltarr(numx)+1.0
unshader2=(yvec ## unitb)
unshader = 1.0 / (unshader * unshader2)

newimages=fltarr(numx,numy,numz)
for j=0,numz-1 do begin
	newimages(*,*,j) = images(*,*,j) * unshader
endfor

mmin=min(newimages)
mmax=max(newimages)
; this is an arbitrary rescaling:
newimages = (newimages-mmin) * (254.0/(mmax-mmin))

return,newimages
end

