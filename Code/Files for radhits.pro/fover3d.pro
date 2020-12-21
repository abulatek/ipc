;function fover3d,image,points,radius=radius,scale=scale,bigflag=bigflag, $
;    rescale=rescale,nodot=nodot
;+
; NAME:
;		fover3d
; PURPOSE:
;		Overlay points onto a 3d image.
; CALLING SEQUENCE:
;		newimage=foverlay(image,points)
; INPUTS:
;		image:	3D data set onto which the points should be overlaid
;		points: (3,npoints) array of overlay points
; OUTPUTS:
;		newimage is ready for movie or tv.  The color palette
;		is adjusted to be the foverlay palette.  Redraws screen.
; KEYWORDS:
;		radius sets size of spheres
;		scale is passed on to circarray:  set scale=[1,1,2] to squash
;			sphere in z-direction
;       rescale: multiplicative factors to be applied to 'points'
;       /big:  make image twice as big in X and Y dimensions
;       /nodot:  don't put dot in center of each overlaid sphere
; PROCEDURE:
;		Rescale the image to leave some color table indices free.
;		Make the rest of the color table into a grey ramp and
;		turn the 3 highest indices into 3 new colors
; MODIFICATION HISTORY:
;		Written by David G. Grier, AT&T Bell Laboratories, 7/91
;		Completely rewritten DGG, The University of Chicago, 3/93.
;
;		3D-ness added 6-23-98 by ERW, also turned into a function
;		(from foverlay.pro)  -- removed roi
;		ERW: 7-1-98: spent some time fixing up color stuff for mac
;		added some error checking
;       ERW: 7-22-06: internalize focirc.pro
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; focirc.pro,  started 6-22-98 by ERW
;
; taken from circarray.pro (7-22-06) to internalize into fover3d.pro
;


function focirc,array,radius=radius,scale=scale,center=center
; returns an array, size equal to "array" variable, with value 1
; everywhere within a circle of diameter of the array size.  Circle
; is at center of array.
;
; 'radius' sets a radius different from the default radius (half the array size)
; 'scale' lets you set a vector for the relative rescaling of x,y,z...
;    ----> [1,1,2] squashes the sphere in the z-direction
; 'center' overrides the default center of the circle

s=size(array)
result=array*0

if (s(0) eq 3) then begin
		sx=s(1) & sy=s(2) & sz=s(3)
		minsize = (sx < sy < sz)
		cx=(sx-1)*0.5 & cy=(sy-1)*0.5 & cz=(sz-1)*0.5
		if keyword_set(center) then begin
			cx=center(0)
			cy=center(1)
			cz=center(2)
		endif
		if keyword_set(radius) then begin
			irad=radius
		endif else begin
			irad=minsize/2
		endelse
		irad = irad*irad
		for i=0,sz-1 do begin
			rad1=(cz-i)*(cz-i)
			if keyword_set(scale) then rad1 = rad1*scale(2)*scale(2)
			for j=0,sy-1 do begin
				rad2 = rad1 + (cy-j)*(cy-j)
				if keyword_set(scale) then begin
					rad2 = rad1 + (cy-j)*(cy-j)*scale(1)*scale(1)
				endif else begin
					rad2 = rad1 + (cy-j)*(cy-j)
				endelse
				for k=0,sx-1 do begin
					if keyword_set(scale) then begin
						rad3 = rad2 + (cx-k)*(cx-k)*scale(0)*scale(0)
					endif else begin
						rad3 = rad2 + (cx-k)*(cx-k)
					endelse
					result(k,j,i) = (rad3 le irad)
				endfor
			endfor
		endfor
endif else begin
	message,'dimension is '+string(s(0)),/inf
	message,'dimension of array is not 3, sorry!',/inf
endelse

return,result

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function fover3d,image,points,radius=radius,scale=scale,bigflag=bigflag, $
    rescale=rescale,nodot=nodot

if not keyword_set(radius) then radius=5

nc = !d.table_size
if nc eq 0 then message,'Device has static color tables: cannot adjust'
red = byte(indgen(nc))
green = red
blue = red
green(nc-3:nc-1) = 0b
red(nc-3)=0b
red(nc-2)=128b
red(nc-1)=255b
blue(nc-3)=255b
blue(nc-2)=128b
blue(nc-1)=128b
tvlct,red,green,blue


output = byte ( ((image*0.98) mod (nc-5)) + floor(image / (nc-5))*256 )
x = reform( points(0,*) )
y = reform( points(1,*) )
z = reform( points(2,*) )
if (keyword_set(rescale)) then begin
	x = x * rescale(0)
	y = y * rescale(1)
	z = z * rescale(2)
endif
if (keyword_set(bigflag)) then begin
	output=big(output)
	x = x * 2
	y = y * 2
endif
x2=n_elements(output(*,0,0))
y2=n_elements(output(0,*,0))
z2=n_elements(output(0,0,*))
if not keyword_set(scale) then scale=[1,1,2]
w=where((x ge 0) and (x lt x2))
x=x(w) & y=y(w) & z=z(w)
w=where((y ge 0) and (y lt y2))
x=x(w) & y=y(w) & z=z(w)
w=where((z ge 0) and (z lt z2))
x=x(w) & y=y(w) & z=z(w)

for i = 0L,n_elements(x)-1L do begin
	minx = long((x(i)-radius) > 0)
	miny = long((y(i)-radius) > 0)
	minz = long((z(i)-radius) > 0)
	maxx = long((x(i)+radius) < (x2-1))
	maxy = long((y(i)+radius) < (y2-1))
	maxz = long((z(i)+radius) < (z2-1))
	foo=[x(i)-minx,y(i)-miny,z(i)-minz]
	blob=focirc(output[minx:maxx,miny:maxy,minz:maxz],  $
			scale=scale,center=foo,radius=radius)
	temp=(foo(2)-1) > 0
	temp = temp < (n_elements(blob(0,0,*))-2)
	blob(*,*,0:temp) = blob(*,*,0:temp) * (nc - 3b)
	temp=(foo(2)+1) < (n_elements(blob(0,0,*))-1)
	temp = temp > 1
	blob(*,*,temp:*) = blob(*,*,temp:*) * (nc - 2b)
	blob(*,*,foo(2)) = (blob(*,*,foo(2)) < 1) * (nc - 1b)
	output[minx:maxx,miny:maxy,minz:maxz] =			$
		output[minx:maxx,miny:maxy,minz:maxz] > blob
	if (not keyword_set(nodot)) then begin
		if ((x(i) ge 1) and (x(i) le (x2-2))) then    $
			output(x(i)-1:x(i)+1,y(i),z(i))=0b
		if ((y(i) ge 1) and (y(i) le (y2-2))) then    $
			output(x(i):x(i),y(i)-1:y(i)+1,z(i))=0b
	endif
endfor

tv,output(*,*,0)
return, output
end

