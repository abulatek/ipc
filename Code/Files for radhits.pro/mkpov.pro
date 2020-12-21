; mkpov.pro			3-30-99    Eric R. Weeks
;
; see http://www.physics.emory.edu/~weeks/idl/mkpov.html
; for more info
; ----------------------------------------------------------------
; pro mkpov,pts,filename,zmag=zmag,bw=bw,radius=radius,nobox=nobox, $
;     margin=margin,camera=camera,lookat=lookat,color=color
;
; pts contains the 3D points
; zmag stretches in z-direction   (example:  zmag=2)
; radius changes size of particles (in same units as original data)
;      if radius is an array, then assumes each particle's radius
;      is specified.
; color = [r,g,b] data for each point
; /bw for black & white
; camera = [x,y,z] to relocate camera
; lookat = [x,y,z] to relocate where camera looks at
; /nobox to remove outer box
; margin = 'whatever' to add a little margin to the box (make the box bigger)
; ----------------------------------------------------------------

pro redsphere,pos,rad=rad
	if (not keyword_set(rad)) then begin
		printf,1,'sphere { <',pos(0),pos(1),pos(2),'>, R1'
	endif else begin
		printf,1,'sphere { <',pos(0),pos(1),pos(2),'>,',rad
	endelse
	printf,1,'   texture { redcolor }}'
end

pro colsphere,pos,rgb,rad=rad
	if (not keyword_set(rad)) then begin
		printf,1,'sphere { <',pos(0),pos(1),pos(2),'>, R1'
	endif else begin
		printf,1,'sphere { <',pos(0),pos(1),pos(2),'>,',rad
	endelse
	printf,1,'  texture { '
	if (n_elements(rgb) eq 3) then begin
printf,1,'    pigment {color rgb <',rgb(0),',',rgb(1),',',rgb(2),'>}'
	endif else begin
printf,1,'    pigment {color rgb <',rgb(0),',',rgb(1),',',rgb(2),'> transmit ',rgb(3),'}'
	endelse
	printf,1,'    finish { ballfin }}}'
end

pro colcylinder,posa,posb
	printf,1,'cylinder {'
	printf,1,'<',posa(0),',',posa(1),',',posa(2),'>,'
	printf,1,'<',posb(0),',',posb(1),',',posb(2),'>,'
	printf,1,'R2'
	printf,1,'open'
	printf,1,'   texture { boxtex }}'
end



pro mkpov,pts,filename,zmag=zmag,bw=bw,radius=radius,nobox=nobox, $
     margin=margin,camera=camera,lookat=lookat,color=color

; pts contains the 3D points
; zmag stretches in z-direction   (example:  zmag=2)
; radius changes size of particles (in same units as original data)
;      if radius is an array, then assumes each particle's radius
;      is specified.
; color = [r,g,b] data for each point
; /bw for black & white
; /nobox to remove outer box
; margin = 'whatever' to add a little margin to the box (make the box bigger)
; camera = [x,y,z] to relocate camera
; lookat = [x,y,z] to relocate where camera looks at


if (not keyword_set(zmag)) then zmag=1
if (not keyword_set(bw)) then bw=0 else bw=1
if (not keyword_set(radius)) then radius=0.5
radflag=n_elements(radius)-1
npts = n_elements(pts(0,*))
if (keyword_set(color)) then begin
	ncol = n_elements(color(0,*))
	if (npts ne ncol) then begin
		message,"size of pts array and color array aren't the same"
	endif
endif
if (radflag gt 0) then begin
	nrad = n_elements(radius)
	if (npts ne nrad) then begin
		message,"size of pts array and radius array aren't the same"
	endif
endif

tr0=pts

if (min(tr0(0,*)) lt 0) then tr0(0,*)=tr0(0,*)+1.0-min(tr0(0,*))
if (min(tr0(1,*)) lt 0) then tr0(1,*)=tr0(1,*)+1.0-min(tr0(1,*))
tr0(2,*)=tr0(2,*)+20.0-min(tr0(2,*))
tr0(2,*)=tr0(2,*)*zmag
avgstd,tr0(0,*),/quiet,xres 
avgstd,tr0(1,*),/quiet,yres
avgstd,tr0(2,*),/quiet,zres 
dx=xres(3,*)-xres(2,*)
dy=yres(3,*)-yres(2,*) 
dz=zres(3,*)-zres(2,*)
zres(0,*)=zres(3,*)+dz*0.5

close,1
openw,1,filename

printf,1,'#version 3.1'
printf,1,'#include "colors.inc"'
printf,1,'#include "shapes.inc"'
printf,1,'#include "textures.inc"'
xbar=(xres(3)-xres(2))*0.5+xres(2)
ybar=(yres(3)-yres(2))*0.5+yres(2)
zbar=(zres(3)-zres(2))*0.5+zres(2)
print,"center of data at:",xbar,ybar,zbar
cameraoffset = (dx>dy)*1.2

printf,1,'camera {'
if (not keyword_set(camera)) then begin
	printf,1,'   location <',xbar,ybar,zres(3)+cameraoffset,'>'
	print,"camera at:        ",xbar,ybar,zres(3)+cameraoffset
endif else begin
	printf,1,'   location <',camera(0),camera(1),camera(2),'>'
endelse
if (not keyword_set(lookat)) then begin
	printf,1,'   look_at  <',xbar,ybar,zbar,'> }'
endif else begin
	printf,1,'   look_at <',lookat(0),lookat(1),lookat(2),'> }'
endelse
printf,1,'light_source { <13,12,65> color White}'
printf,1,'background {color rgb <1,1,1>}'
printf,1,'#declare R1 = ',radius(0)
printf,1,'#declare R2 = ',0.02*(xres(3)-xres(2))
printf,1,'#declare ballfin = finish {'
printf,1,'   reflection 0.0 diffuse 0.4 ambient 0.7 phong 1.0 phong_size 300}'

if (keyword_set(bw)) then begin
	printf,1,'#declare redcolor = texture {'
	printf,1,'   pigment {color rgb <0,0,0>}'
	printf,1,'   finish { ballfin }}'
	printf,1,'#declare boxtex = texture {'
	printf,1,'   pigment {color rgb <0.7,0.7,0.7>}'
	printf,1,'   finish { reflection 0.0'
	printf,1,'      diffuse 0.4 ambient 0.7 phong 0.0 }}'
endif else begin
	printf,1,'#declare redcolor = texture {'
	printf,1,'   pigment {color rgb <1,0,0>}'
	printf,1,'   finish { ballfin }}'
	printf,1,'#declare boxtex = texture {'
	printf,1,'   pigment {color rgb <0.6,0.8,1>}'
	printf,1,'   finish { reflection 0.0'
	printf,1,'      diffuse 0.4 ambient 0.7 phong 0.0 }}'
endelse

nw1=n_elements(tr0(0,*))
if (radflag le 0) then begin
	for i=0,nw1-1 do begin
		if (not keyword_set(color)) then begin
			redsphere,tr0(*,i)
		endif else begin
			colsphere,tr0(*,i),reform(color(*,i))
		endelse
	endfor
endif else begin
	for i=0,nw1-1 do begin
		if (not keyword_set(color)) then begin
			redsphere,tr0(*,i),rad=radius(i)
		endif else begin
			colsphere,tr0(*,i),reform(color(*,i)),rad=radius(i)
		endelse
	endfor
endelse
; ============================================================

if (keyword_set(margin)) then begin
	xres(2)=xres(2)-margin
	yres(2)=yres(2)-margin
	zres(2)=zres(2)-margin
	xres(3)=xres(3)+margin
	yres(3)=yres(3)+margin
	zres(3)=zres(3)+margin
endif

; MAKE THE BOX
if (not keyword_set(nobox)) then begin
	p0=[xres(2),yres(2),zres(2)] & p1=[xres(3),yres(2),zres(2)]
	colcylinder,p0,p1
	p0=[xres(2),yres(3),zres(2)] & p1=[xres(3),yres(3),zres(2)]
	colcylinder,p0,p1
	p0=[xres(2),yres(3),zres(3)] & p1=[xres(3),yres(3),zres(3)]
	colcylinder,p0,p1
	p0=[xres(2),yres(2),zres(3)] & p1=[xres(3),yres(2),zres(3)]
	colcylinder,p0,p1
	p0=[xres(2),yres(2),zres(2)] & p1=[xres(2),yres(3),zres(2)]
	colcylinder,p0,p1
	p0=[xres(3),yres(2),zres(2)] & p1=[xres(3),yres(3),zres(2)]
	colcylinder,p0,p1
	p0=[xres(3),yres(2),zres(3)] & p1=[xres(3),yres(3),zres(3)]
	colcylinder,p0,p1
	p0=[xres(2),yres(2),zres(3)] & p1=[xres(2),yres(3),zres(3)]
	colcylinder,p0,p1
	p0=[xres(2),yres(2),zres(2)] & p1=[xres(2),yres(2),zres(3)]
	colcylinder,p0,p1
	p0=[xres(3),yres(2),zres(2)] & p1=[xres(3),yres(2),zres(3)]
	colcylinder,p0,p1
	p0=[xres(3),yres(3),zres(2)] & p1=[xres(3),yres(3),zres(3)]
	colcylinder,p0,p1
	p0=[xres(2),yres(3),zres(2)] & p1=[xres(2),yres(3),zres(3)]
	colcylinder,p0,p1
endif

close,1

end
