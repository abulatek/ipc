; pivplot.pro		7/11/04 by Eric Weeks
;
; plots output from dpiv
;
;   Please see:
;     http://www.physics.emory.edu/~weeks/idl/piv.html

pro pivplot,pivdata,spacing=spacing,calib=calib,nodot=nodot,scale=scale, $
    _extra=eee

if (not keyword_set(spacing)) then spacing = 5
	; spacing tells how much of the input array to plot
	; spacing=1 plots every point

if (not keyword_set(calib)) then calib = 1
	; calib is the microns per pixel in "pivdata"

if (not keyword_set(scale)) then scale = 1
	; how much to stretch the vectors

nx=n_elements(pivdata(*,0,0))
ny=n_elements(pivdata(0,*,0))
nnx = nx/spacing
nny = ny/spacing
maxdisp=max(pivdata(*,*,0:1))
picsize = [nx,ny]*calib+calib
plot,[0,picsize[0]],[0,picsize[1]],/nodata,/iso,/xs,/ys,_extra=eee

if (not keyword_set(nodot)) then begin
	i = (indgen(nnx*nny) mod nnx) * calib*spacing + calib
	j = (floor(indgen(nnx*nny)/nnx)) * calib*spacing + calib
	oplot,i,j,psym=circ(rad=0.5)
endif

for i=0,nx-1,spacing do begin
	x0 = i*calib + calib
	for j=0,ny-1,spacing do begin
		y0 = j*calib + calib
		dx = pivdata(i,j,0)/maxdisp*calib*scale*spacing*0.7
		dy = pivdata(i,j,1)/maxdisp*calib*scale*spacing*0.7
		oplot,[x0,x0+dx],[y0,y0+dy]
	endfor
endfor

end
