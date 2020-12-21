; ericgr3d			9-20-99 Eric Weeks
;
; this function has no relationship with ericgr2d
; modified slightly ERW 6-7-04
;
; see: http://www.physics.emory.edu/~weeks/idl/gofr0.html
;

;------------------------------------------------------------
function subgr,data,rmin,rmax,deltar,lilnbar=lilnbar,enone=enone

dim=n_elements(data(*,0))
nr=(rmax-rmin)/deltar+1
result=fltarr(nr)
xmin=min(data(0,*),max=xmax)
ymin=min(data(1,*),max=ymax)
if (dim eq 3) then begin
	zmin=min(data(2,*),max=zmax)
endif
x0=xmin+rmax & x1=xmax-rmax
y0=ymin+rmax & y1=ymax-rmax
w1=where(data(0,*) gt x0 and data(0,*) lt x1 and $
	 data(1,*) gt y0 and data(1,*) lt y1,nw1)
if (nw1 eq 0) then message,'no particles ???',/inf
rmin2=rmin*rmin
rmax2=rmax*rmax
npts=n_elements(data(0,*))
one=fltarr(npts)+1.0
if (dim ne 2) then begin
	vol=(zmax-zmin)*(xmax-xmin)*(ymax-ymin)
endif else begin
	vol=(xmax-xmin)*(ymax-ymin)
endelse
lilnbar=lilnbar+npts/vol
enone=enone+nw1

; =======================
; sphere, center at origin, slice off top at z=z0: the surface
; area that is removed is 2piR^2(1-z0/R) so thus the remaining
; surface area (from origin up to z0) is 2 pi R z0, a nice
; simple formula.  Makes sense by dimensional analysis...
; =======================

twopi=2*3.14159265358
rvec=findgen(nr)*deltar+rmin
normz=1.0/(twopi*rvec*deltar)

for i=0L,nw1-1L do begin
	d0=data(*,w1(i))
	dd=one##d0-data
	dis=total(dd*dd,1)
	w2=where(dis gt rmin2 and dis lt rmax2,nw2)
	if (nw2 gt 0) then begin
; if (min(dis(w2)) lt 0.1) then message,'foo',/inf
		newdis=sqrt(dis(w2))
		thehisto=histogram(newdis,max=rmax,min=rmin,binsize=deltar)
		if (dim eq 3) then begin
			z0=zmax-d0(2)
			z1=d0(2)-zmin
			normz=twopi*rvec*( (rvec<z0) + (rvec<z1) )
			normz(0)=1.0
			normz=1.0/normz
			normz(0)=0.0
		endif ; else if dim=2 then normz has already been set
		result=result+thehisto*normz
	endif
endfor

return,result
end


;------------------------------------------------------------
function ericgr3d,data,track=track,rmin=rmin,rmax=rmax, $
	deltar=deltar,noplot=noplot
; assumes pretrack data (last column is timestamp) unless /track
; in which case penultimate column is timestamp.
;
; no harm to have deltar really small, can always rebin later

if (not keyword_set(rmin)) then rmin=0.0
if (not keyword_set(rmax)) then rmax=10.0
if (not keyword_set(deltar)) then deltar=0.01
nel=n_elements(data(*,0))
tel=nel-1
if (keyword_set(track)) then tel=nel-2
dim = 3; needs to be this!

xmin=min(data(0,*),max=xmax)
ymin=min(data(1,*),max=ymax)
dx=xmax-xmin & dy=ymax-ymin
if ((dx lt rmax*2) or (dy lt rmax*2)) then begin
	message,'rmax is too large',/inf
	rmax = (dx < dy)*0.3
	message,'setting rmax to : '+string(rmax),/inf
endif

tmin=min(data(tel,*),max=tmax)
nr=(rmax-rmin)/deltar+1
rvec=findgen(nr)*deltar+rmin

count=0
lilnb=0.0
enone=0L
for t=tmin,tmax do begin
	w=where(data(tel,*) eq t,nw)
	if (nw gt 2) then begin
		message,string(t)+'/'+string(tmax),/inf
		tempgr=subgr(data(0:dim-1,w),rmin,rmax,deltar,lilnbar=lilnb,enone=enone)
		if (count eq 0) then resultgr=tempgr else resultgr=resultgr+tempgr
		count=count+1
		w2=where(resultgr gt 0.0,nw2)
		nnbar=count/(enone*lilnb*deltar)
		if (not keyword_set(noplot) and nw2 gt 1) then begin
			if (dim eq 3) then  $
				plot,rvec(w2),resultgr(w2)*nnbar
			if ((dim eq 2) and ((t mod 10) eq 0)) then  $
				plot,rvec(w2),resultgr(w2)*nnbar
		endif
	endif else begin
		message,'no particles at time t='+string(t),/inf
	endelse
endfor
resultgr=resultgr*nnbar

n=n_elements(resultgr)
result=transpose([[rvec(0:n-1)],[resultgr]])

return,result
end

