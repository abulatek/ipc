; grdatwrap		Eric R. Weeks, 6-7-04
;
; see http://www.physics.emory.edu/~weeks/idl/gofr4.html
;
; data:  (peak position; full width half max; height)
;    (returned from grdatwrap in 'data' variable)
; /text if input is textfiles rather than GDF files

pro grdatwrap,fname,data,text=text,allgr=allgr

f = findfile(fname,count=nf)
; findfile goes to find the file, you can use wildcards if you
; want (then f would be an array of files found, nf would be the
; number of files found).

if (nf eq 0) then message,'no match'
print,'            filename      peak  fwhm height  center of mass'

data=fltarr(3,nf)
for i = 0,nf-1 do begin
	if (keyword_set(text)) then begin
		gr = readtext(f(i),/quiet)
	endif else begin
		gr = read_gdf(f(i))
	endelse
	mg=max(gr(1,*),mi)
	thenorm=total(gr(1,mi-25:mi+25))
	peak=total(gr(1,mi-25:mi+25)*gr(0,mi-25:mi+25))/thenorm
	ipeak=round(peak*100)
	x=gr(0,ipeak-30:ipeak+30)
	y=gr(1,ipeak-30:ipeak+30)

	; a=polyfitw(x,y,y^3,5,yf)
    a = poly_fit(x,y,5,measure_errors=0.1/y,yfit=yf,/double,status=status)
	mg=max(yf,mi)
	newpeak=x(mi)
	w=where(yf gt mg*0.5,nw)
	yderiv=(((((5*a(5)*x)+4*a(4))*x+3*a(3))*x+2*a(2))*x+a(1))
	ww=where(yderiv lt 0)
	if (ww(0) eq 0) then begin
		; kludge here
		sw=shift(ww,-1)-ww
		www=where(sw gt 1)
		ww=ww(www(0)+1:*)
	endif
	del=yderiv(ww(0))/(yderiv(ww(0))-yderiv(ww(0)-1))
	xpos=x(ww(0))- (x(ww(0))-x(ww(0)-1))*del
	plot,x,y,psym=circ(),position=[0.1,0.6,0.72,0.99]
	oplot,x,yf
	oplot,[xpos,xpos],[mg-0.2,mg+0.2]
	oplot,[x(w(0))-0.1,x(w(nw-1))+0.1],[mg*0.5,mg*0.5],lines=1
	plot,gr(0,*),gr(1,*),position=[0.1,0.1,0.72,0.55],/noerase,xr=[2,10]

	data(0,i)=xpos; where we've found it with subpixel accuracy
	data(1,i)=nw*0.01; full width half max
	data(2,i)=mg; this is the height
	print,f(i),xpos,nw*0.01,mg,peak, format="(A20,F10.3,F6.2,F6.2,F10.3)"
	if (i eq 0) then begin
		allgr=fltarr(nf+1,n_elements(gr(0,*)))
		allgr(0,*)=gr(0,*)
	endif
	allgr(i+1,*)=gr(1,*)
endfor

end

