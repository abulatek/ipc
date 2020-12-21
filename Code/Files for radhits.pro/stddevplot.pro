; stddevplot	3-12-99, Eric Weeks (weeks@dept.physics.upenn.edu)
;
; data(0,*) is the x-axis
; data(1,*) is the y-axis
; data(2,*) is the error (std dev?) of the y-data
;
; /invert to reverse x and y axis  (although data must still be in same format:
;          y-axis,x-axis,error of x-data)
; /connect to connect dots
; bars=0.1 to print horizontal bars at ends of error lines with size 0.1

pro stddevplot,data,invert=invert,connect=connect,oplot=oplot,  $
	themin=themin,themax=themax,bars=bars,_extra=eee,maxmin=maxmin


ymin=data(1,*)-data(2,*)
ymax=data(1,*)+data(2,*)

if (keyword_set(maxmin)) then begin
        if (not keyword_set(themax)) then begin
                themax = max(data(4,*))
        endif
        if (not keyword_set(themin)) then begin
                themin = min(data(3,*))
        endif
endif else begin
        if (not keyword_set(themin)) then themin=min(ymin)
        if (not keyword_set(themax)) then themax=max(ymax)
endelse

if keyword_set(connect) then sym=-4 else sym=4

if (not keyword_set(invert)) then begin
	if (not keyword_set(oplot)) then begin
		plot,data(0,*),data(1,*),psym=sym,yrange=[themin,themax],_extra=eee
	endif else begin
		oplot,data(0,*),data(1,*),psym=sym
	endelse
	for i=0,n_elements(data(0,*))-1 do begin
		oplot,[data(0,i),data(0,i)],[ymin(i),ymax(i)]
		if (keyword_set(bars)) then begin
			oplot,[data(0,i)-bars,data(0,i)+bars],[ymin(i),ymin(i)]
			oplot,[data(0,i)-bars,data(0,i)+bars],[ymax(i),ymax(i)]
		endif
	endfor
	if (keyword_set(maxmin)) then begin
		oplot,data(0,*),data(3,*)
		oplot,data(0,*),data(4,*)
	endif
endif else begin
	if (not keyword_set(oplot)) then begin
		plot,data(1,*),data(0,*),psym=sym,xrange=[themin,themax],_extra=eee
	endif else begin
		oplot,data(1,*),data(0,*),psym=sym
	endelse
	for i=0,n_elements(data(0,*))-1 do begin
		oplot, [ymin(i),ymax(i)], [data(0,i),data(0,i)]
		if (keyword_set(bars)) then begin
			oplot,[ymin(i),ymin(i)],[data(0,i)-bars,data(0,i)+bars]
			oplot,[ymax(i),ymax(i)],[data(0,i)-bars,data(0,i)+bars]
		endif
	endfor
	if (keyword_set(maxmin)) then begin
		oplot,data(3,*),data(0,*)
		oplot,data(4,*),data(0,*)
	endif
endelse

end
