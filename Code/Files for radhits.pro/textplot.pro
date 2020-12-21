; textplot.pro,  started 6-20-98 by ERW
;
; see http://www.physics.emory.edu/~weeks/idl/

pro textplot,xdata,ydata
; plots xdata and ydata using text '*'
; if ydata is missing, plots 'xdata' -vs- index
;
; WARNING: may have memory problems if too many points exist, as
; two integer arrays are formed with the same length as xdata -- but
; so far this doesn't seem to be a problem.  It appears to run quickly
; even with large data sets.



z = bytarr(75,22)		; byte image of screen picture
maxx = max(xdata)
minx = min(xdata)
if maxx eq minx then message,'max(x) and min(x) are the same'
numx = n_elements(xdata)
xscale = 1.0/float(maxx-minx)

; 'floor' function used below to force integers, and also force them
; to be in the proper range...
if n_elements(ydata) eq 0 then begin
	; ONLY XDATA DEFINED
	temp = 74.0/float(numx)
	xscale = 21*xscale
	ix = floor(findgen(numx)*temp)
	iy = floor((xdata-minx)*xscale)
endif else begin
	; BOTH XDATA AND YDATA DEFINED
	if numx ne n_elements(ydata) then message,"x and y aren't the same length"
	maxy = max(ydata)
	miny = min(ydata)
	if maxy eq miny then message,'max(y) and min(y) are the same'
	yscale = 21.0/float(maxy-miny)
	xscale = 74.0*xscale
	ix = floor((xdata-minx)*xscale)
	iy = floor((ydata-miny)*yscale)
endelse
z(ix,iy)=1
ix=0
iy=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: contortions below necessary to trick 'print' function into    ;
;       not printing spaces between each character, as would happen   ;
;       if I just tried printing a string array.  (Below, I print out ;
;       five-character chunks by specifying them individually in the  ;
;       call to print.)                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; inelegant, but quick to create in vi:
zz=strarr(32)
zz(0)='     '
zz(1)='*    '
zz(2)=' *   '
zz(3)='**   '
zz(4)='  *  '
zz(5)='* *  '
zz(6)=' **  '
zz(7)='***  '
zz(8)='   * '
zz(9)='*  * '
zz(10)=' * * '
zz(11)='** * '
zz(12)='  ** '
zz(13)='* ** '
zz(14)=' *** '
zz(15)='**** '
zz(16)='    *'
zz(17)='*   *'
zz(18)=' *  *'
zz(19)='**  *'
zz(20)='  * *'
zz(21)='* * *'
zz(22)=' ** *'
zz(23)='*** *'
zz(24)='   **'
zz(25)='*  **'
zz(26)=' * **'
zz(27)='** **'
zz(28)='  ***'
zz(29)='* ***'
zz(30)=' ****'
zz(31)='*****'

; compose the screen picture below
zzz=strarr(15,22)
for j=0,70,5 do begin
    b = z(j,*)+z(j+1,*)*2+z(j+2,*)*4+z(j+3,*)*8+z(j+4,*)*16
    zzz(j/5,*)=zz(b)
endfor


; display screen picture
;
; NOTE: "print,zzz(*,i)" or "print,zzz" put in spaces between each
; string, which is unwanted, thus the kludge below.
;
; NOTE2: print in reverse order, to fix orientation of y-data
;
; NOTE3: print only accepts so many arguments, which is one reason
; to do this in 5-character chunks.  Too bad we don't have printf().
for i=21,0,-1 do begin
	print,zzz(0,i),zzz(1,i),zzz(2,i),zzz(3,i),zzz(4,i),  $
	   zzz(5,i),zzz(6,i),zzz(7,i),zzz(8,i),zzz(9,i),    $
	   zzz(10,i),zzz(11,i),zzz(12,i),zzz(13,i),zzz(14,i)
endfor

end


