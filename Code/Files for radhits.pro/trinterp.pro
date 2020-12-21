; trinterp.pro
;
; started 6-17-98 by ERW
;
; interpolates gaps in tracked data
; /uber added 9-18-98
; /flag added 5-10-00


function trinterp,tarray,lomem=lomem,flag=flag
; tarray is an array of tracked data
; returns a new tracked data array
;
;
; /uber if using uber-tracker data  -- now obsolete, we have uber-autodetection
; /lomem will remove original array to save memory
; /flag to append a column of 1's for interpolated values, 0's for
;       original values

s=size(tarray)
if (s(0) eq 2) then uber=1

result=tarray
if (keyword_set(flag)) then begin
	result=[result,transpose(intarr(s(2)))]
endif

; UBER-TRACKER CODE: added 9-18-98
ndat=n_elements(tarray(*,0))
dp=tarray-shift(tarray,0,1)
; changed at REC's suggestion by ERW: 1-10-03
; w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) ne 1),ngood)
w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) gt 1),ngood)
count = 0L
storeres=0
if (ngood ge 1) then begin
	totdt=total(dp(ndat-2,w))-ngood
	dp=0
	storeres=fltarr(ndat,totdt)
	for i=0L,ngood-1L do begin
		x0=reform(tarray(*,w(i)-1))
		x1=reform(tarray(*,w(i)))
		;dx=x1-x0
		dt=x1(ndat-2)-x0(ndat-2)
		t=1.0-(findgen(dt-1)+1.0)/(dt)
		xx = x0 # t + x1 # (1.0-t)
		storeres(*,count:count+dt-2L)=xx
		count = count + dt - 1L
	endfor
	n=n_elements(storeres(0,*))
	if (keyword_set(flag)) then $
		storeres=[storeres,transpose(intarr(n)+1)]
	if (keyword_set(lomem)) then tarray=0
	result=[[result],[storeres]]
	storeres=0
endif else begin
	; nothing to trinterp, apparently
endelse
result=result(*,sort(result(ndat-2,*)))
result=result(*,sort(result(ndat-1,*)))
result(ndat-2:ndat-1,*)=round(result(ndat-2:ndat-1,*))

if (keyword_set(flag)) then begin
	result(ndat-2:*,*)=result([ndat,ndat-2,ndat-1],*)
endif

return, result
end
; this ends the function

