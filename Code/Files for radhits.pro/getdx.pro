; getdx                   5-22-00   Eric R. Weeks
; patched 8-14-01 to return -1 for nonvalid results
; major bug fixed 6-3-05 for samples with unevenly gridded time stamps
; gdxtrinterp added internally 6-13-05
; Gianguido Cianci: 24 Jan 07. Now rounds timestamp to remedy old
; interpolation bug.

; function getdx,tr,dt,dim=dim
;
; see http://www.physics.emory.edu/~weeks/idl/getdx.html
; for more details.
; =================================================================

; gdxtrinterp.pro -- from "trinterp.pro"
;
; started 6-17-98 by ERW
;
; interpolates gaps in tracked data

function gdxtrinterp,tarray,flag=flag
; tarray is an array of tracked data
; returns a new tracked data array
;
; /flag to append a column of 1's for interpolated values, 0's for
;       original values

s=size(tarray)
result=tarray
if (keyword_set(flag)) then begin
    result=[result,transpose(intarr(s(2)))]
endif

ndat=n_elements(tarray(*,0))

;old interpolation algorithm could cause non integer value in timestamp
;column. This should fix it. ERW's suggestion
tarray[ndat-2,*] = round(tarray[ndat-2,*])

dp=tarray-shift(tarray,0,1)
; changed at REC's suggestion by ERW: 1-10-03
; w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) ne 1),ngood)
w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) gt 1),ngood)
count = 0L
storeres=0
if (ngood ge 1) then BEGIN
    totdt=total(dp(ndat-2,w))-ngood
    dp=0; free up memory
    storeres=fltarr(ndat,totdt)
    for i=0L,ngood-1L do begin
       x0=reform(tarray(*,w(i)-1))
       x1=reform(tarray(*,w(i)))
       dt=x1(ndat-2)-x0(ndat-2)
       t=1.0-(findgen(dt-1)+1.0)/(dt)
       xx = x0 # t + x1 # (1.0-t)
       storeres(*,count:count+dt-2L)=xx
       count = count + dt - 1L
    endfor
    n=n_elements(storeres(0,*))
    if (keyword_set(flag)) then $
       storeres=[storeres,transpose(intarr(n)+1)]
    result=[[result],[storeres]]
    storeres=0
endif else begin
    ; nothing to trinterp, apparently
endelse
; put in Victor Breedveld's patch here, to make sort work in DOS
result=result(*,sort(result(ndat-1,*)))
u=[[-1],uniq(result(ndat-1,*))]
nu=n_elements(u)
for i=1L,nu-1L do begin
    tpart=result(*,u(i-1)+1:u(i))
    result(*,u(i-1)+1:u(i)) = tpart(*,sort(tpart(ndat-2,*)))
endfor
; end of Victor's patch

result(ndat-2:ndat-1,*)=round(result(ndat-2:ndat-1,*))

if (keyword_set(flag)) then begin
    result(ndat-2:*,*)=result([ndat,ndat-2,ndat-1],*)
endif

return, result
end
; this ends the function



function getdx,tr,thedt,dim=dim
if (not keyword_set(dim)) then dim=3

;s=size(tr)
;trin=tr
;trin=[trin,transpose(intarr(s(2)))]

trin=gdxtrinterp(tr,/flag)

nel=n_elements(trin(*,0))
trins=shift(trin,0,-round(thedt(0)))
fl1 = trin(nel-3,*)
fl2 = trins(nel-3,*)
result=trins-trin
; ww=where((result(nel-1,*) eq 0) and (fl1 lt 0.5) and (fl2 lt 0.5))
w=where((result(nel-1,*) ne 0) or $
       (round(result(nel-2,*)) ne round(thedt(0))) or $
       (fl1 gt 0.5) or (fl2 gt 0.5),nw)
result=trins(0:dim,*)-trin(0:dim,*)
trins=0; free up memory
result(dim,*)=total(result(0:dim-1,*)^2,1)
result(dim,*)=sqrt(result(dim,*))
if (nw gt 0) then begin
    result(*,w)=-1
endif

w2=where(trin(nel-3,*) lt 0.5)
result=result(*,w2)
w3=where(result(dim,*) lt 0,nw3)
if (nw3 gt 0) then result(0:dim-1,w3) = 0

return,result
end

