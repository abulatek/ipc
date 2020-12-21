; mkpdf  --  Eric R. Weeks  --  new version 5-20-05
;
; see http://www.physics.emory.edu/~weeks/idl/
;    for more information and software updates
;
;
;
; basically this utilizes very similar code to getdx.pro, which
; I wrote a while ago.


function mkpdf,tr,dt,dim=dim

if (not keyword_set(dt)) then dt=1
if (not keyword_set(dim)) then dim=2

dx=getdx(tr,dt,dim=dim)
w=where(dx(dim,*) gt -0.5,nw)
if (nw gt 0) then begin
	dx=dx(0:dim-1,w)
endif else begin
	dx = 0.0
endelse

return,dx
end

