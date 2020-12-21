; motion.pro			4-17-00   Eric R. Weeks
;
; determines average displacement as a function of time for dt=1
; returns (t,dr,dx,dy)
;
; 6-13-05:  rewritten nearly from scratch, much better
;
; for more information see:
;       http://www.physics.emory.edu/~weeks/idl/motion.html

function motion,tr,dim=dim,smoo=smoo
; smoo strictly for display purposes

if (not keyword_set(smoo)) then smoo=10
if (not keyword_set(dim)) then dim=2

ndat=n_elements(tr(*,0))
dt=1
totaltime=max(tr(ndat-2,*),min=mintime)
result=dblarr(dim+2,totaltime+1)

dx=getdx(tr,dim=dim,dt)
s=sort(tr(ndat-2,*))
u=uniq(tr(ndat-2,s))
nu=n_elements(u)
u=[-1,u]
for i=1L,nu do begin
	tr0=tr(*,s(u(i-1)+1:u(i)))
	t0 = tr0(ndat-2,0)
	dx0=dx(*,s(u(i-1)+1:u(i)))
	result(0,t0-mintime) = t0
	w=where(dx0(dim,*) gt -0.5,nw)
	if (nw gt 0) then begin
		dx00=dx0(*,w)
		result(2:dim+1,t0-mintime)=total(dx00(0:dim-1,*),2)/nw
		result(1,t0-mintime) = nw
	endif
endfor

w=where(result(1,*) gt 0,nw)
foo=result(*,w)
foo(1,*)=sqrt(total(foo(2:dim+1,*)^2,1))
result(*,w)=foo
temp=rm_motion(tr,result(*,w),dim=dim,/just,smoo=smoo)
return,result(*,w)
end
