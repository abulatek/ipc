;
;	A little function for making 'distinct' msd's out of D_par,D_perp
;
;       It returns a data data structure which is (6,ndt) where the
;       the (0,*) column is the dt, in seconds
;           (1,*) column is the Drr, in um^2
;           (2,*) column is the Dtt, in um^2
;           (3:4,*) is the standard error in Drr,Dtt, in um^2
;	    (5,*) column contains N's appropriate for weighted averages 
;
;       NB: errors may be greater than expected due to correlation
;       effects in the data.  Also, error estimates and N weighting
;       are only approximate when using 'lfit'
;
function msdd,msd2p,rmin,rmax,tmax = tmax,a=a,lfit=lfit

if not keyword_set(a) then a = 1D

; read the gdf file if 'msd2p' is a string
sz = size(msd2p)
if sz(1) eq 7 then begin
	f = findfile(msd2p)
	if f(0) eq '' then message,'No Match: '+msd2p
	msd2 = read_gdf(f(0))
endif else msd2 = msd2p

; get the dimensionality
if n_elements(msd2(0,0,*)) eq 9 then dim=3 else dim=2

; get dt's
dt = reform(msd2(0,*,1))
if keyword_set(tmax) then dt = dt(where(dt le tmax))
ndt = n_elements(dt)

;get dr's
dr = reform(msd2(*,0,0))
w = where(dr ge rmin and dr le rmax,nr)
if nr eq 0 then begin
	message,'No dr data in stated [rmin,rmax] interval!',/inf
	return,-1
endif
dr = dr(w)

; calculate the msd_d's
msd2w = msd2(w,*,*)
msd1 = dblarr(6,ndt)
msd1(0,*) = dt
r = reform(msd2w(*,0,0))			; get the radii	

for i=0,ndt-1 do begin
	ns = reform(msd2w(*,i,6))
        if total(ns) gt 0 then begin            
            ww = where(ns gt 0)                         ; no test
            rww = r(ww)
            parr = reform(msd2w(ww,i,2))*rww            ; rDrr
            perpr = reform(msd2w(ww,i,3))*rww           ; rDtt
            parre2 = reform(msd2w(ww,i,4))*rww/ns(ww)   ; sq. error in rDrr
            perpre2 = reform(msd2w(ww,i,5))*rww/ns(ww)  ; sq. error in rDtt

            nwgt = ns(ww)/((rww/a)^2)                   ; weighting function
            tnwgt = total(nwgt)                         ; norm. factor

            if tnwgt gt 0 then begin
               if not keyword_set(lfit) then begin
                   msd1(1,i) = (2D/(3D*a))*total(parr*nwgt)/tnwgt
                   msd1(2,i) = (4D/(3D*a))*total(perpr*nwgt)/tnwgt
               endif else begin
                   fit1 = poly_fit(rww,parr,1)
                   fit2 = poly_fit(rww,perpr,1)
                   msd1(1,i) = fit1(0)*(2D/(3D*a))
                   msd1(2,i) = fit2(0)*(4D/(3D*a))
               endelse

               ; get the error and effective N data
               msd1(3,i) = (2D/(3D*a))*sqrt(total(parre2*nwgt)/tnwgt)
               msd1(4,i) = (4D/(3D*a))*sqrt(total(perpre2*nwgt)/tnwgt)
               msd1(5,i) = tnwgt
             endif    
	endif
endfor

return,msd1
end



