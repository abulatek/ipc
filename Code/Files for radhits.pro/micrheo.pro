;function micrheo,tau,msd,a=a,dim=dim,T=T,clip=clip,width = width
;
;	***Mason-Weitz micro-rheology in the world of IDL!***
;
;    INPUTS:
;	RADIUS 'a' is in microns, 'msd' is in microns^2, 'tau' in seconds,
;	'T' is in Kelvin. The dimensionality 'dim' of 'msd' defaults to 3.  
;	It needs more than a 7-8 points per decade of time/frequency 
;	and really hates noise and long-wavelength ripples in the data.
;
;    OUTPUTS:
;	The output 'res' is a 4 column (4,ntau) array where:
;		res(0,*): is the frequency (s or omega), sec^{-1}.
;		res(1,*): is G(s)  in Pascals
;		res(2,*): is G'(w) in Pascals
;		res(3,*): is G"(w) in Pascals
;	Remember: in MKS (Pascals), water = 0.001 Pascal*secs = 0.01 Poise
;
;    NOTES:
;	G'(w) and G"(w) are clipped at 0.03x G(w) as they are almost 
;	certainly meaningless below that point unless the data is
;	*extremely* clean.  Set 'clip' to less than 0.03 to see more.
;	See Tom Mason's paper: PRL *79*, 3284, (1997) for details.
;
;    REVISION HISTORY:
;		Written by John C. Crocker, U. Penn:	5/'99
;		Made 'width' a keyword, JCC		7/'99
;		Change Gp,Gpp formulae, JCC		7/'99
;
;	This code 'micrheo.pro' is copyright 1999 by John C. Crocker
;	and may be freely distributed and used if properly attributed.
;
pro logderiv,x,f,f2,df,ddf,width=width
;
;	A special purpose routine for finding the first and
;	second logarithmic derivatives of slowly varying,
;	but noisy data.  It returns 'f2'-- a smoother version 
;	of 'f' and the first and second log derivative of f2.
;
np = n_elements(x)
df = fltarr(np)
ddf = fltarr(np)
f2 = fltarr(np)
lx = alog(x)
ly = alog(f)

for i=0,np-1 do begin
	w = exp( -(lx-lx(i))^2 / (2.*width^2) )	; a 'Gaussian'
	ww = where(w gt 0.03)	; truncate the gaussian, run faster
	res = polyfitw(lx(ww),ly(ww),w(ww),2)
	f2(i) = exp(res(0) + res(1)*lx(i) + res(2)*(lx(i)^2))
	df(i) = res(1)+(2d*res(2)*lx(i))
	ddf(i) = 2d*res(2)
endfor

end
;
function micrheo,tau,msd,a=a,dim=dim,T=T,clip=clip,width = width

; set the width to something bigger for noisy data, aok if the data
; is very weakly curved. 
if not keyword_set( width ) then width = 0.7

; set up the 'constants'
kB = 1.38065d-23			; MKS
if not keyword_set(T) then T = 290D	; approx. 17 degC
if not keyword_set(a) then a = 0.5D	; RADIUS in microns
if not keyword_set(dim) then dim=3	; seems to be the DWS/DLS standard
am = double(a)*1d-6			; convert microns to meters
dt = reform(tau)
omega = 1d/double(dt)
msdm = reform(msd)*1d-12		; convert msd to meters
C = dim*kB*T/(3*!pi*am)			; multiply by the dimensionality/3.
foo = (!pi/2d)-1d			; a handy constant
if not keyword_set(clip) $
	then clip = 0.03		; throw away 1.5 decades down

; use 2nd order local formula for G(s)-- good to 1% of G(s)
logderiv,dt,msdm,m,d,dd,width=width
Gs = C/((m*gamma(1d +d))*(1d +(dd/2d)))

; use 2nd order local formula for G'(w),G"(w)-- good to 2% of G(w)
logderiv,omega,Gs,g,da,dda,width=width
;Gp  = Gs*cos(((!pi/2d)+(foo*dda))*da)*(1-dda)		; obsolete
;Gpp = Gs*cos(((!pi/2d)+(foo*dda))*(1-da))*(1-dda)	; obsolete
Gp  = g*(1d/(1d +dda))* ( cos((!pi/2d)*da) - foo*da*dda )
Gpp = g*(1d/(1d +dda))* ( sin((!pi/2d)*da) - foo*(1-da)*dda )

; clip off the suspicious (i.e. unreliable) data
w = where(Gp lt Gs*clip,nw) 
if nw gt 0 then Gp(w)=0
w = where(Gpp lt Gs*clip,nw) 
if nw gt 0 then Gpp(w)=0

if (max(abs(dd)) gt 0.15) or (max(abs(dda)) gt 0.15) then message,$
	"Warning, high curvature in data, moduli may be unreliable!",/inf

return,[transpose(omega),transpose(Gs),transpose(Gp),transpose(Gpp)]

end






