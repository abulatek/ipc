; stkpretrack  -- revised version of epretrack
; 
; Eric R. Weeks 2-15-05, 1-6-08
;
; see:   http://www.physics.emory.edu/~weeks/idl/
;
; routine to analyze time-series stacks or single images to
;    determine where particles are located
;
;  ppretrack -- Peter's version (begun 7/8/97)
;  jpretrack -- John's version (begun 7/8/98)
;  epretrack -- Eric's version (begun 2/15/05)
;
; Options related to feature & bpass:
;     bplo,bphi,dia,sep,min,mass
;
;     img = the image to be examined
;     b = bpass(img,bplo,bphi)
;     f = feature(b,dia,sep,min=min,masscut=mass)
;
; Options related to the image:
;     /invert = invert the image before bpass
;     /field = process even & odd fields separately (only for CCD cameras)
;     /first = only process first & second frames from each file
;              (useful for a quick check)
;     fskip = regrid the time stamp (useful for time-lapse)
;              (this option mostly unused)
;
;
; Additional options:
;     /quiet = supress printing messages
;     thresh = use a threshold (of the specified value) before bpass
;     /nofix = don't try to fix a bug with noran files
;     prefix = "pt." by default
;
;  bug fixed by Kazem Edmond, 6/27/08
; Kazem's change: the first row of the results from stkpretrack.pro
; are all zeroes.  This version removes them prior to return.


function stkpretrack,stk, $
    bplo=bplo,bphi=bphi,dia=dia,sep=sep,min=min,mass=mass, $
    invert=invert,field=field,first=first,fskip=fskip,  $
	  quiet=quiet,thresh=thresh,nofix=nofix

msg='Defaults:'
if (not keyword_set(bplo)) then begin
	bplo = 1 & msg=msg+" bplo=1"
endif
if (not keyword_set(bphi)) then begin
	bphi = 5 & msg=msg+" bphi=5"
endif
if (not keyword_set(dia)) then begin
	dia = 9 & msg=msg+" dia=9"
endif
if (not keyword_set(sep)) then begin
	sep = dia+1;  this is what feature uses as a default
	msg=msg+" sep-unset"
endif
if (not keyword_set(min)) then begin
	min = 0
	msg=msg+" min-unset"
endif
if (not keyword_set(mass)) then begin
	mass = 0
	msg=msg+" mass-unset"
endif
prefixflag=1; means user set prefix
if (not keyword_set(prefix)) then begin
	prefix='pt.'
	prefixflag = 0; means user didn't set prefix
endif

if (not keyword_set(quiet)) then begin
	slen = strlen(msg)
	if (slen lt 10) then msg=msg+" no default values used, all user-defined"
	print,"starting epretrack..."
	print,msg
endif

if (size(stk,/type) eq 7) then begin
	message,'you must use an image stack, not a filename'
endif else begin
	nfiles = 1
	usingfiles = 0
endelse

;  fskip is the frame/field # increment during time lapse video
if not keyword_set(fskip) then fskip=1
rep = 1


	print,'using the image array given to me'

	if keyword_set(invert) then stk = 255b-stk
	if keyword_set(thresh) then stk = (stk > thresh)

	ns = n_elements(stk(0,0,*))
	if ns gt 200 then rep = 25

	ss=size(stk(*,*,0))
	sx = ss(1)

	if keyword_set(first) then ns = 1  ;handy for a quick looksee...

	res = fltarr(6)
	if keyword_set(field) then begin
	   for i = 0,ns-1 do begin
		if (((i+1) mod rep eq 0) and not keyword_set(quiet)) then begin
 			print,'processing fields of frame'$
			+strcompress(i+1)+' out of'+strcompress(ns)+'....'
		endif
		; the next five lines are from J Crocker's "fieldof"
		sz=size(array) & img=reform(stk(*,*,i))
		f=0 & ny2=fix( (sz(2)+(1-f))/2 ) & rows=indgen(ny2)*2 + f
		evenfield = img(*,rows)
		f=1 & ny2=fix( (sz(2)+(1-f))/2 ) & rows=indgen(ny2)*2 + f
		oddfield = img(*,rows)
		im = bpass(evenfield,bplo,bphi,/field)
		f = feature(im(2*bphi:sx-2*bphi,*),dia,sep,min=min, $
				mass=mass,/field,quiet=quiet)
		if f(0) ne -1 then begin
			f(0,*,*) = f(0,*,*)+2*bphi
			nf = n_elements(f(0,*))
			res = [[res],[f,fltarr(1,nf)+2*i*fskip]]
		endif
		im = bpass(oddfield,bplo,bphi,/field)
		f = feature(im(2*bphi:sx-2*bphi,*),dia,sep,mass=mass, $
				min=min,/field,quiet=quiet)
		if f(0) ne -1 then begin
			f(0,*,*) = f(0,*,*)+2*bphi
			f(1,*) = f(1,*)+1
			nf = n_elements(f(0,*))
			res = [[res],[f,fltarr(1,nf)+2*i*fskip+1]]
		endif
	   endfor
	endif else begin
	   for i = 0,ns-1 do begin
		if (((i+1) mod rep eq 0) and not keyword_set(quiet)) then begin
			 print,'processing frame'+$
			strcompress(i+1)+' out of'+strcompress(ns)+'....'
		endif
		im = bpass(stk(*,*,i),bplo,bphi)
		f = feature(im,dia,sep,mass=mass,min=min,quiet=quiet)
		nf = n_elements(f(0,*))
		if (f(0) ne -1) then res = [[res],[f,fltarr(1,nf)+i*fskip]]
	   endfor
	endelse

return,res[*,1:*]
end
