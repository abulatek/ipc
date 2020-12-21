; epretrack  -- revised version of jpretrack (from John C.  Crocker)
; 
; Eric R. Weeks 2-15-05; this version June 8, 2008
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
; Options related to file type:
;     /gdf = use read_gdf
;     /tiff = use read_tiff
;     /multi = use readtiffstack (multiple-image tiff files)
;     /noran = use read_noran (Noran Oz confocal files)
;     /nih = use read_nih (NIH Image files)
;
; Additional options:
;     /quiet = supress printing messages
;     thresh = use a threshold (of the specified value) before bpass
;     /nofix = don't try to fix a bug with noran files
;     prefix = "pt." by default

pro epretrack,stk, $
    bplo=bplo,bphi=bphi,dia=dia,sep=sep,min=min,mass=mass, $
    invert=invert,field=field,first=first,fskip=fskip,  $
	gdf=gdf,tiff=tiff,noran=noran,nih=nih,multi=multi, $
	quiet=quiet,thresh=thresh,nofix=nofix,prefix=prefix

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
	filen = findfile(stk)
	if filen(0) eq '' then message,"No file '"+stk+"' found"
	nfiles = n_elements(filen)
	usingfiles = 1
endif else begin
	nfiles = 1
	usingfiles = 0
endelse

;  fskip is the frame/field # increment during time lapse video
if not keyword_set(fskip) then fskip=1
rep = 1

for j =0,nfiles-1 do begin

	if (usingfiles eq 1) then begin
		print,'reading image stack: '+filen(j)
		if (keyword_set(gdf)) then stk=read_gdf(filen(j))
		if (keyword_set(tiff)) then stk=read_tiff(filen(j))
		if (keyword_set(multi)) then stk=readtiffstack(filen(j))
		if (keyword_set(nih)) then stk=read_nih(filen(j))
		if (keyword_set(noran)) then begin
			stk=read_noran(filen(j),/lomem)
			if (not keyword_set(nofix)) then begin
				nnoranfix=n_elements(stk(0,0,*))
				foo=fltarr(nnoranfix-1)
				; next bit from "imagecor2"
				if (nnoranfix lt 200) then begin
						for i=1,nnoranfix-1 do begin
							foo(i-1)=correlate(stk(*,*,i-1),stk(*,*,i))
						endfor
						temp=min(foo,minorfix)
				endif else begin
						for i=nnoranfix-100,nnoranfix-1 do begin
							foo(i-1)=correlate(stk(*,*,i-1),stk(*,*,i))
						endfor
						temp=min(foo(nnoranfix-100:nnoranfix-2),minorfix)
						minorfix=minorfix+nnoranfix-100
				endelse
			endif
		endif
	endif else begin
		print,'using the image array given to me'
	endelse

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

	if (not keyword_set(nofix) and keyword_set(noran)) then begin
		nn=n_elements(res(*,0))
		res(nn-1,*)=res(nn-1,*) + (nnoranfix-minorfix-1)
		res(nn-1,*)=res(nn-1,*) mod nnoranfix
		s=sort(res(nn-1,*))
		res=res(*,s)
	endif

	if (usingfiles eq 1) then begin
		wname = prefix+filen(j)
	endif else begin
		if (prefixflag eq 0) then begin
			wname = "pretrack.gdf"
			print,"I wasn't given a file name, so writing data"
			print,"to default file: ",wname
		endif else begin
			wname = prefix
		endelse
	endelse
	if (not keyword_set(quiet)) then $
		print,'writing output file to:' + wname
	write_gdf,res(*,1:*),wname
endfor

; minor bug-fix 2-1-12 Eric Weeks
if (keyword_set(invert) and (usingfiles eq 0)) then stk = 255b-stk

end
