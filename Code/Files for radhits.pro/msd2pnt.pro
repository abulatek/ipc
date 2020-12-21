;function msd2pnt,tracks,params,[outfile=outfile,micperpix=micperpix,$
;    timestep=timestep,dim=dim,mydts=mydts,erode=erode,nodedrift=nodedrift]
;
;    PURPOSE:
;
;	MSD2PNT -- a 2 or 3 dimensional space-time software correlator
;
;	This is a little routine for measuring the 2-point mean 
;	squared displacement from tracers in a viscoelastic medium. 
;	This can then be plugged into 'micrheo2pnt' to get the medium's
;	G',G"(w).  It is strictly defined as <x1*x2>(dr,dt), where the
;	average is over time and ensembles.  The result triple counts
;	in time, is logarithmically spaced in 'dt' and 'dr', and can be
;	process 2 or 3 dimensional 'track'ed data.
;
;    REQUIRED INPUTS:
;	'tracks' is EITHER a filename/wildcard that matches a gdf file 
;	  of output from 'track.pro' OR the trajectory array itself.  
;	  NB: If 'tracks' is a wildcard, MSD2PNT will combine the results 
;	  of all the files into one correlation function.
;	'params' is [rmin,rmax,nrbins,maxtime], determining the inner and
;	  outer cutoffs for the radius, the number of log-spaced bins in r
;	  and the maximum log-spaced time in seconds.  Lengths are in um.
;
;    OPTIONAL INPUTS:
;	'outfile' will save the result to a gdf file named 'outfile', which
;	  is useful for batch file operation.
;	'micperpix' is set to the pixel size to convert the input data
;	  file to microns if that has not been done already.  If it is
;	  single number, the magnification is assumed to be isotropic,
;	  otherwise, provide a dim-dimensional vector.
;	'timestep' is the number of seconds between frames (e.g. 1/60.)
;	'dim' is the spatial dimensionality of the data, defaults to 2.
;	'mydts' allows the user to supply a vector of (frame number) dt's
;	  if he/she doesn't want my log-spaced points in time.
;	'erode' drops all data within 'erode' time steps of an end or
;	  gap in a trajectory, to eliminate mistaken track contributions.
;	  Very useful-- usually erode=2 or 3 works pretty well.
;	'nodedrift'  Normally, MSD2PNT subtracts off the mean dx(dt) off
;	  the data prior to calculating the correlation, since drift in the
;	  data tends to overwhelm any small correlations.  Set /nodedrift
;	  if you want to disable this function, e.g. if you are looking
;	  at abnormally correlated systems with no drift.
;
;    OUPUTS:
;	The result 'res' has the form:
;	In 2 dimensions (polar coordinates):
;	  res(*,*,0) contains the log-spaced 'dr' values.
;	  res(*,*,1) contains the log-spaced 'dt' values.
;	  res(*,*,2) is the longitudinal, 'r-r' mean component.
;	  res(*,*,3) is the transverse, 'theta-theta' mean component.
;	  res(*,*,4:5) store the *variances* of the r,theta parts.
;	  res(*,*,6) contains the total number of points used in each bin.
;	In 3 dimensions (spherical coordinates):
;	  res(*,*,0) contains the log-spaced 'dr' values.
;	  res(*,*,1) contains the log-spaced 'dt' values.
;	  res(*,*,2) is the longitudinal, 'r-r' mean component.
;	  res(*,*,3) is the transverse, 'theta-theta' mean component.
;	  res(*,*,4) is the transverse, 'phi-phi' mean component.
;	  res(*,*,5:7) store the *variances* of the r,theta,phi parts.
;	  res(*,*,8) contains the total number of points used in each bin.
;
;    NOTES:
;	  The means are what you want, the variances and numbers are 
;	  intended for use in error estimation of the mean values.
;	  The means correspond to the diagonal components of a 
;	  correlation tensor that would be called 'S' in the 
;	  Fluctuation/Dissipation Theorem in Chaikin & Lubensky.
;	  The units of 'S' are um^2 if the inputs are in um.
;
;    MODIFICATION HISTORY:
;         Written by John C. Crocker, U. Penn:    		6/'99
;	  Modified to include autodetect field-of-view changes  3/'01
;
;       This code 'msd2pnt.pro' is copyright 2001 by John C. Crocker
;       and may be freely distributed and used if properly attributed.
;
pro ltrinterp,tarray
;
; 	started 6-17-98 by ERW, modified for use here 6/99 by JCC
;	interpolates gaps in tracked data (time,id only), zeros elsewhere
; 	tarray is an array of tracked data
;
ndat=n_elements(tarray(*,0))
dp=tarray-shift(tarray,0,1)
w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) ne 1),ngood)
if (ngood ge 1) then begin
	totdt=total(dp(ndat-2,w))-ngood
	dp=0
	storeres=fltarr(ndat,totdt)
	count = 0
	for i=0L,ngood-1L do begin
		dt = tarray(ndat-2,w(i)) - tarray(ndat-2,w(i)-1) 
		timer = tarray(ndat-2,w(i)-1) + findgen(1,dt-1) +1
		storeres(ndat-1,count:count+dt-2) = tarray(ndat-1,w(i)-1)
		storeres(ndat-2,count:count+dt-2) = timer 
		count = count + dt - 1
	endfor
	tarray = [[tarray],[storeres]]
	; watch out on other platforms' sorts!
	tarray = tarray(*,sort(tarray(ndat-2,*)))
	tarray = tarray(*,sort(tarray(ndat-1,*)))
endif
end
;
function laccumulate,list,binparams,dim=dim
;
;  accumulates total and total-squared results for the 'list' data
;  of get_corr, uses logarithmic binning and 'uniq' tricks to be fast.
;

; get the parameters 
lrmin = binparams(0)
lrmax = binparams(1)
lrbinsize = binparams(2)
nbins = binparams(3)

; declare the array we'll accumulate into
res = fltarr(nbins,(2*dim)+1)

; turn the r's into logarithmic bin numbers
r = fix( (alog(list(*,0))-lrmin)/lrbinsize )
nel = n_elements( list(*,0) ) 

; do the sorting and uniq'ing
s = sort(r)
sr = r(s)
slist = list(s,*)
u = [-1,uniq(sr)]
nu = n_elements(u)

for i=0,nu-2 do begin
	; Sadly, the following check is needed because of roundoff error.
	if sr(u(i)+1) ge 0 and sr(u(i)+1) lt nbins then begin
		res(sr(u(i)+1),0:dim-1) = $
			total( slist(u(i)+1:u(i+1),1:dim) ,1 )
		res(sr(u(i)+1),dim:(2*dim)-1) = $
			total( slist(u(i)+1:u(i+1),1:dim)^2 ,1 )
		res(sr(u(i)+1),(2*dim)) = u(i+1)-u(i)
	endif
endfor

return,res

end
;
;   does the funny double-counting, eroding, odd/even lag-time 'where'
;   on track-type data.
;
function do_where,t,dt,erode=erode,nw=nw,even=even,odd=odd

; make some local variables
ms = reform(t(0,*) eq 0)
tm = reform(t(1,*))
id = reform(t(2,*))
sms = shift(ms,-dt)
stm = shift(tm,-dt)
sid = shift(id,-dt)

if keyword_set(even) then parity=0
if keyword_set(odd) then parity=1

if keyword_set(erode) then begin
	mask = fltarr((2*fix(erode))+1.)+1
	bad = dilate(ms,mask)
	bad = bad or shift(bad,-dt)
	sid2 = shift(id,erode)
	sid3 = shift(id,-dt-erode)
	;we require that the point not be in a dilated 'bad'
	;mask (for gaps) nor within erode of an end. 
endif

if keyword_set(even) or keyword_set(odd) then begin
   if dt le 5 then begin
	if keyword_set( erode ) then begin
		w = where( (stm-tm) eq dt and (sid-id) eq 0 and $
		   (sid2-id) eq 0 and (sid3-id) eq 0 and (not bad) and $
		   ((tm mod 2) eq parity),nw)
	endif else begin
		w = where( (stm-tm) eq dt and (not ms) and (not sms) and $
		   (sid-id) eq 0 and ((tm mod 2) eq parity),nw)
	endelse
   endif else begin
	if (dt mod 2) eq 0 then mdt = dt else mdt = dt-1
	tmodt = tm mod mdt
	tar = parity*( (2*fix((dt-1)/4)) +1)
;	tar is an odd number about half as big dt.
	if keyword_set( erode ) then begin
		w = where( (stm-tm) eq dt and (sid-id) eq 0 and $
		   (sid2-id) eq 0 and (sid3-id) eq 0 and (not bad) and $
		   ((tm mod 2) eq parity) and $
		   (tmodt eq tar) ,nw)
	endif else begin
		w = where( (stm-tm) eq dt and (not ms) and (not sms) and $
		   (sid-id) eq 0 and ((tm mod 2) eq parity) and $
		   (tmodt eq tar) ,nw)
	endelse
   endelse
endif else begin
   if dt le 3 then begin
	if keyword_set( erode ) then begin
		w = where( (stm-tm) eq dt and (sid-id) eq 0 and $
		   (sid2-id) eq 0 and (sid3-id) eq 0 and (not bad),nw)
	endif else begin
		w = where( (stm-tm) eq dt and (not ms) and (not sms) and $
		   (sid-id) eq 0 ,nw)
	endelse
   endif else begin
	tmodt = tm mod dt
	if keyword_set( erode ) then begin
		w = where( (stm-tm) eq dt and (sid-id) eq 0 and $
		   (sid2-id) eq 0 and (sid3-id) eq 0 and (not bad) and $
		   ((tmodt eq 0) or (tmodt eq round(dt/2.))) ,nw)
	endif else begin
		w = where( (stm-tm) eq dt and (not ms) and (not sms) and $
		   (sid-id) eq 0 and $
		   ((tmodt eq 0) or (tmodt eq round(dt/2.))) ,nw)
	endelse
   endelse
endelse

return,w

end
;
;   calculates 'brownian correlation' components for track data....
;
function get_corr,t,lbinparams,dt,erode=erode,dim=dim,nodedrift=nodedrift

nreport = 250

; declare some arrays
bres = fltarr(lbinparams(3),(2*dim)+1)
ncol = n_elements(t(*,0))
nid  = max(t(ncol-1))

; get the 'r' ranges (squared).
rminsq = exp(2*lbinparams(0))
rmaxsq = exp(2*lbinparams(1))

ntot = 0
time0 = 0
for p=0,1 do begin ; loop over odd->even and even->odd events

; call the big where function....
if p eq 0 then $
   w = do_where(t([0,ncol-2,ncol-1],*),dt,erode=erode,nw=nw,/even) $
else $
   w = do_where(t([0,ncol-2,ncol-1],*),dt,erode=erode,nw=nw,/odd)
ntot = ntot+nw

if (nw gt 1) then begin  ;otherwise skip to the other parity

; make up the data
xs = fltarr((2*dim),nw)
xs(0:dim-1,*) = t(0:dim-1,w)
st = shift(t(0:dim-1,*),0,-dt)
xs(dim:(2*dim)-1,*) = st(*,w)

; dedrift, if we're allowed to by the user.
if not keyword_set(nodedrift) then begin
	dx = total(xs(dim:2*dim-1,*)-xs(0:dim-1,*),2)/nw
	for i=0,dim-1 do xs(dim+i,*) = xs(dim+i,*) - dx(i) 
endif

; get the time and id info
ti = t(ncol-2,w)
id = t(ncol-1,w)

; sort the data by time
s = sort(ti)
for i = 0,(2*dim)-1 do xs(i,*) = xs(i,s)	; helps with memory usage!
ti = ti(s)
id = id(s)

; get the indices of the unique *times* 
u = uniq(ti)
ntimes = n_elements(u)
u = [-1,u]

; get the maximum number of beads in one frame pair
su = shift(u,-1)
maxn = max( su(0:ntimes-1)-u(0:ntimes-1) )
maxlist = maxn^2/2

; define a triangular raster scan list
bamat = make_array( maxn, maxn, /long, /index ) mod maxn
bbmat = transpose( bamat )
w = where(bbmat gt bamat,nw0)
if (nw0 gt 0) then begin
	bamat = bamat(w)
	bbmat = bbmat(w)
endif

; define the scratchpad (list)
buf = maxlist > 2e4	; 2e4 element lists sort the fastest
list = fltarr(buf,dim+1)
point = -1

; loop over the times
for i = 0D,ntimes-1D do begin

	; get the relevant data for the ith time.
	lxs = xs( *,u(i)+1:u(i+1) )
	ngood = n_elements(lxs(0,*))

	if ngood gt 1 then begin	; we should check!

;	  fast N^2 distance calculator, inspired by 'track'
	  ntri = ngood*(ngood-1)/2.
	  amat = bamat(0:ntri-1)
	  bmat = bbmat(0:ntri-1)
	  rsq = total( (lxs(0:dim-1,amat)-lxs(0:dim-1,bmat))^2 ,1)

	  w = where((rsq lt rmaxsq) and (rsq gt rminsq),nok)

	    if nok gt 0 then begin

;		calculate the sep. vectors
		r   = sqrt(rsq(w))
		amatw = amat(w)
		bmatw = bmat(w)
		rx  = lxs(0,amatw) - lxs(0,bmatw)
		ry  = lxs(1,amatw) - lxs(1,bmatw)
		xa1 = reform(lxs(0,amatw))
		ya1 = reform(lxs(1,amatw))
		xa2 = reform(lxs(dim,amatw))
		ya2 = reform(lxs(dim+1,amatw))
		xb1 = reform(lxs(0,bmatw))
		yb1 = reform(lxs(1,bmatw))
		xb2 = reform(lxs(dim,bmatw))
		yb2 = reform(lxs(dim+1,bmatw))
		dxa = xa2 - xa1
		dya = ya2 - ya1
		dxb = xb2 - xb1
    		dyb = yb2 - yb1

		if dim eq 2 then begin

			; calculate the longitudinal part
			ran = 1-2*(randomu(seed,nok) ge 0.5)  ; randomize
			nx  = rx*ran/r			; unit vector r1
			ny  = ry*ran/r
			ddl = ((dxa*nx)+(dya*ny))*((dxb*nx)+(dyb*ny))

			; calculate the transverse part
			ran = 1-2*(randomu(seed,nok) ge 0.5)  ; randomize
			px  = ny*ran			; ortho unit vector
			py  = -nx*ran
			ddt = ((dxa*px)+(dya*py))*((dxb*px)+(dyb*py))

			; add to the list
			list(point+1:point+nok,0) = r
			list(point+1:point+nok,1) = ddl
			list(point+1:point+nok,2) = ddt
			point = point+nok

		endif else begin

			; get a couple more sep vectors
			rz  = lxs(2,amatw) - lxs(2,bmatw)
			za1 = reform(lxs(2,amatw))
			za2 = reform(lxs(dim+2,amatw))
			zb1 = reform(lxs(2,bmatw))
			zb2 = reform(lxs(dim+2,bmatw))
			dza = za2 - za1
			dzb = zb2 - zb1

			; calculate the longitudinal part
			ran = 1-2*(randomu(seed,nok) ge 0.5)  ; randomize
			nx  = rx*ran/r
			ny  = ry*ran/r
			nz  = rz*ran/r
			ddl = ((dxa*nx)+(dya*ny)+(dza*nz))* $
				((dxb*nx)+(dyb*ny)+(dzb*nz))

			; calculate the phi transverse part
			ran = 1-2*(randomu(seed,nok) ge 0.5)  ; randomize
			rxy = sqrt(rx^2 + ry^2)			
			px1  = ry*ran/rxy 
			py1  = -rx*ran/rxy
			ddp = ((dxa*px1)+(dya*py1))*((dxb*px1)+(dyb*py1))

			; calculate the theta transverse part
			ran = 1-2*(randomu(seed,nok) ge 0.5)  ; randomize
			px2  = -(nz*py1)*ran
			py2  = (nz*px1)*ran
			pz2  = (nx*py1-ny*px1)*ran
			ddt = ((dxa*px2)+(dya*py2)+(dza*pz2))* $
				((dxb*px2)+(dyb*py2)+(dzb*pz2))

			; add to the list
			list(point+1:point+nok,0) = r
			list(point+1:point+nok,1) = ddl
			list(point+1:point+nok,2) = ddt
			list(point+1:point+nok,3) = ddp
			point = point+nok

		endelse
	     endif
	 endif

	; do the running totals if the buffer gets full
	if point gt (buf-maxlist) then begin
		bres = bres + laccumulate(list(0:point,*),lbinparams,dim=dim)
		point=-1
	endif

	if ((i+1)+time0) mod nreport eq 0 then $
		print,round(i+1)+time0,2*ntimes,$
			long(total(bres(*,(2*dim))))+point

endfor  ; time loop

; finish the running totals
if point ge 0 then $
	bres = bres + laccumulate(list(0:point,*),lbinparams,dim=dim)

if p eq 0 then time0 = ntimes
endif	; nw check
endfor  ; parity loop

print,round(i+1)+time0,2*ntimes,long(total(bres(*,(2*dim))))

if ntot gt 0 then return,bres $
else return,-1

end
;
;	A little routine to autodetect when the user has switched
;	the field of view.  It returns the times when the switches occur
;	as a two column list.  A switch is defined as any time where
;       more new id's appear than old id's continue.  If maxdisp is
;	set really large, then this might not be good enough!
;
function lseverfov,trk

; get some info
cid = n_elements(trk(*,0))-1
nid = max(trk(cid,*))
maxt = max(trk(cid-1,*))
mint = min(trk(cid-1,*))

; set up list of id's and times
id = trk(cid,*)
t = trk(cid-1,*)

; sort same
s = sort(t)
st = t(s)
sid = id(s)

; get the time dividers
ut = uniq(st)
nt = n_elements(ut)
ut = [-1,ut]

; make up an id/time table, this is a nifty thing by itself
hash = bytarr(nt,nid)
for i = 0L,nt-1L do hash(i,sid(ut(i)+1:ut(i+1))) = 1b

; find out when we had a lot of turnover
old = bytarr(nt)
new = bytarr(nt)
for i = 1L,nt-1L do begin
	old(i) = total(  hash(i-1,*)*hash(i,*)  )
	new(i) = total( (1b-hash(i-1,*))*hash(i,*)  )
endfor

w = where(new gt old,nw)

if nw eq 0 then res = [mint,maxt] else begin
	res = fltarr(2,nw+1)
	res(0,0) = mint
	res(1,nw) = maxt
	res(0,1:*) = st(ut(w))
	res(1,0:nw-1) = st(ut(w-1))
endelse

return,res

end
;
;	The actual routine itself.
;	'params' is [rmin,rmax,nrbins,dtmax].
;
function msd2pnt,tracks,params,outfile=outfile,micperpix=micperpix,$
    timestep=timestep,dim=dim,mydts=mydts,erode=erode,nodedrift=nodedrift

if not keyword_set(dim) then dim = 2
if not keyword_set(micperpix) then micperpix = dblarr(dim)+1D else begin
	if n_elements(micperpix) eq 1 then $
		 micperpix = dblarr(dim)+micperpix(0)
endelse
if not keyword_set(timestep) then timestep = 1d

; read the gdf file if 'tracks' is a string
sz = size(tracks)
nz = n_elements(sz)
if sz(nz-2) eq 7 then begin
	f = findfile(tracks)
	if f(0) eq '' then message,'No Match: '+tracks
	nf = n_elements(f)
	filebased = 1
endif else begin
	filebased = 0
	nf = 1
endelse

if not keyword_set(mydts) then begin
	; generate the time partition-- about 10 points per decade
	dt = round(1.15^findgen(100))
	dt = dt(uniq(dt))
	w = where( dt le round(params(3)/timestep), ndt )
	if ndt gt 0 then dt = dt(w) else message,'Invalid maximum dt!"
endif else begin
	dt = mydts
	ndt = n_elements(dt)
endelse

; generate the 'r' partition-- as the user wishes.
nbins = params(2)
lmax = alog(params(1)) & lmin = alog(params(0))
lrbinsize = (lmax-lmin)/nbins
rpart = exp( lmin+(findgen(nbins+1)*lrbinsize ))  ; the r partitions
r = sqrt( rpart * shift(rpart,-1) )
r = r(0:nbins-1)				  ; the r bin 'midpoints'

; set up some arrays-- the running sums of the data
data = dblarr(nbins,ndt,(2*dim)+3)	; sL,sT,sL^2,sT^2,N(r,t) and t,r

; do the big nested loop
for i=0,nf-1 do begin
	if filebased then begin
		message,'Reading file: '+f(i),/inf
		trj = read_gdf(f(i))
	endif else trj = tracks
	; convert to um.
	for j=0,dim-1 do trj(j,*) = trj(j,*)*micperpix(j)
	; check for fov changes
	
        ;seg = lseverfov(trj)
	;nseg = n_elements(seg(0,*))
        nseg = 1
        if nseg gt 1 then begin
		message,'Found field of view breaks at time (seconds):',/inf 
		print,reform(seg(0,1:*))*timestep
	endif

	; loop over the different fov's
	for k = 0,nseg-1 do begin
	    if nseg gt 1 then begin
		message,'Processing FOV spanning times (seconds):',/inf
		print,seg(*,k)*timestep
		ncol = n_elements(trj(*,0))
		t = trj(ncol-2,*)
		w = where(t ge seg(0,k) and t le seg(1,k))
		trk = uberize(trj(*,w))
	    endif else trk = trj
	    ltrinterp,trk				; fill in gaps.
	    for j=0,ndt-1 do begin
		if filebased then $
		   message,'Processing file: '+f(i)+$
			'	dt equals: '+strcompress(string(dt(j))),/inf $
		   else message,'	dt equals: '$
			+strcompress(string(dt(j))),/inf
		res = get_corr(trk,[lmin,lmax,lrbinsize,nbins],$
			dt(j),erode=erode,dim=dim,nodedrift=nodedrift)
		; accumulate the running sums
		if res(0) ne -1 then $
		   data(*,j,0:(2*dim)) = data(*,j,0:(2*dim)) + res 
	    endfor
	endfor
endfor

; make the running sums into means and variances.
for i = 0,(2*dim)-1 do data(*,*,i) = data(*,*,i)/data(*,*,2*dim) 
data(*,*,dim:(2*dim)-1) = data(*,*,dim:(2*dim)-1) - (data(*,*,0:dim-1)^2)

; put on dt,dr vectors (slightly redundant data structure, but hey)
for i = 0,ndt-1 do data(*,i,(2*dim)+1) = r
for i = 0,nbins-1 do data(i,*,(2*dim)+2) = dt*timestep

; shift the array so dr,dt is on the front like with msd.pro
data = shift(data,0,0,2)

; optionally write out a file
if keyword_set(outfile) then begin
	message,'Writing file: '+outfile,/inf
	write_gdf,data,outfile
endif

return,data

end




