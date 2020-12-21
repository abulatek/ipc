;function msd,tracks,[maxtime=maxtime,outfile=outfile,micperpix=micperpix,$
;	timestep=timestep,dim=dim,mydts=mydts,erode=erode,minN=minN,quiet=quiet,noplot=noplot,keepdrift=keepdrift]
;
;    PURPOSE:
;
;	MSD -- calculates 2 or 3 dimensional mean-squared displacements
;		and drift rates
;
;    REQUIRED INPUTS:
;	'tracks' is EITHER a filename/wildcard that matches gdf files
;	  from 'track.pro' OR the track output array itself.  The
;	  average is calculated for all valid pairs of positions--
;	  i.e. it is overcounted.
;	  NB: if 'tracks' is a wildcard matching several files, the
;	  routine will combine the results into one MSD.
;
;    OPTIONAL INPUTS:
;	'maxtime' is the maximum 'dt' to calculate, in seconds.  If
;	  this is not set, MSD computes out in dt until fewer than
;	  1000 independent measurements are being averaged.  Setting
;	  'maxtime' or 'mydts' overrides this limitation, of course.
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
;   'minN' sets minimum number of independent measurements; only useful
;     maxtime is not used, e.g. in batch processing of plates
;    noplot allows you to NOT plot anything at the end. Added by GLH 02/18/2011
;    keepdrift does not remove <x>^2
;    OUTPUTS:
;	The result array has several columns:
;	In 2 dimensions:
;	  result is a (7,n_times) array of the form
;	  [time,<x>,<y>,<x^2>,<y^2>,<x^2 + y^2>,<N>], where N is the
;	  approx. independent number of data points in the average.
;	In 3 dimensions:
;	  result is a (9,n_times) array of the form
;	  [time,<x>,<y>,<z>,<x^2>,<y^2>,<z^2>,<x^2 + y^2 + z^2>,<N>],
;	  where N is the independent number of data points in the average.
;	NB: for all variances, <x^2> has had <x>^2 subtracted off.
;
;    NOTES:
;	The variances are presumably what you are interested in.  The
;	means are there merely to give you an idea of the average 'drift'
;	in the data (and because <x>^2 has to be subtracted from the
;	variances).  'N' can be used for error estimation: the error
;	in the variance < x(dt)^2 > ~ < x(dt)^2 >/sqrt( N(dt) ).
;
;    MODIFICATION HISTORY:
;       * Written by John C. Crocker, U. Penn:    6/'99
;
;       This code 'msd.pro' is copyright 1999 by John C. Crocker
;       and may be freely distributed and used if properly attributed.
;
;       * Adapted by Victor Breedveld, UCSB: 5/'02; included minN keyword
;       * Adapted by Victor Breedveld, GT: 12/'03;
;                           - fixed sorting bug in LTRINTERP.PRO under DOS (Windows)
;                             --> interpolation routine now named LTRINTERP_VB
;                           - UNTESTED UNDER LINUX/UNIX
;       * Adapted by Victor Breedveld, GT: 06/'04;
;                           - adapted to work for single particle tracks!!!
;       * Adapted by Victor Breedveld, GT: 07/'04;
;                           - adapted to work with results of poor tracking!!!

pro ltrinterp_vb,tarray
;
; 	started 6-17-98 by ERW, modified for use here 6/99 by JCC, Windows bug fixed 12/03 by VB
;	interpolates gaps in tracked data (time,id only), zeros elsewhere
; 	tarray is an array of tracked data
;
ndat=n_elements(tarray(*,0))
dp=tarray-shift(tarray,0,1)
w=where((dp(ndat-1,*) eq 0) and (dp(ndat-2,*) ne 1),ngood)
if dp(ndat-1,0) eq 0 then begin ;in case tarray is single-particle track
    if n_elements(w) gt 1 then w = w(1:*)
    ngood = ngood -1
endif
if (ngood ge 1) then begin
    totdt=total(dp(ndat-2,w))-ngood
    if totdt gt 0 then begin	; a subtle case, but it happens!
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
	; VB 12/03: fixed sorting problem for Windows based IDL!
	tarray = tarray(*,sort(tarray(ndat-1,*)))
	u=[[-1],uniq(tarray(ndat-1,*))]
	nu=n_elements(u)
	for i=1L,nu-1L do begin
	  tpart=tarray(*,u(i-1)+1:u(i))
      tarray(*,u(i-1)+1:u(i))= tpart(*,sort(tpart(ndat-2,*)))
    endfor
    endif
endif
end
;
;   figures out which 'drs' to use with my big gnarly where
;
function get_drs,t,dt,erode=erode,dim=dim

; declare some arrays
ncol = n_elements(t(*,0))
res = dblarr((2*dim)+2)

; get the rows, we do all for dt le 3, and 'triple count' for bigger dts.
st = shift(t,0,-dt)
if keyword_set( erode ) then begin
	mask = fltarr((2*fix(erode))+1.)+1
	bad = dilate(reform(t(0,*) eq 0),mask)
	bad = bad or shift(bad,-dt)		; oink!
	; next few lines rewritten 8-21-00 ERW to be less memory intensive
	w1=where( (st(ncol-2,*)-t(ncol-2,*) eq dt) and $
        (st(ncol-1,*)-t(ncol-1,*)) eq 0 and (not bad),nw1 )
	;we require that the point not be in a dilated 'bad'
	;mask (for gaps) nor within erode of an end:
	if (nw1 gt 0) then begin
		st2 = shift(t,0,erode)
		w2=where(st2(ncol-1,w1) -t(ncol-1,w1) eq 0,nw2)
		st2 = 0
		if (nw2 gt 0) then begin
			st3 = shift(t,0,-dt-erode)
			w3 = where((st3(ncol-1,w1(w2))-t(ncol-1,w1(w2))) eq 0,nw3)
			if (nw3 gt 0) then begin
				w = w1(w2(w3))
				nw = nw3
				w1=0 & w2=0 & w3=0
			endif else begin
			endelse
		endif else begin
			w=w2 & w2=0 & w1=0
			nw=nw2
		endelse
	endif else begin
		w=w1 & w1=0
		nw=nw1
	endelse
endif else begin
	w = where( (st(ncol-2,*)-t(ncol-2,*)) eq dt and $
	   (t(0,*) ne 0) and (st(0,*) ne 0) and $
	   (st(ncol-1,*)-t(ncol-1,*)) eq 0 ,nw)
endelse

; get the dx's
if (nw gt 0) then begin
	dxyz = t(0:dim-1,w) - st(0:dim-1,w)
	dxyzsq = dxyz^2
    if nw eq 1 then begin ;DEAL WITH CASE OF VERY SPARSE TRACKS
      print, "WARNING: tracks with very sparse data --> Check ERODE and GOODENOUGH"
      dxyz = reform(dxyz,dim,1)
      dxyzsq = reform(dxyzsq,dim,1)
    endif
	; do the running totals
	res(0:dim-1) = total(dxyz,2)
    res(dim:(2*dim)-1) = total(dxyzsq,2)
    res(2*dim+1) = nw
endif

return,res

end
;
;	The actual routine itself.
;
function msd,tracks,maxtime=maxtime,outfile=outfile,micperpix=micperpix,$
	timestep=timestep,dim=dim,mydts=mydts,erode=erode,minN=minN,quiet=quiet, $
    noplot=noplot,keepdrift=keepdrift

if not keyword_set(minN) then minN = 1000		; because I said so!

if not keyword_set(dim) then dim = 2
if not keyword_set(micperpix) then micperpix = dblarr(dim)+1D else begin
	if n_elements(micperpix) eq 1 then $
		 micperpix = dblarr(dim)+micperpix(0)
endelse
if not keyword_set(timestep) then timestep = 1d
if not keyword_set(maxtime) then maxdt = 1e6 else maxdt = maxtime

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
	w = where( dt le round(maxdt/timestep), ndt )
	if ndt gt 0 then dt = dt(w) else message,'Invalid maximum dt!'
endif else begin
	dt = mydts
	ndt = n_elements(dt)
endelse

; set up some arrays-- the running sums of the data
data = dblarr((2*dim)+3,ndt)	; x,y,x^2,y^2,r^2,N(r,t) and t

; do the big nested loop
for i=0,nf-1 do begin
	if filebased then begin
		message,'Reading file: '+f(i),/inf
		trj = read_gdf(f(i))
	endif else trj = tracks
	; convert to um.
	for j=0,dim-1 do trj(j,*) = trj(j,*)*micperpix(j)
	ltrinterp_vb,trj				; fill in gaps.
	j = 0
	repeat begin
		if filebased then $
		   message,'Processing file: '+f(i)+$
			'	dt equals: '+strcompress(string(dt(j))),/inf $
		   else if not keyword_set(quiet) then message,'	dt equals: '$
			+strcompress(string(dt(j))),/inf
	    data(0:(2*dim)+1,j) = data(0:(2*dim)+1,j) + $
			get_drs(trj,dt(j),erode=erode,dim=dim)
		j = j+1
	endrep until (j eq ndt) or (not keyword_set(maxtime) and $
	   not keyword_set(mydts) and (2.*data(2*dim+1,j-1)/dt(j-1) lt minN))
endfor

; truncate the data if we want
if j ne ndt then begin
	w = where((2.*data(2*dim+1,*)/dt ge minN),nw)
	if nw eq 0 then message,$
		'Fewer than '+ strcompress(string(minN)) + ' points for all dt,set MAXTIME manually!'
	data = data(*,w)
	dt = dt(w)
endif

; make the running sums into means and variances.
for i = 0,(2*dim)-1 do data(i,*) = data(i,*)/data(2*dim+1,*)
if ~keyword_set(keepdrift) then data(dim:(2*dim)-1,*) = data(dim:(2*dim)-1,*) - (data(0:dim-1,*)^2)
data(2*dim,*) = total(data(dim:(2*dim)-1,*),1)

; put on dt vectors
data(2*dim+2,*) = dt*timestep

; make up the 'effective N's, expect a factor of two improvement due
; to overcounting.
data(2*dim+1,*) = (dt < 2.)*data(2*dim+1,*)/dt

; shift the time onto the front to make Eric happy.
if ndt eq 1 then data = shift(data,1) else data = shift(data,1,0)

; optionally write out a file
if keyword_set(outfile) then begin
	message,'Writing file: '+outfile,/inf
	write_gdf,data,outfile
endif

; added by ERW on 8-15-04 just 'cuz I like to plot stuff:
if ~keyword_set(noplot) then begin
plot,data(0,*),data(dim+1,*),/xl,/yl,psym=circ()
endif
; (this is <x^2>)

return,data

end
