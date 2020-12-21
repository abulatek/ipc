; catptdata.pro		started 6/18/98 by ERW
; catptdata2.pro	9-30-98
;
; sped up 3-15-99 for processing 1000's of files

function catptdata2,fname

f = findfile(fname,count=nf)
; findfile goes to find the file, you can use wildcards if you
; want (then f would be an array of files found, nf would be the
; number of files found).

if (nf eq 0) then message,'no match'

maxtime=0
j=0
for i = 0,nf-1 do begin
	message, 'loading file:' + f(i),/inf
	featdat = read_gdf(f(i))
	ntime=n_elements(featdat(*,0))-1
	nparticles=n_elements(featdat(0,*))
	featdat(ntime,*) = featdat(ntime,*) + maxtime
	maxtime = max(featdat(ntime,*)) + 1
	if (i eq 0) then begin
		alldata=featdat
	endif else begin
		if (j eq 0) then begin
			somedata=featdat
		endif else begin
			somedata=[[somedata],[featdat]]
		endelse
		j = j + 1
		if (j ge 100) then begin
			alldata = [[alldata],[somedata]]
			j = 0
		endif
	endelse
	;	message, 'gzipping pt file:' + f(i),/inf
	;	spawn,'gzip -v ' + f(i)
endfor
if (nf gt 1) and (j gt 0) then begin
	alldata = [[alldata],[somedata]]
endif

return,alldata
end

