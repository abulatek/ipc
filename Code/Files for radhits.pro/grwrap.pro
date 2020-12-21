; grwrap4  -- modified June 7, 2004 -- Eric Weeks
;

pro grwrap,fname,prefix=prefix,dim=dim,_extra=eee

f = findfile(fname,count=nf)
; findfile goes to find the file, you can use wildcards if you
; want (then f would be an array of files found, nf would be the
; number of files found).

if (nf eq 0) then message,'no match'
if (not keyword_set(prefix)) then prefix='gr.'

if (not keyword_set(dim)) then begin
	dim=3
endif else begin
	if ((dim ne 2) and (dim ne 3)) then begin
		message,"only works with dim=2 or dim=3"
	endif
endelse


for i = 0,nf-1 do begin
	a = read_gdf(f(i))
	message, 'starting examining file:' + f(i),/inf
	if (dim eq 3) then
		alldata=ericgr3d(a,_extra=eee)
	endif else begin
		alldata=ericgr2d(a,_extra=eee)
	endelse
	write_gdf,alldata,prefix + f(i)
endfor

end

