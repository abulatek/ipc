function cattif,fname

f = findfile(fname,count=nf)
for i = 0,nf-1 do begin
	if (i mod 100 eq 0) then message,string(i)+"/"+string(nf)+":"+f(i),/inf
	a=read_tiff(f(i))
	if (i eq 0) then begin
		sz=size(a)
		if (sz(0) eq 2) then begin
			nx=n_elements(a(*,0))
			ny=n_elements(a(0,*))
			result=bytarr(nx,ny,nf)
		endif else begin
			if (sz(0) ne 3) then message,'bad file'
			nx=n_elements(a(*,0,0))
			ny=n_elements(a(0,*,0))
			nz=n_elements(a(0,0,*))
			result=bytarr(nx,ny,nz,nf)
		endelse
	endif
	if (sz(0) eq 2) then begin
		result(*,*,i)=a
	endif else begin
		result(*,*,*,i)=a
	endelse
endfor

return,result
end
