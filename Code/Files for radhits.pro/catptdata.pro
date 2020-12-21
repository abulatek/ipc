; catptdata.pro		started 6/18/98 by ERW

function catptdata,fname
; fname should be filenames, wildcards OK

f = findfile(fname,count=nf)
; findfile goes to find the file, you can use wildcards if you
; want (then f would be an array of files found, nf would be the
; number of files found).

if (nf eq 0) then message,'no match'

flag=0b
count=0L
for i = 0,nf-1 do begin
	message, 'loading file:' + f(i),/inf
	featdat = read_gdf(f(i))
	if (n_elements(featdat) gt 4) then begin
		nparticles=n_elements(featdat(0,*))
		d=fltarr(1,nparticles) + i
		; 'd' contains the label for this cube
		featdat = [featdat,d]
		if (flag eq 0b) then begin
			flag = 1b
			nel=n_elements(featdat(*,0))
			num=nparticles*nf*1.2
			alldata=fltarr(nel,num)
		endif
		if ((count+nparticles) lt (num-10L)) then begin
			alldata(*,count:count+nparticles-1) = featdat
			count=count+nparticles
		endif else begin
			; add some new blank space at end of data
			temp=fltarr(nel,nparticles*10L)
			num=num+nparticles*10L
			alldata=[[alldata],[temp]]
			alldata(*,count:count+nparticles-1) = featdat
			count=count+nparticles
		endelse
		;	message, 'gzipping pt file:' + f(i),/inf
		;	spawn,'gzip -v ' + f(i)
	endif
endfor

return,alldata(*,0:count-1L)
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
