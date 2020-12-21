;
;	A 'lil function to read in three-dimensional binary
;	files from SGI machines- specifically *.mv files
;
;	To view or access the header parameters, 
;	use the 'parse_noran' function.
;
function read_noran,filename,lomem=lomem,res=res

f = findfile(filename)
if f(0) eq '' then message,'No match!'

res = parse_noran(filename,nx=nx,ny=ny,nz=nz,xdim=xdim,ydim=ydim,zdim=zdim)

size = long(nx)*long(ny)*long(nz)

openr,1,filename
tmp = FSTAT(1)

if tmp.size-size le 0 then $
	message,'File appears to be damaged!'

head = bytarr(tmp.size-size)
a = bytarr(nx,ny,nz)

readu,1,head
readu,1,a
close,1

; we need to shift the array or else the first frame is
; actually the last frame-- don't know why!
if (nz gt 1) then  begin
	if (not keyword_set(lomem)) then  a = shift(a,0,0,1)
	print,xdim,"   x",ydim,"   x",zdim," um"
endif else begin
	print,xdim,"   x",ydim," um"
endelse

res=[xdim,ydim,zdim]

return,a

end


