; big.pro,  started 7-9-98 by ERW
;
;


function big,image,fix=fix,scale=scale
; 'image'  gets doubled in size

if (not keyword_set(scale)) then scale=2

s = size(image)
if (s(0) eq 2) then begin
	; 2-D array
	result=rebin(image,s(1)*scale,s(2)*scale)
endif else begin
	; 3-D array
	result=rebin(image,s(1)*scale,s(2)*scale,s(3))
endelse
if (keyword_set(fix)) then begin
	a=bytarr(512,480)
	a(0:s(1)*2-1,0:s(2)*2-1) = result
	result=a
end

return, result
end


