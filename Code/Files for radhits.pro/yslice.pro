; yslice.pro,  started 6-30-98 by Eric R. Weeks
;
; written to help me look at slices thru cubes
; 'result' is optional, to grab the slice
;
; 7-25-06:  ERW: changed default scales to 1, added zscale keyword


pro yslice,cube,y,result,scale=scale,xscale=xscale,zscale=zscale

	if (keyword_set(scale) and keyword_set(zscale)) then begin
		message,'both scale and zscale are set',/inf
		message,'scale should not be used:  taking value from zscale',/inf
	endif

	if (not keyword_set(xscale)) then xscale=1
	if (not keyword_set(y)) then y=0

	if (not keyword_set(zscale)) then begin
		if (not keyword_set(scale)) then scale=1
	endif else begin
		scale = zscale
	endelse

	s=size(cube)
	result=reform(rebin(cube(*,y,*),s(1)*xscale,1,s(3)*scale))
	tv,result
end

