; rm_motion			Eric Weeks, 6-5-99
;
; goal: sample may be drifting, remove some of this drift from the
;       tracked array
;
; 12-15-99: fix a typo that caused errors when tmin > 0;
;           fix so that smoo=0 works
;  6-23-04: added in x, y, z labels
;
; for more information see:
;        http://www.physics.emory.edu/~weeks/idl/motion.html

;function rm_motion,tra,mot,dim=dim,smooth=smooth,justplot=justplot,  $
;	pretrack=pretrack,offset=offset
; use smooth= to set smooth parameter
; /justplot to not change data, only make the nice plot
; /pretrack to operate on pretracked data
; use dim keyword to override default (2D data)

;============================================================
; eintegrate		4-27-99  Eric Weeks
; modified 6-12-00; added to rm_motion as subroutine on 7-3-00

function mot_eintegrate,data

result=reform(data)
result(0)=data(0)
result(0)=0.0
n=n_elements(data)
for i=1L,n-1L do begin
	result(i)=result(i-1)+data(i)
	;result(i)=total(data(0:i))
endfor
return,result
end


;============================================================

function rm_motion,tra,mot,dim=dim,smooth=smooth,justplot=justplot,  $
	pretrack=pretrack,nointegrate=nointegrate,offset=offset
; use smooth= to set smooth parameter
; /justplot to not change data, only make the nice plot
; /pretrack to operate on pretracked data
; /nointegrate to avoid integrating motion data
; /offset to avoid shifting the data away from negative numbers

if (not keyword_set(dim)) then begin
	nel=n_elements(mot(*,0))
	dim=nel-2
endif
if (not keyword_set(smooth)) then smoo=-1 else smoo=smooth
if (n_elements(mot(0,*)) lt smoo-10L) then smoo=-1
; message,'smooth='+string(smoo),/inf
npts=n_elements(mot(0,*))
ndat=n_elements(tra(*,0))
if (keyword_set(pretrack)) then ndat=ndat+1
tmin=mot(0,0)+1

; now I need to smooth the mot array

t=mot(0,*)
ymin=9e9 & ymax=-ymin
for i=0,dim-1 do begin
	if (keyword_set(nointegrate)) then begin
		xmin=min(mot(i+2,*),max=xmax)
	endif else begin
		xmin=min(mot_eintegrate(mot(i+2,*)),max=xmax)
	endelse
	ymin = ymin < xmin
	ymax = ymax > xmax
endfor
trb=0
if (not keyword_set(justplot)) then trb=tra

for i=0,dim-1 do begin
	if (not keyword_set(nointegrate)) then begin
		coord=mot_eintegrate(mot(i+2,*))
	endif else begin
		coord=mot(i+2,*)
	endelse
	if (smoo gt 1) then begin
		coord=smooth(coord,smoo)
		smoo2=smoo/2+3
		line1=linfit(t(0:smoo2),coord(0:smoo2))
		line2=linfit(t(npts-smoo2:*),coord(npts-smoo2:*))
		coord(0:smoo2)=coord(smoo2+1)-line1(1)*t(smoo2+1)+line1(1)*t(0:smoo2)
		coord(npts-smoo2:*)=coord(npts-smoo2-1) -  $
			line2(1)*t(npts-smoo2-1)+line2(1)*t(npts-smoo2:*)
	endif
	if (smoo lt 1) then begin
		line1=linfit(t,coord)
		coord=line1(1)*t+line1(0)
	endif
	ttt=n_elements(t)*[0.1,0.3,0.5,0.7,0.9]
	deltay = (ymax-ymin)*0.05
	if (i eq 0) then begin
		plot,t,coord,yrange=[ymin,ymax]
		xyouts,t(ttt),coord(ttt)+deltay,"X"
	endif
	oplot,t,coord
	if (i eq 1) then begin
		xyouts,t(ttt),coord(ttt)+deltay,"Y"
	endif
	if (i eq 2) then begin
		xyouts,t(ttt),coord(ttt)+deltay,"Z"
	endif
	if (not keyword_set(nointegrate)) then begin
		oplot,t,mot_eintegrate(mot(i+2,*)),psym=3
	endif else begin
		oplot,t,mot(i+2,*),psym=3
	endelse
	if (not keyword_set(justplot)) then begin
		times=reform(round(tra(ndat-2,*)))
		trb(i,*)=tra(i,*)-coord(times-round(tmin))
		if (not keyword_set(offset)) then begin
			; make sure everything is positive !
			xmin=min(trb(i,*))
			if (xmin lt 1.0) then trb(i,*)=trb(i,*)-xmin+1.0
		endif
	endif
endfor


return,trb
end
