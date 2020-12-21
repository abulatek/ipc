;pro plottr,tarray,result,goodenough=goodenough,tv=tv,noconnect=noconnect, $
;  x=x,y=y,dot=dot,timecut=timecut,_extra=eee
;
; see http://www.physics.emory.edu/~weeks/idl/
;    for more information and software updates
;
; plottr.pro  (formerly ericplot2.pro)
;
; started 6-15-98   Eric R. Weeks
; /tv added 8-13-98
; /noconnect added 8-13-98
; uberized 9-21-98
; fixed problem when ID's aren't in order, thank you to Ian Williams
;   for encouraging me to do this fix (6-6-11)
;

; pt_circ
; from John Crocker's circ()
;
;       sets the usersymbol (8) to be an open circle
;       this function returns the number 8 so you can
;       just say psym = circle( ... ) in the plot command       
;
function pt_circ,radius = radius, thick = thick, fill = fill,$
         dx = dx, dy = dy, left = left, right = right

if not keyword_set( radius ) then radius = 1.
if not keyword_set( thick ) then thick = 1.
if not keyword_set( dx ) then dx = 0.
if not keyword_set( dy ) then dy = 0.

t=findgen(37) * (!pi*2/36)

if keyword_set( right ) then t = t(0:18)
if keyword_set( left ) then t = [t(18:*),t(0)]

x = sin(t)*radius + dx
y = cos(t)*radius + dy

if keyword_set( fill ) then $
        usersym,x,y,/fill $
else    usersym,x,y,thick=thick

return,8        
end




; lentrk			Eric Weeks 9-22-98
;
; modified from JCC's len_trk.pro

function pt_lentrk,t
; works for either uber or normal tracked data
;
; returns (2,*) array:  (0,*) is the length, (1,*) is particle #

; UBER TRACKER
; get the indices of the unique id's 
ndat=n_elements(t(*,0))
u = uniq(t(ndat-1,*))
ntracks = n_elements(u)
u = [-1,u]

res = fltarr(2,ntracks)
for i = 1L,ntracks do begin
	res(0,i-1) = t(ndat-2,u(i))-t(ndat-2,u(i-1)+1)
	res(1,i-1) = t(ndat-1,u(i))
endfor

return,res
end

; first a utility function:
; eline.pro    started 8-13-98 by Eric Weeks
;
; linearray=eline(pt1,pt2)
; pt1, pt2 are n-dimensional pofloors (integers)
; result is an array of pofloors connecting them
;
; vectorized 11-2-98
; turned into pt_eline 2-25-99

function pt_eline,pt1,pt2

ndim=n_elements(pt1)
ndim2=n_elements(pt1)
if (ndim ne ndim2) then message,"error! pts must be same dimension"
result=floor(pt1)

delta=pt2-pt1
max=max(abs(delta))
epsilon=float(delta)/float(max)
if (max le 0) then begin
	message,"warning! begin and end pts are identical",/inf
endif else begin
	pt0=float(pt1)
	temp=epsilon # findgen(max+1)
	n=n_elements(pt1)
	result=intarr(n,max+1)
	for i=0,n-1 do begin
		result(i,*)=floor(pt0(i)+temp(i,*))
	endfor
endelse

return,result
end


; uberize.pro			Eric Weeks 9-17-98

function sububerize,tracks,presort=presort,start=start
;
; reassigns the unique ID# to 0,1,2,3...
; /presort will sort on ID# first, then reassign
; start will begin with that ID#
;
; function returns a new uber-track array


ndat=n_elements(tracks(*,0))-1
if (keyword_set(presort)) then begin
	newtracks=tracks(*,sort(tracks(ndat,*)))
endif else begin
	newtracks=tracks
endelse

u=uniq(newtracks(ndat,*))
ntracks=n_elements(u)
u=[-1,u]
for i=1L,ntracks do  newtracks(ndat,u(i-1)+1:u(i)) = i-1

if (keyword_set(start)) then newtracks(ndat,*)=newtracks(ndat,*)+start

return,newtracks
end






pro plottr,tarray2,result,goodenough=goodenough,tv=tv,noconnect=noconnect, $
  x=x,y=y,dot=dot,timecut=timecut,_extra=eee,thick=thick
; tarray is an array of tracked data (2D for the moment)
; goodenough works like plot_tracks -- only tracks at least 'goodenough'
;    in duration will be plotted
; /tv uses tv mode to plot, rather than axes; use x and y to adjust size
; result gets picture from /tv
;
; WARNING: tracked data must be "uberize'd" before using! (if ubertracker)
;
; /dot puts a dot at the end of the trajectory
; timecut limits all trajectories to be that long (an integer)

tarray = sububerize(tarray2)

if (not keyword_set(x)) then x=512
if (not keyword_set(y)) then y=480
if (not keyword_set(noconnect)) then connect=1

	; UBER-TRACKER
	ndat=n_elements(tarray(*,0))
	ntime=max(tarray(ndat-2,*))
	if not keyword_set(goodenough) then goodenough=ntime
	length=pt_lentrk(tarray)
	w=where(length(0,*) ge goodenough,nw)
	if (keyword_set(tv)) then begin
		picture=bytarr(x,y)
		u=uniq(tarray(ndat-1,*))
		ntracks=n_elements(u)
		u=[-1,u]
		if (keyword_set(connect)) then begin
			for i=0L,nw-1L do begin
				j=length(1,w(i))
				traj=tarray(*,u(j)+1:u(j+1))
				k=n_elements(traj(0,*))
				pt0=traj(0:1,0)
				for kk=1L,k-1L do begin
					pt1=traj(0:1,kk)
					lineline=pt_eline(pt0,pt1)
					picture(lineline(0,*),lineline(1,*)) = 255
					pt0=pt1
				endfor
			endfor
		endif else begin
			for i=0L,nw-1L do begin
				j=length(1,w(i))
				traj=tarray(*,u(j)+1:u(j+1))
				picture(traj(0,*),traj(1,*)) = 255b
			endfor
		endelse
		tv,picture
		result=picture
	endif else begin
		plot,tarray(0,*),tarray(1,*),/nodata,/ynozero,_extra=eee,   $
			/ysty,/xsty,/isotropic
		u=uniq(tarray(ndat-1,*))
		ntracks=n_elements(u)
		u=[-1,u]
		if (keyword_set(connect)) then begin
			for i=0L,nw-1L do begin
				j=length(1,w(i))
				traj=tarray(*,u(j)+1:u(j+1))
				if (keyword_set(timecut)) then begin
					t0=traj(ndat-2,0)
					www=where(traj(ndat-2,*) lt t0+timecut)
					traj=traj(*,www)
				endif
				oplot,traj(0,*),traj(1,*),thick=thick
				if (keyword_set(dot)) then begin
					nt=n_elements(traj(0,*))-1
		oplot,[traj(0,nt)],[traj(1,nt)],psym=pt_circ(/fill,radius=1.0),color=128
				endif
			endfor
		endif else begin
			for i=0L,nw-1L do begin
				j=length(1,w(i))
				traj=tarray(*,u(j)+1:u(j+1))
				if (keyword_set(timecut)) then begin
					t0=traj(ndat-2,0)
					www=where(traj(ndat-2,*) lt t0+timecut)
					traj=traj(*,www)
				endif
				oplot,traj(0,*),traj(1,*),psym=3
				if (keyword_set(dot)) then begin
					nt=n_elements(traj(0,*))-1
		oplot,[traj(0,nt)],[traj(1,nt)],psym=pt_circ(/fill,radius=1.0),color=128
				endif
			endfor
		endelse
	endelse

end
; this ends the procedure

