; uberize.pro			Eric Weeks 9-17-98
; see http://www.physics.emory.edu/~weeks/idl

function uberize,tracks,presort=presort,start=start
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



