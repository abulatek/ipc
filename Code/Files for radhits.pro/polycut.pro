;function polycut,inputdata,polygon,f1=f1,f2=f2,invert=invert, $
;     whereflag=whereflag
;
;
; see http://www.physics.emory.edu/~weeks/idl/
;    for more information.
;
; The polycut function is intended to be used to postprocess the concatenated
; data, after perhaps I have used "eclip" to cut out some of it.
;
; This procedure just does a polygonal cut on the pretrack data
;
; /invert:  return all data NOT within convex polygon
; f1, f2:   which columns of data to use as fields for the
;           plotting and cutting
; /whereflag:  return indices of data, like "WHERE", rather
;           than the clipped data itself

; ============================================================
; The routines below are written by John Crocker
; ============================================================

;
;	Allows the user to define a polygonal region by clicking the mouse
;	on a plot. Click the left button to add a vertex, the right one when done.
;	The last vertex is connected to the first, and the polygon is filled.
;
function get_polygon,device=device,data=data,normal=normal,linestyle=linestyle
message,'Left Button to add vertex, Right to quit',/inf

if not keyword_set(linestyle) then linestyle=0

polygon = [-1.,-1.]
i=0

; main event loop
repeat begin

	cursor,x,y,/down,device=device,data=data,normal=normal
	qflag = long( 6 ) AND !ERR
	if qflag eq 0 then begin
	
		i=i+1
		polygon = [[polygon],[x,y]]
	
		if i ge 2 then begin
			plots,[polygon(0,i),polygon(0,i-1)],[polygon(1,i),polygon(1,i-1)],$
				device=device,data=data,normal=normal,linestyle=linestyle
		endif else plots,polygon(0,i),polygon(1,i),psym=3,device=device,data=data,normal=normal

	endif else plots,[polygon(0,i),polygon(0,1)],[polygon(1,i),polygon(1,1)],$
		device=device,data=data,normal=normal,linestyle=linestyle
endrep until (qflag ne 0)

n = n_elements(polygon(0,*))
if n ge 4 then begin
	polygon = polygon(*,1:n-1) 
endif else begin
	polygon = polygon(*,0)
	message,'Warning: Polygon must have more than two vertices!',/inf
endelse	

return,polygon	
end


;
;	A function for determining if a given single point (x,y) is in a 
;	non-convex, possibly self-intersecting polygon; slow. 
;
function pip, pnt, polygon 

	pgn = polygon
	spgn = shift(polygon,0,1)

;	Check to see if the point is a vertex
	w = where( (pgn(0,*) eq pnt(0)) AND (pgn(1,*) eq pnt(1)) )
	if w(0) ne -1 then return,-1

;	Check to see if the particle is on an edge
;	Do the case of a horizontal edge
	on    = ( pgn(1,*) eq pnt(1) )
	possible = where( (on and shift(on,0,1)) ne 0 )
	if possible(0) ne -1 then begin
		p  = pgn( *, possible )
		sp = spgn( *, possible )
		for i=0,n_elements(p(0,*))-1 do begin
			mn = min( [p(0,i),sp(0,i)], max=mx) 
			if (pnt(0) gt mn) AND (pnt(0) lt mx) then return,-1
		endfor	
	endif
;	Now, this can only happen if one vertex end is above, and the other
;	one is below the point.
	below = ( pgn(1,*) lt pnt(1) )
	above = ( pgn(1,*) gt pnt(1) )
	possible = where( ( (below and shift(above,0,1)) OR (above and shift(below,0,1)) ) ne 0 )
	if possible(0) ne -1 then begin
		p = pgn( *, possible )
		sp = spgn( *, possible )
;		do the case of the vertical edge
		w = where( p(0,*) eq sp(0,*) )
		if w(0) ne -1 then begin
			for i=0,n_elements(w)-1 do if p(0,w(i)) eq pnt(0) then return,-1
		endif
;		do the rest of the edges	
		w = where( p(0,*) ne sp(0,*) )
		if w(0) ne -1 then begin
			for i=0,n_elements(w)-1 do begin
				if (pnt(1)-p(1,w(i))) eq ( ((p(1,w(i))-sp(1,w(i)))/( p(0,w(i))-sp(0,w(i)) )) *$
					(pnt(0) -  p(0,w(i))) ) then return,-1
			endfor
		endif
	endif

;	Get rid of 'extra' horizontal vertices to simplify some nasty special cases.
	dyzero = (pgn(1,*) - spgn(1,*)) eq 0
	sdyzero = shift(dyzero,0,-1)
	w = where(( dyzero AND sdyzero ) eq 0 )
	pgn = pgn(*,w)
	spgn = shift(pgn,0,1)
	rpgn = shift(pgn,0,-1)

;	Count stores the number of times the left ray intersects the polygon
	count = 0

;	Test the case of a vertex on the ray, to see if its a crosser
	on    = ( pgn(1,*) eq pnt(1) )
	possible = where( on ne 0 )
	if possible(0) ne -1 then begin
		p  = pgn( *, possible )
		sp = spgn( *, possible )
		rp = rpgn( *, possible )
		for i=0,n_elements(p(0,*))-1 do begin
;		check to see if the vertex is on the left	
			if p(0,i) lt pnt(0) then begin
				above1 = rp(1,i) gt pnt(1)
				above2 = sp(1,i) gt pnt(1)
				below1 = rp(1,i) lt pnt(1)
				below2 = sp(1,i) lt pnt(1)
				if (above1 and below2) or (above2 and below1) then count = count+1
			endif
		endfor		
	endif
;	Test all horizontal edges tangent to the ray, to see if they're crossers
	possible = where( (on and shift(on,0,1)) ne 0 )
	if possible(0) ne -1 then begin
		p  = pgn( *, possible )
		sp = spgn( *, possible )
		for i=0,n_elements(p(0,*))-1 do begin
;			check to see if the edge is on the left	
			if p(0,i) lt pnt(0) then begin
				sspgn = shift(spgn,0,1)
				above1 = sspgn( 1, possible(i) ) gt pnt(1)
				rpgn = shift(pgn,0,-1)
				above2 = rpgn( 1, possible(i) ) gt pnt(1)
				if above1 ne above2 then count = count+1
			endif	
		endfor	
	endif

;	All remaining crossers consist of non-horizontal edges with one end above
;		and the other below the horizontal ray!
	below = ( pgn(1,*) lt pnt(1) )
	above = ( pgn(1,*) gt pnt(1) )
	possible = where( ( (below and shift(above,0,1)) OR (above and shift(below,0,1)) ) ne 0 )
	if possible(0) ne -1 then begin
		p = pgn( *, possible )
		sp = spgn( *, possible )
		for i=0,n_elements(possible)-1 do begin
			if  (( (pnt(1)-p(1,i)) * ((p(0,i)-sp(0,i))/( p(1,i)-sp(1,i) )) ) +$
				p(0,i)) lt pnt(0) then count = count+1
		endfor
	endif

	if fix(count/2.) eq count/2. then return,0 else return,1
end
;
;	are the points p in the convex polygon ply; fast.
;
function pip2, p, ply

res = intarr( 1,n_elements( p(0,*) ) ) +1
nvert = n_elements( ply(0,*) )
t = (ply(0,0) - ply(0,nvert-1)) * (p(1,*) - ply(1,nvert-1)) - $
	(ply(1,0) - ply(1,nvert-1)) * (p(0,*) - ply(0,nvert-1))

;	handle special case for t=0		
w = where( t eq 0, ndeg )
if ndeg gt 0 then res(w) = ( (p(0,w) - ply(0,0)) * (p(0,w) - ply(0,nvert-1)) + $
	(p(1,w) - ply(1,0)) * (p(1,w) - ply(1,nvert-1)) ) le 0	
	
for i = 0, nvert-2 do begin
	ti = (ply(0,i+1) - ply(0,i)) * (p(1,*) - ply(1,i)) - $
		(ply(1,i+1) - ply(1,i)) * (p(0,*) - ply(0,i)) 	
	res = res and (t * ti ge 0)
endfor	
return, reform( res )
end
;
;	Will return 1 if the given points (x,y) lie inside a polygon,
;	0 otherwise. Points must be a (2,n) array!
;
function p_in_poly, pnts, polygon, convex = convex
npnts = n_elements( pnts(0,*) )
res = intarr(npnts)

minx = min(polygon(0,*),max = maxx)
miny = min(polygon(1,*),max = maxy)
w = where( (pnts(0,*) ge minx) and (pnts(0,*) le maxx) $
	and (pnts(1,*) ge miny) and (pnts(1,*) le maxy), n )
if n eq 0 then begin
	message,'No points in specified polygon',/inf
	return,res
endif	
points = pnts(*,w)

if keyword_set( convex ) then begin
	w2 = where( pip2( points, polygon ),n2 )
	if n2 ne 0 then res( w(w2) ) = 1
endif else for i=0D,n-1D do res( w(i) ) = pip( points(*,i), polygon )

return,( res > 0 )
end




; ============================================================
; The routines below was modified by Eric Weeks:
;  function polycut, started 10-20-98 by ERW from JCC's "cutcat"
; ============================================================
function polycut,inputdata,polygon,f1=f1,f2=f2,invert=invert, $
    whereflag=whereflag

if (not keyword_set(f1)) then f1=0
if (not keyword_set(f2)) then f2=1

if n_elements(polygon) lt 3 then begin
	plot,inputdata(f1,*),inputdata(f2,*),psym=3,/yno
	message,'Draw a convex polygon on the data...',/inf
	polygon = get_polygon()
endif

w = where(p_in_poly(inputdata([f1,f2],*),polygon,/convex),ngood)

if (keyword_set(invert)) then begin
	flag = bytarr(n_elements(inputdata[0,*]))
	flag(w) = 1b
	w=where(flag eq 0b,ngood)
endif

if (ngood gt 0) then begin
	result = inputdata(*,w)
endif else begin
	result = inputdata
endelse

; make a plot too
plot,result(f1,*),result(f2,*),psym=3,/yno
print,polygon
if (keyword_set(whereflag)) then result = w

return,result
end
