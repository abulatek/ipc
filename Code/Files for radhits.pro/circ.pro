;
;	sets the usersymbol (8) to be an open circle
;	this function returns the number 8 so you can
;	just say psym = circle( ... ) in the plot command	
;
function circ,radius = radius, thick = thick, fill = fill,$
	 dx = dx, dy = dy, left = left, right = right, num=num


if not keyword_set( num ) then num = 36
if not keyword_set( radius ) then radius = 1.
if not keyword_set( thick ) then thick = 1.
if not keyword_set( dx ) then dx = 0.
if not keyword_set( dy ) then dy = 0.

t=findgen(num+1) * (!pi*2./(1.0*num))

if keyword_set( right ) then t = t(0:num/2)
if keyword_set( left ) then t = [t(num/2:*),t(0)]

x = sin(t)*radius + dx
y = cos(t)*radius + dy

if keyword_set( fill ) then $
	usersym,x,y,/fill $
else	usersym,x,y,thick=thick

return,8	
end
