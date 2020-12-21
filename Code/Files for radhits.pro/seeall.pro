; seeall.pro			10-29-98	Eric Weeks
;
; see http://www.physics.emory.edu/~weeks/idl
;    for more information and software updates
;
; revised 10-28-03 to include "sa_lentrk"

; ------------------------------------------------------------
function sa_lentrk,t
; returns (2,*) array:  (0,*) is the length, (1,*) is particle #

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
; ------------------------------------------------------------


pro seeall,tr,dot=dot,izz=izz,dr=dr,short=short,noz=noz,id=id, $
	zdata=zdata
; currently works only for ubertracker data
; izz sets the 4th graph to be tr(izz,*)
; /dr to plot sqrt(dx*dx+dy*dy) instead of z-coord
; /short to show shortest first
; /noz to suppress 3rd graph
; id=# to only show a particular id#
; zdata gives zdata, overrides 'z'

if (not keyword_set(izz)) then izz=2
if (not keyword_set(zdata)) then zdata=-1

len=sa_lentrk(tr)
on_error,1;			return to $MAIN

s=sort(len(0,*))
if (keyword_set(short)) then s=reverse(s)
x1=0.08
x2=0.48
y1=0.05
y2=0.45
dx=0.5
dy=0.5
if (not keyword_set(dr)) then dr=0

if (keyword_set(id)) then begin
   if (n_elements(id) eq 1) then begin
      w=where(len(1,*) eq id)
      s=w
   endif else begin
      nid=n_elements(id)
      len=len(*,1:nid)
      len(1,*)=id
      s=sort(len(0,*))
      if (keyword_set(short)) then s=reverse(s)
   endelse
endif

eness=n_elements(s)

IF (NOT keyword_set(dot)) THEN BEGIN
   FOR i=eness-1,0,-1 DO BEGIN

; determine the id number      
      id = fix(len(1, s[i]))

; plot the information
      plotx, /xx, /yy, tr, len(1,s(i)), position=[x1,y1+dy,x2,y2+dy], $
             title="trajectory of" + strcompress(id)
      
      plotx,/xx,tr,len(1,s(i)),position=[x1,y1,x2,y2],title="x-coord", $
            /noerase,/xstyle
      plotx,/yy,tr,len(1,s(i)),position=[x1+dx,y1+dy,x2+dx,y2+dy], $
            title="y-coord", /noerase,/xsty
      
; if the fourth plot exists, it's the z-coord or displacement
; so choose the plot's title accordingly
      
      IF (NOT keyword_set(noz)) THEN BEGIN
         IF(keyword_set(dr)) THEN BEGIN
            plotx,/zz,tr,len(1,s(i)),position=[x1+dx,y1,x2+dx,y2], $
                  title="displacement", /noerase,/xsty,izz=izz,dr=dr,dz,dataz=zdata
         ENDIF ELSE BEGIN
            plotx,/zz,tr,len(1,s(i)),position=[x1+dx,y1,x2+dx,y2], $
                  title="z-coord", /noerase,/xsty,izz=izz,dr=dr,dz,dataz=zdata
         ENDELSE
      ENDIF
      
      var=''
      prom='enter=continue, ctrl-d=end' + string(round(len(1,s(i))))
      prom=prom+" (duration"+string(round(len(0,s(i))))+") :"
      if (i gt 0) then begin
         read,prompt=prom,var
      endif else begin
         print,prom
      endelse
   endfor
endif else begin
   for i=eness-1,0,-1 do begin
      plotx,/xx,/yy,tr,len(1,s(i)),position=[x1,y1+dy,x2,y2+dy], $
            title="trajectory",psym=3
      plotx,/xx,tr,len(1,s(i)),position=[x1,y1,x2,y2],title="x-coord", $
            /noerase,/xsty,psym=3
      plotx,/yy,tr,len(1,s(i)),position=[x1+dx,y1+dy,x2+dx,y2+dy], $
            title="y-coord", /noerase,/xsty,psym=3
      if (not keyword_set(noz)) then begin
         plotx,/zz,tr,len(1,s(i)),position=[x1+dx,y1,x2+dx,y2], $
               title="z-coord", /noerase,/xsty,psym=3,izz=izz,dr=dr, $
               dataz=zdata
      endif
      var=''
      prom='enter=continue, ctrl-d=end' + string(round(len(1,s(i))))
      prom=prom+" (duration"+string(round(len(0,s(i))))+") :"
      if (i gt 0) then begin
         read,prompt=prom,var
      endif else begin
         print,prom
      endelse
   endfor
endelse

return

end
