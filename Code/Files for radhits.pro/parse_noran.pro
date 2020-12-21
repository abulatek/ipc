;
;	Bullshit function to parse the headers on Noran
;	files from SGI machines- specifically *.mv files
;	
;	Type "print,parse_noran('filename')" to view 
;	acquisition parameters.  To return numerical values, 
;	use keywords.
;
;---------------------------------------------------------
;
;  Searches for keywords in Noran headers and 
;  parses out strings....
;
function get_item,headstr,trgtstr,string=string
maxlen = 200

pos = strpos( headstr,trgtstr )
if pos eq -1 then return,-1
h = strmid( headstr, pos, maxlen )
;print,'hello',pos

apos = 0
flag = 0
if keyword_set(string) then begin
	repeat begin
		apos = apos+1
		res = strmid(h,apos,3)
		if res eq 'NI_' or apos eq maxlen-3 then $
			flag = 1
	endrep until flag
	if apos ge maxlen-3 then return,-1
	g = strmid(h,0,apos-1)
	return,g
endif else begin
	start = -1
	stop = -1
	repeat begin
		apos = apos+1
		res = strmid(h,apos,1)
		if res eq '0' or res eq '1' or res eq '2' or $
		res eq '3' or res eq '4' or res eq '5' or $
		res eq '6' or res eq '7' or res eq '8' or $
		res eq '9' or res eq '.' then begin
			if start eq -1 then start = apos
			stop = apos
		endif else if start ne -1 then flag = 1
		if apos eq maxlen then flag = 1
	endrep until flag
	if apos ge maxlen then return,-1
	g = strmid(h,start,stop-start+1)
	reads,g,result
	return,result
endelse

end
;
function strgof,num
return,strcompress(string(num))
end
;
;    Here she goes....
;
function parse_noran,filename,nx=nx,ny=ny,nz=nz,$
	xdimension=xdimension,ydimension=ydimension,$
	zdimension=zdimension

f = findfile(filename)
if f(0) eq '' then message,'No match!'

; first get the (beginning of the) header
head = bytarr(3000)
openr,1,filename
readu,1,head
close,1
w = where(head eq 0)
head(w) = 1B
sh = string(head)

; parse out the variables

protocol     = get_item(sh,'Protocol',/string)

objlensmag   = get_item(sh,'ObjLensMag')
objlensna    = get_item(sh,'ObjLensNA')

xlenscalibrt = get_item(sh,'XLensCalibrt')
ylenscalibrt = get_item(sh,'YLensCalibrt')

xdimension   = get_item(sh,'Xdimension')
ydimension   = get_item(sh,'Ydimension')
zdimension   = get_item(sh,'Zdimension')

irisvalue    = get_item(sh,'IrisValue')
slitwidth    = get_item(sh,'SlitWidth')

nx           = get_item(sh,'WIDTH')
ny           = get_item(sh,'HEIGHT')
nz           = get_item(sh,'DIR_COUNT')

zoomvalue    = get_item(sh,'ZoomValue')
adcdwell     = get_item(sh,'ADCDwell')

CR = string(10B)
TB = string(9B)

report = 'Protocol:'+TB+' '+protocol+CR+$
	'Objective:'+TB+strgof(fix(objlensmag))+$
		'X,'+strgof(objlensna)+' NA'+CR+$
	'Lens Calib.:'+TB+strgof(xlenscalibrt)+$
		' x'+strgof(ylenscalibrt)+' um'+CR+$
	'Zoom Scale:'+TB+strgof(zoomvalue)+CR+$
	'XY Pixel Size:'+TB+strgof(xdimension)+$
		' x'+strgof(ydimension)+' um'+CR+$
	'Z Step Size:'+TB+strgof(zdimension)+' um'+CR+$
	'Image Width:'+TB+strgof(fix(nx))+' pixels'+CR+$
	'Image Height:'+TB+strgof(fix(ny))+' pixels'+CR+$
	'Image Depth:'+TB+strgof(fix(nz))+' slices'+CR+$
	'Slit Width:'+TB+strgof(slitwidth)+' um'+CR+$
	'Iris Size:'+TB+strgof(irisvalue)+' mm'+CR+$
	'Pixel Dwell:'+TB+strgof(adcdwell)+' ns/pixel'
		

return,report

end


