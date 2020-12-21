;   Written by John C. Crocker
;
;
;	Saves the data in an array as a tab delimited text file
;	which can be easily read in by a spread sheet program.
; 	default delimiter is the tab
;
pro write_text,data,filename,comma=comma

sz = size(data)
if sz(0) ne 2 then message,'Array argument must be 2 dimensional'

if keyword_set( comma ) then num_delter = ',' else num_delter = string( 9B ) ; tab is ASCII 9

close,1
openw,1,filename

for i=0L,sz(2)-1L do begin
	astring = ''
	for j=0L,sz(1)-2L do begin
		astring = astring + strcompress( string( data(j,i) ), /remove_all ) + num_delter  
	endfor
	astring = astring + strcompress( string( data(sz(1)-1,i) ), /remove_all )
	printf,1,astring
endfor

close,1

end
