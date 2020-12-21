; avgbin.pro -- Eric R. Weeks, 3-12-99 (weeks@physics.emory.edu)
;
; assumes data is in two columns, for example (x-position, velocity)
; then returns 
;     [x-position, average velocity, std dev of velocity, min, max, number]
;
; based on a C program I wrote a while ago that I've always found useful.
;
; see: http://www.physics.emory.edu/~weeks/idl/avgbin.html
;
; this file may be freely distributed, but this header
; must be left intact.

; revised 12-8-05 by Eric R. Weeks
; now states how many bins are empty. Gianguido C. Cianci 3 VII 2006
; now shuts up if you ask it to be /quiet. GCC 28 VIII 2006


; revised 4-19-07 by Scott Franklin
; uses histogram so much faster.

; now checks that data and datab are the same length!! GCC 18 XI 2007 


function avgbin,data,datab,binsize=binsize,quiet=quiet,more=more

if (not keyword_set(datab)) then begin
   data2=data[1,*]
   data1=data[0,*]
endif else begin
   IF n_elements(data) NE n_elements(datab) THEN $
   message, "Your input arrays should be of the same length!"
   data1=data
   data2=datab
endelse


xmin=min(data1,max=xmax)
if (not keyword_set(more)) then more=0
if (not keyword_set(binsize)) then begin
    dist=histogram(data1,binsize=(xmax-xmin)/99.999,rev=ri)
endif else begin
    dist=histogram(data1,binsize=binsize,rev=ri,min=xmin)
endelse


length=(size(dist))(1)
result=make_array(6+3*more,length)
result[0,*]=xmin+indgen(length)*(xmax-xmin)/length
result[5,*]=dist
w1=where(dist eq 1,num1)
w2=where(dist gt 1,num2)
if (num1 gt 0) then begin
    for i=0U,num1-1 do begin
        result[1,w1[i]]=data2[ri[ri[w1[i]]:ri[w1[i]+1]-1]]
        result[2,w1[i]]=0
        result[3,w1[i]]=data2[ri[ri[w1[i]]:ri[w1[i]+1]-1]]
        result[4,w1[i]]=data2[ri[ri[w1[i]]:ri[w1[i]+1]-1]]
        if keyword_set(more) then begin
            result[6,w1[i]]=data2[ri[ri[w1[i]]:ri[w1[i]+1]-1]]
            result[7,w1[i]]=0
            result[8,w1[i]]=0
        endif
    endfor
endif

if (num2 gt 0) then begin
    for i=0,num2-1 do begin
        result[3,w2[i]]=min(data2[ri[ri[w2[i]]:ri[w2[i]+1]-1]],max=lmax)
        result[4,w2[i]]=lmax
        stat=moment(data2[ri[ri[w2[i]]:ri[w2[i]+1]-1]])
        result[1,w2[i]]=stat[0]
        result[2,w2[i]]=sqrt(stat[1])
        if keyword_set(more) then begin
            result[6,w2[i]]=median(data2[ri[ri[w2[i]]:ri[w2[i]+1]-1]])
            result[7,w2[i]]=stat[2]
            result[8,w2[i]]=stat[3]
        endif

    endfor    
endif

if (not keyword_set(quiet)) then message,'WARNING:'+strcompress(length-num1-num2)+ $
  ' bins contain no data',/inf
return,result    
end
