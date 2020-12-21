;function dpiv,img1,img2,maxdisp,spacing=spacing,stretch=stretch,$
;	nopoly=nopoly,fits=fits,denoise=denoise,morefit=morefit
;
;PURPOSE:
;	Compares two images to extract the local motion 
;	from one image to the other.
;       Please see:
;           http://www.physics.emory.edu/~weeks/idl/piv.html
;
;EXAMPLE:
;	result=piccomp6d(data(*,*,0),data(*,*,1),10,sp=4)
;            result = [dx, dy, dr, error]
;MODIFICATION HISTORY:
;	06/04:created by Doug Anderson(andersondoug@gmail.com)
;       07/04:modified a tiny bit by Eric Weeks
;METHOD:
;	A section of img1 is compared to sections of 
;	img2 in the same neighborhood to find the 
;	section of img2 that is most like the section 
;	of img1.  The vector connecting the centroid 
;	of these two sections is the displacement vector 
;	of interest.  Many of these vectors are determined 
;	for a single image pair.  
;ARGUMENTS:
;	img1: n X m array
;	img2: n X m array
;	maxdisp: limits maximum displacement in one 
;		dimension.  So the real max equals 
;		sqrt(2) * maxdisp in 2-D
;KEYWORDS:
;	spacing: defines the spacing between which the 
;		 displacements are sampled, where the 
;		 default=10
;	stretch: multiplies displacement vectors by a 
;		 scalar equal to stretch
;	nopoly: to not use poly_fit for sub-pixel accuracy
;	fits: within polyf, determines the area of the 
;	      polynomial fit
;	denoise:smooths out any discontinous peaks, 
;		default sensitivity is 0.4
;	morefit: improve poly_fit by trial and error
;--------------------------------------------------
function mollify,image,error 
;
;PURPOSE:
;	Smooths out discontinuous noise from a 2-D array
;EXAMPLE:
;	new_img=mollify(image,.4)
;ARGUMENTS
;	image:n X m array
;	error:0 < num < inf, where amount of smoothing 
;		increases as num goes to 0
;METHOD:
;	For a given point, mollify finds the average of the 
;	surrounding points.  If the value at that point is 
;	more than (error*100)% different from the average 
;	then it is set to the average.
img=float(image)
img2=img+abs(min(img))+1
m=abs(min(img))+1
x_s=n_elements(img(*,0))
y_s=n_elements(img(0,*))
                                                              
;four corners
ave=(total(img(0:1,0:1))-img(0,0))/3.+m
ti=img(0,0)+m
var=ave/ti > ti/ave
if (abs(var-1) lt error ) then img(0,0)=ave-m
                                                              
ave=(total(img(0:1,y_s-2:y_s-1))-img(0,y_s-1))/3.+m
ti=img(0,y_s-1)+m
var=ave/ti > ti/ave
if (abs(var-1) lt error) then img(0,y_s-1)=ave-m
                                                              
ave=(total(img(x_s-2:x_s-1,0:1))-img(x_s-1,0))/3.+m
ti=img(x_s-1,0)+m
var=ave/ti > ti/ave
if (abs(var-1) lt error) then img(x_s-1,0)=ave-m
                                                              
ave=(total(img(x_s-2:x_s-1,y_s-2:y_s-1))-img2(x_s-1,y_s-1))/3.+m
ti=img(x_s-1,y_s-1)+m
var=ave/ti > ti/ave
if (abs(var-1) lt error) then img(x_s-1,y_s-1)=ave-m
                                                              
;top and bottom borders
for i=1,x_s-2 do begin
   ave=(total(img(i-1:i+1,0:1))-img(i,0))/5.+m
   ti=img(i,0)+m
   var=ave/ti  > ti/ave
   if (abs(var-1) lt error) then img(i,0)=ave-m
                                                              
   ave=(total(img(i-1:i+1,y_s-2:y_s-1))-img(i,y_s-1))/5.+m    
	ti=img(i,y_s-1)+m
   var=ave/ti  > ti/ave
   if (abs(var-1) lt error) then img(i,y_s-1)=ave-m
endfor

;left and right borders
for i=1,y_s-2 do begin
   ave=(total(img(0:1,i-1:i+1))-img(0,i))/5.+m
   ti=img(0,i)+m
   var=ave/ti  > ti/ave
   if (abs(var-1) lt error) then img(0,i)=ave-m
                                                              
   ave=(total(img(x_s-2:x_s-1,i-1:i+1))-img(x_s-1,i))/5.+m
   ti=img(x_s-1,i)+m
   var=ave/ti  > ti/ave
   if (abs(var-1) lt error) then img(x_s-1,i)=ave-m
endfor

;the middle of the image
for i=1,x_s-2 do begin
   for j=1,y_s-2 do begin
      ave=(total(img(i-1:i+1,j-1:j+1))-img(i,j))/8.+m
      ti=img(i,j)+m
      var=ave/ti > ti/ave
      if (abs(var-1) gt error) then img(i,j)=ave-m
   endfor
endfor
return,img
end
;------------------------------------------------------------            
function lsf2d, image,try1,try2,xxa,yya,accur
;
;PURPOSE:
;	Based on an initial 2-D poly_fit, this function 
;	adjusts the values of the coefficients by trial 
;	and error.
;EXAMPLE:
;	ab=lsf2d(means,a,b,[x0,x1],[y0,y1],2)
;ARGUMENTS:
;	image: 2-D array of data
;	try1: coefficients in the x-dimension from 
;	      poly_fit
;	try2: coefficients in the y-dimension from
;        poly_fit
;	accur: level of precision for the adjustments,
;         an integer x, such that 1<=x<inf
                                                                               
pram=[transpose(try1),transpose(try2)]
pram2=pram
                                                              
for i=0,5 do begin
   if(i eq 0) then begin
      err=0
      for xx=xxa(0),xxa(1) do begin
      	for yy=yya(0),yya(1) do begin
       		predict = (pram(0)+pram(1)*xx+pram(2)*xx^2)$
            *(pram(3)+pram(4)*yy+pram(5)*yy^2)
         	err = err + (image(xx,yy)-predict)^2
       	endfor
    	endfor
   endif
   pos=1.   & powr=1
   err2=0   & mult=0
   multo=pos/(10^powr)
   repeat begin
      pram2(i)=pram(i)*(1+multo)
      err2=0
      for xx=xxa(0),xxa(1) do begin
     		for yy=yya(0),yya(1) do begin
       		predict = (pram2(0)+pram2(1)*xx+pram2(2)*xx^2)$
          	* (pram2(3)+pram2(4)*yy+pram2(5)*yy^2)
     			err2 = err2 + (image(xx,yy)-predict)^2
       	endfor
    	endfor
      if (err2 lt err) then begin
         mult=mult+pos/(10^powr)
         multo=mult+pos/(10^powr)
         err=err2
      endif else if (pos eq 1.) then begin
         pos=-1.
         multo=mult+pos/(10^powr)
      endif else begin
         pos=1.
         powr=powr+1
         multo=mult+pos/(10^powr)
      endelse
   endrep until (powr ge accur)
pram=pram2
endfor
                                                              
return,pram2
end
;--------------------------------------------------
function dpiv,img1,img2,maxdisp,spacing=spacing,stretch=stretch,$
	nopoly=nopoly,fits=fits,denoise=denoise,morefit=morefit

if (~keyword_set(spacing)) then spacing=2
if (~keyword_set(stretch)) then stretch=1
if (not keyword_set(denoise)) then denoise = 0.4

xsize=n_elements(img1(*,0))
ysize=n_elements(img1(0,*))
if (~keyword_set(fits)) then fits=3
ddisp=fix(maxdisp*2+1)
mind=indgen(fits*2+1)
lengthx=fltarr( (xsize-2*ddisp)/spacing+1, (ysize-2*ddisp)/spacing+1)
lengthy=lengthx
error=lengthx
result=fltarr((xsize-2*ddisp)/spacing+1, (ysize-2*ddisp)/spacing+1, 4)
for i=ddisp,xsize-ddisp,spacing do begin
	for j=ddisp,ysize-ddisp,spacing do begin
		tempimg=1.0*img1(i-maxdisp:i+maxdisp-1,j-maxdisp:j+maxdisp-1)
		means=fltarr(ddisp,ddisp)
		for v=i-maxdisp,i+maxdisp do begin
			for w=j-maxdisp,j+maxdisp do begin		
				tempimg2=1.0*img2(v-maxdisp:v+maxdisp-1,w-maxdisp:w+maxdisp-1)
				means(v-i+maxdisp,w-j+maxdisp)=mean((tempimg-tempimg2)^2)
			endfor
		endfor
		w=where((min(means) eq means),nw)
		error((i-ddisp)/spacing,(j-ddisp)/spacing)=min(means)
		if (variance(means) eq 0) then vw_location=[0,0] else begin         
			if (nw ne 1) then vw_location=[0,0]$ 
				else vw_location=[w mod ddisp, w / ddisp]-maxdisp
		endelse
		vw_re=vw_location+maxdisp	
		x0=vw_re(0)-fits & x1=vw_re(0)+fits
		y0=vw_re(1)-fits & y1=vw_re(1)+fits
		if (~keyword_set(nopoly) && (x0 ge 0) && (x1 lt ddisp)$ 
				       && (y0 ge 0) && (y1 lt ddisp)) then begin
			xxx=vw_re(0) & yyy=vw_re(1)
			a=poly_fit(mind+x0,means(x0:x1,yyy),2,yfit=yfitx)
			b=poly_fit(mind+y0,means(xxx,y0:y1),2,yfit=yfity)
			if(keyword_set(morefit)) then begin
				ab=lsf2d(means,a,b,[x0,x1],[y0,y1],2)
				a=ab(0:2) & b=ab(3:5)
			endif
			minx=(-1)*a(1)/(2*a(2))
			miny=(-1)*b(1)/(2*b(2))
			vw_float=[minx,miny]-maxdisp
	    	vw_int=[fix(vw_float(0)),fix(vw_float(1))]
			mind2=indgen((fits*2+1)*1000)/1000.
			x=mind2+vw_re(1)-fits
			if (abs(vw_float(0)-vw_location(0)) lt 1.0) then if $
				(abs(vw_float(1)-vw_location(1)) lt 1.0) then begin 
					vw_location=vw_float
			endif
		endif
		vw_location=vw_location(*)*stretch
		real_location=float([i+vw_location(0),j+vw_location(1)])
		plots,[i,real_location(0)],[j,real_location(1)],/device
		plots,[real_location(0),real_location(0)],[real_location(1),$
			real_location(1)],/device,color=1000
		ii = (i-ddisp)/spacing & jj=(j-ddisp)/spacing
		lengthx(ii,jj)=vw_location(0)
		lengthy(ii,jj)=vw_location(1)
	endfor
endfor

r_disp=sqrt(lengthx^2+lengthy^2)
result(*,*,0)=lengthx
result(*,*,1)=lengthy
result(*,*,2)=r_disp
result(*,*,3)=error

if(denoise gt 0) then begin
	result(*,*,0)=mollify(result(*,*,0),denoise)
	result(*,*,1)=mollify(result(*,*,1),denoise) 		
	result(*,*,2)=mollify(result(*,*,2),denoise) 		
endif
return,result
end
