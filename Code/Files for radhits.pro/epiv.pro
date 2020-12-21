;function epiv,img1,img2,maxdisp,spacing=spacing,stretch=stretch,$
;	nopoly=nopoly,fits=fits,denoise=denoise,morefit=morefit, $
;   window=window
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
;   07/04:modified a tiny bit by Eric Weeks
;   07/04:try some large modifications (ERW)
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
;		 default=1
;	stretch: multiplies displacement vectors by a 
;		 scalar equal to stretch
;	nopoly: to not use poly_fit for sub-pixel accuracy
;	fits: within polyf, determines the area of the 
;	      polynomial fit
;	denoise:smooths out any discontinous peaks, 
;		default sensitivity is 0.4
;	morefit: improve poly_fit by trial and error
;   local:  search for local minimum for correlation,
;       rather than global minimum over entire search window
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
function epiv,img1,img2,maxdisp,spacing=spacing,stretch=stretch,$
	nopoly=nopoly,fits=fits,denoise=denoise,morefit=morefit, $
    local=local,window=window

if (~keyword_set(spacing)) then spacing=1
if (~keyword_set(stretch)) then stretch=1
if (not keyword_set(denoise)) then denoise = 0.4
if ((maxdisp*0.5) ne (floor(maxdisp*0.5))) then $
	message,'golly, I really prefer even numbers for maxdisp',/inf
if (not keyword_set(window)) then window = maxdisp
if ((window*0.5) ne (floor(window*0.5))) then $
	message,'golly, I really prefer even numbers for window',/inf

xsize=n_elements(img1(*,0))
ysize=n_elements(img1(0,*))
if (~keyword_set(fits)) then fits=3
ddisp=fix(maxdisp*2+1)
dddisp = maxdisp/2
dddwin = window/2
mind=indgen(fits*2+1)

result=fltarr((xsize-dddwin*2)/spacing+1, (ysize-dddwin*2)/spacing+1, 4)
sresult=fltarr(xsize-dddwin*2,ysize-(dddwin)*2,ddisp,ddisp)
tempimg1 = 1.0*img1
for i=-maxdisp,maxdisp do begin
	for j=-maxdisp,maxdisp do begin
		tempimg2=1.0*shift(img2,-i,-j)
		prod=(tempimg2-tempimg1)^2
		ans=smooth(prod,window)
		; crop out the non-smooth parts
		ans = ans(window/2:xsize-window/2-1,window/2:ysize-window/2-1)
		sresult(*,*,i+maxdisp,j+maxdisp) = ans
	endfor
endfor
tvscl,mydiff3(img1,img2)
thepic = mydiff3(img1,img2)


for i0=0,xsize-2*dddwin-1,spacing do begin
	for j0=0,ysize-2*dddwin-1,spacing do begin
		means=reform(sresult(i0,j0,*,*))
		if (variance(means) eq 0) then begin
			vw_location=[0,0] 
			mn = -1.0
		endif else begin	
			if (keyword_set(local)) then begin
				z0 = means(maxdisp,maxdisp)
				region = search2d(means,maxdisp,maxdisp,0,z0)
				index_y = region / (SIZE(means))[1] 
				index_x = region - (index_y * (SIZE(means))[1])
				tmeans = means(index_x,index_y)
				mn=min(tmeans)
				w=where(mn eq tmeans,nw)
				if (nw ne 1) then vw_location=[0,0]$ 
					else vw_location=[index_x[w],index_y[w]]-maxdisp
			endif else begin
				mn=min(means)
				w=where(mn eq means,nw)
				if (nw ne 1) then vw_location=[0,0]$ 
					else vw_location=[w mod ddisp, w / ddisp]-maxdisp
			endelse
		endelse
		; NOTE:  vw_location is the displacement vector
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
			minx=(-1.0)*a(1)/(2.0*a(2))
			miny=(-1.0)*b(1)/(2.0*b(2))
			vw_float=[minx,miny]-maxdisp
			if (max(abs(vw_float - vw_location)) lt 1.0) then begin
				vw_location=vw_float
			endif
		endif
		vw_location=vw_location*stretch
		if (max(abs(vw_location)) gt 4) then $
			thepic[i0+dddwin,j0+dddwin] = $
				thepic[i0+dddwin,j0+dddwin]*0.4 + 255b*0.6
		if (max(abs(vw_location)) lt 2) then $
			thepic[i0+dddwin,j0+dddwin] = $
				thepic[i0+dddwin,j0+dddwin]*0.5
		ii = i0/spacing & jj=j0/spacing
		result(ii,jj,3)=mn;              the error
		result(ii,jj,0)=vw_location(0);  the x displacement
		result(ii,jj,1)=vw_location(1);  the y displacement
	endfor
	if ((i0 mod 10) eq 0) then tv,thepic
endfor

result(*,*,2)=sqrt(result(*,*,0)^2 + result(*,*,1)^2)

if(denoise gt 0) then begin
	result(*,*,0)=mollify(result(*,*,0),denoise)
	result(*,*,1)=mollify(result(*,*,1),denoise) 		
	result(*,*,2)=mollify(result(*,*,2),denoise) 		
endif
print,"lengths:  mean = ",mean(result(*,*,2)), $
      " max= ",max(result(*,*,2)), $
      " max/sqrt(2)= ",max(result(*,*,2))/sqrt(2.0)
return,result
end

