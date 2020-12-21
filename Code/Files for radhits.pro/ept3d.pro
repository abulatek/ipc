;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NAME:
;  ept3d
;  
; WARNING:
;  The default settings are to be checked by the user. Don't
;  assume it works right out of the box!
;+
; PURPOSE:
;  Pretracks data in 3-D.
;  This procedure does the whole thing: it checks for correct order of
;  slices in a stack, removes some brightness gradient in the
;  z-direction, optionally pads top and bottom slice, band passes the 
;  data and runs feature3d.
;-
; INPUT:
;  FNAME: Filename to be pretracked. Accepts wildcards. Takes either
;  raw Noran (SGI-movie) or VT-Eye (tiff stacks) or IDL generated gdf 
;  files..
;
; PARAMS:
;   BPLO:   low parameter(s) for bpass (can be 1D or 3D)
;   BPHI:   high parameter(s) for bpass (can be 1D or 3D)
;   DIA:    diameter for feature3d (can be 1D or 3D)
;   SEP:    separation for feature3d (can be 1D or 3D)
;   GDF:    Set /gdf to read gdf files created by IDL.
;   TIFF:   Set /tiff to read tiff files from VT-Eye.
;   NORAN:  Set /noran to read noran files
;   NOFIX:  Set /nofix to avoid the "noran" fix with misplaced slices
;   INVERT: Set /invert to invert the images to be analysed.
;   PAD:    Set /pad to add extra slice on bottom & top
;   DEBUG:  Set /debug to output the corrected image right before bpass
;
; OUTPUT:
;  For each file 'myfile', writes a gdf file 'pt.myfile' with the
;  following data stucture:
;   column 0-> this contains the x centroid positions, in pixels.
;   column 1-> this contains the y centroid positions, in pixels. 
;   column 2-> this contains the z centroid positions, in pixels.
;   column 3-> this contains the integrated brightness.
;   column 4-> this contains the squared radius of gyration. 
;  Columns 5 and 6 are rarely used:
;   column 5-> this contains the peak height of the feature.
;   column 6-> this contains the fraction of voxels above threshold.
;
; EXAMPLE:
;  To pretrack a tiffstack file 'myfile' :
;  ept3d, 'myfile', bphi=[11,11,12], dia=[11,11,12], sep=5, /tiff
;
; HISTORY:
; 5-30-01  Rachel Courtland
;          Modified from Eric Weeks' code
; 8-06-01  Eric Weeks
;          Automatically move last slice to bottom of stack
;          if needed (formerly: /fix).
; 3-24-04  Gianguido Cianci
;          - added some comments here and there
;          - added tiff compatibility
;          - allowed for parameters for bpass3d and feature3d 
;            to be passed at command line
;          - added default switch for vti 3D data (will have to
;            be set when Vt-Eye is calibrated)
;          - default is still for noran 3D stacks:
;            1.0x tube, 100x oil, low-res (256x240 images)
; 7-27-04  Gianguido Cianci
;          - added this header
; 7-23-06  Eric Weeks
;          - modified from gpt3d.pro to be more like epretrack.pro
; OTHER:
;  This uses John Crocker's revised "feature3d"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro ept3d, fname, bplo=bplo,bphi=bphi,dia=dia,sep=sep, $
           gdf=gdf, tiff=tiff, noran=noran, invert=invert, $
           debug=debug, nofix=nofix, pad=pad

message, /inf, "WARNING: default parameters are somewhat arbitrary"

if (not keyword_set(bplo)) then bplo = [1,1,1]
if (not keyword_set(bphi)) then bphi = [5,5,5]
if (not keyword_set(dia)) then   dia = [11,11,11]
if (not keyword_set(sep)) then   sep = dia/2
print,"separation = ",sep

f = findfile(fname,count=nf)
;; findfile goes to find the file(s), you can use wildcards if you want
if (nf eq 0) then message,'no match', /inf

;;loop over files
for i = 0,nf-1 do begin
   message, 'loading file:' + f(i) + '(' + string(nf) + ' total)',/inf
	flag = 0b
   if (keyword_set(gdf)) then begin
      a3 = read_gdf(f(i)) & flag = 1b
   endif else begin
	   if (keyword_set(tiff)) then begin
	      a3 = readtiffstack(f(i)) & flag = 1b
	   endif
		if (keyword_set(noran)) then begin
	      a3 = read_noran(f(i)) & flag = 1b
	   endif
   endelse
	if (flag eq 0b) then message,'need to use /gdf, /tiff, or /noran'
   if (keyword_set(invert)) then begin
      a3 = 255b-a3
   endif
   ;;Get size of images and assume they are all the same!
   if (i eq 0) then begin
      nx=n_elements(a3(*,0,0))
      ny=n_elements(a3(0,*,0))
      nz=n_elements(a3(0,0,*)) 
   endif

	if (keyword_set(noran) and not keyword_set(nofix)) then begin
	   ;;compare correlation between 1st, 2nd and last frame to
	   ;;decide if last (few) frames are supposed to be at the beginning
	   c1=correlate(a3(*,*,0),a3(*,*,1))
	   c2=correlate(a3(*,*,0),a3(*,*,nz-1))
	   count=0
	   while (c2 gt c1*0.8) do begin
		  print,c1,c2
		  ;; last frame really is first frame
		  a3=shift(a3,0,0,1)
		  c1=correlate(a3(*,*,0),a3(*,*,1))
		  c2=correlate(a3(*,*,0),a3(*,*,nz-1))
		  count=count+1
		  if (count ge nz) then c2=-1.0
	   endwhile
	endif
   
   ;;This block fits the average gradient in intensity 
   ;;in the z diraction to a straight line and subtracts it 
   ;;from the picture to compensate for darker slices deeper in
   ;;the sample
   za=total(total(a3,1),1)/nx/ny 
   nzarr=findgen(nz)
   fit=linfit(nzarr,za)
   a4=float(a3) & for j=0,nz-1 do a4(*,*,j)=a4(*,*,j)-(j*fit[1])
   a3=0
   
   ;;Evens out each 2D slice
   a5=unshade2(a4)
   a4=0
   
	if (keyword_set(pad)) then begin
	   ;;prepend 1st slice and append last slice to data to avoid
	   ;;funky edge errors
	   aa=bytarr(nx,ny,nz+2)
	   aa(*,*,0)=a5(*,*,0) & aa(*,*,nz+1)=a5(*,*,nz-1) & aa(*,*,1:nz)=a5
	endif else begin
		aa = a5
	endelse
   a5=0
   if (keyword_set(debug)) then write_gdf,aa,'fig.debug'

   ;;Do a band pass in 3D
   b=bpass3d(aa,bplo,bphi)
   aa=0
   
   ;;Look for features in 3D
   message,'feature3d:' +f(i),/inf
   c=feature3d(b,dia,sep=sep,thresh=0.25)
   b=0
   
   write_gdf,c,'pt.'+f(i)
   c=0
endfor

end
