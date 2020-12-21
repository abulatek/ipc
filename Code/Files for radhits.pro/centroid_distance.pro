PRO centroid_distance, centroids, maxdist, output

;+ 
; Name:
;	CENTROID_DISTANCE
;
; Purpose:
;	Find distances between centroids. This is used with radhits.pro to 
;	eliminate hits that are too close together. 
;
; Inputs: 
;	centroids - [x,y] coordinates of centroids
;	maxdistance - the maximum distance two centroids are allowed to be. 
;		this input parameter will exclude centroids too close together.
;	
; Outputs:
;	output - centroid array with centroids that are too close together
;		excluded. 
;-

; Get dimensions of array
dims = SIZE(centroids, /dimensions)

; Error check for only one centroid
IF size(dims, /dimensions) EQ 1 THEN BEGIN
	output = centroids
	goto, endofscript
ENDIF

; Get distances
distance = DISTANCE_MEASURE(centroids)

; Create arrays to match pairs of indices
i1 = fltarr(dims[1]-1)
i2 = indgen(dims[1]-1) + 1
FOR count=2, dims[1]-1 DO BEGIN
	i1 = [i1, ( fltarr(dims[1]-count) + (count-1) )]
	i2 = [i2, ( indgen(dims[1]-count) + (count) )]
ENDFOR

; Find pairs that are too close together
wheredist = where(distance LT maxdist)

; Set output array elements to centroids and remove values 
IF n_elements(wheredist) EQ 1 THEN BEGIN
	IF wheredist EQ -1 THEN output = centroids
	IF wheredist GE 0 THEN BEGIN
		indices = [i1[wheredist],i2[wheredist]]
		output = removerows(centroids, indices)
		
	ENDIF
ENDIF ELSE BEGIN
	indices = [i1[wheredist],i2[wheredist]]
	induniq = indices[uniq(indices, sort(indices))]
	output = removerows(centroids, induniq)
ENDELSE

endofscript:

END
