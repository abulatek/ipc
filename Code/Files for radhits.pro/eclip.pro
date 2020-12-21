; eclip		

; based on eclip by	Eric Weeks, 9-22-98
; see http://www.physics.emory.edu/~weeks/idl/

; Gianguido C Cianci 28 Aug 2006
;        - if no values are found within the clipping limits, returns a
;          row of -1 with same ncol as input data
;        - if above is true AND wherevar is set, then returns -1
;        - closed if statement after s=size(f3) at the right place
; GCC 21 Sep 2006
;        - Actually implemented changes I thought were working 3 weeks
;          ago.
;        - Added /old flag to retreive old behaviour
;        - Now checks for parameters size to be EQ 3 (and not GT 1 as it
;          used to).
; GCC 22 Sep 2006
;        - Added count=count to return length of result, like in where()

; data should be in the form (*,*)
;
; datanew=eclip(data,[A,B,C]) is equivalent to:
;   w=where(data(A,*) ge B and data(A,*) le C)
;   datanew=data(*,w)
;
; Set /where  to return the 'where' ptrs
; Set /invert to return the complementary set of data
; Set /old to neglect any clipping parameter [f1, ..., f6] that yields and
;     empty set AND keep on clipping if need be.
; Set count=n to save the count of items found in variable n


function eclip,data,$
                f1,f2,f3,f4,f5,f6, $
                invert=invert,wherevar=wherevar, old=old,  count = count


Compile_opt idl2


IF keyword_set(invert) THEN invert = 1 ELSE invert = 0
IF keyword_set(wherevar) THEN wherevar = 1 ELSE wherevar = 0
IF keyword_set(old) THEN old = 1 ELSE old = 0
IF keyword_set(verbose) THEN verbose = 1 ELSE verbose = 0

result = data
wall=lindgen(n_elements(data[0,*]))
keep = wall

somethingleft = 1
cont = 1

s=size(f1)
;; if f1 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f1[0],*] GE  f1[1]) AND  (result[f1[0],*]) LE f1[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = data[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF

s=size(f2)
;; if f2 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f2[0],*] GE  f2[1]) AND  (result[f2[0],*]) LE f2[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = result[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF

s=size(f3)
;; if f3 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f3[0],*] GE  f3[1]) AND  (result[f3[0],*]) LE f3[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = result[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF

s=size(f4)
;; if f4 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f4[0],*] GE  f4[1]) AND  (result[f4[0],*]) LE f4[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = result[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF

s=size(f5)
;; if f5 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f5[0],*] GE  f5[1]) AND  (result[f5[0],*]) LE f5[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = result[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF

s=size(f6)
;; if f6 is a 3x1 array then
IF  s[1] EQ 3 AND cont THEN  BEGIN 

   ;;find the data I want to keep
   w=where((result[f6[0],*] GE  f6[1]) AND  (result[f6[0],*]) LE f6[2],nw)

   IF nw GT 0 THEN BEGIN
      somethingleft = 1         ;redundant I think
      cont = 1
      result = result[*, w]

      keep = keep[w]
   ENDIF ELSE IF old THEN BEGIN
      somethingleft = 0
      cont = 1
      result = data
      keep = wall
   ENDIF ELSE BEGIN
      cont = 0
      somethingleft = 0 
   ENDELSE
ENDIF





;;;;;;;;;;;;;;;;;;;;;;
;;Preparing result!!
;;;;;;;;;;;;;;;;;;;;;;

empty = fltarr(n_elements(data[*, 0]), 1)
empty[*] = -1

temp = lindgen(n_elements(data[0, *]))
temp[keep] = -1
inversekeep = where(temp GE 0)


IF somethingleft AND invert THEN BEGIN
   result = data[*, inversekeep]
   wh = inversekeep
ENDIF ELSE IF somethingleft AND ~invert THEN BEGIN
   ;;result = result
   wh = keep
ENDIF ELSE IF (~somethingleft AND invert) OR (~somethingleft AND old) THEN BEGIN
   result = data
   wh = wall
ENDIF ELSE IF (~somethingleft AND ~invert) OR (~somethingleft AND ~old) THEN BEGIN
   result = empty
   wh = -1
ENDIF

;; Do we need a count too?
IF arg_present(count) THEN BEGIN
   IF wh[0] EQ -1 THEN BEGIN
      count = 0
   ENDIF ELSE count = n_elements(wh)
ENDIF

IF wherevar THEN result = wh

return,  result

END 



