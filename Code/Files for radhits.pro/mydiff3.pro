; mydiff.pro   started 7-15-98 by Eric Weeks from mkmovie3.pro
;
;

function mydiff3,a,b

x=1.0/stdev(a)
y=1.0/stdev(b)
xx=total(a)/n_elements(a)
yy=total(b)/n_elements(b)
temp5=((x*(a-xx)-y*(b-yy))+2.0)*64.0
temp5 = ((temp5 > 0.0) < 255.0)

return,temp5
end

