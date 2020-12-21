;
;	a little tool for looking at other idl programs
;	the memory is the first to go...
;
pro man,fname

name = which(fname)
spawn,"less -X " + name

end