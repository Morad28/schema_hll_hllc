
set pm3d at s 
set palette rgbformulae 33,13,10
set contour
unset clabel
set cntrparam levels 50

 
 
splot 'sol11.dat' using 1:2:3 w lines lw 1 palette notitle
    
