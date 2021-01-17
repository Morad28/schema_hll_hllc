
reset
set dgrid3d
# f(x,y)=sin(1.3*x)*cos(.9*y)+cos(.8*x)*sin(1.9*y)+cos(y*.2*x)
# set xrange [0:1]
# set yrange [0:1]
# set isosample 250, 250
# set table 'test.dat'
# splot 'out.txt' u 1:2:3
# unset table

set datafile separator "\t"
set contour base
set cntrparam level incremental -3, 0.5, 3
unset surface
set table 'cont.dat'
#splot 'out.txt' u 1:2:3
unset table

reset
set xrange [0:1]
set yrange [0:1]
unset key
set palette rgbformulae 33,13,10
p 'out.txt' with image#, 'cont.dat' w l lt -1 lw 1.5