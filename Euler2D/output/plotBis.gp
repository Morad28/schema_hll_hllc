set xrange [0:1]
set yrange [0:1]
set size ratio -1
set iso 100
set samp 100
set cntrparam levels 50
unset key
unset surface
set view map
set contour base
set pm3d 
splot 'sol33.dat' using 1:2:3 w lines lw 1
    
