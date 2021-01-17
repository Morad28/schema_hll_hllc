set xrange [0:1]
set yrange [0:1]
set iso 100
set samp 100
set cntrparam levels 30
set terminal gif animate delay 15 loop 40
set output "anim.gif"

unset key
unset surface
set view map
set contour base
set pm3d 

do for [i=11:40] { splot sprintf('sol%d.dat', i) using 1:2:3 w lines lw 1;  reread }
splot "out.txt" u 1:2:3 w lines lw 1
    
