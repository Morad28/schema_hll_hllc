
set term png size 1980,1050
set output 'res.png'

set multiplot layout 1,3

set xlabel "Position x"
set ylabel "Densite"
plot[-0.01:1.01] "out.txt" u 1:2 w lines



set xlabel "Position x"
set ylabel "Vitesse"
plot[-0.01:1.01] "out.txt" u 1:3 w lines

set xlabel "Position x"
set ylabel "Pression"
plot[-0.01:1.01] "out.txt" u 1:4 w lines

unset multiplot