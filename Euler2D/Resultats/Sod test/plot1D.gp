set size ratio -1

set term png size 1980,1050
set output 'HLLx_x_HLL.png'
set xlabel "Position"
set ylabel "Densite"
plot "HLLy_Sod_test.dat" u 1:3 w lines,  "HLL_Sod_test.txt" u 1:2 w lines

set term png size 1980,1050
set output 'HLLy_x_HLL.png'
set xlabel "Position"
set ylabel "Densite"
plot "HLLy_Sod_test.dat" u 2:3 w lines,  "HLL_Sod_test.txt" u 1:2 w lines

set term png size 1980,1050
set output 'HLLCx_x_HLLC.png'
set xlabel "Position"
set ylabel "Densite"
plot "HLLC2Dx_Sod_test.dat" u 1:3 w lines,  "HLLC_Sod_test.txt" u 1:2 w lines

set term png size 1980,1050
set output 'HLLCy_x_HLLC.png'
set xlabel "Position"
set ylabel "Densite"
plot "HLLC2Dy_Sod_test.dat" u 2:3 w lines,  "HLLC_Sod_test.txt" u 1:2 w lines



