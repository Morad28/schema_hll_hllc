# set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# set output 'contours.3.png'
set dgrid3d
set key at screen 1, 0.9 right top vertical Right noreverse enhanced autotitle nobox
set style textbox  opaque margins  0.5,  0.5 fc  bgnd noborder linewidth  1.0
set view 60, 30, 0.85, 1.1
set samples 50, 50
set isosamples 250, 250
set contour base
set cntrparam levels 31
set cntrparam levels incremental 1,0.005,1.8
set style data lines
set title "contour" 
set xlabel "X axis" 
set xrange [ * : * ] noreverse writeback
set x2range [ * : * ] noreverse writeback
set ylabel "Y axis" 
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zlabel "Z " 
set zlabel  offset character 1, 0, 0 font "" textcolor lt -1 norotate
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
NO_ANIMATION = 1
splot "out.txt" u 1:2:3
# splot x*y*(1-x)*(1-y)