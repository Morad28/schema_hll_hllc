# # set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400 
# # set output 'contours.3.png'
set dgrid3d
set key at screen 1, 0.9 right top vertical Right noreverse enhanced autotitle nobox
set style textbox  opaque margins  0.5,  0.5 fc  bgnd noborder linewidth  1.0
set view 60, 30, 0.85, 1.1
set samples 50, 50
set isosamples 11, 11
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
# # splot x*y*(1-x)*(1-y)

# set pm3d map impl
# set contour
# set style increment user
# do for [i=1:18] { set style line i lc rgb "black"}
# set cntrparam levels incr 0.,0.1,1.
# set palette defined (0 '#352a87', 1 '#0363e1',2 '#1485d4', 3 '#06a7c6', 4 '#38b99e', 5 '#92bf73', 6 '#d9ba56', 7 '#fcce2e', 8 '#f9fb0e')
# set autoscale fix
# splot "out.txt" u 1:2:3 w pm3d notitle

# set pm3d map
# set xrange [0:1]
# set yrange [0:1]
# set cbrange [0:1] 
# set palette rgbformulae 22,13,10
# splot "out.txt" u 1:2:3