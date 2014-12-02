set terminal post eps color enhanced
set out "isom.eps"

set key left top
#set rmargin 0
#set lmargin 0

set xlabel "m"
set xtics 0, 30
set ylabel "seconds"
set logscale y 2

plot [0:150][0.00001:8] 'bench.dat' index 2 using 1:10 with line lt 8 lw 3 title "Phi1",\
'bench.dat' index 2 using 1:11 with line lt 9 lw 3 title "Phi2",\
'bench.dat' index 2 using 1:12 with line lt 10 lw 3 title "Matrix",\
'test_relative.dat' index 0 using 2:($8/1000) with line lt 11 lw 3 title "Magma"
