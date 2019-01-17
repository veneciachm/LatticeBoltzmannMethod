n=150
iter=0
set term gif animate transparent opt delay 10 
set output "CapillaryWave.gif"
set xrange[0:160]
set yrange[50:90]
load "DensityProfile.gnu"
