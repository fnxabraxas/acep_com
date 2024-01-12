
#S=-0.5
#T=1.5
#eps=1.d-8


S=$1
T=$2
eps=$3

inp='payM_'$S'_'$T'_'$eps'.dat'
#inp2='payM_'$S'_'$T'_'$eps2'.dat'
outp='Xeq_'$S'_'$T'_'$eps'.dat'
outl='links_'$S'_'$S'_'$eps'.dat'
outLs='alllinks_'$S'_'$T'_'$eps'.dat'
#outover='overloop_'$S'_'$T'_'$eps'.dat'

dir='S_'$S'_T_'$T'_'eps'_'$eps

cd $dir

date
../mapea_sim <<EOF
$inp
$outp
$outl
$outLs
EOF
date

#cp $outover ciclos/.

cd ..
