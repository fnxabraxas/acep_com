
#S=-0.5
#T=1.5
#eps=1.d-8


S=$1
T=$2
eps=$4
gam=$3

inp='payM_'$S'_'$T'_'$eps'_'$gam'.dat'
#inp2='payM_'$S'_'$T'_'$eps'_gam_'$gam2'.dat'
outp='Xeq_'$S'_'$T'_'$eps'_'$gam'.dat'
outl='links_'$S'_'$S'_'$eps'_'$gam'.dat'
outLs='alllinks_'$S'_'$T'_'$eps'_'$gam'.dat'
#outover='overloop_'$S'_'$T'_'$eps'_gam_'$gam'.dat'

dir='S_'$S'_T_'$T'_eps_'$eps'_gam_'$gam

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
