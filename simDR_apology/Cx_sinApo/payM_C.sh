
#S=-0.5
#T=1.5

b=2
w=0.9
eAF=0
TRE='11'


e=$1
d=$2
gam=$3
eps=$4
#eps=1.d-8

inp='strategies.dat'

dir='S_'$e'_T_'$d'_eps_'$eps'_gam_'$gam
mkdir $dir

outf='payM_'$e'_'$d'_'$eps'_'$gam'.dat'

./payM_C <<EOF
$TRE
$gam
$eAF
$b
$eps
$w
$e
$d
$inp
$outf
EOF

mv $outf $dir/.



