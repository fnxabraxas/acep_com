
#S=-0.5
#T=1.5

e=$1
d=$2
eps=0
#eps=1.d-8

inp='strategies.dat'

dir='S_'$e'_T_'$d'_eps_'$eps
mkdir $dir

outf='payM_'$e'_'$d'_'$eps'.dat'

./payM_C <<EOF
$inp
$eps
$e
$d
$outf
EOF

mv $outf $dir/.



