
#S=-0.5
#T=1.5
#eps=1.d-8


S=$1
T=$2
eps=$3


inp2='recurrentes_'$S'_'$T'_'$eps'.dat'
inp1='Q-markov-chain_'$S'_'$T'_'$eps'.dat'
out='recurrentes_estac_'$S'_'$T'_'$eps'.dat'

dir='S_'$S'_T_'$T'_'eps'_'$eps

cd $dir

../estac_recurren <<EOF
$inp1
$inp2
$out
EOF

#mv $out $inp2

cd ..
