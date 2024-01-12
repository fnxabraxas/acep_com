
#S=-0.5
#T=1.5
#eps=1.d-8


S=$1
T=$2
eps=$4
gam=$3


inp1='alllinks_'$S'_'$T'_'$eps'_'$gam'.dat'
out='Q-markov-chain_'$S'_'$T'_'$eps'_'$gam'.dat'

dir='S_'$S'_T_'$T'_eps_'$eps'_gam_'$gam

cd $dir

../markov <<EOF
$inp1
$out
EOF


cd ..
