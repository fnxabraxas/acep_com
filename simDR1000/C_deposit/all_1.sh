eps=0

for e in 0 0.25 0.5 1
do

for d in 0 1 2 4 8
do


#./payM_C.sh $e $d $eps

#./mapea_sim.sh $e $d $eps #> $out
#mv $out logs

./recurrentes.sh $e $d $eps

./markov.sh $e $d $eps

./probabs.sh $e $d $eps

./estac_recurren.sh $e $d $eps

./estac_recurren_ext1.sh $e $d $eps


done
done
