
for gam in 0 0.5 1 2 4
do

for err in 0 0.01 0.1
do

for e in 0.25 #0 0.25 0.5 1
do

for d in 4 #0 1 2 4 8
do

#for gam in 0 0.5 1 2
#do

#./payM_C.sh $e $d $gam $err

#./mapea_sim.sh $e $d $gam $err #> $out
#mv $out logs

./recurrentes.sh $e $d $gam $err

./markov.sh $e $d $eps $gam $err

./probabs.sh $e $d $eps $gam $err

./estac_recurren.sh $e $d  $gam $err

./estac_recurren_ext1.sh $e $d  $gam $err


done
done

done
done
