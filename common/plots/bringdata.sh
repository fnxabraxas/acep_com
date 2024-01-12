
S="0.25"
T="4"

for eps in "0" "0.01" "0.1"
do

for gam in "0" "0.5" "1" "2" "4"
do


dir='S_'$S'_T_'$T'_eps_'$eps'_gam_'$gam

inpR='recurrentes_estac_'$S'_'$T'_'$eps'_'$gam'.dat'
inpX='Xeq_'$S'_'$T'_'$eps'_'$gam'.dat'
outM='S_'$eps'_T_'$gam'.mat'
echo $outM

#cp ../$dir/$inpR .
#cp ../$dir/$inpX .

#ls $inpR

#./bringdata << EOF
#$inpR
#$inpX
#$outM
#EOF

#rm $inpR
#rm $inpX
#mv $outM data/.

if [ $eps = "0" ]; then eps2="0"; fi
if [ $eps = "0.01" ]; then eps2="0.05"; fi
if [ $eps = "0.1" ]; then eps2="0.1"; fi
outM2='S_'$eps2'_T_'$gam'.mat'
cd data
cp $outM $outM2
cd ..

done
done
