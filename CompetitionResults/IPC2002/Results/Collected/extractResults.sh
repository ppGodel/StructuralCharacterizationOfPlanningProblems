dir=Collected
echo "competition, planner, domain, pn, steps, time" > results02.csv
for d1 in $(ls Collected); do
    echo "$d1"
    adir=$dir/$d1
    for dom in $(ls $adir); do
	echo "$dom"
	bdir=$adir/$dom/Strips
	for plan in $(ls $bdir); do
	    cdir=$bdir/$plan
	    if [ -f $cdir ]; then
      		cat $cdir| awk -v d="$dom" -v p="$plan" -F' ' '{ print "IPC2002, " p ", " d ", pfile" $1 ", " $4 ", " $5}' >> results02.csv
	    fi
	done
	ddir=$bdir/HandCoded
	for planh in $(ls $ddir); do
	    edir=$ddir/$planh
	    if [ -f $edir ]; then
                cat $edir| awk -v d="$dom" -v p="$plan" -F' ' '{ print "IPC2002, " p ", " d ", upfile" \
$1 ", " $4 ", " $5}' >> results02.csv
	    fi
	done
    done
done
