function extractInformation(){
    fn=$(basename $1 | awk -F. '{print $1}')
    pn=$(head -n 1 $1 | sed -nE 's/\(define \(problem ([a-zA-Z0-9\-]*)\).*/\1/p')
#    pn=$(sed -En 's/\(problem (.*)\)/\1/p' $1)
    if [ ! "$pn" == "" ]; then
	echo "$fn, $pn"
    fi
}

dir=domains
echo "domain, fname, pname" > mapnames02.csv
for dom in $(ls $dir); do
    echo "dom: $dom"
    adir=$dir/$dom
    for plan in $(ls $adir); do
	echo "prob: $plan"
	bdir=$adir/$plan
	echo "$bdir"
	if [ -f $bdir ]; then
      	    res=$(extractInformation $bdir)
	    #echo "d: $bdir v: $res"
	    if [ ! "$res" == "" ]; then
		echo "$dom, $res" >> mapnames02.csv
	    fi
	fi
	if [ -d $bdir ]; then
	    for planh in $(ls $bdir); do
		cdir=$bdir/$planh
      		res=$(extractInformation $cdir)
		#echo "dh: $cdir v: $res"
		if [ ! "$res" == "" ]; then
		    echo "$dom, $res" >> mapnames02.csv
		fi
	    done
	fi
    done
done
