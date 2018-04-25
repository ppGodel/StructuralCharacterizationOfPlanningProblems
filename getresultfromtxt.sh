function extractInformation(){
    #planle=$(sed -n "/Begin plan/,/End plan/p" $sp | wc -l)
    pn=$(sed -En 's/Problem name: (.*)/\1/p' $1)
    firstap=$(sed -En 's/Goals first reachable in (.*) steps./\1/p' $1)
    #echo  "$firstap"
    if [ "$firstap" == "" ]; then
	firstap="0"
    fi
    planle=$(sed -En 's/^([0-9]*) total actions in plan/\1/p' $1)
    if [ "$planle" == "" ]; then
	planle="0"
    fi
    graphlength=$(sed -n "/Begin plan/,/End plan/p" $1| sed '1d;$d'| awk '{print $1}' | tail -n 1)
    if [ "$graphlength" == "" ]; then
	graphlength=$(sed -En 's/goals at time ([0-9]*):/\1/p' $1 | tail -n 1)
	if [ "$graphlength" == "" ]; then
	    #graphlength=$(sed -En 's/time: ([0-9]*):/\1/p' $1 | tail -n 1)
	    graphlength="0"	    
	else
	    graphlength=$(($graphlength-1))
	fi
    fi
    if [ ! "$pn" == "" ]; then
	echo "$pn, $firstap, $planle, $graphlength"
    fi
}

jf=$(find $1 -type f -iname '*.txt')
for j in $jf; do
    ei=$(extractInformation $j)
    if [ ! "$ei" == "" ]; then
	echo "$3, $ei" >> $2
    fi
done
