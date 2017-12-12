INPUT=$(pwd)/ScriptFiles/result1.csv
OLDIFS=$IFS

[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while IFS=',' read -r prbname opname domdir comp domname xx
do
    #IFS= read -r _ procStatus <<<"abc success"
    #IFS= read -r _  <<<"xyz fail"
    #echo "DN : $domname"
    #echo "PN : $prbname"
    #echo "DD : $domdir"
    #echo "Competition : $comp"
    #echo "Type : $type"
    rf="$domname-$opname.txt"
    jn="$prbname.json"
    jp="$(pwd)/ExperimentResults/PlanningGraphs/graphs/$comp/$domname/$jn"
    sp="$(pwd)/ExperimentResults/PlanningGraphs/solutions/$comp/$domname/$rf"
    se="0"
    je="0"
    planl="0"
    firstap="0"
    if [ "$comp" == "IPC1998" ]; then
	if [ -f "$sp" ]; then
            se="1"
	    #plan=$(sed -n "/Begin plan/,/End plan/p" $sp  )
	    planle=$(sed -n "/Begin plan/,/End plan/p" $sp | wc -l)
	    if(($planle > 0)); then
		planl=$(($planle-3))
	    fi
	    ss=$(wc -c < $sp)
	    if [ "$ss" -lt "2000" ]; then
		sr="0"
	    fi
	    if [ -f "$jp" ]; then
		je="1"
	    fi
	    firstap=$(sed -En 's/Goals first reachable in (.*) steps./\1/p' $sp)
	    #echo  "$firstap"
	    if [ "$firstap" == "" ]; then
	    	firstap="0"
	    fi

	    
	fi
	#    echo "$jp"

	#    echo "$domname, $prbname, $comp, $se, $planl,  $je "
    fi
    echo "$prbname,$opname,$domdir,$comp,$domname,$se,$planl,$je,$firstap," >> results.csv
#        echo "$domname, $prbname, $comp, $se, $planl,  $je "
done < $INPUT
IFS=$OLDIFS
