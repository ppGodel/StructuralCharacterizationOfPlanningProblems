INPUT=$(pwd)/relations.csv
OLDIFS=$IFS

[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while IFS=',' read -r domname prbname domdir comp type
do
    
    #IFS= read -r _ procStatus <<<"abc success"
    #IFS= read -r _  <<<"xyz fail"
    #echo "DN : $domname"
    #echo "PN : $prbname"
    #echo "DD : $domdir"
    #echo "Competition : $comp"
    #echo "Type : $type"
    rf="$DN-$type-$PN.pddl.txt"
    jn="$pn.json"
    jp="ExperimentResults/PlanningGraphs/graphs/$DN/$type/$jn"
    sp="ExperimentResults/PlanningGraphs/solutions/$DN/$rf"
    se="0"
    je="0"
    planl="0"
    if [ "$comp" == "IPC1998" ]; then
    if [ -f "$sp" ]; then
        se="1"
	#plan=$(sed -n "/Begin plan/,/End plan/p" $sp  )
	planl=$(sed -n "/Begin plan/,/End plan/p" $sp | wc -l)
	
	ss=$(wc -c < $sp)
	if [ $ss < 2000 ]; then
	    sr="0"
	fi
	if [ -f "$jp" ]; then
	    je="1"
	fi
    fi
    echo "$DN, $PN, $DD, $comp, $type, $se, planl , $je" #>> results.csv
    fi
done < $INPUT
IFS=$OLDIFS
