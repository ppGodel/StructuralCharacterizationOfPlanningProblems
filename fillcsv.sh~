INPUT=$(pwd)/relations2.csv
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
    jp="$(pwd)/ExperimentsResults/PlanningGraphs/graphs/$comp/$domname/$jn"
    sp="$(pwd)/ExperimentsResults/PlanningGraphs/solutions/$comp/$domname/$rf"
    se="0"
    je="0"
    planl="0"
    if [ "$comp" == "IPC1998" ]; then
    if [ -f "$sp" ]; then
        se="1"
	#plan=$(sed -n "/Begin plan/,/End plan/p" $sp  )
	planl=$(sed -n "/Begin plan/,/End plan/p" $sp | wc -l)	
	ss=$(wc -c < $sp)
	if [ "$ss" -lt "2000" ]; then
	    sr="0"
	fi
	if [ -f "$jp" ]; then
	    je="1"
	fi
    fi
    echo "$jp"
    #echo "$domname,$prbname,$domdir,$comp,$domname,$se,$planl,$je," #>> results.csv
#    echo "$domname, $prbname, $comp, $se, $planl,  $je "
    fi
done < $INPUT
IFS=$OLDIFS
