#!/bin/bash
RDIR=$(pwd)/ExperimentResults/PlanningGraphs
tg=15
to=10
AFILE=$(pwd)/ScriptFiles/result02.csv
BFILE=$(pwd)/ScriptFiles/result02_1.csv


if [ ! -z "$1" ]; then
    ADIR=$1
else
    ADIR=$(pwd)/CompetitionResults/IPC2002    
fi
if [ ! -z "$2" ]; then
    BBDIR=$2
else
    BBDIR=$(pwd)/ScriptFiles
fi
if [ ! -z "$3" ] && [ $4> 0 ]; then
    COM=$3
else
    COM=IPC2002
fi
if [ ! -z "$4" ] && [ $5> 0 ]; then
    tg=$4
fi
if [ ! -z "$5" ] && [ $6 > 0 ]; then
    to=$5
fi


function createdir()
{
    if [[ ! -d $1 ]]; then
	echo "creating $1"
	mkdir $1
    fi
}


function findlocal()
{
    echo "Procesando $1"
    plf=$(find $1 -maxdepth 1 -type f -iname '*.pddl')
    domf=$(find $1 -maxdepth 1 -type f -iname '*domain*')
    if [ -z "$domf" ]; then
	domf=$2
    fi
    dfn=$(basename "$domf")
#    echo $domf
    for p in $plf; do
#	echo "File $p" 
	pn=$(basename "$p")
	#ppn=$(echo ${pn%.pddl})
	if [ "$pn" == "$dfn" ]; then
	    continue
	fi
#	echo "Archivos $dfn $pn $BBDIR"
#	fp=2
#	hs=0
#	hg=0
	#echo "FFFFILE           $pn $nbc"
#	faux=$(awk -F, -v apn="$pn" -v anbc="$nbc" '$5==anbc && $2==apn {print $0}' $AFILE)
#	IFS=',' read -a values <<< "$faux"
#	fp=${values[5]}
#	hs=${values[6]}
#	hg=${values[7]}
#	ipn=${values[0]}
	#echo "${values[5]} ${values[6]}"
	#awk -F, -v apn="$pn" -v anbc="$nbc" '$2==apn && $5==anbc {print $0}' $AFILE #| while IFS=, read v1 v2 v3 v4 v5 v6 v7 v8 v9;
	#do fp=$v6; hs=$v7; hg=$v8; done	
#	echo "$fp $hs $hg"

#	if [[ ( ! -z "$hs"  &&  "$hs" == "0" ) || ( ! -z "$hg"  &&  "$hg" == "1" ) ]]; then
	if [ ! -f "$RDIR/solutions/$bn/$nbc/$nbc-$pn.txt" ] ; then
	    echo "File $pn will be reprocessed $(date)"
	    createdir $RDIR/solutions/$bn
	    createdir $RDIR/solutions/$bn/$nbc
	    createdir $RDIR/graphs/$bn
	    createdir $RDIR/graphs/$bn/$nbc
	    
	    $BBDIR/blackbox -o $domf -f $p -x -M 32760 -maxauto 50 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > "$RDIR/solutions/$bn/$nbc/$nbc-$pn.txt"
	    planle=$(sed -n "/Begin plan/,/End plan/p" "$RDIR/solutions/$bn/$nbc/$nbc-$pn.txt" | wc -l)
	    if(($planle > 0)); then
		    planl=$(($planle-2))
		    echo "plan finded! $planl $(date)"
		    awk -F, -v pl="$planl" -v apn="$pn" -v anbc="$nbc" 'BEGIN{FS=OFS=","} $5==anbc && $2==apn {$7=pl}1' $AFILE > $BFILE
#		    echo "$bf"
#		    echo "$bf" > $BFILE
	    fi
	    mv "$(pwd)/*.json" "$RDIR/graphs/$bn/$nbc/"
#	else
#	    echo "File exists $pn skip"
	    
	fi
	
	
    done
    
    for directory in $1*; do
	if [[ -d $directory ]] && [ "$directory" != "$1" ]; then
	    echo "directorio y p $directory $domf"
	    findlocal $directory/ $domf
	fi
    done
}


for dp in $ADIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/domains/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
		#		if [ "$nbc" != "gripper" ] && [ "$nbc" != "assembly" ] && [ "$nbc" != "logistics" ];then
		echo "Dominio" $nbc
		findlocal $dcp
		#		fi
		#		for typ in $dcp/*; do
		#		    typn=$(basename $typ)
		#		    echo "Tipo" $typn
		#		    domf=$(find $dcp -type f -iname '*domain*')
		#		    domn=$(basename $domf)
		#		#echo $(find $dcp -type f -iname '*.pddl')
		#		    filelist=$(find $dcp -type f -iname '*.pddl' | sort -n)
		#		    for f in $filelist; do
		#			fn=$(basename $f)
		#			if [ "$fn" != "$domn" ]; then			 
		#			    $BBDIR/blackbox -o $domf -f $f -x -M 9999 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > "$(pwd)/ExperimentsResults/PlanningGraphs/$nbc-$typn-$fn.txt"
		#			fi
		#		    done
		#		done
		#		fi
	    fi
	done
    fi
done
