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
    for p in $plf; do
	pn=$(basename "$p")
	if [ "$pn" == "$dfn" ]; then
	    continue
	fi
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
	    fi
	    mv "$(pwd)/*.json" "$RDIR/graphs/$bn/$nbc/"
	else
	    echo "File exists $pn skip"
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
		echo "Dominio" $nbc
		findlocal $dcp
	    fi
	done
    fi
done
