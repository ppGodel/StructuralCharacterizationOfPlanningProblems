#!/bin/bash
ADIR=$(pwd)/CompetitionsResults/IPC1998
BBDIR=$(pwd)/Files
tg=10
to=10

notin=(gripper assembly)

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
	echo "Archivos $dfn $pn $BBDIR"
	if [ ! -f "$RDIR$nbc-$typn-$fn.txt" ] ; then
	    echo "File NOT exists $p"
	    $BBDIR/blackbox -o $domf -f $f -x -M 9999 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > "$(pwd)/ExperimentsResults/PlanningGraphs/$nbc-$typn-$fn.txt"
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


if [ ! -z "$1" ]; then
    ADIR=$1
fi
if [ ! -z "$2" ]; then
    BBDIR=$2
fi
if [ ! -z "$3" ] && [ $4> 0 ]; then
    tg=$3
fi
if [ ! -z "$4" ] && [ $5 > 0 ]; then
    to=$4
fi

for dp in $ADIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/domains/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
		if [ "$nbc" != "gripper" ] && [ "$nbc" != "assembly" ] && [ "$nbc" != "logistics" ];then
		    echo "Dominio" $nbc
		    findlocal $dcp
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
		fi
	    fi
	done
    fi
done
