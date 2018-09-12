#!/bin/bash
function findlocal()
{
    echo "Procesando $1"
    plf=$(find $1 -maxdepth 1 -type f)
    domf=$(find $1 -maxdepth 1 -type f -iname '*.pddl')
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
	txtf="$RDIR$nbc-$typn-$pn.txt"
	if [ -f "$txtf" ] ; then
	    echo "File exists $pn skip"	   
	else
	    $BBDIR/blackbox -o $domf -f $p -x -M 32760 -maxauto 200 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > "$txtf"
	    
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
    HOME=$1
else
    HOME=$pwd
fi
if [ ! -z "$2" ]; then
    compet=$2
else
    compet="IPC2002"
fi
#if [ ! -z "$2" ]; then
#    ADIR=$2   
#fi
#if [ ! -z "$3" ]; then
#    BBDIR=$3
#fi
#if [ ! -z "$4" ] && [ $4> 0 ]; then
#    tg=$3
#fi
#if [ ! -z "$5" ] && [ $5 > 0 ]; then
#    to=$4
#fi


ADIR=$HOME/CompetitionResults/$compet
BBDIR=$HOME/ScriptFiles
RDIR=$HOME/ExperimentResults/PlanningGraphs/Solutions/$compet
tg=15
to=10

for dp in $ADIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/domains/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
#		if [ "$nbc" == "Blocks" ];then #||[ "$nbc" = "assembly" ];then
		    echo "Dominio" $nbc
		    for typ in $dcp/*; do
			typn=$(basename $typ)
			echo "Tipo" $typn
			findlocal $typ/
			#domf=$(find $typ -type f -iname '*domain*')
			#domn=$(basename $domf)
			#echo $(find $dcp -type f -iname '*.pddl')
			#filelist=$(find $typ -type f -iname '*.pddl' | sort -n)
			#for f in $filelist; do
			#    fn=$(basename $f)
			#    if [ "$fn" != "$domn" ]; then
			#	echo "$domf $f"
			#    fi
			#done
		    done
#		fi
	    fi
	done
    fi
done
