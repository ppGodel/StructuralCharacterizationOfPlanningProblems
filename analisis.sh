#!/bin/bash
ADIR=$(pwd)/CompetitionsResults/IPC*
BBDIR=$(pwd)/Files
tg=100
to=100

if [ ! -z "$1" ]; then
    ADIR=$1
fi
if [ ! -z "$2" ]; then
    BBDIR=$2
fi
if [ ! -z "$3" ] && [ $4> 0 ]; then
    tg=$4
fi
if [ ! -z "$4" ] && [ $5 > 0 ]; then
    to=$5
fi

for dp in $ADIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/domains/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
		echo "Dominio" $nbc
		domf=$(find $dcp -type f -iname '*domain*')
		domn=$(basename $domf)
		#echo $(find $dcp -type f -iname '*.pddl')
		filelist=$(find $dcp -type f -iname '*.pddl' | sort -n)
		for f in $filelist; do
		    fn=$(basename $f)
		    if [ "$fn" != "$domn" ]; then
			echo "$BBDIR/blackbox -o $domf -f $f -x -M 9999 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > $(pwd)/ExperimentsResults/PlanningGraphs/$domn-$fn.txt "
		    fi
		done
	    fi
	done
    fi
done
