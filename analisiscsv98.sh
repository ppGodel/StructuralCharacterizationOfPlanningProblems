#!/bin/bash

BDIR=$(pwd)
if [ ! -z "$1" ]; then
    BDIR=$1
fi
comp=IPC1988
if [ ! -z "$2" ]; then
    comp=$2
fi
echo "base path $BDIR $comp"
GDIR=$BDIR/ExperimentResults/PlanningGraphs/graphs/$comp
CDIR=$BDIR/ExperimentResults/AnalisisResults/data/$comp
SDIR=$BDIR/ScriptFiles
tg=15
to=10
AFILE=$BDIR/ScriptFiles/result$comp.csv
#BFILE=$(pwd)/ScriptFiles/result98_1.csv
echo "Exp $GDIR"
function analysejson()
{
    echo "Procesando $1"
    jsonf=$(find $1 -maxdepth 1 -type f -iname '*.json')
    for j in $jsonf; do
	jn=$(basename "$j")
	faux=$(awk -F, -v apn="$jn" -v anbc="$nbc" '$5==anbc && $2==apn {print $0}' $AFILE)
	IFS=',' read -a values <<< "$faux"
	fp=${values[5]}
	hs=${values[6]}
	hg=${values[7]}
	ipn=${values[0]}
	#echo "$fp $hs $hg"
	echo "File $pn will be reprocessed $(date) \n with values $CDIR $nbc"
	python3 $SDIR/readjson.py $j T $CDIR/$nbc/
	
    done
    
#    for directory in $1*; do
#	if [[ -d $directory ]] && [ "$directory" != "$1" ]; then
#	    echo "directorio y p $directory $domf"
#	    findlocal $directory/ $domf
#	fi
#    done
}


if [ ! -z "$2" ]; then
    comp=$2
fi
if [ ! -z "$3" ] && [ $4> 0 ]; then
    tg=$3
fi
if [ ! -z "$4" ] && [ $5 > 0 ]; then
    to=$4
fi

for dp in $GDIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
#		if [ "$nbc" != "gripper" ] && [ "$nbc" != "assembly" ] && [ "$nbc" != "logistics" ];then
		if [ "$nbc" == "mystery" ];then
		    echo "Dominio" $nbc
		    analysejson $dcp
		fi

	    fi
	done
    fi
done
