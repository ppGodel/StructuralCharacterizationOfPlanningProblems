#!/bin/bash

BDIR=$(pwd)
if [ ! -z "$1" ]; then
    BDIR=$1
fi
comp=IPC1998
if [ ! -z "$2" ]; then
    comp=$2
fi
echo "base path $BDIR $comp"
GDIR=$BDIR/ExperimentResults/PlanningGraphs/graphs/$comp
CDIR=$BDIR/ExperimentResults/AnalisisResults/data/$comp
SDIR=$BDIR/ScriptFiles
AFILE=$BDIR/ScriptFiles/result$comp.csv
#BFILE=$(pwd)/ScriptFiles/result98_1.csv
#echo "Exp $GDIR"
function analysejson()
{
    echo "Procesando $1 del dominio $nbc"
    jsonf=$(find $1 -maxdepth 1 -type f -iname '*.json' | sort -R)
    for j in $jsonf; do
	jn=$(basename "$j")
#	faux=$(awk -F, -v apn="$jn" -v anbc="$nbc" '$5==anbc && $2==apn {print $0}' $AFILE)
#	IFS=',' read -a values <<< "$faux"
#	fp=${values[5]}
#	hs=${values[6]}
#	hg=${values[7]}
#	ipn=${values[0]}
	#echo "$fp $hs $hg"
	#if [ ! -e $j ]; then
	    echo "File $j will be processed $(date)"
	    python3 $SDIR/readjson.py $j T $CDIR/$nbc/ T $nbc T
	#else
	#    echo "File $j already processed"
	#fi
    done
    
#    for directory in $1*; do
#	if [[ -d $directory ]] && [ "$directory" != "$1" ]; then
#	    echo "directorio y p $directory $domf"
#	    findlocal $directory/ $domf
#	fi
#    done
}

echo $comp
for dp in $GDIR;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
#		if [ "$nbc" == "Blocks" ];then # && [ "$nbc" != "mprime" ] && [ "$nbc" != "movie" ] && [ "$nbc" != "mystery" ];then
#		if [ "$nbc" == "mystery" ];then
		    echo "Dominio" $nbc
		    analysejson $dcp $nbc
#		fi

	    fi
	done
    fi
done
