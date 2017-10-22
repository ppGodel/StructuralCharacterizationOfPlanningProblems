#!/bin/bash

ADIR=$(pwd)
for dp in $ADIR/CompetitionsResults/IPC*;
do
    if [[ -d $dp ]]; then
        bn=$(basename "$dp")
	echo "Competencia" $bn
	for dcp in $dp/domains/*; do
	    if [[ -d $dcp ]]; then
		nbc=$(basename $dcp)
		#echo "Problema" $nbc
		echo $dcp
		echo $(find $dcp -iname '*.ppdl')
		for f in $(find $dcp -iname '*.ppdl' ); do
		    echo "instancia:" $f
                    #fn=$(basename $f)    
		done
	    fi
	done
    fi
       
#    cd $i/domains
#    for j in $(find -maxdepth 1 -type d );
#    do
#	cd $j
#	for k in $(find -maxdepth 1 -type f -name '*.pddl' );
#	do
#	    echo "Ejecuando: $i/domains$j$k"
#	done
#	echo "output: $i"
#    done
done
