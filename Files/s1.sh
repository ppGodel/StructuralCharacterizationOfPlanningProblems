#!/bin/bash 
if [ -f $1 ] &&  [ -f $2 ] && [ "$3"!="" ] ; then  
    dp=$1
    pp=$2
    on=$3
    tg=3600
    to=1800
    if [ "$4"!="" ] && [ $4> 0 ]; then
#	echo "p4 $4"
	tg=$4
    fi
    if [ "$5"!="" ] && [ $5 > 0 ]; then
#	echo "p5 $5"
	to=$5
    fi
#    echo "Works"
    ./blackbox -o $dp -f $pp -x -M 9999 -solver -maxsec $tg graphplan -then -maxsec $to walksat -then -maxsec $to satz -then -maxsec $to compact > $on &
else
    echo "Hay Error en parámetros $1 $2 $3"
fi 