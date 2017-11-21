compres=$(pwd)/CompetitionsResults/IPC1998/uniform-strips-round1.results
AFILE=$(pwd)/compres.csv
cn=1

if [ ! -z "$1" ]; then
    compres=$1
fi

if [ ! -z "$2" ]; then
    AFILE=$2
fi

if [ ! -z "$3" ]; then
    cn=$3
fi

if [ "$cn" != "0"  ]; then
  echo "" > $AFILE  
fi

faux=$(tail $compres -n 1)
IFS=' ' read -a values <<< "$faux"
for i in $(seq 1 ${values[1]})
do
    j=$(($i+1))
    probres=$(sed -n "/^$i /,/^$j /p" $compres)
    fl=$(echo "$probres" | head -n 1 )
    IFS=' ' read -a v1 <<< "$fl"
    pn=$(echo "${v1[1]}" | awk '{print tolower($0)}')
    rf=$(echo "$probres" | tail -n +3 | head -n -2)
    while IFS=' ' read -a valores;
    do
	echo "$pn,${valores[1]},${valores[3]},${valores[4]}," >> $AFILE	
    done <<< "$rf"
done 
