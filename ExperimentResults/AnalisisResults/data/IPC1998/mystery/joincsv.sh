dom=mysty
head strips-$dom-x-1.csv -n 1 > strips-$dom-all.csv
awk 'FNR > 1' strips-$dom-x-*.csv >> strips-$dom-all.csv
