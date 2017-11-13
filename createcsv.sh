fl=$(grep '\(\s*problem .*' -RPHo)
fl1=${fl//:\(problem/,}
echo "$fl1">> relations.csv
sed -i "s/)/,/g" relations.csv
wc --lines  relations.csv
awk -F, '{ print $2 "," $1 }' relations.csv > relations2.csv
bn=$(awk -F',' '{ print $2 }' relations2.csv)
fbn=$(basename $bn)
echo "$fbn" > aux.csv
paste -d',' aux.csv relations2.csv > relations.csv
awk -F,  '{ print $2 ", " $1 ", " $3 }' relations.csv > relations2.csv

