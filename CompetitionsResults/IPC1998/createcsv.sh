fl=$(grep '(problem .*)' -R --with-filename -Po  )
fl1=echo${fl//:problem/,}
fl2=echo${fl1//)/\n}
echo -e ${fl1//)/\n} >> relations.csv
