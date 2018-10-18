for file in *.eps; do mv "$file" "${file//[[:space:]]}"; done
find . -maxdepth 1 -iname "hist*$1*.eps" -exec epstopdf {} ";"
find . -maxdepth 1 -iname "hist*$1*.pdf" > output
pdfunite $(cat output) $2.pdf
find . -name "hist*$1*.pdf" -exec rm {} ";"
