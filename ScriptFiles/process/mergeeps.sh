for file in *.eps; do mv "$file" "${file//[[:space:]]}"; done
find . -iname "hist*$1*.eps" -exec epstopdf {} ";"
find . -iname "hist*$1*.pdf" > output
pdfunite $(cat output) $2.pdf
find . -name "hist*$1*.pdf" -exec rm {} ";"
