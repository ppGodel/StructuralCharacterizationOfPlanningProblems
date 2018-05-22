find . -name "$1*.eps" -exec epstopdf {} ";"
pdfunite $1*.pdf $2.pdf
find . -name "$1*.pdf" -exec rm {} ";"
