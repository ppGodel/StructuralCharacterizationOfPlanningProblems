for f in *.json; do
    a="$(echo $f | sed s/mysty/mystery/)"
    mv "$f" "$a"
    echo "$f $a"
done