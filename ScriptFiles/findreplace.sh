# bashrc
function file_replace() {
  for file in $(find . -type f -name "*$1*"); do
    mv $file $(echo "$file" | sed "s/$1/$2/");
  done
}

file_replace -- -
