#/bin/bash
if [ -d $1 ]; then
  dir=$1
  mkdir -p out/$1
  for file in `ls $1 | grep -E '\.ml$'`; do
    # echo "### $file"
    tmp=$(mktemp)
    ml2hfl "$dir/$file" > $tmp 2> /dev/null
    if [ $? -ne 0 ]; then
      echo "$dir/$file failed"
      rm $tmp
    else
      mv $tmp "out/$dir/${file%.*}.in"
    fi
  done
else
  echo "$dir is not directory"
  exit 1
fi
