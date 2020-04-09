#/bin/bash
if [ -d $1 ]; then
  dir=$1
  mkdir -p out/$1
  for file in `ls $1 | grep -E '\.ml$'`; do
    # echo "### $file"
    ret=$(ml2hfl "$dir/$file" 2> /dev/null)
    if [ $? -ne 0 ]; then
      echo "$dir/$file failed"
    else
      echo "$ret" > "out/$dir/${file%.*}.in"
    fi
  done
else
  echo "$dir is not directory"
  exit 1
fi
