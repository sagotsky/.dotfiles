#!/bin/bash

# adds a script to ~/bin
# script cds into a folder and executes a binary
# this is stupid, but HIB mandates it...

FILE=$1

find $FILE -printf '%h %f \n' | while read -a line ; do
  # grab $2 or base name as name for new file
  if [[ $# == 2 ]] ; then
    NAME=$2
  else 
    NAME=${line[1]}
  fi

  # append pwd when path isn't absolute
  if [[ ! -e "/$FILE" ]] ; then
   line[0]=$PWD/${line[0]} 
  fi

  cat > ~/bin/$NAME <<EOF
#!/bin/sh

cd ${line[0]}
./${line[1]}
EOF

  chmod +x ~/bin/$NAME
  
done
