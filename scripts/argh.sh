#!/bin/bash

###
# argh.sh
#
# arg helper.  or something.  makes it easy to treat variables in a 
# bash script as option flags.  also sets up usage docs.
#
# Usage:
#
#  1. Run the script and eval its results
#     eval "$(./argh.sh $@)"
#  2. Use variable substitutions to provide defaults (example sets $VAR to 'default')
#     VAR="${VAR:-default}"
#  3. Annotate your variables. hash-dash comments (#-) marks them for argh, provides usage text.
#     PORT="${PORT:8080}"  #- runs demo server on port (default: 8080)
#  4. (Optional) Provide additional notes/text with hash-dash comments with no var
#     #- Description: script.sh runs a demo script
#  5. (Optional) Set up an rc file.  script.sh would use ~/.scriptrc and options are specified
#     as name: value
###


# get the parent script
CMD=$(ps -ocommand= -p $PPID | cut -f 2 -d' ')
RC="$(basename $CMD)" && RC="$HOME/.${RC%%.*}rc"
[[ -x "$CMD" ]] || CMD=$(which $CMD)

# get ARGS from command line, set vars from shell script
ARGS="$(echo $@ | sed -e 's/--/\n/g')"

# RE matches magic argh vars.  Should probably make the RE more specific
function list_opts {
  grep '#-' "$CMD" 
}

# help message.  this gets eval'ed, hence the echo echo
function opts_help {
  echo -e "echo '\n$(basename $CMD) usage:\n'"
  list_opts | while read line ; do
    opt=$(echo $line | cut -f 1 -d= | tr [:upper:] [:lower:])
    def=$(echo $line | sed -e 's/.*#-//')

    if [[ "$(echo $opt | wc -w)" -gt 1 ]] ; then
      echo "echo $def"
    else 
      echo -e "echo '--$opt %%$def'"
    fi
  done
  echo -e "echo '-h %% Display this help message\n'"
  echo exit
}

# Display help
if [[ "$ARGS" == *-h* ]] ; then
  opts_help | column -t -s%%
  exit
fi

# Or return var set commands
for opt in $(list_opts | cut -f1 -d=) ; do
  value=$(echo "$ARGS" | grep -i $opt | sed -e 's/ *$//' -e 's/\w* //') # use value, or arg name if it's a bool
  [[ "$value" == '' && -f "$RC" ]] && value=$(grep -i "^$opt: " $RC | cut -f 2- -d:)
  echo "$opt='$value'"
done

