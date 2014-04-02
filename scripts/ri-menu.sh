#!/bin/bash

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
RI="$(which ri)"

# List all objects and their methods for RI lookup
TERM=$(( /usr/bin/env ruby <<-EOF
  require 'rails'
  [Class, Module].each do |type| 
    ObjectSpace.each_object(type) do |obj| 
      obj.instance_methods.each do |method|
        print "#{obj}##{method}\n"
      end
    end
  end
EOF
) |  dmenu -b -l 20 -i
)

[[ $? && "$TERM" != "" ]] && xterm -bg '#202025' -fa 'Source Code Pro-10' -e "bash -i -c '$RI $TERM '"


