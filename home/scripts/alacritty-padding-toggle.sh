#!/bin/sh

alacritty_yml="$XDG_CONFIG_HOME/alacritty/alacritty.yml"
sed -E -i "s/(.*&window_padding ).*/\1$1/" $alacritty_yml
