#!/bin/bash

# gtk3-theme-preview
# The world's stupidest gtk3 theme previewer

ORIGINAL_GTK_THEME="$GTK_THEME"

gtk3_themes() {
  for themes in ~/.themes /usr/share/themes ; do
    find $themes -name gtk-3.0
  done | 
    sed -e 's/.*\/\(.*\)\/gtk-3.0/\1/' |
    sort |
    uniq
}

pick_theme() {
  gtk3_themes | zenity --list --text="Pick a theme" --column=Theme --height 600 --width 350 2>/dev/null
}


THEME="$(pick_theme)"

if [[ "$THEME" == "" ]] ; then
  [[ "$GTK_THEME" != "" ]] && echo "Selected: '$GTK_THEME'.  To make it permanent, export GTK_THEME='$GTK_THEME'"
else
  export GTK_THEME="$THEME"
  $0 &
  export GTK_THEME="$ORIGINAL_GTK_THEME"
fi

