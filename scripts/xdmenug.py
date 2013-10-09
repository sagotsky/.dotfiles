#!/usr/bin/env python2

# uses dmenu for selecting from xdg-menu entries
#
# seems like an odd choice for a dmenu fan, but sometimes you really want 
# your distro's control panel

# https://pyxdg.readthedocs.org/en/latest/_modules/xdg/Menu.html

from xdg.Menu import parse, Menu, MenuEntry
import subprocess

dmenu_opts = '-b -i -l 20 -fn -*-terminus-bold-r-*-*-16 -nf #fff -nb #000 -sf #f00 -sb #300' #make this an arg later
dmenu = 'dmenu ' + dmenu_opts.strip()
dmenu = dmenu.split(' ')

def parse_menu(menu):
  items = {}
  for submenu in menu.Entries:
    if isinstance(submenu, Menu):
      items[submenu.getName() + '/'] = submenu

    elif isinstance(submenu, MenuEntry):
      items[submenu.DesktopEntry.getName()] = submenu.DesktopEntry.getExec()
  return items

active_menu = parse()

bin = ''
while bin == '':
  menus = parse_menu(active_menu)

  pipes = subprocess.Popen(dmenu,
      stdout=subprocess.PIPE,
      stdin=subprocess.PIPE)

  selected, err = pipes.communicate('\n'.join(menus.keys()))
  menu = menus[selected.strip()]

  if isinstance(menu, str):
    bin = menu
  else:
    active_menu = menu

print bin
