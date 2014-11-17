#!/usr/bin/env ruby

displays = %x{disper --list}.split("\n")
display_res = {}
while displays.any? do
  (display, res) = displays.shift(2) 
  display = display.split(' ')[-1]
  res = res.split(', ')[-1]
  display_res[display] = res
end

combos = display_res.map{ |k,v| "#{k} [#{v}]" }.tap{ |c| c = c.combination(2).to_a.join('|') if c.length > 1}


dmenu = IO.popen('dmenu ', 'r+')
dmenu.write combos.join("\n")
dmenu.close_write
selected =  dmenu.gets


