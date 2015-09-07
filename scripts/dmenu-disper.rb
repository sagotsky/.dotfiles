#!/usr/bin/env ruby

displays = %x{disper --list}.split("\n")
display_res = {}
while displays.any? do
  (display, res) = displays.shift(2) 
  display = display.split(' ')[-1]
  res = res.split(', ')[-1]
  display_res[display] = res
end

combos = display_res.map{ |k,v| "#{v} [#{k}]".center(16) }
if combos.count > 1
  # should this also list single screens?
  combos = combos.permutation(2).to_a.map{ |p| p.join(' | ') } 
  opts = "-l #{[5, combos.count].min} "
  dmenu = IO.popen("dmenu #{opts}", 'r+')
  dmenu.write combos.join("\n")
  dmenu.close_write
  selected =  dmenu.gets
else 
  selected = combos.first
end 

displays = selected.scan(/\[.*?\]/).map{|s| s.delete('[]')}.join ','
resolutions = selected.split(/\s+/).grep(/\d+x\d+/).join ','

`xrandr -s 1366x768 ; sleep .5` # setting this first somehow prevents the screen overlap I think
puts "disper -e #{displays} -r #{resolutions} -t bottom ; xrandr --dpi 96"
`disper -e #{displays} -r #{resolutions} -t bottom ; xrandr --dpi 96`


