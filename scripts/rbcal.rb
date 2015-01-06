#!/usr/bin/env ruby

require 'date'
require 'time'
require 'trollop'
require 'pry'
require 'timeout'

opts = Trollop::options do
  opt :calendars, 'Comma separated list of calendars to display', default: ''
  opt :bar, 'Options to pass into bar.  e.g. --bar="-b -f \'Source Code Pro\' -p"', default: ''
end

# could it leave off opt and use method_missing instead?

#class entry 
class Event
  EVENT_FIELDS = %W[start_date start_time end_date end_time link something title location description calendar]
  attr_accessor *EVENT_FIELDS.map(&:to_sym)

  def initialize(gcalcli_line)
    fields = gcalcli_line.split("\t")
      
    EVENT_FIELDS.each do |field|
      send "#{field}=", fields.shift
    end
  end 

  def more
    ["<b><a href='#{link}'>#{title}</a></b>", '', "#{start_time}-#{end_time}", location, description].compact.join "\n"
  end

  def soon?
    start = Time.parse("#{start_date} #{start_time}")
    (start - Time.now < 900)
  end
end

def time_range
  start = Time.now - 600
  finish = Time.now + 3600*24*14 
  time = "#{start.hour}:#{start.min} #{finish.year}-#{finish.month}-#{finish.day}"
end 

def day(date)
  days = %w[Sun Mon Tue Wed Thu Fri Sat ]
  days[Date.parse(date).wday]
end

def clickable(title, index)
  "%{A:#{index}:}#{title}%{A}"
end

# quicker way to color?  clears up some of the entry.soon conditional since we can just set colors there
def c(text, colors)
  text = "%{B#{colors[:bg]}}#{text}%{B-}" if colors[:bg]
  text = "%{F#{colors[:fg]}}#{text}%{F-}" if colors[:fg]
  text
end

def sleepwalk(seconds)
  # do a thing until the timeout breaks it
  begin 
    Timeout::timeout(seconds) { yield } 
  rescue Timeout::Error
  end 
end

bar = IO.popen("bar #{opts[:bar]}", 'r+')
bar.write "loading agenda ... #{opts[:calendars]}\n"
#todo trap kill so we can clean bar
#todo object around bar?

while true do 
  calendar_opts = opts[:calendars].split(',').map{ |cal| "--calendar '#{cal}'"}.join ' '

  agenda = `gcalcli #{calendar_opts} agenda #{time_range} --tsv --nomilitary --details all`.split("\n").map do |line|
    Event.new line
  end

  last = nil
  txt = agenda.each.with_index.reduce([]) do |txt, (entry, index)|
    txt << c("#{day entry.start_date}", fg: 'orangered2') unless last == entry.start_date
    last = entry.start_date

    colors = entry.soon? ? {bg: 'dimgray', fg: 'white'} : {fg: 'dimgray'}
    txt << c(entry.start_time, colors) unless entry.start_time == '00:00'

    txt << "#{clickable entry.title, index}"
  end

  bar.write "#{txt.join(' ').slice(0, 1200)}\n"

  sleepwalk 60 do 
    # this approach mostly works, but there will be a delay while gcalcli is waiting.  
    # not sure how ugly it would be to make a network fetching thread or a bar reading thread 
    # (although if bar gets its own wrapper, maybe it could get hidden in there...)
    while show_entry = bar.gets.to_i do 
      msg = agenda[show_entry].more.gsub '&', '&amp;'
      `zenity --info --text "#{msg}" --timeout 2 &`
    end 
  end
end 
