#!/usr/bin/env ruby

require 'date'
require 'time'
require 'trollop'
require 'pry'
require 'timeout'

opts = Trollop::options do
  opt :calendars, 'Comma separated list of calendars to display', default: ''
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
    # I think this fails becasue of the : in the time being read by bar.  \: doesn't fix it.
    %w[title start_time location description].map{ |f| send(f)}.join("\n")
  end

  def soon?
    start = Time.parse("#{start_date} #{start_time}")
    (start - Time.now < 900)
  end
end

def time_range
  start = Time.now - 600
  finish = Time.now + 3600*24*21 
  time = "#{start.hour}:#{start.min} #{finish.day}/#{finish.month}/#{finish.year}"
end 

def day(date)
  days = %w[Sun Mon Tue Wed Thu Fri Sat ]
  days[Date.parse(date).wday]
end

def fg(color, str)
  "%{F#{color}}#{str}%{F-}"
end

def bg(color, str)
  "%{B#{color}}#{str}%{B-}"
end

def clickable(title, more)
  "%{A:#{more}:}#{title}%{A}"
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

bar_opts = "-p -f '-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*' -g 1279x12+1280+0"
bar = IO.popen("bar #{bar_opts}", 'r+')
bar.write "loading bar...\n"
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
    while show_entry = bar.gets.to_i do 
      puts agenda[show_entry].more
    end 
  end
end 
