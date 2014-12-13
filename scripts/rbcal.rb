#!/usr/bin/env ruby

require 'date'
require 'time'
require 'trollop'
require 'pry'

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
    #%w[title start_time location description].map{ |f| send(f)}.join("\t")
    ''
  end

  def soon?
    start = Time.parse("#{start_date} #{start_time}")
    (start - Time.now < 900)
  end
end

# tsv lacks calendar...
puts "loading... #{opts[:calendars]}"

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

while true do 
  calendar_opts = opts[:calendars].split(',').map{ |cal| "--calendar '#{cal}'"}.join ' '

  agenda = `gcalcli #{calendar_opts} agenda #{time_range} --tsv --nomilitary --details all`.split("\n").map do |line|
    Event.new line
  end

  last = nil
  txt = agenda.reduce([]) do |txt, entry|
    txt << fg('orangered2', "#{day entry.start_date}") unless last == entry.start_date
    last = entry.start_date

    if entry.soon?
      txt << "#{bg 'dimgray', fg('white', entry.start_time)}" if entry.start_time != '00:00'
    else 
      txt << "#{fg 'dimgray', entry.start_time}" if entry.start_time != '00:00'
    end 

    txt << "#{clickable entry.title, entry.more}"
  end

  puts txt.join(' ').slice(0, 1200)
  STDOUT.flush

  sleep 60
end 
