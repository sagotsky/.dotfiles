#!/usr/bin/env ruby

require 'date'

calendars = ['Jon Sagotsky', 'J&J', 'jsagotsky']

#class entry 
class Event
  EVENT_FIELDS = %W[start_date start_time end_date end_time link something title location description]
  attr_accessor *EVENT_FIELDS.map(&:to_sym)

  def initialize(gcalcli_line)
    fields = gcalcli_line.split("\t")
      
    EVENT_FIELDS.each do |field|
      send "#{field}=", fields.shift
    end
  end

  def link_to_full
    "%{A:#{full}:}#{title}%{A}"
  end 

  def full
    %w[title start_time location description].map{ |f| send(f)}.join("\n")
  end
end

# tsv lacks calendar...
puts 'loading...'

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

while true do 
  calendar_opts = calendars.map{ |cal| "--calendar '#{cal}'"}.join ' '
  agenda = `gcalcli #{calendar_opts} agenda #{time_range} --tsv --details all`.split("\n").map do |line|
    Event.new line
  end

  last = nil
  txt = agenda.reduce([]) do |txt, entry|
    txt << fg('orangered2', "#{day entry.start_date}") unless last == entry.start_date
    last = entry.start_date

    txt << "#{fg 'dimgray', entry.start_time}" if entry.start_time != '00:00'
    txt << "#{entry.title}"
  end
  puts txt.join ' '
  STDOUT.flush

  sleep 60
end 
