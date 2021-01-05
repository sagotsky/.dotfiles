#!/usr/bin/env ruby

require "io/console"
require "timeout"
require "pry"
require "thor"

module WodClock
  module ShellColor
    def red(text)
      "\e[1;31m #{text}\e[0m"
    end

    def blue(text)
      "\e[1;34m #{text}\e[0m"
    end
  end

  class Cli < Thor
    class_option :sets,
      desc: "Number of sets",
      required: false,
      type: :numeric,
      aliases: "-s"

    class_option :set_duration,
      desc: "Length of each set",
      aliases: "-d",
      type: :numeric,
      required: false

    class_option :countdown,
      desc: "Count down to 0?",
      aliases: "-c",
      type: :boolean,
      required: false

    desc "rest", "Timer keeps rest breaks short and consistent"
    def rest
      args = { sets: 5, set_duration: 90 }.merge(timer_options)
      timer = new_timer(Rest, args)
      Clock.new(timer).call
    end

    desc :emom, "Every minute, on the minute"
    def emom
      args = { sets: 5, set_duration: 60 }.merge(timer_options)
      timer = new_timer(Emom, args)
      Clock.new(timer).call
    end

    desc :tabata, "2:1 HIIT:Rest"
    def tabata
      args = { sets: 8, set_duration: 30 }.merge(timer_options)
      timer = new_timer(Tabata, args)
      Clock.new(timer).call
    end

    private

    def timer_options
      options.transform_keys(&:to_sym).slice(:sets, :set_duration)
    end

    def countdown?
      !!options[:countdown]
    end

    def new_timer(klass, args)
      timer = klass.new(**args)
      timer = Countdown.new(timer) if countdown?
      timer
    end
  end

  # just print the damn info.  we'll make big letters later
  class BasicFormatter
    include ShellColor

    def render(timer_output)
      print_cr " "*80

      if timer_output
        print_cr [blue(timer_output.current_set), format_seconds(timer_output.seconds, timer_output.color)].join("\t")
      else
        print_cr Time.now
      end
    end

    private

    def print_cr(str)
      print "\r#{str}"
    end

    def format_seconds(total_seconds, color)
      min = total_seconds / 60
      sec = (total_seconds % 60).to_s

      min_sec = "#{min}:#{"%02d" % sec}"

      if color && ShellColor.instance_method(color)
        send(color, min_sec)
      else
        min_sec
      end
    end
  end

  class Clock
    def initialize(timer)
      @timer = timer
      @formatter = BasicFormatter.new
    end

    def call
      # Intro.new # todo: 3-2-1 GO!
      loop do
        sleeper = Thread.new { sleep 1 }
        key_reader = Thread.new { Thread.current[:keypress] = Timeout::timeout(1) { STDIN.getch } rescue nil }
        [sleeper, key_reader].each(&:join)
        keypress = key_reader[:keypress]

        output = timer_cycle(keypress)
        render(output)
      end
    end

    private

    def timer_cycle(keypress)
      @timer.input(keypress)
      exit if @timer.done?
      @timer.tick
    end

    def render(output)
      @formatter.render output
    end
  end

  class TimerTemplate
    attr_reader :current_set, :sets, :set_duration, :seconds

    def initialize(sets:, set_duration: 60)
      @current_set = 1
      @sets = sets
      @set_duration = set_duration
      @seconds = 0
    end

    # once a second, emit output.  See: TimerOutput
    def tick
      # noop
    end

    # are we there yet?
    def done?
      @current_set >= @sets
    end

    # react ot user input.  optional
    def input(key)
      # noop
    end

    private

    def next_set!
      @current_set += 1
      # todo: some sort of bell.
      @seconds = 0
    end

  end

  TimerOutput = Struct.new(:seconds, :current_set, :color, keyword_init: true)

  class Rest < TimerTemplate
    def initialize(sets:, set_duration: 60)
      super
      @seconds = nil # doesn't start until we have input
    end

    def input(key)
      @seconds ||= 0 if key
    end

    def tick
      if @seconds && @seconds >= @set_duration
        next_set!
        @seconds = nil
      end

      if @seconds
        @seconds += 1
        TimerOutput.new(seconds: @seconds, current_set: @current_set)
      else
        nil
      end
    end
  end

  class Emom < TimerTemplate
    def tick
      @seconds += 1
      next_set! if @seconds >= @set_duration

      TimerOutput.new(seconds: @seconds, current_set: @current_set)
    end
  end

  class Tabata < Emom
    def tick
      super.tap do |output|
        if output.seconds <= @set_duration / 3
          output.color = :red
        else
          output.color = :blue
        end
      end
    end
  end

  # hijacks another timer so we can change the output
  class Countdown
    def initialize(timer)
      @timer = timer
    end

    def tick
      @timer.tick.tap do |output|
        next unless output # if it's nil, just keep it nil

        output.seconds = @timer.set_duration - output.seconds
        output.current_set = @timer.sets - output.current_set + 1
      end
    end

    def done?
      @timer.done?
    end

    def input(*args)
      @timer.input(*args)
    end
  end

end

WodClock::Cli.start
