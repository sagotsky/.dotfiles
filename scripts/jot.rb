#!/usr/bin/env ruby

# Jot
#
# Cli notetaking app inspired by twitter.
#
# jot @bucket       

#bucket = ARGV[0] || '@jot'
#path = "#{ENV['HOME']}/.jot/#{bucket.sub('@', '')}" 
#lines = IO.readlines(path)

#if ARGV.length == 1
  #width = lines.length.to_s.length
  #lines.each_with_index do |line, index|
    #print format("%#{width}d| %s", index, line)
  #end 
#else 
  #note = ARGV[1..ARGV.length].join ' '
  #File.open(path, 'a').write note
#end

require 'pry'
require 'colorize'

module Jot
  class Jot
    DIR = ENV['HOME'] + '/.jot'

    attr_accessor :action
    attr_accessor :lines
    attr_accessor :file


    def initialize
      action = List if ARGV.length == 0

      while ARGV.length > 0 
        action = Show
        arg = ARGV.shift
        
        case arg
        when /^@/    #bucket
          file = arg.gsub /^@/, ''
          @file = "#{Jot::DIR}/#{file}"
          
        when /^#/     #range
          @tag = arg # wtf does tagging do?  

        when /^\d+/ #d[\d\-,]*/
          @lines = parse_range arg
          
        #cut off - necessary? might be implicit below
        when /^--$/
          action ||= Jot::Append
          break

        else 
          action = Action::ALL.find{ |klass| klass.match(arg) } 
          action ||= Append
          ARGV.unshift(arg)
          break
        end
      end 

      @action = action.new ARGV
    end

    def run
      # get before/after, then compare.  items in before but not in after were deleted.  vice versa added.  different index changed.

      #puts @action.run self
      lines = @action.run self
      #puts lines.join("\n")
      puts report lines, green: [1], blue: [2]
    end

    def parse_range(range)
      range.split(',').flat_map do |num|
        case num
        when /^\d+$/
          num.to_i
        when /^(\d+)\-(\d+)$/
          ($1..$2).map(&:to_i)
        end
      end
    end 

    def read(path)
      IO.readlines path
    end

    def prepend_index(lines)
      width = lines.length.to_s.length
      lines.each_with_index.map do |line, index|
        format("%#{width}d| %s", index, line.chomp)
      end
    end

    def update_file
      unless File.exists? file
        Dir.mkdir(Jot::DIR) unless Dir.exists? Jot::DIR
        File.new(file, 'w').close
      end

      lines =  IO.readlines(file)
      yield lines if block_given?
      File.open(file, 'w') do |fh|
        fh.puts lines
      end
    end

    # after running an action, print the lines in the file using hash to determine colors
    # all should use result hash, except deletions.  how to hanlde those?
    def report(text, color_lines = {red: [1,2,3]})
      color_lines.each do |color, lines|
        lines.each do |n|
          text[n] = text[n].colorize color
        end
      end
      text
    end

  end



  # A command line action.
  class Action
    ALL = []
    attr_accessor :args

    def self.inherited(subclass)
      ALL << subclass
    end

    # Returns true if this action should be used.
    #   Default: match name of class
    def self.match(arg)
      self.to_s.downcase == "jot::#{arg.downcase}"
    end

    def initialize(args)
      @args = args
    end

    # Runs the action defined by this class.  Must be overridden.
    def run(jot)
      raise "Action registered but undefined.  #{self.class} missing run method."
    end 
  end

  class Show < Action
    def run(jot)
      lines = jot.prepend_index(jot.read(jot.file))
      if jot.lines 
        lines = lines.each_with_index.reduce([]) do |ret, (line, i)|
          ret << line if jot.lines.include? i
          ret
        end
      end
      lines
    end
  end

  class List < Action
    def run(jot)
      Dir["#{Jot::DIR}/*"].map do |path|
        "@#{File.basename(path)}"
      end.join(" ")
    end
  end

  class Shift < Action
    def help
      'Move line(s) vertically.'
    end

    def initialize(args)
      direction, count = *args
      if direction == 'shift' && count.nil?
        raise "Missing argument.  shift needs to know how many lines to adjust by."
      end

      count ||= 1
      count = -1 * count.to_i if direction == 'up'

      super([direction, count])
    end

    def self.match(arg)
      super || ['up', 'down'].include?(arg)
    end

    def run(jot)
      return unless jot.lines

      direction, count = *args
      lines = jot.lines.sort
      lines = lines.reverse if count > 0

      jot.update_file do |text|
        lines.each do |i|
          line = text.delete_at i
          text.insert i+count, line
        end
      end
    end
  end

  class Move < Action
    # move needs 2 whole files and the range
    def help
      'Move line(s) to new bucket'
    end
  end

  class Append < Action
    def run(jot)
      jot.update_file do |text|
        text.push(args.join(' '))
      end
    end
  end
end 


j = Jot::Jot.new
j.run

=begin

Todo
====

- Contemplate the irony of an inline todo list inside a todo script
- Is the arg order correct?  jot @bucket del 1,2,3,4-10 #feels more natural
- Range aliases?  First is silly, but last might be nice.  Or is that what tail is for?
- Is child of really needed?
- Movement
  - jot @a 1 to @b    ## is this copy or move?  
      bucket, range, cmd, arg
- what is the point of tags?  honestly?
  - could they also be a range? 

  VSO jot @bucket up 1 1-4 up 1      # sounds more natural

  SVO jot @bucket 1-4 up 1      # follows naturlay progression of previewing a selection then changing it.
                                # already coded
                                # takes args more cleanly if range is explicit.  

- flip, del, edit, in-de dent
- random?  should it pop or just print?

- Color
  Three things are required:
    hue
    lines
    text (so that delete can use the initial text instead of processed
  Most of the time only  hue will be provided.
=end
