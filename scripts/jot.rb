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
          self.file = "#{Jot::DIR}/#{file}"
          
        when /^#/     #range
          p 'tagging'
          self.tag = arg

        when /^\d+/ #d[\d\-,]*/
          self.lines = parse_range arg
          #self.mode = (ARGV.length > 0) ? ARGV.shift.to_sym : self.mode
          #mode is show or it'll be set
          
        #cut off - necessary? might be implicit below
        when /^--$/
          #ARGV.shift
          action ||= Jot::Append
          break

        else 
          action = Action::ALL.find{ |klass| klass.match(arg) } 
          action ||= Append
          ARGV.unshift(arg)
          break
        end
      end 

      self.action = action.new ARGV
    end

    def run
      puts self.action.run self
    end

    def file_format_lines
      # booo!  this is only good for printing.  why is width hardcoded?
      lines = IO.readlines(file)
      width = lines.length.to_s.length

      lines.each_with_index.map do |line, index|
        format("%#{width}d| %s", index, line.chomp)
      end
    end

    def update_file
      lines = IO.readlines(file)
      yield lines if block_given?
      File.open(file, 'w') do |fh|
        fh.puts lines
      end
    end

  end


  class Options
    attr_accessor :file
    attr_accessor :lines
    attr_accessor :text
    attr_accessor :action

    
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

        
    def to_s(arg=nil)
      instance_variables.map { |i| "#{i}: #{instance_variable_get i}" }.to_s
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
      self.args = args
    end

    # Runs the action defined by this class.  Must be overridden.
    def run(jot)
      raise 'Action registered but undefined.'
    end 
  end

  class Show < Action
    def run(jot)
      jot.file_format_lines.join("\n")
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

    def self.match(arg)
      super || ['up', 'down'].include?(arg)
    end

    def action(jot)

    end

    # need to know: range, movement arg
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
      # self.send show?
    end
  end
end 


j = Jot::Jot.new
#p j.opts
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
=end
