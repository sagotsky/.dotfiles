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
    MODES = [:show, :append, :list, :shift, :remove]
    DIR = ENV['HOME'] + '/.jot'

    attr_accessor :opts

    def initialize
      self.opts = Options.new ARGV
    end

    def run
      p opts
      self.send(opts.mode) if MODES.include?(opts.mode)
    end

    def file_get_lines
      lines = IO.readlines(opts.file)
      width = lines.length.to_s.length

      lines.each_with_index.map do |line, index|
        format("%#{width}d| %s", index, line.chomp)
      end
    end

    def update_file
      lines = IO.readlines(opts.file)
      yield lines if block_given?
      File.open(opts.file, 'w') do |fh|
        fh.puts lines
      end
    end


    # *dent
    # flip

    # move/copy
    def shift # up and down map to this
      
    end


    def remove
      update_file do |lines|
        # go backwards to preserve indices
        opts.lines.sort.reverse.each do |n|
          lines.delete_at n 
        end
      end
      self.send(:show, true)
    end

    def show(all=false)
      puts opts.file
      file_get_lines.each_with_index do |line, i|
        puts line if opts.lines.nil? || opts.lines.include?(i) || all
      end
    end

    def append
      update_file do |text|
        text.push opts.text
      end
      self.send(:show, true)
    end

    def list
      puts(Dir["#{DIR}/*"].map do |path|
        "@#{File.basename(path)}"
      end.join(" "))
    end

    def edit

    end

  end


  class Options
    attr_accessor :file
    attr_accessor :lines
    attr_accessor :text
    attr_accessor :mode

    def initialize(args = [])
      self.mode = :list if args.length == 0

      while args.length > 0
        self.mode = :show
        arg = args.shift
        
        case arg

        when /^@/
          file = arg.gsub /^@/, ''
          self.file = "#{Jot::DIR}/#{file}"
          
        when /^#/
          p 'tagging'
          self.tag = arg

        when /^\d+/ #d[\d\-,]*/
          self.lines = parse_range arg
          self.mode = (args.length > 0) ? args.shift.to_sym : self.mode
          
          # this is where we start losing things.  range, op is great.  what else?  @jot 1-3 down 2;  taking another arg is fine.  what about indented children?
          args = []

        when /^--$/
          self.text = args.shift(args.length).join(' ')
          self.mode = :append

        else 
          # is this still sensible?  preshifting arg works well until here.
          self.text = "#{arg} #{args.shift(args.length).join(' ')}"
          self.mode = :append
        end
      end 
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

        
    def to_s(arg=nil)
      instance_variables.map { |i| "#{i}: #{instance_variable_get i}" }.to_s
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
- What is the point of tags?  Honestly?
  - Could they also be a range? 


=end
