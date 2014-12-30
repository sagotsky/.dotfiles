#!/usr/bin/env ruby

# Cli for querying and launching steam games.  Maybe more eventually.
require 'thor'
require 'steam_codec'
require 'pry'

class Shteam < Thor
  class_option :steam_home, aliases: :s, desc: "Path to your steam files", default: "~/.steam"

  desc "ls", "List your installed steam apps"
  #option :l, type: :boolean, desc: "List detailed info"
  #option :h, type: :boolean, desc: "Use human readable numbers"
  def ls
    show_fields = options[:l] ? %w[name appid stateflags sizeondisk] : %w[name]
    output = @apps.map do |id, app|
      app.select{ |field| show_fields.include?(field) }
    end

    puts output.map{ |o| o.values.join("\t")}.join("\n")
  end


  desc "exec [APP]", "Launch a game by id or name"
  def exec(app_id_or_name)
    app = @apps.values.find do |a| 
      a['appid'] == app_id_or_name.to_i
    end 

    app ||= @apps.values.find do |a|
      a['name'].downcase.match app_id_or_name.downcase
    end

    if app
      puts app['name']
      id = app['appid']
      %x(steam steam://rungameid/#{id} )
    else 
      STDERR.puts "No steam app matches '#{app_id_or_name}'"
      exit 1
    end
  end

  # TODO shim command
  # creates a shim file
  # symlink to shim to an app
  # symlink all apps.
  # not sure how to do all this in one easy to use command
  #desc "shim [APP]", "Create a shortcut to launch a steam app by id or name"
  #option :path,  aliases: :p, desc: "Directory to place shortcut in", default: "Same as Shteam.rb"

  #def shim(app)
  #end 


  def initialize(*args)
    super

    @apps = {}
    fields = %w[AppID name StateFlags Universe installdir LastUpdated
      UpdateResult SizeOnDisk buildid LastOwner BytesToDownload BytesDownloaded
      AutoUpdateBehavior AllowOtherDownloadsWhileRunning UserConfig
      MountedDepots
    ]
    
    options[:steam_home].gsub!('~', ENV['HOME'])
    Dir.glob("#{options[:steam_home]}/steam/SteamApps/*.acf") do |file|
      acf = SteamCodec::ACF::loadFromFile(File.open(file))
      @apps[acf.get('AppID')] = fields.map do |field|
        [field.downcase, acf.get(field)]
      end.to_h
    end
  end
end

Shteam.start ARGV
