#!/usr/bin/env ruby

require 'thor'
require 'yaml'
require 'pry'
require 'open3'

class DbMint < Thor
  desc "reset", "Resets db on dev env"
  def reset
    Sh.rake_db_drop
    Sh.create_db_from_template
  end 

  desc "mint_setup", "Instructions for setting up db mint for the first time"
  def mint_setup
    puts <<-EOF
      # Create a new environment
      cp config/environments/development.rb config/environments/mint.rb

      # Configure a DB for your new env, changing your user, host, etc as needed
      $EDITOR config/database.yml
      mint: 
        adapter: postgresql
        database: mint_db
        host: localhost
        username: postgres

    EOF
  end

  desc "mint_migrate", "Runs pending migrations on mint"
  def mint_migrate
    check_current_branch!
    Sh.db_migrate(:mint)
  end

  desc "mint_rebuild FILE", "Rebuilds mint db from FILE"
  def mint_rebuild(file)
    check_current_branch!

    Sh.rake_db_drop(:mint)
    Sh.rake_db_create(:mint)
    Sh.restore_db(file, :mint) 
    Sh.db_migrate(:mint)
    Sh.rake_plm_users_create_all(:mint)
  end

  desc "mint_download_and_rebuild", "Downloads a db and rebuilds your mint db environment from it"
  def mint_download_and_rebuild
    mint_rebuild(do_download_db)
  end

  desc "download_db", "Downloads a recent copy of the sanitized db"
  def download_db
    puts do_download_db
  end

  private

  def check_current_branch!
    raise "Switch to current to run migrations on your mint db" unless current?
  end 

  def do_download_db
    destination = "/tmp/developer_db.#{timestamp}.pgdump"

    begin
      puts "#{File.exists?(destination) ? 'Resuming' : 'Starting'} download: #{destination}"
      Rsync.new('developer@10.208.0.207:developer_db.pgdump', destination).run3
    rescue Exception => e
      puts "Exception occurred: #{e}"
      print "Would you like to resume download? (Y/n) "
      raise e if STDIN.gets.chomp == 'n'
      retry 
    end

    destination
  end

  def current?
    Sh.git_branch == 'current'
  end

  def timestamp
    Time.now.strftime('%y-%m-%d')
  end
end

class Sh
  class << self
    def git_branch
      `git rev-parse --abbrev-ref HEAD`.strip!
    end

    def db_migrate(env = :development)
      pg_config(env)
      `bundle exec rake db:migrate RAILS_ENV=#{env}`
    end 

    def rake_db_drop(env = :development)
      `bundle exec rake db:drop RAILS_ENV=#{env}`
    end 

    def rake_db_create(env = :development)
      `bundle exec rake db:create RAILS_ENV=#{env}`
    end 

    def rake_plm_users_create_all(env = :development)
      `bundle exec rake plm:users:create_all`
    end

    def create_db_from_template(env = :development)
      `createdb -U #{pg_user(env)} -T #{pg_database(:mint)} #{pg_database(env)}`
    end 

    def restore_db(pg_dump, env = :development)
      `pg_restore -U #{pg_user(env)} -d #{pg_database(env)} -j4 -v -c #{pg_dump}`
    end 

    private 

    def pg_user(env)
      pg_config(env).fetch 'username'
    end

    def pg_database(env)
      pg_config(env).fetch 'database'
    end

    def pg_config(env)
      @pg_config ||= {}
      @pg_config[env] ||= YAML.load_file("./config/database.yml").fetch(env.to_s)
    end 
  end 
end

class Rsync
  TIMEOUT = 2

  def initialize(target, destination)
    @target = target.strip
    @destination = destination.strip
  end

  def run
    # popen3 would be nice for explicitly getting stdout/stderr, but one of the pipes tends to block on eof?/closed?/gets.  IO.select?
    Open3.popen2e(command) do |_, stds|
      until stds.eof? do
        line = stds.gets
        puts line ? line : '.' if line 
        sleep 1
      end
    end
  end

  def run3
    Open3.popen3(command) do |stdin, stdout, stderr|
      stdin.close_write
      until stdout.eof? && stderr.eof? do
        IO.select([stdout, stderr]).tap do |ready, _, _|
          puts ready.map &:gets 
        end
      end
    end 
  end

  private

  def command
    "rsync -P --timeout #{TIMEOUT} --rsh=ssh #{@target} #{@destination}"
  end
end

DbMint.start ARGV
