# frozen_string_literal: true

require 'rubygems'

class GemLoaderInstaller
  LOAD_ERROR = :load_error
  def initialize
    @gems_path = Gem.path.find { |path| path =~ /rbenv/ }
    @gems = Dir.entries("#{@gems_path}/gems") if @gems_path
  end

  def require_gems(dependencies)
    $g = dependencies.map do |gem_name|
      if @gems
        latest = @gems.grep(/#{gem_name}/).max
        $LOAD_PATH << [@gems_path, latest, 'lib'].join('/') if latest
      end

      begin
        require gem_name
      rescue LoadError
        `bundle exec gem install #{gem_name}`
        LOAD_ERROR
      end
    end
  end

  def require_gems!(dependencies)
    if require_gems(dependencies).index(LOAD_ERROR)
      puts 'Missing dependencies in pryrc.  Try again'
      # exit!
    end
  end
end

# GemLoaderInstaller.new.require_gems! %w[
#   pry-theme
#   pry-loudmouth
#   awesome_print
# ]
def capture_stdout(&block)
  silent_stdout = $stdout
  # $stdout = StringIO.new
  begin
    yield
  ensure
    $stdout = original_stdout
  end
end

begin
  silent_stdout { require "awesome_print" }
  AwesomePrint.pry!
rescue
end
Pry.config.theme = 'railscasts'
Pry.config.pager = false # fix bug in which less eats cursor

# define !!!! to kill process
Pry::Commands.block_command '!!!!', 'Die hard' do
  `kill -9 #{$PROCESS_ID}`
end

__END__
# still no byebug
# Pry::Commands.block_command 'bb', 'byebug' do
#   require 'pry-byebug'
#   Pry::Commands.rename_command("bb-next", "next")
#   Pry::Commands.rename_command("bb-break", "break")
#   Pry::Commands.rename_command("bb-step", "step")
# end

# vi:syntax=ruby
