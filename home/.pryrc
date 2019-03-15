# https://github.com/pry/pry/wiki/FAQ#wiki-awesome_print
require 'rubygems'

Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
end if defined?(Bundler)
$:.uniq!

require_gems = %w[
  awesome_print
  byebug
  pry-byebug
  pry-theme
  pry-loudmouth
]

if Object.const_defined?(:Bundler)
  gems_path = "#{Bundler.bundle_path}/gems"
  gems = Dir.entries("#{Bundler.bundle_path}/gems")
  require_gems.each do |gem_name|
    latest = gems.grep(/#{gem_name}/).max
    $LOAD_PATH << [gems_path, latest, 'lib'].join('/') if latest
    require gem_name # try require_without_bootsnap if this fails
  end
else
  require_gems.each { |gem_name| require gem_name }
end

AwesomePrint.pry!
Pry.config.theme = 'railscasts'
Pry.config.pager = false # fix bug in which less eats cursor

# define !!!! to kill process
Pry::Commands.block_command '!!!!', 'Die hard' do
  `kill -9 #{$PROCESS_ID}`
end
# still no byebug
# Pry::Commands.block_command 'bb', 'byebug' do
#   require 'pry-byebug'
#   Pry::Commands.rename_command("bb-next", "next")
#   Pry::Commands.rename_command("bb-break", "break")
#   Pry::Commands.rename_command("bb-step", "step")
# end

# vi:syntax=ruby
