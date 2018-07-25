# https://github.com/pry/pry/wiki/FAQ#wiki-awesome_print
require 'rubygems'

Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
end if defined?(Bundler)
$:.uniq!


require 'awesome_print'
require 'pry-theme'
require 'pry-loudmouth'

AwesomePrint.pry!
Pry.config.theme = 'railscasts'
Pry.config.pager = false # fix bug in which less eats cursor

# define !!!! to kill process
Pry::Commands.block_command '!!!!', 'Die hard' do
  `kill -9 #{$PROCESS_ID}`
end
