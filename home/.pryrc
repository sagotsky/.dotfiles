# https://github.com/pry/pry/wiki/FAQ#wiki-awesome_print
require 'rubygems'

Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
end if defined?(Bundler)
$:.uniq!


begin
  Pry.config.print = proc { |output, value| output.puts ((value.respond_to?(:ai) ? value.ai : nil) || value)  }
rescue LoadError
end

require 'pry-theme'
require 'pry-loudmouth'
Pry.config.theme = 'railscasts' # does awesome print use this theme?
