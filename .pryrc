# https://github.com/pry/pry/wiki/FAQ#wiki-awesome_print
require 'rubygems'

Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
end if defined?(Bundler)
$:.uniq!


begin
  Pry.config.prompt = proc { |obj, nest_level, _| "\n#{obj}:#{nest_level}> \a\n$ " }
  Pry.config.print = proc { |output, value| output.puts ((value.respond_to?(:ai) ? value.ai : nil) || value)  }
rescue LoadError 
end

Pry.hooks.add_hook :before_session, 'set_title' do
  Process.setproctitle 'pry'
end

Pry.hooks.add_hook :after_session, 'set_title' do
  Process.setproctitle 'ruby'
end

require 'pry-theme'
Pry.config.theme = 'railscasts' # does awesome print use this theme?
