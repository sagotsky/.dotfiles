#!/usr/bin/env ruby

# List all keys stored in memcache.
# Credit to Graham King at http://www.darkcoding.net/software/memcached-list-all-keys/ for the original article on how to get the data from memcache in the first place.

require 'net/telnet'

headings = %w(id expires bytes cache_key)
rows = []

localhost = Net::Telnet::new("Host" => "localhost", "Port" => 11211, "Timeout" => 3)
matches = localhost.cmd("String" => "stats items", "Match" => /^END/).scan(/STAT items:(\d+):number (\d+)/)

slabs = matches.inject([]) { |items, item| items << Hash[*['id','items'].zip(item).flatten]; items }

longest_key_len = 0
slabs.each do |slab|
  localhost.cmd("String" => "stats cachedump #{slab['id']} #{slab['items']}", "Match" => /^END/) do |c|
    matches = c.scan(/^ITEM (.+?) \[(\d+) b; (\d+) s\]$/).each do |key_data|
      cache_key, bytes, expires_time = key_data
      rows << [slab['id'], Time.at(expires_time.to_i), bytes, cache_key]
      longest_key_len = [longest_key_len,cache_key.length].max
    end
  end
end

row_format = %Q(|%8s | %28s | %12s | %-#{longest_key_len}s |)
puts row_format%headings
rows.each{|row| puts row_format%row}

localhost.close
