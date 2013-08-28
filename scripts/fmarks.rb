#!/usr/bin/ruby

# Uses dmenu to select bookmarks from firefox sqlite file
# Will replace wildcards for keyword searches
# All args are passed to demnu

require "rubygems"
require "sqlite3"

dmenu_opts = ARGV.count ? ARGV.join(' ') : '-l 20'
db = SQLite3::Database.new "/home/sagotsky/.mozilla/firefox/zvczrnum.default/places.sqlite"
db.results_as_hash = true

# get the bookmarks
rows = db.execute <<-SQL
  SELECT url, mb.title, mk.keyword
  FROM moz_bookmarks mb 
  LEFT JOIN moz_places mp on mb.fk = mp.id
  LEFT JOIN moz_keywords mk on mk.id = mb.keyword_id
  WHERE mb.type = 1
  ORDER BY mb.position ASC
SQL

dmenu = IO.popen('dmenu ' + dmenu_opts, 'r+') 
bookmarks = Hash.new
keywords = Hash.new

# send them to dmenu, nonblocking
rows.each do |row|
  if row['keyword']
    keywords[row['keyword']] = row['url']
  end

  keyword = (row['keyword']) ? "#{row['keyword']} | "  : ''
  title = (row['title']) ? row['title'] : row['url']
  display = "#{keyword}#{title}\n"
  bookmarks[display] = row
  dmenu.write display
end

# get result from dmenu
dmenu.close_write
selected = dmenu.gets

# do keyword replacement or just get hte url
if selected && keywords.key?(selected.split(' ')[0])
  args = selected.split(' ')
  keyword = args[0]
  args.shift
  url = keywords[keyword]
  url.gsub!('%s', args.join(' '))
else
  url = selected ? bookmarks[selected]['url'] : ''
end

print url

