#!/usr/bin/ruby

require "rubygems"
require "sqlite3"

db = SQLite3::Database.new "/home/sagotsky/.mozilla/firefox/zvczrnum.default/places.sqlite"
db.results_as_hash = true

menu = (ARGV[0]) ? ARGV[0] : 'Bookmarks Menu'

p menu + ""
rows = db.execute <<-SQL
  SELECT url,mb.title,type
  FROM moz_bookmarks mb LEFT JOIN moz_places mp on mb.fk = mp.id
  WHERE parent = (
    SELECT id FROM moz_bookmarks WHERE title = '#{menu}'
  )
  ORDER BY mb.position ASC
SQL

rows.each do |row|
  case row['type']
  when 1
    # Folder
  when 2
    # Link
  when 3
    # Separator
  p row['title']
end
