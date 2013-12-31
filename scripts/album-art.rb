#!/usr/bin/env ruby

# Get itunes cover art.  Try for 1200x1200, then 600x600, then 100x100.

require 'open-uri'
require 'json'
require 'text'

resolutions = ['1200', '600', '100']
artist = URI::encode(ARGV[0])
album =  URI::encode(ARGV[1])
url = "https://itunes.apple.com/search?term=#{album}&media=music&entity=album&attribute=albumTerm"

files = []
json = JSON.parse(open(url).read)
if json['resultCount'] > 0
  json['results'].each do |result|
    if (artist.upcase == URI::encode(result['artistName']).upcase )
      files.push result['artworkUrl100']
    end
  end
else
  # No exact match for album.  Try artist instead, then filter closest album
  url = "https://itunes.apple.com/search?term=#{artist}&media=music&entity=album&attribute=artistTerm"
  json = JSON.parse(open(url).read)
  all_albums = {}
  json['results'].each do |result|
    if album.upcase.include? result['collectionName'].upcase
      dist =  Text::Levenshtein.distance(result['collectionName'], album)
      all_albums[dist] = result
    end
  end

  if all_albums.length > 0
    match = all_albums.keys.sort.first
    files = [all_albums[match]['artworkUrl100']]
  end
end


resolutions.each do |res|

  files.map{|f| f.sub('100x100', "#{res}x#{res}")}.each do |file|
    url = URI.parse(file)
    req = Net::HTTP.new(url.host, url.port)
    res = req.request_head(url.path)
    if (res.code == '200')
      print file
      exit
    end
  end
end
