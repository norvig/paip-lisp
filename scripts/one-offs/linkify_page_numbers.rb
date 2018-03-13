#!/usr/bin/env ruby

require "byebug"

in_path = "PAIP.txt"
out_path = "PAIP.txt.linkify"

chapter = 0
in_file = File.open(in_path, "r").read
out_file = File.open(out_path, "w")

in_file.each_line do |line|
  line = line.chomp
  page_match = line.match(/page (\d+)/i)
  if page_match
    # not checking if already linked yet...
    num = page_match[1]
    # byebug
    # puts "pausing..."
    line.gsub!(/page \d+/, "[page #{num}](#page-#{num})")
  end
  out_file.puts line
end