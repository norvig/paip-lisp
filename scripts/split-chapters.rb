#!/usr/bin/env ruby

# require "byebug"
in_path = "PAIP.txt"
out_path = "docs"

in_file = File.open(in_path, "r").read
out_file = ""

in_file.each_line do |line|
  line.chomp!

  chapter_match = line.match(/^## (.+)$/)
  if chapter_match
    chapter = chapter_match[1].downcase
    chapter.gsub!(" ", "")
    line.gsub!(/^##/, "#") # promote sections a level
    out_file = File.open("#{out_path}/#{chapter}.md", "w")
    out_file.puts "#{line} {docsify-ignore}"
    next
  end

  if line.match(/\f/)
    next # eat the formfeed
  end

  # eat anything after the page number marker
  page_id = line.match(/(<a id='page-.+?'><\/a>)/)
  if page_id
    # byebug
    out_file.puts page_id[1]
    next
  end

  out_file.puts line
end

