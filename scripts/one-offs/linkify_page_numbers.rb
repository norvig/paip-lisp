#!/usr/bin/env ruby

require "byebug"

in_path = "PAIP.txt"
out_path = "PAIP.txt.linkify"

def parse_chapter_file(line)
  chapter_match = line.match(/^## (.+)$/)

  return nil unless chapter_match

  chapter = chapter_match[1].downcase
  chapter.gsub!(" ", "")
  chapter
end

page_to_chapters = {}
chapter = 0
in_file = File.open(in_path, "r").read
out_file = File.open(out_path, "w")

# index the pages within the chapters
in_file.each_line do |line|
  line.chomp!
  if parse_chapter_file(line)
    chapter = parse_chapter_file(line)
    next
  end

  page_id = line.match(/(<a id='page-(.+?)'><\/a>)/)
  if page_id
    page = page_id[2]
    page_to_chapters[page] = chapter
    next
  end
end

# add links to page references
in_file.each_line do |line|
  line = line.chomp
  page_match = line.match(/page (\d+)/i)
  if page_match
    # not checking if already linked yet...
    num = page_match[1]
    chap = page_to_chapters[num]
    line.gsub!(/page #{num}/, "[page #{num}](#{chap}.md#page-#{num})")
  end
  out_file.puts line
end