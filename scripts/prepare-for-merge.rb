#!/usr/bin/env ruby

require "byebug"
chap_path = ARGV.first || "docs/chapter10.md"
out_path = "#{chap_path}.merge"

chap_file = File.open(chap_path, "r")
out_file = File.open(out_path, "w")

until chap_file.eof?
  line = chap_file.readline
  line.gsub!(/ {docsify-ignore}$/, "")
  line.gsub!(/^#/, "##") # promote section to header
  out_file.puts "\f" if line.match(/<a id=.page-/)
  out_file.puts line
end


# copy text file, up to given chapter

# merge in chapter

# copy rest of text file

# while line = chap_file.readline
#   line.chomp!
#   line.gsub!(/^#/, "##")
#   # byebug
#   out_file.puts line
# end