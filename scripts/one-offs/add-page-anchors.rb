#!/usr/bin/env ruby

in_path = "PAIP.txt"
out_path = "PAIP.txt.markers"

chapter = 0
in_file = File.open(in_path, "r").read
out_file = File.open(out_path, "w")
page_num = -9 # experimentally determined...

in_file.each_line do |line|
  line.chomp!
  out_file.puts line
  if line.match(/\f/)
    if page_num >= 3
      out_file.print "<a id='page-#{page_num}'></a>"
    end
    page_num += 1
    # skipping pages. these are removed section headers.
    page_num = 109 if page_num == 107
    page_num = 265 if page_num == 262
    page_num = 509 if page_num == 507
    page_num = 753 if page_num == 751
  end
end
