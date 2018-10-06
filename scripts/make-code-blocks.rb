#!/usr/bin/env ruby

require 'byebug'

debug = false
# if true, won't emit non-code lines

def code?(str)
  return false unless str =~ /`/
  return true if str[0] == '`' && str[-1] == '`'

  false
end

def convert_code_line(str)
  str.gsub(/^`/, '').gsub(/`$/, '')
end

def convert_code_block(arr)
  arr.each_with_index do |line, idx|
    arr[idx] = convert_code_line(line)
  end

  arr.unshift '```lisp'
  arr << '```'

  arr
end

lines = []

warn 'reading file...'
while (line = ARGF.gets)
  lines << line.chomp
end

warn 'parsing file...'
index = 0
while index < lines.length
  STDERR.printf '.'
  line = lines[index]
  unless code?(line)
    puts line unless debug
    # print line?
    # we might just modify in place
    index += 1
    next
  end

  code_block = []

  while index < lines.length
    line = lines[index]
    if line == ''
      index += 1
      next
    end
    if code?(line)
      code_block << line
      index += 1
    else
      index -= 1
      break
    end
  end

  block = convert_code_block(code_block)

  # change in place? or just print?
  puts block.join "\n"

end

warn "\n"
