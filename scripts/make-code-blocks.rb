#!/usr/bin/env ruby

require 'byebug'

debug = false
# if true, won't emit non-code lines

def code?(str)
  return false unless str =~ /`/

  # allow leading spaces
  return false unless str.match('^( | )*`.*`$')

  # let's make sure there aren't more ` in the middle
  # return false if str[1 .. -2] =~ /`/

  true
end

def convert_code_line(str)
  # only doing first and last
  # `first segment` something `second segment` gets highlighted
  # won't eat ` within code fragment
  str.sub(/`/, '').gsub(/`$/, '')
end

def convert_code_block(arr)
  puts '```lisp'
  arr.each do |line|
    puts convert_code_line(line)
  end
  puts '```'
end

lines = []

warn 'reading file...'
while (line = ARGF.gets)
  lines << line.chomp
end

warn 'parsing file...'

while (line = lines.shift)
  STDERR.printf '.'
  unless code?(line)
    puts line unless debug
    next
  end

  code_block = [line]

  while (line = lines.shift)
    next if line == ''

    if code?(line)
      code_block << line
    else
      lines.unshift line
      break
    end
  end

  convert_code_block(code_block)
  puts

end

warn "\n"
