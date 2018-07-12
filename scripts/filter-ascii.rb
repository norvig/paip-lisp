#!/usr/bin/env ruby

# switch non-ascii characters to ascii equivalents or html entities

# references:
# https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
# https://dev.w3.org/html5/html-author/charref

# e.g. filter-ascii.rb -n docs/chapter?.md

# require 'byebug'

odd_chars = []
substitutions = {
  "—" => "-", "−" => "-", "–" => "-",
  "“" => '"', "”" => '"', '˜' => '"', '″' => '"',
  '’' => "'", "‘" => "'", "′" => "'",
  "⇒" => "=>", "⇐" => "<=",
  " " => "  ", # non-breaking spaces
  "•" => "",
  "…" => "...",

  # typos?
  "Κ" => "K",
  "Ζ" => "Z",
  "â" => "a",
  "©" => "@",
  "ô" => "o",

  # accents
  "à" => "&agrave;",
  "ç" => "&ccedil;",
  "è" => "&egrave;",
  "é" => "&eacute;",
  "ï" => "&iuml;",
  "ö" => "&ouml;",

  # greek
  "∝" => "&alpha;",
  "ε" => "&epsilon;",
  "λ" => "&lambda;", "Λ" => "&Lambda;",
  "π" => "&pi;",
  "∑" => "&Sigma;",
  "φ" => "&phi;", "ϕ" => "&Phi;",
  "χ" => "&Chi;",

  # logic symbols
  "∃" => "&exist;",
  "∧" => "&and;",
  "∀" => "&forall;",
  "¬" => "&not;",
  "⊃" => "&Superset;",
  "→" => "&rarr;",
  "∨" => "&or;",

  # yet more symbols!
  "×" => "x",
  "≡" => "=", # maybe?
  "∫" => "&int;",
  "≠" => "&ne;",
  "≈" => "&asymp;",
  "≤" => "&le;",
  "√" => "&radic;",
  "≫" => ">>", "»" => ">>",
  "⋯" => "...",
  "±" => "&plusmn;",

  # unclear, refer to book pdf
  # "ߣ" => "", # right-to-left?
  # î
  # °
  # ®
}

only_print_non_ascii_lines = false
if ARGV[0] == "-n"
  # only print non-ascii lines
  only_print_non_ascii_lines = true
  ARGV.shift
end

while line = ARGF.gets
  unless line.ascii_only? # /[\u0080-\u00ff]/
    line.sub!('Ø̸', "&Oslash;") # two chars, doesn't fit below
    line.each_char do |char|
      unless char.ascii_only?
        # byebug if char == "’"
        subst = substitutions.fetch(char, "!!!(char) #{char}")
        line.sub!(char, subst)
        # byebug
        # puts "char: #{char}"
        unless odd_chars.include?(char)
          odd_chars << char
        end
      end
    end
    # byebug
    if only_print_non_ascii_lines && !line.ascii_only?
      puts "non-ascii line: #{line}"
    end
  end # ascii only

  puts line unless only_print_non_ascii_lines
end

# puts "non-ascii odd_chars: #{odd_chars.join("\t")}"