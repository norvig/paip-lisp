-- fix pandoc links
-- called from make-epub.sh

function Link (link)
  -- fix links between chapters, to anchors
  -- e.g. turn chapter15.md#p514 into #p514
  -- via https://stackoverflow.com/questions/48169995/pandoc-how-to-link-to-a-section-in-another-markdown-file
  link.target = link.target:gsub('.+%.md%#(.+)', '#%1')

  -- e.g. turn chapter15.md into #chapter15_top
  link.target = link.target:gsub('(.+)%.md$', '#%1_top')

  return link
end
