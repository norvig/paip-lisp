pandoc \
  -o PAIP-alpha.epub \
  --lua-filter scripts/fix-links-single-file.lua \
  meta/title.txt \
  docs/{frontmatter.md,about.md,preface.md,chapter?.md,chapter??.md,appendix.md,bibliography.md} \
  --epub-cover-image=docs/_media/paip-cover.gif \
  --css=meta/stylesheet.css \
  --epub-metadata=meta/metadata.xml \
  --table-of-contents
