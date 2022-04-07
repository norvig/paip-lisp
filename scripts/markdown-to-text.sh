#!/bin/bash

# turning the markdown into plaintext, for OCR comparisons
# results: sentence per line, for nicer diffs

OUTPUT=ocr/PAIP.txt
echo > $OUTPUT
for i in frontmatter.md preface.md $(seq -f chapter%g.md 1 25) appendix.md bibliography.md; do
  echo "converting $i..."
  pandoc - --wrap=preserve -t plain \
    < docs/$i >> $OUTPUT
  echo "---" >> $OUTPUT
done
