#!/bin/bash
# Build the manual: generate the reference chapter, run the code blocks (which also
# makes the pictures), then write html/ (a page per chapter) and cl-cairoplot.pdf.
#   docs/build.sh            everything
#   docs/build.sh --no-run   only Pandoc, with the pictures as they are
# The PDF uses Typst if it is installed, otherwise LuaLaTeX.

set -e
cd "$(dirname "$0")"
pages=(src/*.md)

if [ "$1" != "--no-run" ]; then
    echo "Generating the reference..."
    sbcl --non-interactive --load tools/make-reference.lisp 2>&1 | grep -i 'error\|wrote\|undocumented' || true
    echo "Running the code blocks..."
    sbcl --non-interactive --load tools/run-docs.lisp 2>&1 | grep -v '^;' | grep -v '^$' \
        | grep -v 'SBCL\|public domain\|BSD-style\|distribution for' || true
fi

# Code lines must fit the page: about 76 characters in the PDF and the HTML
awk 'FNR == 1 { inb = 0 } /^```/ { inb = !inb; next }
     inb && length($0) > 76 { printf "Code line too wide (%d): %s:%d\n", length($0), FILENAME, FNR }' \
    "${pages[@]}"

echo "HTML..."
rm -rf html && mkdir -p html
pandoc --metadata-file=metadata.yaml "${pages[@]}" --lua-filter=filters/images.lua --to chunkedhtml --split-level=1 --toc --standalone \
       --css style.css --syntax-highlighting=pygments --resource-path=. -o html.zip
(cd html && unzip -q -o ../html.zip && rm ../html.zip)
# The zip has its own top directory
inner=$(find html -mindepth 1 -maxdepth 1 -type d | head -1)
[ -n "$inner" ] && [ "$(ls html | wc -l)" -eq 1 ] && mv "$inner"/* html/ && rmdir "$inner"
cp style.css html/
mkdir -p html/images && cp images/*.png html/images/

echo "PDF..."
if command -v typst >/dev/null; then engine=typst; else engine=lualatex; fi
pandoc --metadata-file=metadata.yaml "${pages[@]}" --lua-filter=filters/images.lua --dpi=96 --pdf-engine=$engine --syntax-highlighting=tango --resource-path=. \
       -o cl-cairoplot.pdf
echo "Done: docs/html/index.html and docs/cl-cairoplot.pdf ($engine)"
