#!/usr/bin/env bash
set -xEeuo pipefail

    xclip -selection clipboard -o \
    | perl -pe 's$</?ac:structured-macro[^>]*>$$g; s%  <ac:plain-text-body><\!\[CDATA\[%<pre>%g; s%\]\]></ac:plain-text-body>%</pre>%g;' \
    | pandoc -r html -w gfm \
    | perl -ne 's/ *$//;              s/^(#+) \**([^*]+)\**/$1 $2/;           /^\*\*$/ || print;' \
    > $1.md
