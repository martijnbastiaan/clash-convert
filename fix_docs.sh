#!/bin/bash
set -e

docs='dist-newstyle/build/*/*/*/doc/html/*'
find $docs -type f -name '*.html' -exec ./fix_doc.py {} \;
