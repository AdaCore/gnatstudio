#!/bin/sh
# Warning: this scripts must be run using gnu textutils (sort, uniq)

grep -- '-".*"' ../gvd/*.ad[sb] > skeleton.tmp
grep -- '-(".*"' ../gvd/*.ad[sb] >> skeleton.tmp
sed -e 's/.*\(".*"\).*/msgid  \1/g' < skeleton.tmp | sort | uniq | \
grep -v 'msgid  " *"$' | sed -e 's/\(".*"\)/\1\
msgstr \1\
/g' > skeleton.po.new
rm skeleton.tmp
