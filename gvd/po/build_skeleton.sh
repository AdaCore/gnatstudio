#!/bin/sh
# Warning: this script must be run using the C locale, otherwise sort might
# behave differently.

grep -- '-".*"' ../gvd/*.ad[sb] > skeleton.tmp
grep -- '-(".*"' ../gvd/*.ad[sb] >> skeleton.tmp
sed -e 's/.*\(".*"\).*/msgid  \1/g' < skeleton.tmp | sort | uniq | \
grep -v 'msgid  " *"$' | sed -e 's/\(".*"\)/\1\
msgstr \1\
/g' > skeleton.po.new
rm skeleton.tmp
