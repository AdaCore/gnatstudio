#!/bin/sh
# Warning: this script must be run using the C locale, otherwise sort might
# behave differently.

dir="aunit \
	browsers \
	builder \
	common \
	gbuilder \
	glide \
	gvd \
	kernel \
	metrics \
	prj_editor \
	src_editor \
	syntax \
	vcs \
	vdiff \
	vsearch \
	widgets"

rm -f skeleton.tmp

for d in $dir; do
   grep -- '-".*"' ../$d/*/*.ad[sb] >> skeleton.tmp
   grep -- '-(".*"' ../$d/*/*.ad[sb] >> skeleton.tmp
done

sed -e 's/.*\(".*"\).*/msgid  \1/g' < skeleton.tmp | sort | uniq | \
grep -v 'msgid  " *"$' | sed -e 's/\(".*"\)/\1\
msgstr \1\
/g' > skeleton.po.new
rm skeleton.tmp

