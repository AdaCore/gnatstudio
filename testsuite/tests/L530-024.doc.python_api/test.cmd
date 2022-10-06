if [ -f $GPS_SRC_DIR/docs/users_guide/generate.py ]; then
   doc_prefix=$GPS_SRC_DIR/docs/users_guide
else
   # find the location of the shared doc utilities based on sortxml
   sortxml_executable=` which sortxml `
   utils_bin=` dirname $sortxml_executable `
   doc_prefix=$utils_bin/../share/doc/
fi

for f in perspectives6.xml properties.json ; do
    rm -f $GNATSTUDIO_HOME/.gnatstudio/$f
done

# Launch GS once on a project, so that the "recent projects" menu
# gets generated in the doc.

# Use gnatstudio than $GNATSTUDIO deliberately, so that the
# GNAT Studio manual is listed in the doc.

$GNATSTUDIO -Pp.gpr --load=python:exit.py > /dev/null
PATH=.:$PATH gnatstudio --load=python:$doc_prefix/generate.py --traceoff=TESTSUITE --traceon=GPS.VCS.MODULE --traceon=GNATSTUDIO.MENU_GENERATION > /dev/null

# Any difference in the output here indicates that some function
# now exported to python is not documented.
# You need to edit  docs/users_guide/GPS.py, and run
#   make generate
# in the docs/users_guide directory

for file in GPS GPS.Browsers menus ; do

   tr -d '\r' < $file.rst > $file.rst.stripped
   diff $doc_prefix/$file.rst $file.rst.stripped

done
