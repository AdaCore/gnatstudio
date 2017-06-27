# This script adds support for building all the main units found in a
# project tree.
##
# Put this file in $HOME/.gps/plug-ings/build.py
# Launch gps: a new menu is available in Build/Make/All Main Units
##
# Or in batch mode (to generate a batch file to execute in command line mode):
# gps -Pproject --hide --eval="python:build.compile_and_exit('foo.sh')"

import GPS

GPS.parse_xml("""
   <action name='Compile all main units' category="Builder">
       <shell lang='python'>build.compile_recursive()</shell>
   </action>
   <menu action='Compile all main units'>
       <title>/Build/Make/All Main Units</title>
   </menu>
""")


def compile_recursive(to_file=''):
    """Prepares all the commands needed to compile all the executables found
       in the project tree. If TO_FILE is provided, the commands are written
       in that file, otherwise they are run within GPS directly"""

    if to_file != '':
        out = file(to_file, 'w')

    for p in GPS.Project.root().dependencies(recursive=True):
        for main in p.get_attribute_as_list("main"):
            if to_file != '':
                out.write("gnatmake -P" + p.file().name() + " " + main + "\n")
            else:
                GPS.File(main).make()

    if to_file != '':
        out.close()


def compile_and_exit(to_file=''):
    compile_recursive(to_file)
    GPS.exit()
