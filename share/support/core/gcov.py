""" Provides the "Tools/Gcov/Compute coverage files" and "Remove coverage
files" menus, which executes gcov automatically.

This script will also perform checks along the way to guide through the
procedure of obtaining gcov info.
The output of the gcov process is displayed in a separate console.
At the end of the processing, the open editors are decorated with coverage
information.

Note that GPS calls gcov so that the .gcov files are generated
 - in the directory pointed to by the "GCOV_ROOT" environment variable, or
 - in the object directory of the root project, if this variable is not set

"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import os
import re
from gps_utils import interactive
from GPS import MDI, Project, Process, CodeAnalysis

# A class to display the output of gcov in a separate console.


class Gcov_Process (GPS.Console, GPS.Process):

    def on_output(self, unmatched, matched):
        self.write(unmatched + matched)

    def on_exit(self, status, remaining_output):
        self.write(remaining_output)
        if status == 0:
            self.write("process terminated successfully")
        else:
            self.write("process terminated [" + str(status) + "]")

        # Show coverage report
        analysis = CodeAnalysis.get("Coverage")

        if GPS.Project.root().is_harness_project():
            original = GPS.Project.root().original_project().file()
            analysis.add_gcov_project_info(original)
        else:
            analysis.add_all_gcov_project_info()

        analysis.show_analysis_report()

        self.kill()

    def on_input(self, input):
        self.send(input)

    def on_destroy(self):
        self.kill()

    def __init__(self, process, args="", directory=""):
        GPS.Console.__init__(self, "Executing gcov",
                             on_input=Gcov_Process.on_input,
                             on_destroy=Gcov_Process.on_destroy,
                             force=True)

        GPS.Process.__init__(self, process + ' ' + args, ".+",
                             remote_server="Build_Server",
                             directory=directory,
                             on_exit=Gcov_Process.on_exit,
                             on_match=Gcov_Process.on_output)


def using_gcov(context):
    return GPS.Preference('Coverage-Toolchain').get() == 'Gcov'


@interactive(name='gcov compute coverage files',
             filter=using_gcov)
def run_gcov():
    "Run gcov to generate the coverage files"
    # Verify that the version of gcov is recent enough to support response
    # files and reading of .gc?? data in multiple directories.

    try:
        p = Process("gcov --version")
        out = p.get_result()
        p = re.compile("[1-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]")
        found = p.findall(out)
        if not found:
            MDI.dialog("Could not find a date in the output of gcov.")
        else:
            date = found[0]
            if date < 20071005:
                MDI.dialog("Your version of gcov is dated " + str(date) +
                           ".\nThis plugin requires gcov for GNAT dated " +
                           "20071005 or later.")
                return
    except:
        MDI.dialog("""Could not read gcov version number.

Make sure you are using gcov for GNAT dated 20071005 or later.""")

    # Determine the root project
    root_project = Project.root()

    # Determine where to create the gcov info
    GCOV_ROOT = os.getenv("GCOV_ROOT")

    if not GCOV_ROOT:
        root_object_dirs = root_project.object_dirs(False)
        if not root_object_dirs:
            MDI.dialog("""The root project does not have an object directory.
 Please add one, or set the enviroment variable GCOV_ROOT to
 the directory where you would like the gcov files to be
 generated.""")
            return
        else:
            gcov_dir = root_object_dirs[0]

    else:
        gcov_dir = GCOV_ROOT

    if not os.access(gcov_dir, os.R_OK and os.W_OK):
        MDI.dialog(""" Could not access the directory:

   """ + gcov_dir + """

Please point the environment variable GCOV_ROOT to a directory
on which you have permission to read and write.
         """)

    input_file = os.path.abspath(os.path.join(gcov_dir, "gcov_input.txt"))

    # List all the projects
    projects = root_project.dependencies(True)
    # List all object dirs
    object_dirs = root_project.object_dirs(True)

    # Write the response file
    res = file(input_file, 'wb')

    gcda_file_found = False
    gcno_file_found = False

    for p in projects:
        sources = p.sources(False)

        for s in sources:
            n = s.path
            basename = n[max(n.rfind('\\'), n.rfind('/')) + 1:len(n)]
            unit = basename[0:basename.rfind('.')]

            for object_dir in object_dirs:
                gcda = object_dir + os.sep + unit + ".gcda"

                # If we have not yet found at least one .gcno file, attempt to
                # find one. This is to improve the precision of error messages,
                # and detect the case where compilation was successful but the
                # executable has never been run.

                if not gcno_file_found:
                    gcno = object_dir + os.sep + unit + ".gcno"
                    if os.access(gcno, os.F_OK):
                        gcno_file_found = True

                if os.access(gcda, os.F_OK):
                    gcda_file_found = True
                    # Write one entry in response file

                    # Escape all backslashes.
                    gcda = gcda.replace('\\', '\\\\')

                    res.write('"' + gcda + '"' + "\n")
                    break

    res.close()

    file(input_file).read()

    if not gcno_file_found:
        # No gcno file was found: display an appropriate message.
        MDI.dialog(""" No ".gcno" file was found in any of the object directories.

Make sure you have compiled the sources of interest with
the "Code coverage" flags.""")

    else:
        if not gcda_file_found:
            # Some gcno files were found, but no gcna files.
            MDI.dialog(""" No ".gcda" file was found in any of the object directories.

Make sure you have run the executable(s) at least once.
""")

        else:
            # Run gcov
            Gcov_Process("gcov", "@%s" % input_file, directory=gcov_dir)


@interactive(name='gcov remove coverage files',
             filter=using_gcov)
def remove_gcov():
    "Cleanup the gcov coverage files"

    if not MDI.yes_no_dialog(
       "This will remove all .gcov and .gcda files, are you sure ?"):
        return

    # Look in all the projects

    for p in Project.root().dependencies(True):
        object_dirs = p.object_dirs(False)

        if len(object_dirs) > 0:
            object_dir = object_dirs[0]

            # Browse in the object dirs
            for f in os.listdir(object_dir):
                #  if f is a .gcda or a .gcov, remove it
                if f.find(".gcda") != -1 or f.find(".gcov") != -1:
                    os.remove(object_dir + os.sep + f)
