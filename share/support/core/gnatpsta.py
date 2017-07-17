"""Display Ada standard package

This script adds a new menu in /Help/GNAT Runtime/Standard to display the
Standard package. This package contains the definition of basic types
for the Ada runtime, but doesn't exist as a file on the disk since it can't
be described in pure Ada.
It properly handles older versions of GNAT, which came with a gnatpsta
command line utility, as well as new versions where the gcc driver itself
must be used.
"""

###########################################################################
# No user customization below this line
############################################################################

from GPS import MDI, EditorBuffer
import GPS
import tempfile
import os_utils
import shutil
from gps_utils import interactive

tmp_dir = ""


def on_exit(self, status, remaining_output):
    shutil.rmtree(tmp_dir)

    if status != 0:
        GPS.Console("Messages").write("error: failed to display standard.ads",
                                      mode="error")

    buffer = EditorBuffer.get_new()
    buffer.delete()   # delete any text inserted via templates
    buffer.insert(buffer.at(1, 1), remaining_output)
    buffer.set_lang('ada')
    buffer.current_view().set_read_only(True)
    MDI.get_by_child(buffer.current_view()).rename('package Standard')


@interactive(name="Display standard.ads")
def display():
    # Two possible ways here: older versions of GNAT still have the
    # gnatpsta utility, whereas for more recent versions we need to
    # compile a file with -gnatS. Try gnatpsta first:

    path = None

    if os_utils.locate_exec_on_path("gnatpsta") != "":
        GPS.Process(['gnatpsta'], on_exit=on_exit)
    else:
        # We do not create the file on the disk, because: - we cannot create a
        # temporary file and delete it immediately, since GPS will then display
        # the dialog that the file has changed on disk.  - we cannot create the
        # file in the project's object_dir, since the latter might not exist,
        # or worse could also be a source_dir which would confuse the compiler.

        global tmp_dir

        tmp_dir = tempfile.mkdtemp()
        path = tmp_dir + "/p.ads"
        f = open(path, "w")
        f.write("package p is end p;")
        f.close()

        cmdline = ['gprbuild', '-c', '-gnatc', '-gnatS', '-q']

        target = GPS.Project.root().get_attribute_as_string("target")
        if target:
            cmdline.append('--target=%s' % target)

        runtime = GPS.Project.root().get_attribute_as_string(
            "runtime", index="Ada")
        if runtime:
            cmdline.append('--RTS=%s' % runtime)

        cmdline.append('p.ads')

        GPS.Process(cmdline, directory=tmp_dir, on_exit=on_exit)
