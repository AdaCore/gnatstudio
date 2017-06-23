"""This plugin will automatically pretty-print a source file each time it is
saved on disk by calling gnatpp. See also autoformat.py for a lighter version
using the built-in editor reformatter instead. Warning: note that if you save
files often, this will generate many calls to gnatpp with no protection/lock,
so you may end up with multiple gnatpp processes running at the same time.
"""

import gnatpp
import gps_utils


@gps_utils.hook('file_saved')
def on_file_saved(file):
    gnatpp.gnatpp(file)
