"""This plugin will automatically pretty-print a source file each time it is
saved on disk by calling gnatpp. See also autoformat.py for a lighter version
using the built-in editor reformatter instead. Warning: note that if you save
files often, this will generate many calls to gnatpp with no protection/lock,
so you may end up with multiple gnatpp processes running at the same time.
"""

import GPS


# Actions to be performed each time a file is saved
def on_file_saved(hook, file):
    GPS.execute_action("pretty print (force save)")

# Register the callback on the "file_saved" hook
GPS.Hook("file_saved").add(on_file_saved)
