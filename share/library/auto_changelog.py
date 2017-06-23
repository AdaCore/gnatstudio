"""Automatically filling ChangeLog entries

When a Visual differences for a file is open, and the user edits the changelog
for this file, this package will automatically fill the changelog with stubs
with names of subprograms where differences have been found.
"""

from GPS import EditorBuffer, File, Hook, Locations


def on_file_edited(hook, file):
    name = file.path
    l = len(name)

    #  If the file that has been opened is not a changelog, return.
    if (name[l - 4:l] != '$log'):
        return

    #  Get the basename of the log file to find the actual source.
    basename = name[max(name.rfind('\\'), name.rfind('/')) + 1:l - 4]

    # Eliminate potential $x trailing
    trail = basename.rfind('$')
    if trail != -1:
        basename = basename[0:trail]

    source = File(basename)
    buffer = EditorBuffer.get(source, False, False)

    log = EditorBuffer.get(file, False, False)

    #  If the log is not empty, exit.
    if log.get_chars() != '':
        return

    #  Query the Locations View for a list of visual differences for the source
    locations = Locations.list_locations("Visual differences", basename)

    prev = ""

    #  Find out which is the "enclosing" program/package.
    loc = buffer.at(buffer.lines_count() - 1, 1)
    global_proc = loc.subprogram_name()

    i = 0
    while i < len(locations) / 2:
        loc = buffer.at(
            locations[2 * i].line(),
            locations[2 * i].column())

        prog = loc.subprogram_name()

        #  If there are multiple changes within the same subprogram, display
        #  the subprogram only once
        if (prog != prev) and (prog != global_proc):
            log.insert(log.end_of_buffer(), "(" + prog + "): \n")
            prev = prog

        i = i + 1

    # Jump to the end of the first line
    log.current_view().goto(log.at(1, 1).end_of_line())


Hook("file_edited").add(on_file_edited)
