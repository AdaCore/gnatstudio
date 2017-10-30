"""Provides a contextual menu in editors that opens the source file referenced
at the cursor's position.
The file's location may be absolute or relative to the projects source
folders. The file name can be followed by ":" and a line number, to go to
that specific line number.

This contextual menu will work with:
  - include clauses in C-headerfiles
  - Absolute references in the sources.

If the file name includes spaces, you should first select the file name. This
will skip the automatic detection of file name, and take the whole selection
has a file name if such a file is found on the disk.
"""

import GPS
import re
import gps_utils
import os
from text_utils import get_selection_or_line


file_pattern = \
    u'((?:[a-zA-Z]:)?(?:[\\\\/]?[\w\d._$-]+)+)(?::(\d+)(?::(\d+))?)?'
# The regexp pattern to search file file:line:column references on the
# current line.

std_include_path = ["/usr/include", "/usr/local/include"]
# Standard search paths for files

# __sep = u'[:\'\" <>*?]'
# file_pattern = "(?:^|" + __sep + ")" \
#  + u'((?:[/\\]?[\w\d._]+)+)(?::(\d+)(?::(\d+))?)?' \
#  + "(?:$|" + __sep + ")"
# A second version of the pattern which only matches inside specific
# separators (or beginning/end of line)

############################################################################
# No user customization below this line
############################################################################

file_pattern_re = re.compile(file_pattern)


class __contextData(object):
    pass


def __filter(context):
    """Checks whether the contextual menu should be displayed"""
    if context.file() is None:
        return False

    ed = GPS.EditorBuffer.get(open=False)
    if not ed:
        return False

    try:
        (ed, start, end) = get_selection_or_line(ed, context.location())
    except:
        return False  # No file information in the context
    text = ed.get_chars(start, end)

    data = __contextData()
    context.open_file = data
    data.file = ""
    data.line = 0
    data.column = 0

    cursor_col = context.location().column()

    if ed.selection_end() != ed.selection_start():
        data.file = text   # No post-processing
    else:
        # Try to find the filename we clicked on
        pos = 0
        while pos < len(text):
            m = file_pattern_re.search(text, pos)
            if m and m.start() <= cursor_col and m.end() >= cursor_col:
                data.file = m.group(1)
                if m.group(2):
                    data.line = int(m.group(2))
                if m.group(3):
                    data.column = int(m.group(3))
                break
            elif m:
                pos = m.end()
            else:
                return False

    if data.file == "":
        return False

    if os.path.exists(data.file):
        return True
    else:
        # Let GPS search in all source dirs and predefined paths
        f = GPS.File(data.file)
        if os.path.exists(f.path):
            data.file = f.path
            return True

        # Search with just the basename (otherwise "src/file.c" where
        # "src/" is a source_dir would not be found)
        f = GPS.File(os.path.basename(data.file))
        if os.path.exists(f.path):
            data.file = f.path
            return True

        # One more try, include standard include paths for C files
        for p in std_include_path:
            f = os.path.join(p, data.file)
            if os.path.exists(f):
                data.file = f
                return True

        # Special case for C files: #include accepts directories that are
        # not necessarily in the source dirs

        # Handle the case where the include statement contains a directory.
        if os.path.splitext(data.file)[1] in \
                [".h", ".hh", ".cfg", ".c", ".gen"]:
            for p in GPS.Project.root().source_dirs(True):
                f = os.path.join(p, data.file)
                if os.path.exists(f):
                    data.file = f
                    return True

    return False


def __label(context):
    """Returns the label to use for the contextual menu"""
    data = context.open_file
    return "Open <b>" + os.path.basename(data.file) + "</b>"


@gps_utils.interactive(
    name='open file at cursor location',
    contextual=__label,
    filter=__filter,
    static_path="Open file")
def __activate():
    """
    Open the file specified at the cursor's location, for instance in a C
    import statement.
    Line numbers are also analyzed when possible ("file:line")
    """
    context = GPS.contextual_context()
    data = context.open_file

    try:
        ed = GPS.EditorBuffer.get(GPS.File(data.file))
        view = ed.current_view()
        loc = ed.at(line=data.line, column=data.column)
        view.goto(loc)
        GPS.MDI.get_by_child(view).raise_window()
    except:
        pass
