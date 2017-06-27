"""miscellaneous text utilities, on top of text_utils, used by other plug-ins
"""


import GPS
import text_utils

# inserts text at current position in current file with a newline at the end


def insert_line(text):
    GPS.Editor.insert_text("\n" + text)
# end insert_line


# replaces current line in current_file with specified new line
def replace_line(current_file, new_line):
    line_num = GPS.Editor.cursor_get_line(current_file)
    line = GPS.Editor.get_chars(current_file, line_num, 0)
    if line[len(line) - 1] == '\n':
        GPS.Editor.replace_text(current_file, line_num,
                                0, new_line, 0, len(line) - 1)
    else:
        GPS.Editor.replace_text(current_file, line_num,
                                0, new_line, 0, len(line))
    # end if
# end replace_line


# get current line from current file
def get_line():
    file = GPS.current_context().file().name()
    line_num = GPS.current_context().location().line()
    str = GPS.Editor.get_chars(file, line_num, 0)
    return str[:-1]  # omit the '\n'
# end get_line


# move up 'count' lines in the current file
def up(count=1):
    file = GPS.current_context().file().name()
    line = GPS.current_context().location().line()
    GPS.Editor.cursor_set_position(file, line - count)
# end up


# move down 'count' lines in the current file
def down(count=1):
    text_utils.next_line(count)
# end down


# attempt to move up 'count' lines in the current file, returning
# success/failure indication
def attempt_up(count=1):
    line = GPS.current_context().location().line()
    if line - count > 0:
        file = GPS.current_context().file().name()
        GPS.Editor.cursor_set_position(file, line - count)
        return True
    else:
        return False
    # end if
# end attempt_up


# return 'width' blanks
def blanks(width):
    return ' ' * width
# end blanks
