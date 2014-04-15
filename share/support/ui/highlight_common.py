from highlight_framework import *


tag_string_escapes = new_style("General", "string_escapes", '#875162')
tag_comment_notes = new_style("General", "comment_notes", "#407A8E")
ws = r"[^\S\n]*?"

hl_comment_notes = simple("(?:TODO|NOTE){0}:".format(ws),
                          tag=tag_comment_notes)
hl_inside_strings = simple(r"(?:\\.|\%[^\"]?)", tag=tag_string_escapes)
