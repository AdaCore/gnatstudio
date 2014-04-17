from highlight_framework import *


tag_string_escapes = new_style("General", "string_escapes", '#875162')
tag_comment_notes = new_style("General", "comment_notes", "#407A8E")

tag_block = existing_style("Src-Editor-Block-Variant")
tag_type = existing_style("Src-Editor-Type-Variant")
tag_keyword = existing_style("Src-Editor-Keywords-Variant")
tag_comment = existing_style("Src-Editor-Comments-Variant")
tag_string = existing_style("Src-Editor-Strings-Variant")
tag_number = existing_style("Src-Editor-Numbers-Variant")

ws = r"[^\S\n]*?"

hl_comment_notes = simple("(?:TODO|NOTE){0}:".format(ws),
                          tag=tag_comment_notes)
hl_inside_strings = simple(r"(?:\\.|\%[^\"]?)", tag=tag_string_escapes)
