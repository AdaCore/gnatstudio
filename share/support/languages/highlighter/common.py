from highlighter.interface import *


tag_string_escapes = new_style(lang="General",
                               name="string escapes",
                               label="String escapes",
                               doc='',
                               foreground_colors=('#875162', '#DA7495'))
"""
Style for escapes in strings, such as \\n or \\t

:type: Style
"""

tag_comment_notes = new_style(lang="General",
                              name="comment_notes",
                              label="TODO and NOTE in comments",
                              doc='',
                              foreground_colors=("#407A8E", "#907A8E"))
"""
Style for notes in comments. Used for highlighting TODO and NOTE strings in
comments.

:type: Style
"""

tag_default = existing_style("Src-Editor-Reference-Style", "default")
"""
Default style

:type: Style
"""

tag_block = existing_style("Src-Editor-Block-Variant", "blocks")
"""
Style for blocks

:type: Style
"""

tag_type = existing_style("Src-Editor-Type-Variant", "types")
"""
Style for types

:type: Style
"""

tag_keyword = existing_style("Src-Editor-Keywords-Variant", "keywords")
"""
Style for keywords

:type: Style
"""

tag_comment = existing_style("Src-Editor-Comments-Variant", "comments")
"""
Style for comments

:type: Style
"""

tag_string = existing_style("Src-Editor-Strings-Variant", "strings")
"""
Style for strings

:type: Style
"""

tag_number = existing_style("Src-Editor-Numbers-Variant", "numbers")
"""
Style for numbers

:type: Style
"""

ws = r"[^\S\n]*?"

hl_comment_notes = simple("(?:TODO|NOTE){0}:".format(ws),
                          tag=tag_comment_notes)
hl_inside_strings = simple(r"(?:\\.|\%[^\"]?)", tag=tag_string_escapes)
