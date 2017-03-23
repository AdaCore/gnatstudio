from highlighter.common import *

keywords = [
    "assign", "backspace", "block ""data", "call", "close", "common",
    "continue", "data", "dimension", "do", "else", "else ""if", "end",
    "endfile", "endif", "entry", "equivalence", "external", "format",
    "function", "goto", "if", "implicit", "inquire", "intrinsic", "open",
    "parameter", "pause", "print", "program", "read", "return", "rewind",
    "rewrite", "save", "stop", "subroutine", "then", "write"
    "allocatable", "allocate", "case", "contains", "cycle", "deallocate",
    "elsewhere", "exit", "include", "interface", "intent", "module",
    "namelist", "nullify", "only", "operator", "optional", "pointer",
    "private", "procedure", "public", "recursive", "result", "select",
    "sequence", "target", "use", "while", "where"
]

hl_cont_line = simple(r"\\\n", tag=tag_type)

hl_strings = region_template(
    matchall=False, tag=tag_string,
    highlighter=(hl_cont_line, hl_inside_strings)
)

register_highlighter(
    language="fortran 90",
    spec=(
        # Match comments lines
        region(r"!", "\n", tag=tag_comment, highlighter=(hl_comment_notes,)),

        # Match string literals
        hl_strings(r"'", r"'|[^\\]$"),
        hl_strings(r'"', r'"|[^\\]$'),

        region(
            r"'", r"'|[^\\]$", matchall=False, tag=tag_string,
            highlighter=(hl_cont_line, hl_inside_strings)
        ),

        words(keywords, tag=tag_keyword),
    ),
    igncase=True
)
