from highlight_framework import *
from keyword import kwlist

strings_subhighlighter = (
    simple(r"(?:\\.|\%.?)",
           tag=newtag("string_escapes", foreground_rgba='green')),
)

register_highlighter(
    language="python",
    spec=(
        # Match multiline strings
        region(r'"""|\'\'\'', r'"""|\'\'\'', multiline=True, tag="string",
               name="multiline_string",
               highlighter=strings_subhighlighter),

        # Match string literals
        region(r'"|\'', r'"|\'', multiline=False, tag="string",
               highlighter=strings_subhighlighter),

        # Match comments lines
        region(r"#", "\n", multiline=False, tag="comment", ),

        # Match number literals
        simple(r"\b[0-9]*\.?[0-9]+\b", tag="number"),

        # Match keywords
        words(kwlist, tag="keyword")
    )
)
