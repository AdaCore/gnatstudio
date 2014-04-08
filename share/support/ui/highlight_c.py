from highlight_framework import *

############
# Literals #
############

string_literal = region(
    r'"', r'"|$', matchall=False, tag="string",
    highlighter=(
        simple(r"(?:\\.|\%.)",
               tag=newtag("string_escapes", foreground_rgba='green')),
    )
)
character_literal = simple(r"'(?:\\.|.)?'", matchall=False, tag="character")
number_literal = simple(r"\b[0-9]*\.?[0-9]+\b", tag="number")

############
# Comments #
############

comments_subhl = (words(["TODO", "NOTE"],
                  tag=newtag("comment_notes", foreground_rgba="red")),)

c99_comment = region(r"//", r"$", tag="comment", name="comment",
                     highlighter=comments_subhl)

multiline_comment = region(r"/\*", r"\*/", tag="comment",
                           name="ml_comment", highlighter=comments_subhl)

preprocessor_comment = region(
    r"#if 0", "#endif", multiline=True,
    name="preprocessor_comment", tag="comment",
    highlighter=(
        region_ref("preprocessor_comment"),
    )
)
pp_words = "|".join(
    ["define", "if", "elif", "else", "endif", "ifndef", "error", "import",
     "undef", "include", "using", "ifdef", "line", "pragma"]
)

preprocessor_directive = region(
    r"#(?:{0})".format(pp_words), "$", multiline=False,
    tag=newtag(name="preprocessor", foreground_rgba="yellow", prio=1),
    highlighter=(
        string_literal,
        simple(r'\<.*?\>', tag="string", matchall=False),
        character_literal,
        number_literal,
        c99_comment,
        multiline_comment
    )
)

register_highlighter(
    language="c",
    spec=(
        string_literal,
        character_literal,
        number_literal,
        c99_comment,
        multiline_comment,
        preprocessor_comment,
        preprocessor_directive,
        # Match keywords
        words(
            "auto|break|case|char|const|continue|default|do|double|else|"
            "enum|extern|float|for|goto|if|int|long|register|return|"
            "short|signed|sizeof|static|struct|switch|typedef|union|"
            "unsigned|void|volatile|while", tag="keyword"
        )
    )
)
