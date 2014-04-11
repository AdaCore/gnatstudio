from highlight_framework import *

############
# Literals #
############

string_literal = region(
    r'"', r'"|[^\\]$', matchall=False, tag="string",
    highlighter=(
        simple(r"\\\n", tag="type"),
        simple(r"(?:\\.|\%.)",
               tag=new_style("General", "string_escapes", 'green'))
    )
)

character_literal = simple(r"'(?:\\.|.)?'", matchall=False, tag="character")
number_literal = simple(r"\b[0-9]*\.?[0-9]+\b", tag="number")

############
# Comments #
############

comments_subhl = (
    simple(r"\\\n", tag="type"),
    words(["TODO", "NOTE"], tag=new_style("General", "comment_notes", "red"))
)

c99_comment = region(r"//", r"$", tag="comment", name="comment",
                     highlighter=comments_subhl)

multiline_comment = region(r"/\*", r"\*/", tag="comment", name="ml_comment",
                           highlighter=comments_subhl)

ws = r"[^\S\n]*?"

preprocessor_comment = region(
    r"^{0}#{1}if{2}0".format(ws, ws, ws), r"^{0}#{1}endif".format(ws, ws),
    name="preprocessor_comment",
    tag="comment",
    highlighter=(region_ref("preprocessor_comment"),))

pp_words = "|".join(["define", "if", "elif", "else", "endif", "ifndef",
                     "error", "import", "undef", "include", "using", "ifdef",
                     "line", "pragma"])

################
# Preprocessor #
################

preprocessor_directive = region(
    r"^{0}#{1}(?:{2})".format(ws, ws, pp_words), r'$',
    tag=new_style("C", "preprocessor", "#606090", prio=1),
    highlighter=(
        simple(r"\\\n", tag="type"),
        string_literal,
        simple(r'\<.*?\>', tag="string", matchall=False),
        character_literal,
        number_literal,
        c99_comment,
        multiline_comment
    )
)

#######################
# General highlighter #
#######################

register_highlighter(
    language="c",
    spec=(
        string_literal, character_literal, number_literal,
        c99_comment, multiline_comment, preprocessor_comment,
        preprocessor_directive,
        # Match keywords
        words("auto|break|case|const|continue|default|do|else|enum|extern|for"
              "|goto|if|register|return|sizeof|static|struct|switch|typedef"
              "|union|void|volatile|while", tag="keyword"),

        words("int|long|char|float|short|unsigned|double|signed", tag="type")
    )
)
