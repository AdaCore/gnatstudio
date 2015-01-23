from highlighter.common import *

hl_cont_line = simple(r"\\\n", tag=tag_type)

############
# Literals #
############

string_literal = region(
    r'"', r'"|[^\\]$', matchall=False, tag=tag_string,
    highlighter=(hl_cont_line, hl_inside_strings)
)

character_literal = simple(r"'(?:\\.|.)?'", tag=tag_string)
number_literal = simple(r"\b[0-9]*\.?[0-9]+\b", tag=tag_number)

############
# Comments #
############

comments_subhl = (hl_cont_line, hl_comment_notes)

c99_comment = region(r"//", r"$", tag=tag_comment, name="comment",
                     highlighter=comments_subhl)

multiline_comment = region(r"/\*", r"\*/", tag=tag_comment, name="ml_comment",
                           highlighter=comments_subhl)

preprocessor_comment = region(
    r"^{0}#{1}if{2}0".format(ws, ws, ws), r"^{0}#{1}endif".format(ws, ws),
    name="preprocessor_comment",
    tag=tag_comment,
    highlighter=(region_ref("preprocessor_comment"),))

pp_words = "|".join(["define", "if", "elif", "else", "endif", "ifndef",
                     "error", "import", "undef", "include", "using", "ifdef",
                     "line", "pragma"])

################
# Preprocessor #
################

preprocessor_directive = region(
    r"^{0}#{1}(?:{2})".format(ws, ws, pp_words), r'$',
    tag=new_style("C", "preprocessor", ("#606090", "#A0A0F0"), prio=0),
    highlighter=(
        hl_cont_line,
        string_literal,
        simple(r'\<.*?\>', tag=tag_string),
        character_literal,
        number_literal,
        c99_comment,
        multiline_comment
    ),
    matchall=False
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
              "|union|void|volatile|while", tag=tag_keyword),

        words("int|long|char|float|short|unsigned|double|signed",
              tag=tag_type)
    )
)
