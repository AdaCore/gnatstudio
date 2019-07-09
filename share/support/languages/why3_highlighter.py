from highlighter.common import words, simple, tag_number, tag_keyword, region,\
    tag_comment, register_highlighter, tag_string, tag_type

number_literal = simple(r"\b[0-9]*\.?[0-9]+\b", tag=tag_number)

keywords = "|".join(["theory", "type", "constant", "function", "predicate",
                     "inductive", "axiom", "lemma", "goal", "use", "clone",
                     "prop", "meta", "scope", "import", "export", "end",
                     "forall", "exists", "not", "true", "false", "if", "then",
                     "else", "let", "in", "match", "with", "as", "epsilon"])

comment_region = region(r"\(\*", "\*\)", tag=tag_comment)

ada_keywords = "|".join(["True", "False"])

task_specific = "|".join(["Local Context", "Goal"])

register_highlighter(
    language="why",
    spec=(
        number_literal,
        # Match comments lines
        comment_region,

        # Match keywords
        words(keywords, tag=tag_keyword),
        words(ada_keywords, tag=tag_string),
        words(task_specific, tag=tag_type)
    )
)
