from highlighter.common import words, tag_keyword, region, tag_comment, \
        hl_comment_notes, hl_inside_strings, simple, tag_number, \
        register_highlighter, tag_string
import GPS


# Load language definition before the gps_started hook, and before we load
# the project.
XML = """
<Language>
  <Name>matlab</Name>
  <Body_Suffix>.m</Body_Suffix>
  <Obj_Suffix>-</Obj_Suffix>
  <Context>
    <Can_Indent>True</Can_Indent>
    <Syntax_Highlighting>False</Syntax_Highlighting>
    <Case_Sensitive>True</Case_Sensitive>
  </Context>
</Language>
"""
GPS.parse_xml(XML)

register_highlighter(
    language="matlab",
    spec=(
        # Match keywords
        words(["break", "case", "catch", "continue", "else", "elseif",
               "end", "for", "function", "global", "if", "otherwise",
               "persistent", "return", "switch", "try", "while", "classdef",
               "methods"],
              tag=tag_keyword),

        # Match comments lines
        region(r"%", "\n", tag=tag_comment, highlighter=(hl_comment_notes,)),

        # Match strings
        region(r"'", r"'|$", tag=tag_string, highlighter=(hl_inside_strings,)),

        # Match number literals
        simple(r"\b[0-9]*\.?[0-9]+\b", tag=tag_number),
    )
)
