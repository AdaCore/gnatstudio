from modules import Module
from highlighter.common import *

XML = """
<Language>
  <Name>matlab</Name>
  <Body_Suffix>.m</Body_Suffix>
  <Obj_Suffix>.pyc</Obj_Suffix>
  <Context>
    <Can_Indent>True</Can_Indent>
    <Syntax_Highlighting>False</Syntax_Highlighting>
    <Case_Sensitive>True</Case_Sensitive>
  </Context>
</Language>
"""

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


# noinspection PyMethodMayBeStatic
class MatlabSupport(Module):
    def gps_started(self):
        GPS.parse_xml(XML)
