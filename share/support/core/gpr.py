"""
Adds support for editing .gpr files (GPS project files)

This file provides syntax highlighting when editing .gpr files.
Generally, such files are best edited through the menu
  /Project/Edit Project Properties
but it is sometimes more convenient to edit them by hand.

You need to reload your project file (/Project/Reload Project) to
force GPS to take into account your changes.
"""


import GPS

XML = r"""<?xml version="1.0"?>
<GPS>
  <Language>
    <Name>Project File</Name>
    <Parent>Ada</Parent>
    <Body_Suffix>.gpr</Body_Suffix>
    <Keywords>(a(bstract|t|ll)|case|e(nd|xte(nds|rnal))|for|is|</Keywords>
    <Keywords>li(brary|mited)|null|</Keywords>
    <Keywords>others|p(ackage|roject)|renames|type|use|w(hen|ith)|</Keywords>
    <Keywords>(aggregate|aggregate library|abstract|standard|</Keywords>
    <Keywords>configuration) project)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>--</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Constant_Character>&apos;</Constant_Character>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
  </Language>
</GPS>
"""

GPS.parse_xml(XML)
