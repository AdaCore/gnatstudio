"""
Adds support for editing .cgpr files (project configuration files)

This file provides syntax highlighting when editing .cgpr files.

You need to reload your project file (/Project/Reload Project) to
force GNAT Studio to take into account your changes.
"""


import GPS

XML = r"""<?xml version="1.0"?>
<GNAT_Studio>
  <Language>
    <Name>Project Configuration File</Name>
    <Parent>Ada</Parent>
    <Body_Suffix>.cgpr</Body_Suffix>
    <Keywords>(a(bstract|t|ll)|case|e(nd|xte(nds|rnal))|for|is|</Keywords>
    <Keywords>li(brary|mited)|null|</Keywords>
    <Keywords>others|p(ackage|roject)|renames|type|use|w(hen|ith)|</Keywords>
    <Keywords>(aggregate|aggregate library|abstract|standard|</Keywords>
    <Keywords>configuration) project)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>^\s*\-\-\s*[^\s]</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Constant_Character>&apos;</Constant_Character>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
  </Language>
</GNAT_Studio>
"""

GPS.parse_xml(XML)
