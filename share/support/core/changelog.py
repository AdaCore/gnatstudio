"""
This file adds support for editing ChangeLog files.
It provides syntax highlighting in these files, and a
text alias "hdr" which expands to the following line:
    date user_name <user_name@>

You do not need to load this file if you never edit Changelog files
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS

XML = r"""<?xml version="1.0"?>
<GPS>
  <Language>
    <Name>Changelog</Name>
    <Spec_Suffix>.changelog</Spec_Suffix>
    <Keywords>[0-9]+|</Keywords>
    <Keywords>\(.*\).*:</Keywords>
    <Context>
      <Comment_Start>*</Comment_Start>
      <Comment_End>:</Comment_End>
      <String_Delimiter>&quot;</String_Delimiter>
      <Constant_Character>&apos;</Constant_Character>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
  </Language>
  <alias name="hdr">
    <param name="USER" environment="true"/>
    <text>%D  %(USER)  &lt;%(USER)@&gt;</text>
  </alias>
</GPS>
"""

GPS.parse_xml(XML)
