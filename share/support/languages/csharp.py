"""
This plugin adds support for C# syntax highlighting and Outline.
"""


XML = r"""<?xml version="1.0" ?>
<GPS>
  <Language>
    <Name>C#</Name>
    <Spec_Suffix>.cs</Spec_Suffix>
    <Keywords>(a(bstract|s)|b(ase|ool|reak|yte)|c(a(se|tch)|h(ar|ecked)|lass|on(st|tinue))|e(lse|num|vent|xplicit|xtern)|f(alse|inally|ixed|or|oreach)|goto|i(mplicit|nt|nterface|nternal|[fns])|lo(ck|ng)|n(amespace|ew|ull)|o(bject|perator|ut|verride)|p(arams|rivate|rotected|ublic)|r(eadonly|e(f|turn))|s(byte|ealed|hort|izeof|tackalloc|tatic|tring|truct|witch)|t(his|hrow|rue|ry|ypeof)|u(int|long|nchecked|nsafe|short|sing)|virtual|volatile|void|while)\b</Keywords>
    <Parent>c</Parent>
    <Context>
      <New_Line_Comment_Start>//</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
      <Quote_Character>\</Quote_Character>
      <Constant_Character>&apos;</Constant_Character>
    </Context>

    <Categories>
      <Category>
        <Name>use</Name>
        <Pattern>^\s*using\s+([^;]*);$</Pattern>
        <Index>1</Index>
      </Category>
      <Category>
        <Name>namespace</Name>
        <Pattern>^\s*namespace\s+([\w][\w\d_.]+)</Pattern>
        <Index>1</Index>
      </Category>
      <Category>
        <Name>function</Name>
        <Pattern>^\s*((abstract|public|private|protected|static)\s+)*[\w\d_*]+\s+([\w\d_]+)\s*\([^\)]*\)</Pattern>
        <Index>3</Index>
      </Category>
      <Category>
        <Name>class</Name>
        <Pattern>^\s*((abstract|public|private|static)\s+)*class\s+(\w[\w\d_]+)\s*(\:\s*\w*)?</Pattern>
        <Index>3</Index>
      </Category>
    </Categories>
  </Language>
</GPS>
"""

import GPS
GPS.parse_xml(XML)
