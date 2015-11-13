"""
This plugin adds support for the ParaSail language.

The following features are provided:

  * GPR project support:
     - "ParaSail" becomes available in the list of languages for projects
       when creating or editing projects through the GPS inteface.

       When editing projects outside of GPS, all is needed is to add ParaSail
       to the list of supported languages. For instance, a project containing
       only ParaSail sources would contain:

          for Languages use ("parasail");


     - the default naming scheme is set to .psi, .psl.

  * Syntax highlighting:
     - detection of ParaSail keywords
     - detection of strings

  * Outline:
     - top level functions, classes and interfaces are shown in the Outline
"""


import GPS

language_description = """
   <Language>
    <Name>ParaSail</Name>
    <Spec_Suffix>.psi</Spec_Suffix>
    <Body_Suffix>.psl</Body_Suffix>
    <Keywords>^(a(bs(tract)?|ll|nd)|block|c(ase|lass|on(current|st|tinue))|e(ach|ls(e|if)|nd|x(it|tends|ports))|f(or(ward)?|unc)|global|i(f|m(plements|port)|n(terface)?|s)|l(ambda|o(cked|op))|mod|n(ew|ot|ull)|o(f|p(tional)?|r)|private|queued|re(f|m|turn|verse)|s(eparate|ome)|t(hen|ype)|until|var|w(hile|ith)|xor)\\b</Keywords>
    <Context>
      <New_Line_Comment_Start>//</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>package</Name>
        <Pattern>^interface\s+([\w\d+_]+)(\s|\(|&lt;)</Pattern>
        <Index>1</Index>
      </Category>

      <Category>
        <Name>class</Name>
        <Pattern>^class\s+([\w\d+_]+)(\s|\()</Pattern>
        <Index>1</Index>
      </Category>

      <Category>
        <Name>function</Name>
        <Pattern>^func\s+([\w\d+_]+)(\s|\()</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>

    <Project_Field name="compiler_command" index="parasail">Parasail Interpreter</Project_Field>
  </Language>
"""

GPS.parse_xml(language_description)
