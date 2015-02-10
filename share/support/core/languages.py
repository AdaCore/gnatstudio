"""
Adds syntax highlighting for a number of languages

This script provides basic support when editing languages other
than Ada and C. In particular, it provides syntax highlighting,
and support for showing the entities in the Outline View.

Based on these examples, you can add support for your own
languages.
"""


import GPS

XML = r"""<?xml version="1.0" ?>
<GPS>
  <Language>
    <Name>Texinfo</Name>
    <Body_Suffix>.texi</Body_Suffix>
    <Keywords>^@\w+</Keywords>
    <Wordchars>@</Wordchars>
    <Context>
      <New_Line_Comment_Start>@c\W</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
    <Categories>
      <Category>
        <Name>node</Name>
        <Pattern>^@node\s+([^\n,]+)</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
  </Language>

  <Language>
    <Name>rest</Name>
    <Body_Suffix>.rst</Body_Suffix>
  </Language>

  <Language>
    <Name>Autoconf</Name>
    <Body_Suffix>configure.in</Body_Suffix>
    <Keywords>^(AC_(SUBST|DEFINE(_UNQUOTED)?)\((\w+)|A[CM]_\w+)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>dnl </New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>AWK</Name>
    <Body_Suffix>.awk</Body_Suffix>
    <Keywords>^(BEGIN|END|ARG(C|IND|V)|CONVFMT|E(NVIRON|RRNO)|FI(ELDWIDTHS|LENAME)|F(NR|S)|IGNORECASE|N(F|R)|O(F(MT|S)|RS)|R(LENGTH|S|START|UBSEP)|break|continue|delete|e(xit|lse)|f(or|unction)|getline|if|next|printf?|return|while|atan2|c(lose|os|time)|exp|gsub|in(dex|t)|l(ength|og)|match|rand|s(in|plit|printf|qrt|rand|ub(str)?|ystem)|t(ime|o(lower|upper)))\b</Keywords>
    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>Perl</Name>
    <Body_Suffix>.pl</Body_Suffix>
    <Keywords>^(if|until|while|els(if|e)|unless|d(o|ump)|for(each)?|exit|die|BEGIN|END|return|exec|eval|local|my|continue|goto|last|next|redo)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>procedure</Name>
        <Pattern>^sub\s+([-\w\d+_:]+)\s*(\([^)]*\))?\s*{</Pattern>
        <Index>1</Index>
      </Category>
      <Category>
        <Name>package</Name>
        <Pattern>^package\s+([-\w\d+_:]+);</Pattern>
        <Index>1</Index>
      </Category>
      <Category>
        <Name>variable</Name>
        <Pattern>^([$@%][-\w\d+_:]+)\s*=</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
    <Project_Field
     name="compiler_command"
     index="perl">Perl Interpreter</Project_Field>
  </Language>

  <Language>
    <Name>Pascal</Name>
    <Body_Suffix>.pas</Body_Suffix>
    <Keywords>^(absolute|abstract|assembler|automated|cdecl|default|dispid|dynamic|export|external|far|forward|Index|inline|message|name|near|nodefault|overload|override|pascal|private|protected|public|published|read|readonly|register|reintroduce|resident|resourcestring|safecall|stdcall|stored|virtual|write|writeonly|and|array|as|asm|at|begin|case|class|const|constructor|contains|destructor|dispinterface|div|do|downto|else|end|except|exports|file|finalization|finally|for|function|goto|if|implementation|implements|in|inherited|initialization|interface|is|label|library|mod|nil|not|of|object|on|or|out|package|packed|procedure|program|property|raise|record|repeat|requires|result|self|set|shl|shr|then|threadvar|to|try|type|unit|uses|until|var|while|with|xor)\b</Keywords>

    <Context>
      <New_Line_Comment_Start>//</New_Line_Comment_Start>
      <Comment_Start>(*</Comment_Start>
      <Comment_End>*)</Comment_End>
      <String_Delimiter>'</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
    <Project_Field
     name="compiler_command"
     index="delphi">Delphi Compiler</Project_Field>
  </Language>

  <Language>
    <Name>Fortran 90</Name>
    <Body_Suffix>.f</Body_Suffix>

    <Categories>
      <Category>
        <Name>procedure</Name>
        <Pattern>^[\s\d]*(program|function|subroutine)\s+(\w+)\b</Pattern>
        <Index>2</Index>
      </Category>
      <Category>
        <Name>package</Name>
        <Pattern>^[\s\d]*module\s+(\w+)\s*(!|$)</Pattern>
        <Index>1</Index>
      </Category>
      <Category>
        <Name>type</Name>
        <Pattern>^[\s0-9]*type\s+(\w+)\b</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
    <Project_Field
     name="compiler_command"
     index="fortran 90">Fortran Compiler</Project_Field>
  </Language>

  <Language>
    <Name>M4</Name>
    <Body_Suffix>.m4</Body_Suffix>
    <Keywords>^(m4_)?(builtin|change(com|quote|word)|d(e(bug(file|mode)|cr|f(ine|n))|iv(ert|num)|nl|umpdef)|e(rrprint|syscmd|val)|f(ile|ormat)|gnu|i(f(def|else)|n(c(lude|r)|d(ex|ir)))|l(en|ine)|m(4(exit|wrap)|aketemp)|p(atsubst|opdef|ushdef)|regexp|s(hift|include|ubstr|ys(cmd|val))|tra(ceo(ff|n)|nslit)|un(d(efine|ivert)|ix))\b</Keywords>
    <Context>
      <New_Line_Comment_Start>dnl </New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>Makefile</Name>
    <Keywords>^((s?include|if(n?(eq|def))?|e(lse|ndif))\b|\n[^#]+:)</Keywords>
    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>False</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>type</Name>
        <Pattern>^ *([^ \n\t#:=]+([ \t]+([^ \t\n#:=]+|\$[({][^ \t\n#})]+[})]))*)[ \t]*:([ \t]*$|([^=\n].*$))</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
  </Language>

  <Language>
    <Name>Shell</Name>
    <Body_Suffix>.sh</Body_Suffix>
    <Keywords>^(e(cho|xport|val)|getopts|newgrp|pwd|read(only)?|t(est|imes|ype|hen|rap)|ulimit|hash|cd|s(et|hift)|u(mask|nset)|wait|do|el(if|se)|if|until|while|function|done|esac|f(i|or)|in|return|CDPATH|IFS|OPT(ARG|IND)|PS(1|2)|COLUMNS|EDITOR|H(OME|USHLOGIN)|L(ANG|C_(COLLATE|CTYPE|MESSAGES|MONETARY|NUMERIC|TIME)|INES|OGNAME)|MAIL(CHECK|PATH)?|PA(GER|TH)|SHELL|TERM(CAP|INFO)?|VISUAL)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>procedure</Name>
        <Pattern>^\s*(function\s+)?([\w_][\w\d_]+)\s*\(\)</Pattern>
        <Index>2</Index>
      </Category>
    </Categories>
  </Language>

  <Language>
    <Name>TCL</Name>
    <Body_Suffix>.tcl</Body_Suffix>
    <Keywords>^(global|upvar|inherit|p(ublic|r(otected|ivate))|common|itk_option|variable|if|then|else(if)?|for(each)?|break|continue|while|eval|case|switch|default|exit|error|p(roc|uts)|return|uplevel|constructor|destructor|itcl_class|loop|for_(array_keys|recursive_glob|file)|method|body|configbody|set)\b</Keywords>

    <Context>
      <New_Line_Comment_Start>#</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>javascript</Name>
    <Body_Suffix>.js</Body_Suffix>
    <Keywords>^(break|case|catch|continue|default|delete|do|else|false|finally|for|function|if|in|instanceof|new|null|return|switch|this|throw|true|try|typeof|var|void|while|with)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>//</New_Line_Comment_Start>
      <Comment_Start>/*</Comment_Start>
      <Comment_End>*/</Comment_End>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>function</Name>
        <Pattern>^function\s+([-\w\d+_:]+)\s</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
  </Language>

  <Language>
    <Name>Java</Name>
    <Body_Suffix>.java</Body_Suffix>
    <Keywords>^(abstract|assert|boolean|break|byte|case|catch|char|class|const|continue|default|do|double|else|enum|extends|final|finally|float|for|goto|if|implements|import|instanceof|int|interface|long|native|new|package|private|protected|public|return|short|static|strictpf|super|switch|synchronized|this|throw|throws|transient|try|void|volatile|while)\b</Keywords>
    <Context>
      <New_Line_Comment_Start>//</New_Line_Comment_Start>
      <Comment_Start>/*</Comment_Start>
      <Comment_End>*/</Comment_End>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>GNAT Expanded Code</Name>
    <Parent>Ada</Parent>
    <Body_Suffix>.dg</Body_Suffix>
  </Language>

  <Language>
    <Name>Asm</Name>
    <Body_Suffix>.s</Body_Suffix>
  </Language>

  <Language>
    <Name>Asm_Cpp</Name>
    <Body_Suffix>.S</Body_Suffix>
  </Language>

  <Language>
    <Name>Asm2</Name>
    <Body_Suffix>.asm</Body_Suffix>
  </Language>
</GPS>
"""

GPS.parse_xml(XML)
