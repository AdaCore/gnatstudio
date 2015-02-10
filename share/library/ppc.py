"""
Adds highlighting of all branches and labels for PPC assembly code
"""


import GPS

XML = r"""<?xml version="1.0"?>
<GPS>
  <Language>
    <Name>PPC Asm</Name>
    <Body_Suffix>.s</Body_Suffix>
    <Keywords>^(b|b(a|c|c(a|ctr|ctrl|l|la|lr|lrl|tr|trl)|d(nz|nz(a|f|fa|fl|fla|flr|flrl|l|la|lr|lrl|t|ta|tl|tla|tlr|tlrl)|z|z(a|f|fa|fl|fla|flr|flrl|l|la|lr|lrl|t|ta|tl|tla|tlr|tlrl))|eq|eq(a|ctr|ctrl|l|la|lr|lrl)|f|fa|fctr|fctrl|fl|fl(a|r)|flrl|ge|gea|gectr|gectrl|gel|gel(a|r)|gelrl|gt|gta|gtctr|gtctrl|gtl|gtl(a|r)|gtlrl|l|la|le|lea|lectr|lectrl|lel|lel(a|r)|lelrl|lr|lrl|lt|lta|lt(ctr|ctrl)|ltl|ltl(a|r)|ltlrl|ne|ne(a|ctr|ctrl|l)|nel(a|r|rl)|ng|ng(a|ctr|ctrl|l)|ngl(a|r|rl)|nl|nla|nlctr|nlctrl|nll|nll(a|r)|nllrl|ns|ns(a|ctr|ctrl|l|l(a|r|rl))|nu|nu(a|ctr|ctrl|l)|nul(a|r|rl)|rlrl|so|so(a|ctr|ctrl)|sol|sol(a|r)|solrl|t|ta|tctr|tctrl|tl|tl(a|r)|tlrl|un|un(a|ctr|ctrl|l)|unl(a|r|rl))|sc|trap|tw(eq|eqi|ge|gei|gt|gti|le|lei|lge|lgei|lgt|lgti|lle|llei|llt|llti|lng|lngi|lnl|lnli|lt|lti|ne|nei|ng|ngi|nl|nli))\b</Keywords>

    <Context>
      <String_Delimiter>&quot;</String_Delimiter>
      <New_Line_Comment_Start>[-\w\d+_:]+:|#</New_Line_Comment_Start>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>

    <Categories>
      <Category>
        <Name>function</Name>
        <Pattern>^([-\w\d+_:]+):$</Pattern>
        <Index>1</Index>
      </Category>
    </Categories>
  </Language>
</GPS>
"""

GPS.parse_xml(XML)
