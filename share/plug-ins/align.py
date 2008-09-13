"""Contextual menu for aligning text

This script provides a number of contextual menus to help align the
text in the selected region following a number of criteria. Some of these
criteria are Ada specific, but could easily be changed for other
languages. The contextual menus that do not apply will not be visible
when editing other languages

 - Aligning on use clauses (Ada specific)
   For example:
       with Ada.Text_IO; use Ada.Text_IO;
       with Foo; use Foo;
   becomes
       with Ada.Text_IO; use Ada.Text_IO;
       with Foo;         use Foo

 - Alignining colons (Any language)
   For example:
       Foo_With_Long_Name   :   Integer;
       Foo : Integer;
   becomes
       Foo_With_Long_Name : Integer;
       Foo                : Integer;

 - Aligning on reserve word 'is' (Ada specific)
   For example:
       type Type_With_Long_Name is new Integer;
       type Foo is new Natural;
   becomes:
      type Type_With_Long_Name is new Integer;
      type Foo                 is new Natural;

 - Aligning Ada formal parameters (Ada specific)
   Aligns the colons, modes and format types in formal parameter specifications
   For example,
      procedure Q( This : in out Integer;
                   That_One : in Float := 0.0;
                   Yet_Another : access Integer;
                   Result : out Integer;
                   Default : Boolean );
   becomes
      procedure Q( This        : in out Integer;
                   That_One    : in     Float := 0.0;
                   Yet_Another : access Integer;
                   Result      :    out Integer;
                   Default     :        Boolean );

 - Aligning arrows (Ada specific)
   aligns the => symbols (this implementation supports only 9 levels
   of arrows).
   For example,
      Call (A => 2,
            Long_Name => 3);
   becomes
      Call (A         => 2,
            Long_Name => 3);

 - Aligning record representation clauses (Ada specific)
   For example,
      for T use
         record
            x at 0 range 0 .. 7;
            yyyy at 12 range   0 .. 7;
            xx at 0 range 0 .. 7;
            k at 12345    range    0 .. 7;
         end record;
   becomes
      for T use
         record
            x    at 0     range 0 .. 7;
            yyyy at 12    range 0 .. 7;
            xx   at 0     range 0 .. 7;
            k    at 12345 range 0 .. 7;
         end record;

 - Aligning assignments (Ada specific)
   For example,
       A := 2;
       Long_Name := 3;
   becomes
       A         := 2;
       Long_Name := 3;
"""


############################################################################
## No user customization below this line
############################################################################

import re
import GPS

def range_align_on (top, bottom, sep, replace_with=None):
   """Align each line from top to bottom, aligning, for each line, sep in
      the same column. For instance:
          a sep b
          long    sep    short
      becomes:
          a    sep b
          long sep short
      sep is a regular expression.
      top and bottom are instances of GPS.EditorLocation
      replace_with is the text that should replace the text matched by sep.
      It can do backward references to parenthesis groups in sep by using the
      usual \1, \2,... strings. All the replacement texts will occupy the same
      length in the editor, that is they will also be aligned.
   """

   if bottom.beginning_of_line() == bottom:
      bottom = bottom.forward_char(-1)

   if not replace_with:
      replace_with = sep
   sep_re = re.compile (sep)
   pos = 0
   replace_len = 0
   line = top.beginning_of_line ()

   while line <= bottom:
      chars   = top.buffer().get_chars (line, line.end_of_line())
      matched = sep_re.search (chars)
      if matched:
         pos = max (pos, len (chars[:matched.start()].rstrip()) + 1)
         try:
            sub = sep_re.sub (replace_with, matched.group())
         except:
            sub = matched.group()
         if sub == " : out ":
            replace_len = max (replace_len, len (sub) + 3)
         else:
            replace_len = max (replace_len, len (sub))
      prev = line
      line = line.forward_line ()
      if prev == line:
         break

   if pos != 0:
     line = top.beginning_of_line ()
     while line <= bottom:
        chars   = top.buffer ().get_chars (line, line.end_of_line())
        matched = sep_re.search (chars)
        if matched:
           width  = pos - len (chars[:matched.start()].rstrip()) - 1
           try:
              sub = sep_re.sub (replace_with, matched.group())
           except:
              sub = matched.group()
           width2 = replace_len - len (sub)

           # special case for out parameters, spaces before
           if sub == " : out ":
              sub = " :    out "
              width2 = width2 - 3

           top.buffer().delete (line, line.end_of_line())
           # do not left-strip if a single char as this will remove the \n
           if len (chars[matched.end():]) == 1:
              top.buffer().insert \
                  (line, chars[:matched.start()].rstrip() \
                      + (' ' * width) + sub + (' ' * width2) \
                      + chars[matched.end():])
           else:
              top.buffer().insert \
                  (line, chars[:matched.start()].rstrip() \
                      + (' ' * width) + sub + (' ' * width2) \
                      + chars[matched.end():].lstrip())
        prev = line
        line = line.forward_line ()
        if prev == line:
           break

def buffer_align_on (sep, replace_with=None, buffer=None):
   """Align the current selection in buffer, based on the separator sep.
      See the description for range_align_on"""
   if not buffer:
      buffer = GPS.EditorBuffer.get ()
   top    = buffer.selection_start ()
   bottom = buffer.selection_end ()
   tmark  = top.create_mark ("top")
   bmark  = bottom.create_mark ("bottom")
   if top == bottom:
      GPS.MDI.dialog ("You must first select the intended text")
      return
   try:
      buffer.start_undo_group ()
      buffer.indent (top, bottom)
      top = buffer.get_mark ("top").location ()
      bottom = buffer.get_mark ("bottom").location ()
      range_align_on (top, bottom, sep, replace_with)
      # re-select the region to be able to call back this routine
      top = buffer.get_mark ("top").location ()
      bottom = buffer.get_mark ("bottom").location ()
      buffer.select (top, bottom)
   finally:
      top.buffer().finish_undo_group ()

def align_colons ():
   """Aligns colons (eg in object and record type declarations"""
   buffer_align_on (sep=":(?!=)", replace_with=" : ")

def align_reserved_is ():
   """Aligns reserved word 'is' (eg in type declarations)"""
   buffer_align_on (sep=" is ")

def align_use_clauses ():
   """Aligns use-clauses occuring in an Ada context clause"""
   buffer_align_on (sep=" use ")

def align_arrows ():
   """Aligns the '=>' symbols"""
   # The algorithm is the following:
   #   - indent the selection
   #   - for N 1 .. 9:
   #      - replace level Nth of => by a special tag @>
   #      - aling on the special tag @> replacing it with =>
   buffer = GPS.EditorBuffer.get ()
   top    = buffer.selection_start ()
   bottom = buffer.selection_end ()
   tmark  = top.create_mark("top")
   bmark  = bottom.create_mark("bottom")
   if top == bottom:
      GPS.MDI.dialog ("You must first select the intended text")
      return
   try:
      buffer.start_undo_group()
      for lr in range(9):
         buffer.indent (top, bottom)
         top = buffer.get_mark ("top").location()
         bottom = buffer.get_mark ("bottom").location()
         chars = buffer.get_chars (top, bottom)
         level = -1
         for k in range(len(chars)):
            if chars[k] == '(':
               level = level + 1
            elif chars[k] == ')':
               level = level - 1
            elif (k + 4 < len(chars)) and (chars[k:k+4] == "case"):
               level = level + 1
            elif (k + 8 < len(chars)) and (chars[k:k+8] == "end case"):
               level = level - 1
            elif (level == lr) and (k + 2 < len(chars)) and (chars[k:k+2] == "=>"):
               chars = chars[:k] + "@>" + chars[k+2:]
         buffer.delete (top, bottom)
         buffer.insert (top, chars)
         tmark = top.create_mark("top")
         bmark = bottom.create_mark("bottom")
         top = buffer.get_mark ("top").location()
         bottom = buffer.get_mark ("bottom").location()
         range_align_on (top, bottom, sep="@>", replace_with=" => ")
   finally:
      top.buffer().finish_undo_group()

def align_assignments ():
   """Aligns the ':=' symbols in selected text"""
   buffer_align_on (sep=":=", replace_with=" := ")

def align_formal_params():
   """Aligns the colons, modes, and formal types in parameter specifications"""
   ## The regexp needs the three nested groups, since we want \\1 to always
   ## returns at least the empty string
   buffer_align_on (sep=":\s*(((in\s+out|out|in|access) )?)",
                    replace_with=" : \\1")

def align_record_rep_clause ():
   """Aligns the various parts of a record representation clause"""
   buffer_align_on (sep=" at ")
   buffer_align_on (sep=" range ")

def on_gps_started (hook_name):
   GPS.parse_xml ("""
     <action name="Align formal parameters" output="none" category="Ada">
        <description>Aligns colons, modes, and types of Ada formal parameters in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_formal_params()</shell>
     </action>
     <contextual action="Align formal parameters" >
        <Title>Align/Formal parameters</Title>
     </contextual>

     <action name="Align colons" output="none" category="Ada">
        <description>Aligns colons and trailing text in current selection</description>
        <filter module="Source_Editor" />
        <shell lang="python">align.align_colons()</shell>
     </action>
     <contextual action="Align colons" >
        <Title>Align/Colons</Title>
     </contextual>

     <action name="Align use clauses" output="none" category="Ada">
        <description>Aligns Ada use-clauses in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_use_clauses()</shell>
     </action>
     <contextual action="Align use clauses" >
        <Title>Align/Use clauses</Title>
     </contextual>

     <action name="Align reserved is" output="none" category="Ada">
        <description>Aligns reserved word 'is' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_reserved_is()</shell>
     </action>
     <contextual action="Align reserved is" >
        <Title>Align/Reserved word 'is'</Title>
     </contextual>

     <action name="Align arrows" output="none" category="Ada">
        <description>Aligns Ada arrow symbol '=>' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_arrows()</shell>
     </action>
     <contextual action="Align arrows" >
        <Title>Align/Arrow symbols</Title>
     </contextual>

     <action name="Align assignments" output="none" category="Ada">
        <description>Aligns Ada assignment symbol ':=' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_assignments()</shell>
     </action>
     <contextual action="Align assignments" >
        <Title>Align/Assignment symbols</Title>
     </contextual>

     <action name="Align record representation clause" output="none" category="Ada">
        <description>Aligns content of record representation clause in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">align.align_record_rep_clause()</shell>
     </action>
     <contextual action="Align record representation clause" >
        <Title>Align/Record representation clause</Title>
     </contextual>

""")

GPS.Hook ("gps_started").add (on_gps_started)
