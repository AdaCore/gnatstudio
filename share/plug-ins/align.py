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
import sys
import GPS
from gps_utils import *

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
   fin = False  # whether formal in are found
   fout = False # whether formal out are found

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
            fout = True
         elif sub == " : in " or sub == " : in out ":
            fin = True
         replace_len = max (replace_len, len (sub))
      prev = line
      line = line.forward_line ()
      if prev == line:
         break

   # special case when in and out are used
   if fin == True and fout == True:
      replace_len = 10

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

           # special case for out parameters, spaces before only if there is
           # also some in parameters.
           if sub == " : out " and fin == True:
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

def get_comas(l):
   n=0
   enabled=True
   res = []
   for k in range(0, len(l)-1):
      if l[k] == '"':
         enabled = not enabled
      elif enabled and l[k] == ',':
         res.append(k)
         n=n+1
   res.insert(0,n)
   return res

def max_min (e1, e2):
   res=[min(e1[0], e2[0])]
   for k in range(1,res[0]+1):
      if k == 1:
         m = max(e1[k],e2[k])
      else:
         m = max(e1[k]+m-e1[k-1],e2[k]+m-e2[k-1])
      res.append(m)
   return res

def in_rw_ada_file (context):
   return in_ada_file(context) and is_writable (context)

@interactive ("Ada", in_rw_ada_file, contextual="Align/Colons",
              name="Align colons")
def align_colons ():
   """Aligns colons (eg in object and record type declarations) and trailing text in current selection"""
   buffer_align_on (sep=":(?!=)", replace_with=" : ")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Comas",
              name="Align comas")
def align_comas ():
   """Aligns comas (eg actual parameters or arguments in pragmas) in current selection"""
   buffer = GPS.EditorBuffer.get ()
   top    = buffer.selection_start ()
   bottom = buffer.selection_end ()
   tmark  = top.create_mark ("top")
   bmark  = bottom.create_mark ("bottom")

   if top == bottom:
      GPS.MDI.dialog ("You must first select the intended text")
      return

   if top.beginning_of_line() != top:
      top = top.beginning_of_line()

   if bottom.beginning_of_line() == bottom:
      bottom = bottom.forward_char(-1)

   try:
      content = []
      data = []
      chars = ""
      buffer.start_undo_group()

      line = top.beginning_of_line ()

      while line <= bottom:
         content.append(top.buffer().get_chars (line, line.end_of_line()))
         line = line.forward_line ()

      for l in content:
         data.append(get_comas(l))
      mm = reduce (max_min, data)

      for l in range(0,len(content)):
         nl=""
         for c in range(0,mm[0]+1):
            if c == 0:
               nl = nl + content[l][:data[l][c+1]+1]
               nl = nl + ' ' * (mm[c+1] - len(nl) + 1)
            elif c == mm[0]:
               nl = nl + content[l][data[l][c]+1:]
            else:
               nl = nl + content[l][data[l][c]+1:data[l][c+1]+1]
               nl = nl + ' ' * (mm[c+1] - len(nl) + 1)
         chars = chars + nl

      buffer.delete (top, bottom)
      buffer.insert (top, chars)
      tloc = buffer.get_mark ("top").location ()
      bloc = buffer.get_mark ("bottom").location ()
      buffer.select (tloc, bloc)
   except:
      GPS.Console ().write (str (sys.exc_info ()) + "\n")
   finally:
      top.buffer().finish_undo_group()

@interactive ("Ada", in_rw_ada_file, contextual="Align/Reserved word 'is'",
              name="Align reserved is")
def align_reserved_is ():
   """Aligns reserved word 'is' (eg in type declarations) in current selection"""
   buffer_align_on (sep=" is ")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Reserved word 'renames'", name="Align reserved renames")
def align_renaming ():
   """Aligns reserved word 'renames' in current selection"""
   buffer_align_on (sep=" renames ")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Use clauses",
              name="Align use clauses")
def align_use_clauses ():
   """Aligns Ada use-clauses in current selection"""
   buffer_align_on (sep=" use ")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Arrow symbols",
              name="Align arrows")
def align_arrows ():
   """Aligns Ada arrow symbol '=>' in current selection"""
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
   found  = False
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
            elif chars[k] == '\n':
               found = False
            elif k + 4 < len(chars) and chars[k:k+4] == "case":
               level = level + 1
            elif k + 8 < len(chars) and chars[k:k+8] == "end case":
               level = level - 1
            elif level == lr and k + 2 < len(chars) and chars[k:k+2] == "=>" and not found:
               chars = chars[:k] + "@>" + chars[k+2:]
               found=True
         buffer.delete (top, bottom)
         buffer.insert (top, chars)
         tmark = top.create_mark("top")
         bmark = bottom.create_mark("bottom")
         top = buffer.get_mark ("top").location()
         bottom = buffer.get_mark ("bottom").location()
         range_align_on (top, bottom, sep="@>", replace_with=" => ")
         top = buffer.get_mark ("top").location ()
         bottom = buffer.get_mark ("bottom").location ()
         buffer.select (top, bottom)
   except:
      GPS.Console ().write (str (sys.exc_info ()) + "\n")
   finally:
      top.buffer().finish_undo_group()

@interactive ("Ada", in_rw_ada_file, contextual="Align/Assignment symbols",
              name="Align assignments")
def align_assignments ():
   """Aligns Ada assignment symbol ':=' in current selection"""
   buffer_align_on (sep=":=", replace_with=" := ")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Formal parameters",
              name="Align formal parameters")
def align_formal_params():
   """Aligns the colons, modes, and formal types in parameter specifications"""
   ## The regexp needs the three nested groups, since we want \\1 to always
   ## returns at least the empty string
   buffer_align_on (sep=":\s*(((in\s+out|out|in|access) )?)",
                    replace_with=" : \\1")

@interactive ("Ada", in_rw_ada_file, contextual="Align/Record representation clause", name="Align record representation clause")
def align_record_rep_clause ():
   """Aligns the various parts of a record representation clause"""
   buffer_align_on (sep=" at ")
   buffer_align_on (sep=" range ")
