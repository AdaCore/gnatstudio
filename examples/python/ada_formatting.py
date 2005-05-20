from string import *
from misc_text_utils import *
import re
import GPS


# All these functions work on the text in a selection.


# this function aligns use-clauses occuring in a context clause
# for example,
#     with Ada.Text_IO;use Ada.Text_IO;
#     with Foo; use Foo;
# becomes
#     with Ada.Text_IO; use Ada.Text_IO;
#     with Foo;         use Foo;
#
def align_use_clauses ():
   """aligns use-clauses occuring in a context clause"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   max_use_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      use_pos = find(line,'use ')
      if use_pos != -1:
         left_part = line[:use_pos]
         use_pos = len(rstrip(left_part)) + 1
         if use_pos > max_use_pos:
            max_use_pos = use_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_use_pos == 0:
      GPS.MDI.dialog("No use-clauses found to align!")
      return
   #end if
   #print "max use_clause pos is", max_use_pos
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      use_pos = find(line,'use ')
      if use_pos != -1:
         left_part = line[:use_pos]
         right_part = line[use_pos+3:]
         width = max_use_pos - len( rstrip(left_part) )
         replace_line (current_file, rstrip(left_part) + blanks(width) + 'use ' + lstrip(right_part))
      #end if
      current_line = current_line + 1
   #end loop
#end align_use_clauses



# useful for aligning colons in object declarations and record type declarations.
# note that between the colon and the following word only one blank will remain.
def align_colons ():
   """Aligns colons (eg in object and record type declarations"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   # calculate new position for rightmost colon
   max_colon_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1 and find(line,'=') != colon_pos+1: # not assignment ":="
         #print "found colon at ", colon_pos, "  find for '=' returns ", find(line,'=')
         left_part = line[:colon_pos]
         colon_pos = len(rstrip(left_part)) + 1
         if colon_pos > max_colon_pos:
            max_colon_pos = colon_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_colon_pos == 0:
      GPS.MDI.dialog("No colons found to align!")
      return
   #end if
   # Python strings start at zero instead of one so we have to add one back
   max_colon_pos = max_colon_pos + 1
   #print "max colon pos is", max_colon_pos
   # now replace the text up to the colons and the new colons so that the new colons line up
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1:
         left_part = line[:colon_pos]
         right_part = line[colon_pos+1:]
         width = max_colon_pos - len( rstrip(left_part) ) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + ': ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
#end align_colons



def align_reserved_is ():
   """Aligns reserved word 'is' (eg in type declarations)"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   # calculate new position for rightmost colon
   max_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      pos = find(line,' is ')
      if pos != -1:
         left_part = line[:pos]
         pos = len(rstrip(left_part)) + 1
         if pos > max_pos:
            max_pos = pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_pos == 0:
      GPS.MDI.dialog("No reserved words 'is' found to align!")
      return
   #end if
   # Python strings start at zero instead of one so we have to add one back
   max_pos = max_pos + 1
   #print "max pos is", max_pos
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      pos = find(line,' is ')
      if pos != -1:
         left_part = line[:pos]
         right_part = line[pos+3:]
         width = max_pos - len( rstrip(left_part) ) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + 'is ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
#end align_reserved_is





# aligns the colons, modes, and formal types in formal parameter specifications.
# For example,
#
#   procedure Q( This : in out Integer;
#                That_One : in Float := 0.0;
#                Yet_Another : access Integer;
#                Result : out Integer;
#                Default : Boolean );
#
# becomes
#
#   procedure Q( This        : in out Integer;
#                That_One    : in     Float := 0.0;
#                Yet_Another : access Integer;
#                Result      :    out Integer;
#                Default     :        Boolean );
#
def align_formal_params():
   """Aligns the colons, modes, and formal types in formal parameter specifications"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()

   #line up the formal parameter names on the same starting column
   open_paren_pos = -1  #-1 signifies "not found"
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      open_paren_pos = find(line,'(')
      if open_paren_pos != -1:
         paren_line = current_line
         break;
      #end if
      current_line = current_line + 1
   #end while
   if open_paren_pos == -1:
      print "No opening left parenthesis found!"
      return
   #end if
   formal_start_column = open_paren_pos + 1
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      if current_line != paren_line:
         #insert enough blanks to align the formal param name
         replace_line(current_file, blanks(formal_start_column) + lstrip(line) )
      else: #handle the procedure|function|entry too
         left_part = line[:open_paren_pos]
         right_part = line[open_paren_pos:]
         width = formal_start_column - len( rstrip(left_part) ) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while

   # calculate new position for rightmost colon
   max_colon_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1 and find(line,'=') != colon_pos+1: # not assignment ":="
         #print "found colon at ", colon_pos, "  find for '=' returns ", find(line,'=')
         left_part = line[:colon_pos]
         colon_pos = len(rstrip(left_part)) + 1
         if colon_pos > max_colon_pos:
            max_colon_pos = colon_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_colon_pos == 0:
      GPS.MDI.dialog("No colons found to align!")
      return
   #end if
   # Python strings start at zero instead of one so we have to add one back
   max_colon_pos = max_colon_pos + 1
   #print "max colon pos is", max_colon_pos
   # now replace the text up to the colons and the new colons so that the new colons line up
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1:
         left_part = line[:colon_pos]
         right_part = line[colon_pos+1:]
         width = max_colon_pos - len( rstrip(left_part) ) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + ': ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while

   # NOTE : the 'magic numbers' that appear below with max_typemark_pos correspond
   #        to the lengths of the strings used for the modes when the line is replaced

   #find the largest column number for the type-mark
   max_typemark_pos = 0
   current_line = top_line;
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1 and find(line,'=',colon_pos+1) != 0: # not assignment ":="
         pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
         match = re.search (pattern, line)
         n1 = match.group(3)
         n2 = match.group(5)
         n3 = match.group(7)
         if n1 == 'in':
            if n2 == 'out': # mode 'in out'
               max_typemark_pos = 8
            else: # mode 'in'
               if max_typemark_pos == 5: # seen mode 'out' already
                  max_typemark_pos = 8
               elif 4 > max_typemark_pos:
                  max_typemark_pos = 4
               #end if
            #end if
         elif n1 == 'out': # mode 'out'
            if max_typemark_pos == 4: # mode 'in' already detected so indent the 'out'
               max_typemark_pos = 8
            elif 5 > max_typemark_pos: # just mode 'out' seen so far
               max_typemark_pos = 5
            #end if
         elif n1 == 'access':
            max_typemark_pos = 8
         else: # default mode 'in'
            if 1 > max_typemark_pos:
               max_typemark_pos = 1
            #end if
         #end if
      #end if
      current_line = current_line + 1
   #end while

   # In the following block, the silly '+ 1' is retained for clarity, so that the
   # numbers correspond to their uses above

   # now line up the text after the colons, ie the modes and formals' types, etc.
   current_line = top_line;
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      colon_pos = find(line,':')
      if colon_pos != -1 and find(line,'=',colon_pos+1) != 0: # not assignment ":="
         #parse line with before ':' n1 n2 n3
         pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
         match = re.search (pattern, line)
         n1 = match.group(3)
         n2 = match.group(5)
         n3 = match.group(7)
         if n1 == 'in':
            if n2 == 'out': # mode 'in out'
               #parse line with before ':' n1 n2 rest
               pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
               match = re.search (pattern, line)
               before = match.group(1)
               rest = match.group(7)
               replace_line ( current_file, before + ': in out ' + rest )
            else: # mode 'in'
               width = max_typemark_pos - 4 + 1
               #parse line with before ':' n1 rest
               pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
               match = re.search (pattern, line)
               before = match.group(1)
               rest = match.group(5)
               replace_line (current_file,  before + ': in' + blanks(width) + rest)
            #end if
         elif n1 == 'out': # mode 'out'
            if max_typemark_pos == 8: # mode is 'in out' or 'access' for some other param
               width = max_typemark_pos - 8 + 1
               mode = ':    out'
            else:
               width = max_typemark_pos - 5 + 1
               mode = ': out'
            #end if
            #parse line with before ':' n1 rest
            pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
            match = re.search (pattern, line)
            before = match.group(1)
            rest = match.group(5)
            replace_line (current_file, before + mode + blanks(width) + lstrip(rest))
         elif n1 == 'access':
            #parse line with before ':' n1 rest
            pattern = re.compile ("^(.*):([ \t]*)([a-zA-Z0-9_.]+)([ \t]*)(.*)", re.IGNORECASE)
            match = re.search (pattern, line)
            before = match.group(1)
            rest = match.group(5)
            replace_line (current_file, before + ': access ' + rest)
         else: # default mode 'in'
            width = max_typemark_pos - 1 + 1  # where 1 is length of mode string
            before = line[:colon_pos]
            rest = line[colon_pos+1:]
            replace_line (current_file, before + ':' + blanks(width) + lstrip(rest))
         #end if
      #end if
      current_line = current_line + 1
   #end while
#end align_formal_params



# aligns the '=>' symbols
def align_arrows ():
   """Aligns the '=>' symbols"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   # calculate new position for rightmost arrow
   max_arrow_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      arrow_pos = find(line,'=>')
      if arrow_pos != -1:
         left_part = line[:arrow_pos]
         arrow_pos = len(rstrip(left_part)) + 1
         if arrow_pos > max_arrow_pos:
            max_arrow_pos = arrow_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_arrow_pos == 0:
      GPS.MDI.dialog("No arrows found to align!")
      return
   #end if
   # put a blank between the longest LHS and the arrow
   max_arrow_pos = max_arrow_pos + 1
   #print max_arrow_pos
   # now make them line up
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      arrow_pos = find(line,'=>')
      if arrow_pos != -1:
         left_part = line[:arrow_pos]
         right_part = line[arrow_pos+2:]
         width = max_arrow_pos - len(rstrip(left_part)) - 1
         replace_line (current_file, rstrip(left_part) + blanks(width) + '=> ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
#end align_arrows



# aligns the parts of a record representation clause
# for example, makes this:
#
#   for T use
#      record
#         x at 0 range 0 .. 7;
#         yyyy at 12 range   0 .. 7;
#         xx at 0 range 0 .. 7;
#         k at 12345    range    0 .. 7;
#      end record;
#
# become this:
#
#   for T use
#      record
#         x    at 0     range 0 .. 7;
#         yyyy at 12    range 0 .. 7;
#         xx   at 0     range 0 .. 7;
#         k    at 12345 range 0 .. 7;
#      end record;
#
def align_record_rep_clause ():
   """Aligns the various parts of a record representation clause"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   # calculate new position for rightmost arrow
   max_at_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      at_pos = find(line,' at ')
      if at_pos != -1:
         left_part = line[:at_pos]
         at_pos = len(rstrip(left_part)) + 1
         if at_pos > max_at_pos:
            max_at_pos = at_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_at_pos == 0:
      GPS.MDI.dialog("No reserved word 'at' found to align in representation clause!")
      return
   #end if
   # now line up occurrences of " at "
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      at_pos = find(line,' at ')
      if at_pos != -1:
         left_part = line[:at_pos]
         right_part = line[at_pos+4:]
         width = max_at_pos - len(rstrip(left_part)) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + ' at ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
   # do the same for 'range'
   max_range_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      range_pos = find(line,' range ')
      if range_pos != -1:
         left_part = line[:range_pos]
         range_pos = len(rstrip(left_part)) + 1
         if range_pos > max_range_pos:
            max_range_pos = range_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_range_pos == 0:
      GPS.MDI.dialog("No reserved word 'range' found to align in representation clause!")
      return
   #end if
   # now line up occurrences of " range "
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      range_pos = find(line,' range ')
      if range_pos != -1:
         left_part = line[:range_pos]
         right_part = line[range_pos+7:]
         width = max_range_pos - len(rstrip(left_part)) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + ' range ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
   # do the same for '..'
   max_dots_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      dots_pos = find(line,'..')
      if dots_pos != -1:
         left_part = line[:dots_pos]
         dots_pos = len(rstrip(left_part)) + 1
         if dots_pos > max_dots_pos:
            max_dots_pos = dots_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_dots_pos == 0:
      print "No '..' found to align in representation clause!"
      return
   #end if
   # now line up occurrences of " .. "
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      dots_pos = find(line,'..')
      if dots_pos != -1:
         left_part = line[:dots_pos]
         right_part = line[dots_pos+2:]
         width = max_dots_pos - len(rstrip(left_part)) - 1
         replace_line(current_file, rstrip(left_part) + blanks(width) + ' .. ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
#end align_record_rep_clause



# aligns the ':=' symbols in selected text
def align_assignments ():
   """Aligns the ':=' symbols in selected text"""
   try:
      current_file = GPS.current_context().file().name()
   except:
      GPS.MDI.dialog("Cannot get current file name")
      return
   #end try
   try:
      top_line = GPS.current_context().start_line()
   except:
      GPS.MDI.dialog("You must first select the intended text")
      return
   #end try
   bottom_line = GPS.current_context().end_line()
   # calculate new position for rightmost assignment
   max_assignment_pos = 0
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      assignment_pos = find(line,':=')
      if assignment_pos != -1:
         left_part = line[:assignment_pos]
         assignment_pos = len(rstrip(left_part)) + 1
         if assignment_pos > max_assignment_pos:
            max_assignment_pos = assignment_pos
         #end if
      #end if
      current_line = current_line + 1
   #end while
   if max_assignment_pos == 0:
      GPS.MDI.dialog("No assignment symbols found to align!")
      return
   #end if
   # put a blank between the longest LHS and the assignment
   max_assignment_pos = max_assignment_pos + 1
   #print max_assignment_pos
   # now make them line up
   current_line = top_line
   while current_line <= bottom_line:
      GPS.Editor.cursor_set_position (current_file, current_line, 0)
      line = get_line()
      assignment_pos = find(line,':=')
      if assignment_pos != -1:
         left_part = line[:assignment_pos]
         right_part = line[assignment_pos+2:]
         width = max_assignment_pos - len(rstrip(left_part)) - 1
         replace_line (current_file, rstrip(left_part) + blanks(width) + ':= ' + lstrip(right_part) )
      #end if
      current_line = current_line + 1
   #end while
#end align_assignments



GPS.parse_xml ("""
     <action name="Align formal parameters" output="none">
        <description>Aligns colons, modes, and types of Ada formal parameters in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_formal_params()</shell>
     </action>
     <contextual action="Align formal parameters" >
        <Title>Align/Formal parameters</Title>
     </contextual>     

     <action name="Align colons" output="none">
        <description>Aligns colons and trailing text in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_colons()</shell>
     </action>
     <contextual action="Align colons" >
        <Title>Align/Colons</Title>
     </contextual>
     
     <action name="Align use clauses" output="none">
        <description>Aligns Ada use-clauses in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_use_clauses()</shell>
     </action>
     <contextual action="Align use clauses" >
        <Title>Align/Use clauses</Title>
     </contextual>

     <action name="Align reserved is" output="none">
        <description>Aligns reserved word 'is' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_reserved_is()</shell>
     </action>
     <contextual action="Align reserved is" >
        <Title>Align/Reserved word 'is'</Title>
     </contextual>

     <action name="Align arrows" output="none">
        <description>Aligns Ada arrow symbol '=>' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_arrows()</shell>
     </action>
     <contextual action="Align arrows" >
        <Title>Align/Arrow symbols</Title>
     </contextual>

     <action name="Align assignments" output="none">
        <description>Aligns Ada assignment symbol ':=' in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_assignments()</shell>
     </action>
     <contextual action="Align assignments" >
        <Title>Align/Assignment symbols</Title>
     </contextual>

     <action name="Align record representation clause" output="none">
        <description>Aligns content of record representation clause in current selection</description>
        <filter module="Source_Editor" language="ada" />
        <shell lang="python">ada_formatting.align_record_rep_clause()</shell>
     </action>
     <contextual action="Align record representation clause" >
        <Title>Align/Record representation clause</Title>
     </contextual>

""")

