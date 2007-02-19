"""Provides end-of-block completion for the Ada language

This script defines an action named Block_Completion that, based on
GPS block information, ends the current block with the proper ending
statement.
The current implementation handles the if, case, loop (with optional
label), procedure, function, declare block (with optional label), package
spec and body.

Example of use:
  in an Ada file, type the following (leave the cursor at the '_' character):
      if Foo then
         null;
      _
  and then select the menu /Edit/Complete Block
  This automatically adds "end if;" on the line following the statement
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Scripts)

action_name = "Block Completion"
menu_name   = "/Edit/Complete Block"
## Name of the action and the menu defined by this package.


############################################################################
## No user customization below this line
############################################################################

import re, string, GPS

# BLOCKS_DEFS is a dictionary describing the action to be done for each block.
#
# <block_name> : [<ending_pattern>, <completion pattern>]
#
# <block_name>          : this is the name used internally by GPS
# <ending_pattern>      : the string to use to close the corresponding block.
#                         this string can contain replacement tag \1 \2, see
#                         below
# <completion_patttern> : optional, this is a regular expression pattern to
#                         match against the first line of the block to
#                         retrieve information for creating the final ending
#                         pattern.

BLOCKS_DEFS = {
    'CAT_IF_STATEMENT'   : ['end if;', ''],
    'CAT_CASE_STATEMENT' : ['end case;', ''],
    'CAT_LOOP_STATEMENT' : [r'end loop \1;', r'\s*([^ ]+)\s*:\s*loop.*'],
    'CAT_PROCEDURE'      : [r'end \1;', r'\s*procedure\s+([^ \n]+).*'],
    'CAT_FUNCTION'       : [r'end \1;', r'\s*function\s+([^ \n]+).*'],
    'CAT_DECLARE_BLOCK'  : [r'end \1;', r'\s*([^ ]+)\s*:\s*declare.*'],
    'CAT_PACKAGE'        : [r'end \2;', r'\s*package\s+(body\s+)?([^ \n]+).*'],
    'CAT_STRUCTURE'      : ['end record;', ''],
    'CAT_CLASS'          : ['end record;', ''],
    'CAT_PROTECTED'      : [r'end \2;', r'\s*protected\s+(body\s+)?([^ \n]+).*'],
    'CAT_ENTRY'          : [r'end \1;', r'\s*entry\s+([^ \n]+).*']
    }

def on_gps_started (hook_name):
   "Initializes this module."
   global action_name, menu_name

   init = "<action name='" + action_name + """' category='Editor'>
     <description>End the current Ada block, by providing the appropriate "end" statement</description>
      <filter language="ada"
              error='""" + action_name + """ requires an Ada file' />
      <shell lang="python" output="none">block_completion.block_complete("%F");</shell>
   </action>

   <menu action='""" + action_name + """' before="Refill">
      <title>""" + menu_name + """</title>
   </menu>"""

   GPS.parse_xml (init)

def block_complete_on_location (buffer, location):
    block = location.block_type();

    if not BLOCKS_DEFS.has_key (block):
        return;

    (term, pattern) = BLOCKS_DEFS [block];

    if pattern != '':
        # Retreive the line at the start of the block

        start = GPS.EditorLocation (buffer, location.block_start_line(), 1);
        end = GPS.EditorLocation (buffer, location.block_start_line() + 1, 1);

        bs_content = buffer.get_chars (start, end);

        re_pattern = re.compile (pattern, re.IGNORECASE | re.DOTALL);

        if re_pattern.match (bs_content):
            term = re_pattern.sub (term, bs_content);
        else:
            # The pattern does not macth the content, remove the tags
            term = term.replace (r' \1', '');
            term = term.replace (r'\1', '');
            term = term.replace (r' \2', '');
            term = term.replace (r'\2', '');

    buffer.start_undo_group();
    buffer.insert (location, term);
    buffer.indent (location, location);
    buffer.finish_undo_group();

def block_complete (filename):
    file = GPS.File (filename);

    # Only Ada language supported
    if file.language().lower() != "ada":
        return;

    eb = GPS.EditorBuffer.get (file);
    ev = eb.current_view();
    el = ev.cursor();
    block_complete_on_location (eb, el);

GPS.Hook ("gps_started").add (on_gps_started)
