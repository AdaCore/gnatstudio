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
  and then select the menu /Edit/More Completion/Complete Block
  This automatically adds "end if;" on the line following the statement
"""


############################################################################
## No user customization below this line
############################################################################

action_name = "Block Completion"
## Name of the action and the menu defined by this package.

import re
import GPS
logger = GPS.Logger("block_complete")

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
    'CAT_IF_STATEMENT':       ['end if;', ''],
    'CAT_CASE_STATEMENT':     ['end case;', ''],
    'CAT_CASE_INSIDE_RECORD': ['end case;', ''],
    'CAT_LOOP_STATEMENT':     [r'end loop \1;', r'\s*([^ ]+)\s*:.*?loop.*'],
    'CAT_RETURN_BLOCK':       [r'end return;', ''],
    'CAT_PROCEDURE':          [r'end \1;', r'.*?procedure\s+([^ \n(]+).*'],
    'CAT_FUNCTION':           [r'end \1;', r'.*?function\s+([^ \n(]+).*'],
    'CAT_DECLARE_BLOCK':      [r'end \1;', r'\s*([^ ]+)\s*:\s*declare.*'],
    'CAT_SIMPLE_BLOCK':       [r'end \1;', r'\s*([^ ]+)\s*:\s*declare.*'],
    'CAT_PACKAGE':            [r'end \2;',
                               r'.*?package\s+(body\s+)?([^ \n]+).*'],
    'CAT_STRUCTURE':          ['end record;', ''],
    'CAT_CLASS':              ['end record;', ''],
    'CAT_PROTECTED':          [r'end \3;',
                               r'\s*protected\s+((body|type)\s+)?([^ \n]+).*'],
    'CAT_TASK':               [r'end \3;',
                               r'\s*task\s+((body|type)\s+)?([^ \n]+).*'],
    'CAT_ENTRY':              [r'end \1;', r'\s*entry\s+([^ \n(]+).*']
}


def on_gps_started(hook_name):
    GPS.Hook("character_added").add(block_complete_on_keyword)
    "Initializes this module."
    init = """<action name='%(action)s' category='Editor'>
      <description>End the current Ada block, by providing the appropriate "end" statement</description>
       <filter language="ada" error='%(action)s requires an Ada file' />
       <shell lang="python" output="none">block_completion.block_complete("%%F");</shell>
    </action>""" % {"action": action_name}
    GPS.parse_xml(init)


def block_complete_on_location(buffer, location):
    # Check if we need to insert a new-line character
    start = GPS.EditorLocation(buffer, location.line(), 1)
    end = GPS.EditorLocation(buffer, location.line(), location.column())

    block = location.block_type()
    logger.log(str(block))

    if block not in BLOCKS_DEFS:
        return

    (term, pattern) = BLOCKS_DEFS[block]

    if pattern != '':
        # Retrieve the line at the start of the block

        start = GPS.EditorLocation(buffer, location.block_start_line(), 1)
        end = location

        bs_content = buffer.get_chars(start, end)
        logger.log(bs_content)

        re_pattern = re.compile(pattern, re.IGNORECASE | re.DOTALL)

        term_re_str = ""
        if re_pattern.match(bs_content):
            term = re_pattern.sub(term, bs_content)
            termarray = term[:-1].split(" ")
            termarray[1] = "({0})?".format(termarray[1])
            term_re_str = ".*?" + r'\s*'.join(termarray + [";"])
        else:
            # The pattern does not match the content, remove the tags
            term = term.replace(r' \1', '')
            term = term.replace(r'\1', '')
            term = term.replace(r' \2', '')
            term = term.replace(r'\2', '')

    if not term_re_str:
        term_re_str = ".*?" + r'\s*'.join(term[:-1].split(" ") + [";"])

    term_re = re.compile(term_re_str, re.IGNORECASE | re.DOTALL)
    end_block_chars = buffer.get_chars(
        location.block_end(), location.block_end().forward_line(-1)
    )

    print term_re_str.__repr__()
    print end_block_chars
    print term_re.match(end_block_chars)

    if not term_re.match(end_block_chars):
        buffer.start_undo_group()

        # A new-line character is inserted if there is some text on the left
        # of the current cursor position.
        if buffer.get_chars(start, end).strip() != "":
            buffer.insert(location, '\n')
            location = location.forward_line()

        buffer.insert(location, term)
        buffer.indent(location, location)
        buffer.finish_undo_group()


def block_complete(filename):
   file = GPS.File(filename)

   # Only Ada language supported
   if file.language().lower() != "ada":
      return

   eb = GPS.EditorBuffer.get (file)
   ev = eb.current_view()
   el = ev.cursor()
   block_complete_on_location(eb, el)

def block_complete_on_keyword(a, b, c):
    print a, b, c
    print "IN BLOCK COMPLETE ON KW"
    ed = GPS.EditorBuffer.get()
    curs = ed.current_view().cursor()
    if chr(c) == "\n":
        kw = ed.get_chars(curs, curs.forward_line(-1)).strip().split(" ")[-1].strip()
        print kw.__repr__()
        if kw in ["begin", "then", "else", "loop"]:
            block_complete_on_location(ed, curs)
            ed.current_view().goto(curs)
            GPS.execute_action("/Edit/Format Selection")
    # block_complete_on_location(ed, ed.current_view().cursor())


GPS.Hook("gps_started").add(on_gps_started)
