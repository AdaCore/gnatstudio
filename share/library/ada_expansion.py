"""This module provides Ada construct expansion for selected reserved words.

Expansion only occurs when the supported reserved word is the last word on
the line and the cursor is immediately after it.

The supported constructs and the corresponding reserved words that can be expanded
include the following:

   *  [named] basic loops ('loop')
   *  [named] while-loops ('loop')
   *  [named] for-loops ('loop')
   *  [named] declare-block statements ('declare')
   *  [named] block statements ('begin')

   *  if-then statements ('if' and/or 'then')
   *  case statements ('case' and/or 'is')
   *  select statements ('select')
   *  accept statements ('do')
   *  extended return statements ('do')

   *  record/end-record pairs ('record')
   *  variant parts in record types ('case' and/or 'is')
   *  case-, if-, and declare-expressions (an open parenthesis followed by
      the corresponding reserved word)

Additionally:

   *  'use' will expand into a use-clause matching a with-clause found ending
      on the same line. Each package name in the with-clause is included in
      the use-clause.

   *  'record' in a record representation clause will insert the corresponding
      record type's component names (if any), each with "at  range " appended,
      followed by the "end record" completion

   *  'begin' will expand to include the "end" and corresponding name (if any)

   *  'is' after a program unit spec will expand to include the rest of the declaration
      content

   *  'is' after an entry spec will expand into the 'begin'/'end [name];' completion

   *  'exception' in a handled sequence of statements will expand to include the first
      'when  =>'

   *  'when' will expand to "when  => "

The cursor is positioned appropriately after expansion occurs.

For example, if the user has entered the following text, with the cursor at the
end of the line, as indicated by '|'

   Foo : loop|

and enters the bound action key, the text will be expanded into the following:

   Foo : loop
      |
   end loop Foo;

where the vertical bar '|' indicates the cursor location after expansion
(depending on the user's preference)."""

# ################## No user customization below this point ##########

import sys
import GPS
import re
import block_completion
import libadalang as lal
from collections import namedtuple


def on_gps_started(hook_name):
    "Initializes this module."
    init = """<action name='%(action)s' category='Editor'>
<description>
This module provides Ada construct expansion for selected reserved words.
</description>
<filter language="ada" error='%(action)s requires an Ada file' />
<shell lang="python"
 output="none">ada_expansion.expand_syntax("%%F");</shell>
   </action>""" % {
        "action": action_name
    }
    GPS.parse_xml(init)


Record_Components = namedtuple("Record_Components", "names longest_name_length")


GPS.Preference("Plugins/ada_expansion/explain").create(
    "When no expansion occurs, explain why",
    "boolean",
    """Print a brief explanation in the Messages view.""",
    True,
)


action_name = "Conditionally expand Ada syntax"
# Name of the GNAT Studio action this module creates. Changing this variable
# has no effect, since the action is created as soon as this module
# is loaded

default_action_key = "primary-Return"
# To change the default expansion key, you should go to the menu
# /Edit/Key shortcuts, and select the action action_name in the Ada
# category. Changing the default action_key here has no effect, since
# the default key is set as soon as GNAT Studio is loaded
#
# Although any key can be bound to this action, some version of the
# 'return' key, eg ctrl-return, is good because the 'return' key would
# usually (but not always) be the key normally pressed in the contexts in
# which expansion occurs.


def expand_syntax(filename):
    """Expand selected Ada reserved words."""

    file = GPS.File(filename)
    # Only Ada language supported
    if file.language().lower() != "ada":
        return

    eb = GPS.EditorBuffer.get(file)
    ev = eb.current_view()
    el = ev.cursor()

    do_expansion(eb, el)


# end expand_syntax


# ################## No public API below this point ############################


def do_expansion(buffer, location):
    """conditionally expand the (reserved) word just before the cursor"""

    expansion_candidate = ""
    formatted_line = ""

    input_line = current_line(
        buffer, location
    ).rstrip()  # strip trailing blanks; omit the newline

    if input_line.lstrip() == "":
        explain("There's nothing to expand here.")
        return

    if within_comment(input_line):
        explain("You're in a comment.")
        return

    expansion_candidate = potential_expansion_candidate(input_line)

    if not positioned_for_expansion(location, expansion_candidate, input_line):
        explain(
            "You're not at the end of the line and after the last expandable "
            + "reserved word on the line."
        )
        return

    # change the letter casing of the reserved word in the input_line
    formatted_line = input_line.replace(
        expansion_candidate, reserved_word_cased(expansion_candidate), 1
    )

    # convert to lowercase for sake of comparable values in expand_construct()
    expansion_candidate = expansion_candidate.lower()

    expand_construct(buffer, location, formatted_line, expansion_candidate)


# end do_expansion


BEGIN_BLOCKS = {
    "CAT_PROCEDURE",
    "CAT_FUNCTION",
    "CAT_DECLARE_BLOCK",
    "CAT_SIMPLE_BLOCK",
    "CAT_PACKAGE",
    "CAT_TASK",
    "CAT_ENTRY",
}

# each block in the dictionary contains a regex pattern and a number
# corresponding to the group number in the regex to use to determine
# if the unit is a spec or a body (or a type)
CONCURRENCY_BLOCKS = {
    "CAT_PROTECTED": [1, r"^.*protected\s*(type|body)?.*$"],
    "CAT_TASK": [1, r"^.*task\s*(type|body)?.*$"],
}


def expand_begin(buffer, location, formatted_line):
    """
    emit a new blank line, the 'end [id];', and position
    the cursor on the new blank line.
    """

    if within_declare_expression(buffer, location):
        explain("You're in a declare-expression. They don't have an 'end'.")
        return

    unit = buffer.get_analysis_unit()
    node = unit.root.lookup(lal.Sloc(location.line(), location.column()))
    if not node:
        log_error("could not get analysis node")
        return

    node = node.parent
    completion_name = None

    if isinstance(node, (lal.SubpBody, lal.PackageBody, lal.EntryBody, lal.TaskBody)):
        if isinstance(node, lal.PackageBody):
            completion_name = node.f_package_name.text
        elif isinstance(node, lal.SubpBody):
            completion_name = node.f_subp_spec.f_subp_name.f_name.text
        else:
            completion_name = node.f_name.f_name.text

        # does the name in the "end name;" match the name in the declaration?
        # otherwise wrong "end"
        if node.f_end_name and node.f_end_name.text == completion_name:
            explain("The completion for " + completion_name + " already exists.")
            return

    elif isinstance(node, (lal.BeginBlock, lal.DeclBlock)):
        if isinstance(node.parent, lal.NamedStmt):
            completion_name = node.parent.f_decl.f_name.text

        if (
            not node.f_end_name
        ):  # has no "end name;" but may have "end;" (possibly a parse error)
            if not node.sloc_range.start.line == node.sloc_range.end.line:
                # that is, not just the one word "begin", assume it includes "end;"
                explain(
                    "The completion for this block already exists"
                    + (
                        " (but without the name '" + completion_name + "' included!)."
                        if completion_name
                        else "."
                    )
                )
                return

        if (
            completion_name
            and node.f_end_name
            and node.f_end_name.text == completion_name
        ):
            explain("The completion for " + completion_name + " already exists.")
            return

    if location.block_type() not in BEGIN_BLOCKS:
        explain("That's not a location to expand 'begin'.")
        return

    with buffer.new_undo_group():
        end_word = " " + completion_name if completion_name else ""
        location_on_entry = location
        replace_line(buffer, location, formatted_line)
        buffer.insert(location, "\n")
        location = location.forward_line()
        buffer.insert(location, reserved_word_cased("end") + end_word + ";")
        buffer.indent(location, location)
        buffer.insert(location_on_entry, "\n")
        buffer.indent(location_on_entry, location_on_entry)


# end expand_begin


def expand_is(buffer, location, formatted_line):
    """emit differing completions, depending on what precedes 'is' at the very end."""

    if within_case_expression(buffer, location):
        explain("You're in a case-expression. They don't have an 'end case'.")
        return

    block = location.block_type()
    unit_name = location.block_name()

    with buffer.new_undo_group():
        replace_line(buffer, location, formatted_line)

        if block == "CAT_ENTRY":
            expand_is_within_entry(buffer, location)
            return

        if block == "CAT_PACKAGE":
            expand_package_decl(buffer, location, unit_name)
            return

        if block in ("CAT_PROCEDURE", "CAT_FUNCTION"):
            expand_subrogram_body(buffer, location, unit_name)
            return

        if block in ("CAT_CASE_STATEMENT", "CAT_CASE_INSIDE_RECORD"):
            expand_case_is(buffer, location, formatted_line)
            return

        if block in CONCURRENCY_BLOCKS:
            # use group_num to check for presence of 'body' in pattern
            (group_num, pattern) = CONCURRENCY_BLOCKS[block]
            re_pattern = re.compile(pattern, re.IGNORECASE | re.DOTALL)
            match = re.search(re_pattern, formatted_line)
            if match:
                is_body = (
                    match.group(group_num) and match.group(group_num).lower() == "body"
                )
            else:
                log_error("could not match CONCURRENCY_BLOCKS pattern!")
                return

            if block == "CAT_TASK":
                expand_task_decl(buffer, location, unit_name, is_body)

            elif block == "CAT_PROTECTED":
                expand_protected_decl(buffer, location, unit_name, is_body)


# end expand_is


def expand_package_decl(buffer, location, unit_name):
    # insert 'end [name];'
    buffer.insert(location, "\n" + reserved_word_cased("end ") + unit_name + ";")
    buffer.indent(location, location)
    # insert blank line before completion and indent
    location = location.end_of_line()
    buffer.insert(location, "\n")
    buffer.indent(location, location)


# end expand_package_decl


def expand_subrogram_body(buffer, location, unit_name):
    buffer.insert(location, reserved_word_cased("\nbegin"))
    buffer.indent(location, location)
    location = location.forward_line().end_of_line()
    # insert 'end [name];'
    buffer.insert(location, "\n" + reserved_word_cased("end ") + unit_name + ";")
    buffer.indent(location, location)
    # insert blank line before completion and indent
    location = location.end_of_line()
    buffer.insert(location, "\n")
    buffer.indent(location, location)


# end expand_subrogram_body


def expand_task_decl(buffer, location, unit_name, is_body):
    if is_body:
        buffer.insert(location, reserved_word_cased("\nbegin"))
        buffer.indent(location, location)
        location = location.forward_line().end_of_line()
    # insert 'end [name];'
    buffer.insert(location, "\n" + reserved_word_cased("end ") + unit_name + ";")
    buffer.indent(location, location)
    # insert blank line before completion and indent
    location = location.end_of_line()
    buffer.insert(location, "\n")
    buffer.indent(location, location)


# end expand_task_decl


def expand_protected_decl(buffer, location, unit_name, is_body):
    final_blank_line_location = location.end_of_line()

    if not is_body:
        buffer.insert(location, reserved_word_cased("\nprivate"))
        buffer.indent(location, location)
        location = location.forward_line().end_of_line()

    # insert 'end [name];'
    buffer.insert(location, "\n" + reserved_word_cased("end ") + unit_name + ";")
    buffer.indent(location, location)

    # insert blank line before completion and indent
    location = location.end_of_line()
    buffer.insert(location, "\n")
    buffer.indent(location, location)

    if not is_body:
        # insert final blank line after 'is' and leave cursor there
        buffer.insert(final_blank_line_location, "\n")
        buffer.indent(final_blank_line_location, final_blank_line_location)


# end expand_protected_decl


def expand_is_within_entry(buffer, location):
    """insert a begin-blankline-end after 'is' but without a trailing blank
    line after 'is'"""

    buffer.insert(location, "\n" + reserved_word_cased("begin"))
    buffer.indent(location, location)
    location = location.forward_line().end_of_line()

    block_completion.block_complete_on_location(buffer, location)
    buffer.indent(location, location)

    # insert blank line after 'begin' and leave cursor there
    buffer.insert(location, "\n")
    buffer.indent(location, location)


# end expand_is_within_entry


def expand_declare(buffer, location, formatted_line):
    """emit a new blank line after 'declare', the 'begin', another new blank
    line, and 'end', and then position the cursor between 'declare' and 'begin'
    OR
    for a declare-expression emit the 'begin' and blank line (and nothing else)"""

    within_decl_expr = within_declare_expression(buffer, location)
    initial_location = location

    with buffer.new_undo_group():
        # properly letter-case 'declare' word
        replace_line(buffer, location, formatted_line)

        # insert 'begin' and a blank line in both cases, ie declare-block
        # and declare-expression
        line_after_begin = insertline_reserved_word_and_blank_line(
            buffer, location, "begin"
        )

        # get this here because the above may have invoked indentation
        # this will be the location of the blank line after the initial line
        # containing 'declare'
        last_blank_line = initial_location.end_of_line()

        # Get the location to insert a blank line after 'begin'. We need to do
        # this even though the call to insertline...() emitted a blank line
        # because the completion will replace that blank line (and it is easier
        # to get the blank line after 'begin' to indent this way).
        # Location is still on the initial line so we go to the line after the
        # initial line to get to the end of 'begin' on the next line down.
        blank_after_begin_location = location.forward_line().end_of_line()

        # go to the line after the line containing the 'begin'
        location = line_after_begin

        if not within_decl_expr:  # emit the completion too
            # generate the completion
            block_completion.block_complete_on_location(buffer, location.end_of_line())
            # insert the blank line after 'begin' that the completion replaced
            buffer.insert(blank_after_begin_location, "\n")
            buffer.indent(blank_after_begin_location, blank_after_begin_location)

        # insert the blank line after 'declare' and leave the cursor there
        buffer.insert(last_blank_line, "\n")
        buffer.indent(last_blank_line, last_blank_line)


# end expand_declare


def expand_while(buffer, location, formatted_line):
    """emit the 'loop', a new blank line, and the "end loop", then position the
    cursor between 'while' and 'loop'"""

    with buffer.new_undo_group():
        complete_with_new_line_and_cursor(buffer, location, formatted_line, "loop")


# end expand_while


def expand_if(buffer, location, formatted_line):
    """emit the 'then', a new blank line, and the "end if;" and then position
    the cursor between 'if' and 'then'
    unless this is a if-expression where we just append the 'then'"""

    with buffer.new_undo_group():
        if within_if_expression(buffer, location):
            condition_line = location.line()
            condition_col = location.column() + 1
            formatted_line = formatted_line + "  then"
            replace_line(buffer, location, formatted_line)
            buffer.current_view().goto(buffer.at(condition_line, condition_col))
        else:
            complete_with_new_line_and_cursor(buffer, location, formatted_line, "then")


# end expand_if


def expand_then(buffer, location, formatted_line):
    """emit a new blank line, and the "end if;" and then position
    the cursor on the blank line"""

    if within_if_expression(buffer, location):
        explain("You're in an if-expression. They don't have an 'end if'.")
        return

    with buffer.new_undo_group():
        complete_with_new_line(buffer, location, formatted_line)


# end expand_then


def expand_case(buffer, location, formatted_line):
    """emit the 'is', a new blank line, and the "end case;" and then
    position the cursor between 'case' and 'is', unless this is a case-expression
    where we just emit the 'is'"""

    extension_text = reserved_word_cased(" is")

    with buffer.new_undo_group():
        if within_case_expression(buffer, location):
            # append one of the two blanks used for the final cursor position
            replace_line(buffer, location, formatted_line + " ")
            location = location.end_of_line()
            # create a mark for the final cursor pos after the new blank
            cursor_pos = location.create_mark()
            # append the extension text on the same line
            buffer.insert(location, extension_text)
            # move the cursor to the blanks inserted before the appended extension_word
            buffer.current_view().goto(cursor_pos.location())
        else:
            # append one of the two blanks used for the final cursor position
            replace_line(buffer, location, formatted_line + " ")
            location = location.end_of_line()
            # create a mark for the final cursor pos after the new blank
            cursor_pos = location.create_mark()
            # append the extension text on the same line
            buffer.insert(location, extension_text)
            # get the location for the new blank line
            newline_eol = location.end_of_line()
            block_completion.block_complete_on_location(buffer, newline_eol)
            # insert the case-alternative line and indent, at the new line's location
            buffer.insert(newline_eol, "\nwhen  =>")
            buffer.indent(newline_eol, newline_eol)
            # move the cursor to the blanks inserted before the appended extension_word
            buffer.current_view().goto(cursor_pos.location())


# end expand_case


def expand_case_is(buffer, location, formatted_line):
    """The "is" is already present and the cursor is after the 'is', so we emit
    a new line containing "when  => ", and another line containing the "end case;"
    and then position the cursor between 'when' and '=>'. Note that the check
    for a case-expression has already been made."""

    with buffer.new_undo_group():
        replace_line(buffer, location, formatted_line)
        location = location.end_of_line()
        block_completion.block_complete_on_location(buffer, location)
        # insert the case-alternative line and indent
        buffer.insert(location, "\nwhen ")
        buffer.indent(location, location)
        location = location.forward_line().end_of_line()
        # create a mark for the final cursor pos after the 'when '
        cursor_pos = location.create_mark()
        buffer.insert(location, " => ")
        # move the cursor to the blanks inserted before the appended extension_word
        buffer.current_view().goto(cursor_pos.location())


# end expand_case_is


def expand_when(buffer, location, formatted_line):
    """if we are in an appropriate context, eg case statement, emit the 'when', a blank,
    and the arrow" and then position the cursor between 'when' and the arrow"""

    if not within_context_for_when_arrow(buffer, location):
        explain("This is not a context for expanding 'when' into 'when =>'.")
        return

    with buffer.new_undo_group():
        condition_line = location.line()
        formatted_line = formatted_line + "  => "
        replace_line(buffer, location, formatted_line)
        condition_col = (
            formatted_line.lower().rfind("when") + 6
        )  # ie + len('when ') + 1
        buffer.current_view().goto(buffer.at(condition_line, condition_col))


# end expand_when


def expand_select(buffer, location, formatted_line):
    """emit a blank line followed by "end select;" and then position
    the cursor on the blank line"""
    # NB: block_complete_on_location() does not support select statements

    complete_with_new_line_and_end_word(
        buffer, location, formatted_line, reserved_word_cased("select")
    )


# end expand_select


def expand_do(buffer, location, formatted_line):
    """emit the completion for extended_return and accept statements,
    including a new blank line, and position
    the cursor at the end of that new line"""

    # Note block_complete_on_location() handled extended returns but not
    # accept statements. Therefore, if this is an extended_return_statement
    # then we just use the block_completion routine.
    # Otherwise, for accept statements, we must do it here, manually.

    unit = buffer.get_analysis_unit()
    node = unit.root.lookup(lal.Sloc(location.line(), location.column()))

    if not node:
        log_error("could not get analysis node")
        return

    with buffer.new_undo_group():
        if isinstance(node, lal.ExtendedReturnStmt):
            replace_line(buffer, location, formatted_line)
            end = location.end_of_line()
            complete_with_new_line(buffer, end, formatted_line)

        elif isinstance(node, lal.AcceptStmtWithStmts):
            complete_with_new_line_and_end_word(
                buffer, location, formatted_line, node.f_end_name
            )

        else:
            # try regex match for accept statement
            pattern = re.compile(r"^\s*accept\s*(\w*)\s*.*do\s*.*$", re.IGNORECASE)
            match = re.search(pattern, formatted_line)
            if match:
                end_name = match.group(1)
                complete_with_new_line_and_end_word(
                    buffer, location, formatted_line, end_name
                )
                return

            # try regex match for extended return statement
            pattern = re.compile(r"^\s*return\s*.*\s*.*do\s*.*$", re.IGNORECASE)
            match = re.search(pattern, formatted_line)
            if match:
                complete_with_new_line_and_end_word(
                    buffer, location, formatted_line, reserved_word_cased("return")
                )
            else:
                log_error("no libadalang or regex match")


# end expand_do


def expand_exception(buffer, location, formatted_line):
    """Expand 'exception' into reserved word cased format and new line on the
    next line down containing 'when  => ' with the cursor between 'when' and arrow"""
    unit = buffer.get_analysis_unit()
    if not unit:
        log_error("could not get node analysis unit")
        return

    node = unit.root.find(
        lambda x: x.sloc_range.start.line <= location.line()
        and isinstance(x, lal.HandledStmts)
    )

    if not node:
        explain("You're not in a handled sequence of statements.")
        return

    with buffer.new_undo_group():
        replace_line(buffer, location, formatted_line)
        location = location.end_of_line()
        buffer.insert(location, "\nwhen ")
        buffer.indent(location, location)
        location = location.forward_line().end_of_line()
        # create a mark for the final cursor pos after the 'when '
        cursor_pos = location.create_mark()
        buffer.insert(location, " => ")
        # move the cursor to the blanks inserted before the appended extension_word
        buffer.current_view().goto(cursor_pos.location())


# end expand_exception


def expand_use(buffer, location, formatted_line):
    """if there is a with-clause on the same line, append a use-clause for all
    those package names to the end of the same line"""

    start_of_line = buffer.at(location.line(), 1)

    unit = buffer.get_analysis_unit()
    with_node = unit.root.find(
        lambda x: x.sloc_range.end.line == location.line()
        and isinstance(x, lal.WithClause)
    )

    if not with_node:
        explain("You're not on the same line as (the end of) a with-clause.")
        return

    formatted_line = formatted_line + " "  # blank after the 'use'

    # append the unit names and commas
    count = 1
    for unit_name in with_node.f_packages:
        formatted_line = formatted_line + unit_name.text
        if count != len(with_node.f_packages):
            formatted_line = formatted_line + ", "
            count += 1

    formatted_line = formatted_line + ";"

    with buffer.new_undo_group():
        replace_line(buffer, start_of_line, formatted_line)


# end expand_use


def expand_record(buffer, location, formatted_line):
    """expand 'record' into either a simple completion and blank line, for a regular
    record type declaration, or, in the case of a record representation clause for an
    existing record type, into the individual component clauses, one per record
    component (including discriminants), followed by the completion"""

    unit = buffer.get_analysis_unit()
    if not unit:
        log_error("Could not get node analysis unit")
        return

    with buffer.new_undo_group():
        if unit.root.find(
            lambda x: isinstance(x, lal.NullRecordDef)
            and x.sloc_range.end.line == location.line()
        ):
            buffer.insert(location, ";")
            return

        node = unit.root.lookup(lal.Sloc(location.line(), location.column()))
        if isinstance(node, lal.Aggregate):
            explain("You're in a null record aggregate")
            return

        # for all other cases, emit the completion
        buffer.insert(location, "\n" + reserved_word_cased("end record;"))
        buffer.indent(location, location)

        # having emitted the completion, the lal parser can now recognize whether we
        # are expanding a RecordRepClause, but only if we first reparse the buffer!
        unit = buffer.get_analysis_unit()

        rep_clause = unit.root.find(
            lambda x: x.sloc_range.start.line == location.line()
            and isinstance(x, lal.RecordRepClause)
        )

        if not rep_clause:
            # just insert the blank line for a record type decl
            buffer.insert(location, "\n")
            buffer.indent(location, location)
            location = location.end_of_line()
            return

        record_type_name = rep_clause.f_name.text
        type_decl = unit.root.find(
            lambda x: isinstance(x, lal.BaseTypeDecl)
            and x.f_name.text == record_type_name
        )
        if not type_decl:
            explain(
                "Could not find declaration for record type '"
                + record_type_name
                + "' in this compilation unit. Does it exist?"
            )
            return

        components = all_record_components(record_type_name, type_decl)
        if not components:
            log_error(
                "Record representation clause for type '"
                + record_type_name
                + "' is incomplete."
            )
            return

        if not components.names:  # a null record
            explain(
                "Record type '"
                + record_type_name
                + "' has no components. "
                + "This expansion is legal but of questionable value."
            )
            return

        process_record_rep_clause(buffer, location, components)


# end expand_record


def process_record_rep_clause(buffer, location, components):
    """insert all the corresponding record type's components, if any (could be
    a null record), each with "at record" appended"""

    first_component = True
    for name in components.names:
        buffer.insert(
            location,
            "\n"
            + name
            + blanks(components.longest_name_length - len(name))
            + reserved_word_cased(" at  range "),
        )
        buffer.indent(location, location)
        if first_component:
            # get the possibly indented line containing the first component
            # clause inserted
            newline = current_line(buffer, location.forward_line())
            # create the mark for cursor placement after expansion completes
            column = newline.lower().find("range")
            cursor_pos = buffer.at(location.line() + 1, column).create_mark()
            first_component = False
        location = location.forward_line().end_of_line()
    # move the cursor to between 'at' and 'range' on the first such line
    buffer.current_view().goto(cursor_pos.location())


# end process_record_rep_clause


def all_record_components(record_type_name, type_decl):
    """return the list of component names for a record type with the name specified,
    along with the length of the longest of these names"""

    names = []

    if type_decl.p_discriminants_list:
        # append those discriminants' names to results
        for discrim in type_decl.p_discriminants_list:
            for id in discrim.f_ids:
                names.append(id.text)

    # get the base type's RecordTypeDef to get the remaining components
    for child in type_decl.children:
        if child:
            if isinstance(child, lal.RecordTypeDef):
                type_def = child
                break

    if not type_def:
        log_error(
            "Libadalang could not find type definition for record type '"
            + record_type_name
            + "'"
        )
        return None

    # get the record type's component names, if any components defined
    # (could be a null record)
    components = type_def.f_record_def.f_components
    for c in components.f_components:  # a list of ComponentDecl
        for id in c.f_ids:
            names.append(id.text)

    longest_name_length = max([len(name) for name in names]) if names else 0

    return Record_Components(names, longest_name_length)


# end all_record_components


def complete_with_new_line(buffer, location, formatted_line):
    """emit the completion, including a new blank line, and position
    the cursor at the end of that new line"""

    # we don't need label and expansion_candidate args since we use
    # the block_completion action.
    # BUT note that the action doesn't handle labels not on the same
    # line as the expansion word so we may want to do it manually
    # anyway, in which case the args will be needed

    with buffer.new_undo_group():
        # Replace the existing line in order to put the label, if
        # any, and the reserved word into the proper casing
        replace_line(buffer, location, formatted_line)
        # generate the completion
        block_completion.block_complete_on_location(buffer, location.end_of_line())
        # insert the blank line and indent, ready for user input
        buffer.insert(location, "\n")
        buffer.indent(location, location)


# end complete_with_new_line


def complete_with_new_line_and_cursor(buffer, location, new_line, extension_word):
    """Eg, for a 'while' candidate, append the 'loop' and replace the existing line,
    emit a new blank line, and the "end loop", then position the cursor between the
    'while' and 'loop'"""

    extension_text = reserved_word_cased(" " + extension_word)

    # append one of the two blanks used for the final cursor position
    replace_line(buffer, location, new_line + " ")
    location = location.end_of_line()
    # create a mark for the final cursor pos after the new blank
    cursor_pos = location.create_mark()
    # append the extension text on the same line
    buffer.insert(location, extension_text)
    # get the location for the new blank line
    newline_eol = location.end_of_line()
    block_completion.block_complete_on_location(buffer, newline_eol)
    # insert the blank line and indent, at the new line's location
    buffer.insert(newline_eol, "\n")
    buffer.indent(newline_eol, newline_eol)
    # move the cursor to the blanks inserted before the appended extension_word
    buffer.current_view().goto(cursor_pos.location())


# end complete_with_new_line_and_cursor


def complete_with_new_line_and_end_word(buffer, location, formatted_line, word):
    """Expand a block that ends with 'end <word>;' and put the cursor on
    the blank line inserted. Note that <word> could be either a
    reserved word such as 'select' or it could be an identifier, eg the
    entry name in an accept statement."""

    location_on_entry = location

    with buffer.new_undo_group():
        replace_line(buffer, location, formatted_line)
        buffer.insert(location, "\n")
        location = location.forward_line()
        buffer.insert(location, reserved_word_cased("end ") + word + ";")
        buffer.indent(location, location)
        buffer.insert(location_on_entry, "\n")
        buffer.indent(location_on_entry, location_on_entry)


# complete_with_new_line_and_end_word


expanders = {
    "begin": expand_begin,
    "declare": expand_declare,
    "while": expand_while,
    "loop": complete_with_new_line,
    "if": expand_if,
    "then": expand_then,
    "case": expand_case,
    "record": expand_record,
    "select": expand_select,
    "do": expand_do,
    "is": expand_is,
    "use": expand_use,
    "when": expand_when,
    "exception": expand_exception,
}


def expand_construct(buffer, location, formatted_line, expansion_candidate):
    """conditionally expand the construct indicated by the candidate reserved word"""

    handler = expanders.get(expansion_candidate, None)
    if handler:
        handler(buffer, location, formatted_line)
    else:
        explain("'" + expansion_candidate + "' is not expandable.")


# end expand_construct


def insertline_reserved_word_and_blank_line(buffer, location, reserved_word):
    """insert the reserved word on a new line and then a blank line after that"""

    # insert reserved_word
    buffer.insert(location, "\n" + reserved_word_cased(reserved_word))
    buffer.indent(location, location)

    # insert blank line after reserved_word (which is not the final blank line inserted)
    location = location.forward_line().end_of_line()
    buffer.insert(location, "\n")
    buffer.indent(location, location)

    return location.forward_line()


# end insertline_reserved_word_and_blank_line


def positioned_for_expansion(location, word, input_line):
    """location must be immediately after to the last word on the line and
    at the end of the line"""

    word_start = input_line.rfind(word)
    if not word_start == -1:
        return (
            location.column() == word_start + len(word) + 1
            and location.column() == len(input_line) + 1
        )
    else:  # didn't find word
        return False


# end positioned_for_expansion


def within_declare_expression(buffer, location):
    return within_statement_expression(lal.DeclExpr, buffer, location)


# end within_declare_expression


def within_case_expression(buffer, location):
    return within_statement_expression(lal.CaseExpr, buffer, location)


# end within_case_expression


def within_if_expression(buffer, location):
    return within_statement_expression(lal.IfExpr, buffer, location)


# end within_if_expression


def within_comment(line):
    # since we only expand the word at the end of the line, we needn't reverse
    # the loop: a comment starting anywhere on the line will always include the
    # potential expansion word

    parts = line.strip().split()
    for word in parts:
        if word.startswith("--"):
            return True

    return False


# end within_comment


def within_statement_expression(category, buffer, location):
    MAP = {lal.IfExpr: "if", lal.DeclExpr: "declare", lal.CaseExpr: "case"}

    unit = buffer.get_analysis_unit()
    node = unit.root.lookup(lal.Sloc(location.line(), location.column()))

    if not node:
        log_error("could not get analysis node")
        return False

    if isinstance(node, category):
        return True

    # For such things as "(if" and "(case" the node kind can be Aggregate
    # when not lal.IfExpr and so on, depending on the state of the source text.
    #
    # Therefore, use lal to to scan the tokens for the opening left parenthesis
    # followed eventually by the reserved_word. We skip the only allowed tokens
    # between '(' and the if/declare/case reserved word.
    token = node.token_start
    while token:
        if token.text == "(":
            break
        if token.sloc_range.end.line > location.line():
            break
        token = token.next

    if not token or token.text != "(":
        return False
        # no '(' found so not an if/declare/case-expression

    token = token.next

    reserved_word = MAP[category]

    # found '(', now the next non-trivia token must be the reserved word
    while token and token.sloc_range.end.line <= location.line():
        if token.text.lower() == reserved_word:
            return True
        elif token.is_trivia:
            token = token.next
        else:
            return False

    return False


# end within_statement_expression


def within_context_for_when_arrow(buffer, location):
    unit = buffer.get_analysis_unit()
    if not unit:
        log_error("could not get node analysis unit")
        return False

    node = unit.root.find(
        lambda x: x.sloc_range.start.line == location.line()
        and isinstance(
            x,
            (
                lal.CaseStmt,
                lal.CaseStmtAlternative,
                lal.CaseExpr,
                lal.CaseExprAlternative,
                lal.VariantPart,
                lal.ExceptionHandler,
                lal.SelectStmt,
                lal.SelectWhenPart,
            ),
        )
    )
    if node:
        return True

    # sometimes lal doesn't recognize the context so we have to fall back

    block = location.block_type()

    if block in (
        "CAT_CASE_STATEMENT",
        "CAT_CASE_INSIDE_RECORD",
        "CAT_EXCEPTION_HANDLER",
        "CAT_SELECT_STATEMENT",
    ):
        return True

    # do it the hard way, by parsing tokens...
    if within_case_expression(buffer, location):
        return True

    return False


# end within_context_for_when_arrow


def potential_expansion_candidate(line):
    """last word on the line."""

    last_nonblank = line.strip().split()[-1]
    pattern = re.compile(r"(\W*)(\w*)", re.IGNORECASE)
    match = re.search(pattern, last_nonblank)
    if match:
        return match.group(2)
    else:
        return ""


# end potential_expansion_candidate


def indentation_pref():
    return GPS.Preference("Ada-Indent-Level").get()


# end indentation_pref


def reserved_word_cased(word):
    pref = str.lower(GPS.Preference("Ada-Reserved-Casing").get())
    if pref == "upper":
        return word.upper()
    elif pref == "mixed":
        return word.title()
    elif pref == "lower":
        return word.lower()
    elif pref == "unchanged":
        return word
    elif pref == "smart_mixed":
        # we punt on Smart_Mixed
        return word

    log_error("unrecognized Ada-Reserved-Casing pref " + pref)
    # and then fall back to returning the input
    return word


# end reserved_word_cased


def identifier_cased(word):
    pref = str.lower(GPS.Preference("Ada-Ident-Casing").get())
    if pref == "upper":
        return word.upper()
    elif pref == "mixed":
        return word.title()
    elif pref == "lower":
        return word.lower()
    elif pref == "unchanged":
        return word
    elif pref == "smart_mixed":
        # we punt on Smart_Mixed
        return word

    log_error("unrecognized Ada-Ident-Casing pref " + pref)
    # and then fall back to returning the input
    return word


# end identifier_cased


def explain(s):
    if GPS.Preference("Plugins/ada_expansion/explain").get():
        GPS.Console("Messages").write(action_name + ": " + s + "\n", mode="text")


# end explain


def log_error(s):
    name = sys._getframe(1).f_code.co_name
    GPS.Console("Messages").write(
        action_name + ": " + name + " : " + s + "\n", mode="text"
    )


# end log_error


def replace_line(buffer, location, new_line):
    start = location.beginning_of_line()
    end = location.end_of_line()
    buffer.delete(start, end - 1)
    buffer.insert(start, new_line)


# end replace_line


def current_line(buffer, location):
    start = buffer.at(location.line(), 1)
    end = buffer.at(location.line(), location.end_of_line().column())
    line = buffer.get_chars(start, end - 1)  # omit the newline
    return line


# end current_line


def blanks(count):
    return count * " "


# end blanks


#  debugging ######################################################################


def debug(s):
    # return
    name = sys._getframe(1).f_code.co_name
    GPS.Console("Messages").write(name + ": " + s + "\n", mode="text")


# end debug


def debug_string(text, s):
    GPS.Console("Messages").write(text + " '" + s + "'\n", mode="text")


# end debug_string


def debug_location(text, location):
    debug(text + " => " + str(location.line()) + ", " + str(location.column()))


# end debug_location


#  debugging ######################################################################


GPS.Hook("gps_started").add(on_gps_started)
