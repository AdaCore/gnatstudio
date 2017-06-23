"""This module provides minimally-intrusive Ada 95 reserved word and
construct expansion in GPS.

Specifically, if an abbreviation of a reserved word is either on a line by
itself or follows only a label, that abbreviation is expanded into the
full spelling.  Note that not all reserved words are candidates for
expansion: they must be long enough for expansion to be of use in the
first place.

Additionally, some (not all) constructs that require either a trailing
identifier or trailing reserved word are expanded to include that
identifier or reserved word. These include the following:

   named block statements
   named basic loops
   named while-loops
   named for-loops

   if-then statements
   case statements
   select statements
   record/end-record pairs

Finally, begin-end pairs for program units are expanded to include the
corresponding unit name.

Expansions follow the user's letter casing preferences for reserved words
and identifiers.

A reserved word that is spelled fully does not require expansion of the
word itself, but, for the sake of minimal intrusion, also does not invoke
construct expansion.  Thus a person who types everything will not be
intruded upon except for the case in which a user-defined identifier
matches an abbreviation for a reserved word.  To mitigate this effect the
minimum abbreviation length may be set to a larger value by altering the
variable "min_abbreviation" declared below.


Examples:


A named loop is required to have the loop name follow the
"end loop".  If the user enters this text:

   Foo : loo

and enters the expansion key (the control-space key by default)
it will be expand into the following:

   Foo : loop
      |
   end loop Foo;

where the vertical bar '|' indicates the cursor location after expansion.
The cursor indentation is controlled by the user's syntax indentation
preference.

This expansion is also done for begin-end pairs, For example, if the user
enters:

   procedure Foo is
   beg

it will be expanded into

   procedure Foo is
   begin
      |
   end Foo;

Nested declarations are ignored such that the correct name is used by the
expansion.

For another example, this time without an identifier but with the required
completion:

   if

is expanded into

   if | then
   end if;
"""
import sys
import GPS
import string
import re
from misc_text_utils import replace_line, insert_line, get_line, attempt_up, \
    blanks, up, down
import text_utils


############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Tools/Scripts)

min_abbreviation = 3
# We use the minimum abbreviation length to determine if the word
# should be expanded into a reserved word.  Hence, any word of length less
# than this value will not be expanded.
# This doesn't make reserved word expansion less a "problem" if the user
# perceives it as such, but it does mitigate it somewhat.

action_name = "Conditionally expand Ada syntax"
# Name of the GPS action this module creates. Changing this variable
# has no effect, since the action is created as soon as this module
# is loaded

default_action_key = "primary-h"
# To change the default expansion key, you should go to the menu
# /Edit/Key shortcuts, and select the action action_name in the Ada
# category. Changing the default action_key here has no effect, since
# the default key is set as soon as GPS is loaded


# ################## No user customization below this point ##########

GPS.parse_xml("""
   <action name='""" + action_name + """' output="none" category="Ada">
      <description>Ada syntax-based reserved word and """
              """construct expansion</description>
      <filter module="Source_Editor" language="ada" />
      <shell lang="python">ada_expansion.expand_syntax()</shell>
   </action>
   <key action='""" + action_name + """'>""" + default_action_key + """</key>
""")


def expand_syntax():
    """Expand selected Ada 95 reserved words and syntax."""
    requires_space_key = do_expansion()
    if requires_space_key:
        GPS.Editor.insert_text(' ')

# ################## No public API below this point ##################


def debug(s):
    "comment-out this return statement to enable debugging statements..."
    return
    name = sys._getframe(1).f_code.co_name
    GPS.Console("Messages").write(name + ": " + s + "\n")


def do_expansion():
    """conditionally expands the word just before the space key is hit,
       and returns a boolean indicating whether a space is required to
       be entered afterward"""
    try:
        current_file = GPS.current_context().file().name()
    except:
        # Indicate that a blank character is required since this routine is not
        # going to be doing anything
        return True

    orig_word = ""
    word = ""
    found_colon = False
    new_line = ""

    line_num = GPS.Editor.cursor_get_line(current_file)
    column_num = GPS.Editor.cursor_get_column(current_file)

    line = GPS.Editor.get_chars(current_file, line_num, 0)
    line = string.rstrip(line)

    # we only expand if the cursor is at the correct position
    if column_num != len(line) + 1:  # +1 because GPS columns don't start at 0
        return True

    if string.strip(line) == '':
        return True

    label = potential_label(current_file)
    debug("potential_label() returned '" + label + "'")

    # check for situations like this: "foo : declare"
    first_colon_pos = string.find(line, ':')
    if first_colon_pos == -1:  # didn't find a colon
        orig_word = string.lower(string.strip(line))
        found_colon = False
    else:  # found a colon, maybe an assignment
        pattern = re.compile("^([ \t]*)(.*):(.*)($|--)", re.IGNORECASE)
        match = re.search(pattern, line)
        remainder = match.group(3)
        if string.find(remainder, "=") == 0:
            # found assignment, not just a colon
            return True
        orig_word = string.lower(string.strip(remainder))
        found_colon = True

    if len(orig_word) >= min_abbreviation:
        debug("orig_word is '" + orig_word + "'")
        word = expanded_abbreviation(orig_word, expansion_words)
        debug("expanded word is '" + word + "'")
        if word == '':  # no expansion found
            # we leave it as they typed it since it is not a word of interest
            return True  # so that a space key is emitted
    else:  # allowed to expand but abbreviation was not long enough
        word = orig_word

    if label != '':
        # replace occurrence of label with identifier_case(label) within line
        # note we cannot assign label first since we are searching for it in
        # the call to replace
        line = string.replace(line, label, identifier_case(label))
        label = identifier_case(label)

    new_line = line[:len(line) - len(orig_word)] + word_case(word)
    debug("new_line is '" + new_line + "'")

    # note we cannot prepend the blank to the label before we do the following
    # search
    if found_colon:
        width = string.find(new_line, label)
    else:
        width = len(new_line) - len(word)

    # and now we can prepend the blank to the label for subsequent use
    if label != '':
        label = ' ' + label

    if word == "begin":
        replace_line(current_file, new_line)
        # NOTE: do not 'improve' the following sequence of statements by
        # merging the call to "insert_line(blanks(width + syntax_indent()))"
        # to here, after the call to replace_line(...) above, The reason is
        # that associated_decl() would be affected by the insert_line()
        # effect, in that it would start below the 'begin' we just inserted
        # via the replace_line() call and thus be mislead to give the wrong
        # result.
        if label != '':
            insert_line(blanks(width + syntax_indent()))
            insert_line(blanks(width) + word_case('end') + label + ';')
        else:  # no label, try the decl unit name
            unit_name = associated_decl(current_file)
            if unit_name != '':
                insert_line(blanks(width + syntax_indent()))
                insert_line(blanks(width) + word_case('end') +
                            ' ' + unit_name + ';')
            else:  # no label and no decl unit name
                insert_line(blanks(width + syntax_indent()))
                insert_line(blanks(width) + word_case('end') + ';')
        up()
        text_utils.goto_end_of_line()
        return False

    elif word == "declare":
        replace_line(current_file, new_line)
        insert_line(blanks(width + syntax_indent()))
        insert_line(blanks(width) + word_case('begin'))
        insert_line(blanks(width) + word_case('end') + label + ';')
        up(2)
        text_utils.goto_end_of_line()
        return False

    elif word == "while":
        new_line = new_line + word_case('  loop')
        replace_line(current_file, new_line)
        insert_line(blanks(width) + word_case('end loop') + label + ';')
        GPS.Editor.cursor_set_position(
            current_file, line_num, len(new_line) - 4)
        return False

    elif word == "loop":
        replace_line(current_file, new_line)
        insert_line(blanks(width) + word_case('end loop') + label + ';')
        up()
        text_utils.goto_end_of_line()
        insert_line(blanks(width + syntax_indent()))
        return False

    elif word == "for":
        # expand word here since it must not be an attr def clause
        if within_Ada_statements(current_file):
            new_line = new_line + ' ' + word_case(' loop')
            replace_line(current_file, new_line)
            insert_line(blanks(width) + word_case('end loop') + label + ';')
            # place the cursor at the loop variable declaration
            GPS.Editor.cursor_set_position(
                current_file, line_num, len(new_line) - 4)
            return False

    elif word == "if":
        new_line = new_line + word_case('  then')
        replace_line(current_file, new_line)
        insert_line(blanks(width) + word_case('end if;'))
        GPS.Editor.cursor_set_position(
            current_file, line_num, len(new_line) - 4)
        return False

    elif word == 'case':
        new_line = new_line + word_case('  is')
        replace_line(current_file, new_line)
        insert_line(blanks(width) + word_case('end case;'))
        GPS.Editor.cursor_set_position(
            current_file, line_num, len(new_line) - 2)
        return False

    elif word in ('record', 'select'):
        replace_line(current_file, new_line)
        insert_line(blanks(width + syntax_indent()))
        insert_line(blanks(width) + word_case('end ') + word_case(word) + ';')
        up()
        text_utils.goto_end_of_line()
        return False

    else:
        if word != orig_word:
            # we've expanded the word but it isn't one of the interesting ones
            # above so we just make the expansion take effect
            replace_line(current_file, new_line)
            return True

    return True  # ie emit a blank


# words to expand whenever the trigger key is hit immediately after the word

expansion_words = (
    'abort', 'abstract', 'accept', 'access', 'aliased', 'array', 'begin',
    'case', 'constant', 'declare', 'delay', 'delta', 'digits', 'else',
    'elsif', 'entry', 'exception', 'exit', 'for', 'function', 'generic',
    'if', 'limited', 'loop', 'others', 'package', 'pragma', 'private',
    'procedure', 'protected', 'raise', 'range', 'record', 'renames',
    'requeue', 'return', 'reverse', 'select', 'separate', 'subtype',
    'tagged', 'task', 'terminate', 'type', 'until', 'when', 'while', 'with')


def word_case(word):
    pref = string.lower(GPS.Preference("Ada-Reserved-Casing").get())
    if pref == "upper":
        return string.upper(word)
    elif pref == "mixed":
        return word.title()
    elif pref == "lower":
        return string.lower(word)
    elif pref == "unchanged":
        return word
    else:
        # we punt on Smart_Mixed
        return word


def identifier_case(id):
    pref = string.lower(GPS.Preference("Ada-Ident-Casing").get())
    if pref == "upper":
        return string.upper(id)
    elif pref == "mixed":
        return id.title()
    elif pref == "lower":
        return string.lower(id)
    elif pref == "unchanged":
        return id
    else:
        # we punt on Smart_Mixed
        return id


def associated_decl(current_file):
    original_line_num = GPS.Editor.cursor_get_line(current_file)
    original_column_num = GPS.Editor.cursor_get_column(current_file)

    block_count = 0
    expecting_declaration = False
    result = ""

    # we immediately attempt to go up a line to start searching because
    # we want to skip the line we are manipulating.
    # Note that if we cannot go up initially we return the null string
    # as the result, but that makes sense because this function will
    # never be called in such a case when writing legal Ada code.  For
    # example, legal Ada never has a "begin" on the very first line.
    going_up = attempt_up()
    while going_up:
        prev_line = get_line()
        search_begin_line = word_case(prev_line)
        if string.find(search_begin_line, 'begin') != -1:
            if block_count == 0:
                break
            else:
                block_count = block_count + 1

        elif significant_end(prev_line):
            block_count = block_count - 1
            expecting_declaration = True

        elif found_separated("procedure|function", prev_line):
            if not instantiation(prev_line, current_file):
                if expecting_declaration:
                    # found decl for previously encountered begin/end
                    expecting_declaration = False
                else:  # use this one
                    pattern = re.compile(
                        '^([ \t]*)(procedure|function)([ \t]*)'
                        '([a-zA-Z0-9_."=/<>+\-&*]+)(.*)',
                        re.IGNORECASE | re.DOTALL)
                    match = re.search(pattern, prev_line)
                    result = match.group(4)
                    break

        elif found_separated("task", prev_line):
            # we ignore task declarations
            if found_separated("body", prev_line):
                if expecting_declaration:
                    # found decl for previously encountered begin/end
                    expecting_declaration = False
                else:  # use this one
                    pattern = re.compile(
                        '^([ \t]*)task([ \t]*)body([ \t]*)'
                        '([a-zA-Z0-9_.]+)(.*)',
                        re.IGNORECASE | re.DOTALL)
                    match = re.search(pattern, prev_line)
                    result = match.group(4)
                    break

        elif found_separated("entry", prev_line):
            if expecting_declaration:
                # found decl for previously encountered begin/end
                expecting_declaration = False
            else:
                # use this one
                pattern = re.compile(
                    '^([ \t]*)entry([ \t]*)([a-zA-Z0-9_.]+)(.*)',
                    re.IGNORECASE | re.DOTALL)
                match = re.search(pattern, prev_line)
                result = match.group(3)
                break

        elif found_separated("package", prev_line):
            if found_separated("body", prev_line):
                if expecting_declaration:
                    # found decl for previously encountered begin/end
                    expecting_declaration = False
                else:
                    # use this one
                    pattern = re.compile(
                        '^([ \t]*)package([ \t]*)body([ \t]*)'
                        '([a-zA-Z0-9_.]+)(.*)',
                        re.IGNORECASE | re.DOTALL)
                    match = re.search(pattern, prev_line)
                    result = match.group(4)
                    break

        going_up = attempt_up()
    GPS.Editor.cursor_set_position(
        current_file, original_line_num, original_column_num)
    return identifier_case(result)


def found_separated(word, this_line):
    pattern = re.compile("([ \t]*)(" + word + ")([ \t]*)", re.IGNORECASE)
    match = re.search(pattern, this_line)
    return match is not None


def instantiation(prev_line, current_file):
    original_line_num = GPS.Editor.cursor_get_line(current_file)
    original_column_num = GPS.Editor.cursor_get_column(current_file)
    # check for an instantiation *on the same line* as the subprogram decl
    pattern = re.compile("([ \t]*)is([ \t]*)new(.*)",
                         re.DOTALL | re.IGNORECASE)
    match = re.search(pattern, prev_line)
    if match is not None:
        return True
    # check for instantiation on next line down
    down()
    next_line = get_line()
    if found_separated("new", next_line):
        GPS.Editor.cursor_set_position(
            current_file, original_line_num, original_column_num)
        return True
    else:
        GPS.Editor.cursor_set_position(
            current_file, original_line_num, original_column_num)
    return False


def expanded_abbreviation(word, words):
    if word == "":
        return ""
    for W in words:
        if string.find(W, string.lower(word)) == 0:
            return W
    return ""


def significant_end(this_line):
    """does this_line contain either "end;" or "end <identifier>;"?"""
    target_line = string.lower(this_line)
    if string.find(target_line, 'end;') != -1:
        return True
    pattern = re.compile(
        "^([ \t]*)end([ \t]*)(.*);(.*)($|--)", re.IGNORECASE | re.DOTALL)
    match = re.search(pattern, target_line)
    if match is None:
        return False
    if match.group(3) not in ('loop', 'record', 'if', 'case', 'select'):
        return True
    return False


def within_Ada_statements(current_file):
    line_num = GPS.Editor.cursor_get_line(current_file)
    column_num = GPS.Editor.cursor_get_column(current_file)
    up_count = 0
    result = False
    block_count = 0

    going_up = attempt_up()
    while going_up:
        up_count = up_count + 1
        prev_line = get_line()
        prev_line = string.lower(prev_line)
        if string.find(prev_line, 'begin') != -1:  # found it
            if block_count == 0:
                result = True
                break
            else:
                block_count = block_count + 1
        elif significant_end(prev_line):
            block_count = block_count - 1
        going_up = attempt_up()
    # now return cursor to original position
    GPS.Editor.cursor_set_position(current_file, line_num, column_num)
    debug("returning " + str(result))
    return result


def potential_label(current_file):
    if not within_Ada_statements(current_file):
        return ""
    label = ""
    label_line = get_line()
    label_line = string.rstrip(label_line)  # strip trailing whitespace
    if string.find(label_line, ':') == -1:  # no colon on this line
        # look on the previous line for a stand-alone label, ie "foo :" or
        # "foo:"
        # Rather than go hunting, the label, if any, must be only 1 line up.
        # This will be ok since a label is never the first line of a program
        # unit.
        line_num = GPS.Editor.cursor_get_line(current_file)
        column_num = GPS.Editor.cursor_get_column(current_file)
        going_up = attempt_up()
        if going_up:
            label_line = get_line()
            # found a colon, which might be for a label
            if string.find(label_line, ':') != -1:
                pattern = re.compile(
                    "^([ \t]*)(.*):(.*)", re.IGNORECASE | re.DOTALL)
                match = re.search(pattern, label_line)
                remainder = string.strip(match.group(3))
                if remainder == '':  # right syntax so far
                    temp_label = match.group(2)
                    if temp_label != '':  # found a label
                        label = string.strip(temp_label)

        # now return cursor to original position
        GPS.Editor.cursor_set_position(current_file, line_num, column_num)
    else:  # found ':'
        pattern = re.compile("^([ \t]*)(.*):(.*)", re.IGNORECASE)
        match = re.search(pattern, label_line)
        remainder = string.lstrip(match.group(3))
        label = match.group(2)
        # found assignment operation ":="
        if remainder and remainder[0] == '=':
            debug("returning label '" + label + "'")
            return label

        # Treat as a label, even if it won't be, such as in variable
        # declarations.
        # Since we only use it where allowed, this isn't a problem.
        label = string.strip(label)

    debug("returning label '" + label + "'")
    return label


def syntax_indent():
    # we make this a function so that we will catch any user changes in
    # the preference without having to reload this module
    return GPS.Preference("Ada-Indent-Level").get()
