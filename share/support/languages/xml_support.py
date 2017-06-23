"""This file provides enhanced support for editing XML files

It provides a number of new menus, displayed only when an XML file is
being edited:
  - /XML/Check Well formedness
    The document is parsed, and any error is shown in the Locations window
    so that the document can be fixed. This doesn't check the validity with
    a DTD or an XML schema, just basic syntax check.

  - /XML/Escape Selection
    Replace all XML special characters in the current selection (or current
    line) by their matching XML entity (e.g. "&amp;")

  - /XML/Move to ...
    Various submenus to move around the XML file. You can bind key shortcuts
    to these through the /Edit/Key Shortcuts editor.

  - /XML/View as tree
    Open a read-only widget that shows the organization of the XML file
"""

############################################################################
# No user customization below this line
############################################################################

from GPS import EditorBuffer, Console, Locations, XMLViewer, parse_xml
from gps_utils import interactive, in_xml_file
import xml.sax
import xml.sax.handler
import xml.sax.saxutils
import traceback


class StopProcessing (Exception):
    pass


class GPSErrorHandler (xml.sax.handler.ErrorHandler):

    def __init__(self):
        self.output = ''

    def add_error(self, prefix, exception):
        self.output += '%s:%s:%s: %s %s\n' % (
            exception.getSystemId(),
            exception.getLineNumber(),
            exception.getColumnNumber(),
            prefix,
            exception.getMessage())

    def error(self, exception):
        self.add_error('error:', exception)

    def fatalError(self, exception):
        self.add_error('fatal error:', exception)
        raise StopProcessing

    def warning(self, exception):
        self.add_error('warning:', exception)


@interactive('XML', filter=in_xml_file, name='xml check well formedness')
def check_wf():
    """Check whether the current XML document is well-formed"""
    try:
        file = EditorBuffer.get().file()
        handler = xml.sax.handler.ContentHandler()
        errors = GPSErrorHandler()
        xml.sax.parse(file.path, handler, errors)

        Locations.remove_category('XML well-formedness')
        if not errors.output:
            Console().write('Document is well-formed\n')
        else:
            Console().write(errors.output)
            Locations.parse(errors.output, 'XML well-formedness')
    except StopProcessing:
        Locations.parse(errors.output, 'XML well-formedness')
    except xml.sax.SAXParseException:
        Console().write('Unexpected error while parsing the XML document')
    except:
        Console().write('Unexpected error %s' % (traceback.format_exc(), ))


@interactive('XML', filter=in_xml_file, name='xml escape selection')
def quote_selection():
    """
    Replace XML special characters in the current selection (or current
    selection) by their equivalent XML entities.
    """
    buffer = EditorBuffer.get()
    (min, max) = (buffer.selection_start(), buffer.selection_end())
    if min == max:
        (min, max) = (buffer.current_view().cursor().beginning_of_line(),
                      buffer.current_view().cursor().end_of_line())
    with buffer.new_undo_group():
        text = buffer.get_chars(min, max)
        text = xml.sax.saxutils.escape(text)
        buffer.delete(min, max)
        buffer.insert(min, text)


@interactive('XML', filter=in_xml_file, name='xml view as tree')
def view_as_tree():
    try:
        buffer = EditorBuffer.get()
        view = XMLViewer(buffer.file().path)
        view.parse_string(buffer.get_chars())
    except:
        pass


@interactive("XML", filter=in_xml_file, name="XML move to next open tag")
def next_open_tag():
    """Move to the next opening tag"""
    buffer = EditorBuffer.get()
    loc = buffer.current_view().cursor() + 1
    try:
        while loc.get_char() != "<" \
                or (loc + 1).get_char() == "/" \
                or (loc + 1).get_char() == "!":
            loc += 1
        buffer.current_view().goto(loc)
    except:
        # End of file
        pass


@interactive("XML", filter=in_xml_file, name="XML move to next close tag")
def next_close_tag():
    """Move to the next closing tag"""
    buffer = EditorBuffer.get()
    loc = buffer.current_view().cursor() + 1
    try:
        while loc.get_char() != "<" \
                or (loc + 1).get_char() != "/":
            loc += 1
        buffer.current_view().goto(loc)
    except:
        # End of file
        pass


@interactive("XML", filter=in_xml_file, name="XML move to matching close tag")
def goto_matching_tag():
    """Go to the matching closing tag"""
    buffer = EditorBuffer.get()
    loc = buffer.current_view().cursor()
    try:
        while loc.get_char() != '<':
            loc -= 1
        start = loc + 1
        end = start.forward_word()
        loc = start.search('</' + buffer.get_chars(start, end - 1),
                           dialog_on_failure=False,
                           case_sensitive=True)
        if loc:
            buffer.current_view().goto(loc[0])
    except:
        pass


parse_xml('''
  <Language>
    <Name>XML</Name>
    <Body_Suffix>.xml</Body_Suffix>
    <Keywords>(&lt;/?\w+&gt;?)|(/?&gt;)</Keywords>
    <Wordchars>&lt;&gt;/</Wordchars>
    <Context>
      <Comment_Start>&lt;!--</Comment_Start>
      <Comment_End>--&gt;</Comment_End>
      <String_Delimiter>&quot;</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>HTML</Name>
    <Spec_Suffix>.html</Spec_Suffix>
    <Parent>XML</Parent>
  </Language>''')
