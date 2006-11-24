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
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)


############################################################################
## No user customization below this line
############################################################################

from GPS import *
from os.path import *
import xml.sax, xml.sax.handler, xml.sax.saxutils, traceback

xml_menu = None

class GPSErrorHandler (xml.sax.handler.ErrorHandler):
   def __init__ (self):
      self.output = ""
   def add_error (self, prefix, exception):
      self.output = self.output + \
         exception.getSystemId() + ":" +  \
         `exception.getLineNumber()` + ":" + \
         `exception.getColumnNumber()` + ": " + prefix + " " + \
         exception.getMessage() + "\n"
   def error (self, exception):
      self.add_error ("error:", exception)
   def fatalError (self, exception):
      self.add_error ("fatal error:", exception)
   def warning (self, exception):
      self.add_error ("warning:", exception)

def check_wf (menu):
   """Check whether the current XML document is well-formed"""
   try:
      file = EditorBuffer.get ().file()
      handler = xml.sax.handler.ContentHandler ()
      errors  = GPSErrorHandler ()
      tmp = xml.sax.parse (file.name(), handler, errors)

      Locations.remove_category ("XML well-formedness")
      if errors.output == "":
         Console().write ("Document is well-formed\n")
      else:
         Console().write (errors.output)
         Locations.parse (errors.output, "XML well-formedness")
   except xml.sax.SAXParseException, inst:
      Console().write ("Unexpected error while parsing the XML document")
   except:
      Console().write ("Unexpected error " + traceback.format_exc())

def quote_selection (menu):
   """Replace XML special characters in the current selection (or current
      selection) by their equivalent XML entities"""
   buffer = EditorBuffer.get ()
   (min, max) = (buffer.selection_start(), buffer.selection_end())
   if min == max:
      (min, max) = (buffer.current_view().cursor().beginning_of_line(),
                    buffer.current_view().cursor().end_of_line())
   buffer.start_undo_group()
   text = buffer.get_chars (min, max)
   text = xml.sax.saxutils.escape (text)
   buffer.delete (min, max)
   buffer.insert (min, text)
   buffer.end_undo_group()

def view_as_tree (menu):
   try:
     buffer = EditorBuffer.get()
     view = XMLViewer (buffer.file().name())
     view.parse_string (buffer.get_chars())
   except:
     pass

def next_open_tag (menu):
   """Move to the next opening tag"""
   buffer = EditorBuffer.get()
   loc = buffer.current_view().cursor() + 1
   try:
      while loc.get_char() != "<" \
        or (loc + 1).get_char() == "/" \
        or (loc + 1).get_char() == "!":
        loc = loc + 1
      buffer.current_view().goto (loc)
   except:
      # End of file
      pass

def next_close_tag (menu):
   """Move to the next closing tag"""
   buffer = EditorBuffer.get()
   loc = buffer.current_view().cursor() + 1
   try:
      while loc.get_char() != "<" \
        or (loc + 1).get_char() != "/":
        loc = loc + 1
      buffer.current_view().goto (loc)
   except:
      # End of file
      pass

def goto_matching_tag (menu):
   """Go to the matching closing tag"""
   buffer = EditorBuffer.get()
   loc = buffer.current_view().cursor()
   try:
     while loc.get_char() != "<":
       loc = loc - 1
     start = loc + 1;
     end   = start.forward_word()
     loc = start.search ("</" + buffer.get_chars (start, end - 1),
                         dialog_on_failure = False,
                         case_sensitive = True)
     if loc:
        buffer.current_view().goto (loc[0])
   except:
     pass

def create_xml_menu():
   global xml_menu
   if not xml_menu:
      xml_menu = Menu.create ("/XML", ref="Help", add_before=1)
      Menu.create ("/XML/Check Well Formedness", on_activate=check_wf)
      Menu.create ("/XML/Escape Selection", on_activate=quote_selection)
      Menu.create ("/XML/View as Tree", on_activate=view_as_tree)
      Menu.create ("/XML/Move To Next Open Tag", on_activate=next_open_tag)
      Menu.create ("/XML/Move To Next Close Tag", on_activate=next_close_tag)
      Menu.create ("/XML/Move To Matching Close Tag",
                   on_activate=goto_matching_tag)

def destroy_xml_menu():
   global xml_menu
   if xml_menu:
      xml_menu.destroy()
      xml_menu = None

def context_changed (hook, context):
   """Called when the current context has changed"""
   try:
      if context.file().language() == "xml" \
        or context.file().language() == "html":
         create_xml_menu ()
      else:
         destroy_xml_menu ()
   except:
      destroy_xml_menu ()

parse_xml ("""
  <Language>
    <Name>XML</Name>
    <Spec_Suffix>.xml</Spec_Suffix>
    <Keywords>&lt;/?\w+&gt;?</Keywords>
    <Wordchars>&lt;&gt;/</Wordchars>
    <Context>
      <Comment_Start>&lt;!--</Comment_Start>
      <Comment_End>--&gt;</Comment_End>
      <String_Delimiter>"</String_Delimiter>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>True</Case_Sensitive>
    </Context>
  </Language>

  <Language>
    <Name>HTML</Name>
    <Spec_Suffix>.html</Spec_Suffix>
    <Parent>XML</Parent>
  </Language>

  <filter_or name="xml_based">
     <filter language="xml" />
     <filter language="html" />
  </filter_or>

  <action name="XML move to next open tag" category="XML">
     <filter id="xml_based" />
     <shell lang="python">xml_support.next_open_tag (None)</shell>
  </action>
  <action name="XML move to next close tag" category="XML">
     <filter id="xml_based" />
     <shell lang="python">xml_support.next_close_tag (None)</shell>
  </action>
  <action name="XML move to matching close tag" category="XML">
     <filter id="xml_based" />
     <shell lang="python">xml_support.goto_matching_tag (None)</shell>
  </action>
""")

Hook ("context_changed").add (context_changed)

