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
## No user customization below this line
############################################################################

from GPS import *
from gps_utils import *
from os.path import *
import xml.sax, xml.sax.handler, xml.sax.saxutils, traceback

xml_menu = None

class StopProcessing (Exception):
   pass

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
      raise StopProcessing
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
   except StopProcessing:
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

@interactive ("XML", filter=in_xml_file, name="XML move to next open tag")
def next_open_tag ():
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

@interactive ("XML", filter=in_xml_file, name="XML move to next close tag")
def next_close_tag ():
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

@interactive ("XML", filter=in_xml_file, name="XML move to matching close tag")
def goto_matching_tag ():
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
      Action ("XML move to next open tag").menu ("/XML/Move To Next Open Tag")
      Action ("XML move to next close tag").menu ("/XML/Move To Next Close Tag")
      Action ("XML move to matching close tag") \
         .menu ("/XML/Move To Matching Close Tag")

def destroy_xml_menu():
   global xml_menu
   if xml_menu:
      xml_menu.destroy()
      xml_menu = None

def context_changed (hook, context):
   """Called when the current context has changed"""
   try:
      if context.file().language() in ["xml", "html"]:
         create_xml_menu ()
      else:
         destroy_xml_menu ()
   except:
      destroy_xml_menu ()

parse_xml ("""
  <Language>
    <Name>XML</Name>
    <Spec_Suffix>.xml</Spec_Suffix>
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
  </Language>
""")

Hook ("context_changed").add (context_changed)
