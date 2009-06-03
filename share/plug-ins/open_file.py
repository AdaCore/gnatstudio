"""Provides a contextual menu in editors that opens the source file referenced
at the cursor's position.
The file's location may be absolute or relative to the projects source
folders. The file name can be followed by ":" and a line number, to go to
that specific line number.

This contextual menu will work with:
  - include clauses in C-headerfiles
  - Absolute references in the sources.

If the file name includes spaces, you should first select the file name. This
will skip the automatic detection of file name, and take the whole selection
has a file name if such a file is found on the disk.
"""

file_pattern = u'((?:[/\\]?[\w\d._$:-]+)+)(?::(\d+)(?::(\d+))?)?'
# The regexp pattern to search file file:line:column references on the
# current line.

std_include_path = ["/usr/include", "/usr/local/include"]
# Standard search paths for files

#__sep = u'[:\'\" <>*?]'
#file_pattern = "(?:^|" + __sep + ")" \
#  + u'((?:[/\\]?[\w\d._]+)+)(?::(\d+)(?::(\d+))?)?' \
#  + "(?:$|" + __sep + ")"
# A second version of the pattern which only matches inside specific
# separators (or beginning/end of line)


############################################################################
## No user customization below this line
############################################################################

import GPS, re
from os.path import *
from text_utils import *

def open_and_raise_editor (filename, line=0, column=0):
   """Open the editor for filename, raise the window, and display the
      specified line/column if they are specified"""
   ed   = GPS.EditorBuffer.get (GPS.File (filename))
   view = ed.current_view ()
   loc  = GPS.EditorLocation (ed, line=line, column=column)
   view.goto (loc)
   GPS.MDI.get_by_child (view).raise_window()

class OpenFileContextual (GPS.Contextual):
   """A contextual menu to open the file specified at the cursor's location.
      Line numbers are also analyzed when possible ("file:line")"""

   def on_activate (self, context):
      """Called when the contextual menu is activated"""
      open_and_raise_editor (self.file, self.line, self.column)

   def on_filter (self, context):
      """Checks whether the contextual menu should be displayed"""
      if not isinstance (context, GPS.FileContext):
         return False

      ed  = GPS.EditorBuffer.get ()
      (ed, start, end) = get_selection_or_line (ed, context.location ())
      text = ed.get_chars (start, end)

      self.file = ""
      self.line = 0
      self.column = 0

      cursor_col = context.location().column ()

      if ed.selection_end() != ed.selection_start():
         self.file = text   # No post-processing
      else:
         # Try to find the filename we clicked on
         pos = 0
         while pos < len (text):
            m = self.file_pattern.search (text, pos)
            if m and m.start() <= cursor_col and m.end() >= cursor_col:
               self.file = m.group (1)
               if m.group (2):
                  self.line = int (m.group (2))
               if m.group (3):
                  self.column = int (m.group (3))
               break
            pos = m.end()

      if self.file == "":
         return False

      if exists (self.file):
         return True
      else:
         # Let GPS search in all source dirs and predefined paths
         f = GPS.File (self.file)
         if exists (f.name()):
            self.file = f.name ()
            return True

         # Search with just the basename (otherwise "src/file.c" where
         # "src/" is a source_dir would not be found)
         f = GPS.File (basename (self.file))
         if exists (f.name ()):
            self.file = f.name ()
            return True

         # One more try, include standard include paths for C files
         for p in std_include_path:
            f = join (p, self.file)
            if exists (f):
               self.file = f
               return True

      return False

   def on_label (self, context):
      """Returns the label to use for the contextual menu"""
      return "Open <b>"  + basename (self.file) + "</b>"

   def __init__ (self):
      """Initializes the contextual menu"""
      try:
         GPS.Contextual ("OpenFileContextual").destroy()
      except:
         pass

      self.file_pattern = re.compile (file_pattern)
      self.file = ""
      self.line = 0
      self.column = 0
      GPS.Contextual.__init__ (self, "OpenFileContextual")
      self.create (on_activate=self.on_activate,
                   filter=self.on_filter,
                   label=self.on_label)
      self.show()

OpenFileContextual()


