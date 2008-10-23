"""This file provides various classes to help highlight patterns in
   files.
   The Highlighter class should be considered abstract, and provides
   support for (re)highlighting editors when needed.
   The Regexp_Highlighter is a concreate implementation to highlight
   editors based on regular expressions. One example is for instance
   to highlight tabs or trailing spaces on lines, when this is considered
   improper style:

   Regexp_Highlighter (name="tabs style", regexp="\t+|\s+$",
                       bg_color="#FF7979")

   Another example is to highlight TODO lines. Various conventions exist
   to mark these in the sources, but the following should catch some of
   these:

   Regexp_Highlighter (name="todo", regexp="TODO.*|\?\?\?.*",
                       bg_color="#FF7979")

   Another example is a class to highlight Spark comments. This should
   only be applied when the language is spark

   class Spark_Highlighter (Regexp_Highlighter):
      def must_highlight (self, buffer):
         return buffer.file().language().lower() == "spark"
   Spark_Highlighter (name="spark", regexp="--#.*$", fg_color="red")
"""

from GPS import *

try:
   import gobject
   gobject_available = 1
except:
   gobject_available = 0
   
class Highlighter ():
  """This class provides a way to easily highlight text in an editor.
     When possible, the highlighting is done in the background, in
     which case it is also done on the fly every time the file is
     modified. If pygtk is not available, the highlighting is only done
     when the file is opened or saved"""

  def do_highlight (self, buffer, overlay, start, end):
     """Do the highlighting in the range of text.
        This needs to be overridden to do anything useful"""

     pass

  def must_highlight (self, buffer):
     """Return True if highlighting should be done in this buffer.
        The default is to higlight all buffers, but some highlightings
        might apply only to specific languages for instance"""

     return True

  def __init__ (self, name,
                context_lines=3,
                fg_color="grey", bg_color="",
                weight="",   # or "bold", "normal", "light"
                style="",    # or "normal", "oblique", "italic"
                editable=True): # or None
     """Create a highlighter object.
        This then needs to be attached to one or more buffers through
        the monitor() function below.
        name is the name of the highlighter, and is used in particular
        to create the overlays used for the highlighting. They must be
        unique.
        context_lines is the number of lines (plus or minus) around the
        current location that get refreshed when a local highlighting is
        requested.
        fg_color, bg_color, weight, style, editable are the properties
        of the overlay
     """

     self.buffers = []  # or None if highlighting all buffers
     self.name = name
     self.context_lines = context_lines
     self.fg_color = fg_color
     self.bg_color = bg_color
     self.weight   = weight
     self.style    = style
     self.editable = editable

     Hook ("file_edited").add (self.__do_whole_highlight)
     Hook ("file_saved").add (self.__do_whole_highlight)
     Hook ("file_changed_on_disk").add (self.__do_whole_highlight)
     if gobject_available:
        Hook ("character_added").add (self.__do_context_highlight)

     for l in EditorBuffer.list ():
        self.highlight (l)

  def highlight (self, buffer, loc=None):
     """Refresh highlighting in one specific buffer.
        If loc (an instance of GPS.EditorLocation) is specified, the
        location is only tested refreshed around that line"""

     # Check whether highlighting should be done in this buffer
     if not self.must_highlight (buffer):
        return
     
     over = self.__create_overlay (buffer)

     if loc:
        start = loc
        start.forward_line (-self.context_lines)
        end   = loc
        end.forward_line (self.context_lines)
     else:
        start = buffer.beginning_of_buffer ()
        end   = buffer.end_of_buffer ()

     start = start.beginning_of_line ()
     end   = end.end_of_line ()

     try:
        buffer.remove_overlay (over, start, end)
     except:
        pass

     self.do_highlight (buffer, over, start, end)

  def __create_overlay (self, buffer):
     """Create the overlay to use in a specific buffer, and returns it.
        If the overlay already exists, it is returned as is."""

     over = buffer.create_overlay (self.name) # Return existing one or create
     if not over.__dict__.has_key ('created'):
        if self.fg_color:
           over.set_property ("foreground", self.fg_color)
        if self.bg_color:
           over.set_property ("background", self.bg_color)
        if self.weight:
           over.set_property ("weight", self.weight)
        if self.style:
           over.set_property ("style", self.style)
        over.set_property ("editable", self.editable)
        over.created = True
     return over

  def __do_whole_highlight (self, hook_name, file):
     """Called from GPS hooks to request the refresh of the whole file"""

     self.highlight (EditorBuffer.get (file))

  def __do_context_highlight (self, hook_name, file):
     """Called from GPS hooks to request the refresh of a few lines
        around the cursor. The number of context lines was specified when
        the highlighted was created"""

     buffer = EditorBuffer.get (file)
     self.highlight (buffer, loc=buffer.current_view().cursor())


class Regexp_Highlighter (Highlighter):
  """A specific class of highlighters based on regexps.
     Example of use:
       Regexp_Highlighter ("spark", "--#.*$", fg_color="red")
  """

  def __init__ (self, name, regexp,
                context_lines=3,
                fg_color="grey", bg_color="",
                weight="",   # or "bold", "normal", "light"
                style="",    # or "normal", "oblique", "italic"
                editable=True): # or None
     Highlighter.__init__ (self, name, context_lines=context_lines,
                           fg_color=fg_color, bg_color=bg_color,
                           weight=weight, style=style, editable=editable)
     self.regexp = regexp

  def do_highlight (self, buffer, overlay, start, end):
     """Do the highlighting in the range of text"""
     while True:
        start = start.search (self.regexp, regexp=True, dialog_on_failure=False)
        if not start or start[0] > end:
           return
        buffer.apply_overlay (overlay, start [0], start [1] - 1)
        start = start [1] + 1

