""" When the cursor stops on an entity in an editor, this plugin highlights
    all local references to this entity.

"""


############################################################################
## no user customization below this line
############################################################################

import GPS
from gps_utils import *

# Constants

LINES_IN_ONE_BATCH=2 # Number of lines to process in one batch
TIMEOUT=50           # Interval (in milliseconds) between two batches

default_colors =  {
  "label"      : "lightblue",
  "literal"    : "lightblue",
  "object"     : "#ffbeee",
  "subprogram" : "#ffcf90",
  "packgage/namespace" : "lightgreen",
  "type"               : "lightgreen",
  "unknown"    : "lightblue" }

editor_location_styles = {}

# Register some preferences

# Whether to display occurrences in the speed bar
GPS.Preference ("Plugins/auto_highlight_occurrences/speedbar").create (
      "Show in speedbar", "boolean",
      "Whether to display the matches in the speed bar in editors."
      " You must restart gps to take changes into account.",
      False)

# The default colors
for k in default_colors:
    pref_name = ("Plugins/auto_highlight_occurrences/color_" +
                  k.replace("/", "_"))
    GPS.Preference (pref_name).create (
          "Highlight color for " + k, "color",
          "color used to highlight matching occurrences."
          " You must restart gps to take changes into account",
          default_colors[k])

# The main class
class LocationHighlighter:
    """ Class to handle the highlighting of local occurrences. """

    def process_one_line (self, line):
        """ Look at one line, and highlight results if necessary
        """

        # Get the string on the line
        beg_loc = GPS.EditorLocation (self.buffer, line, 1)
        end_loc = beg_loc.end_of_line()

        s = self.buffer.get_chars (beg_loc, end_loc)

        index = s.find(self.entity_name)

        while index > 0:
            # Cet the entity at this match

            # GPS.Entity might raise an exception: catch it
            try:
                e=GPS.Entity(self.entity_name, self.file, line, index+1, fast=True)
            except:
                e=None

            if e:
                # We have found an entity: verify whether it is the same as the one
                # we are interested in.

                if e.declaration()==self.declaration:
                    # It is the same entity: create a message
                    msg = GPS.Message (
                        "dynamic occurrences",
                        self.file,
                        line,
                        index+1,
                        "",
                        2)
                    msg.set_style(self.style, len(self.entity_name))

                    self.messages += [msg]

            index=s.find(self.entity_name, index+1)

    def process_one_batch(self):
        """ Process a batch of search locations.
        """

        counter = LINES_IN_ONE_BATCH

        found = True

        while counter > 0 and found:
            # Look at one pair of lines
            found = False

            if self.current_line_going_up > 0:
                self.process_one_line(self.current_line_going_up)
                self.current_line_going_up -= 1
                found = True

            if self.current_line_going_down < self.last_line:
                self.process_one_line(self.current_line_going_down)
                self.current_line_going_down += 1
                found = True

            counter -= 1

        # If we have found something during the last pass, return True
        return counter == 0 and found

    def on_timeout (self, timeout):
        try:
            if not self.process_one_batch():
                self.timeout.remove()
                self.timeout=None
        except:
            timeout.remove()
            self.timeout=None

    def __init__ (self, context, entity):
        # Get the current buffer

        self.buffer = GPS.EditorBuffer.get()
        self.entity = entity  # The original entity
        self.entity_name = entity.name()
        self.messages = []    # The registered messages

        self.timeout = None
        self.file=self.buffer.file()
        self.declaration=self.entity.declaration()


        cat = self.entity.category()
        if cat in editor_location_styles:
            self.style = editor_location_styles[cat]
        else:
            self.style = editor_location_styles["unknown"]

        line=context.location().line()

        self.current_line_going_up = line - 1
        self.current_line_going_down = line

        self.last_line = self.buffer.lines_count()

        # Process the immediate surroundings immediately
        # If we still have text matches after that, register a timeout to
        # process them

        if self.process_one_batch():
            self.timeout = GPS.Timeout(TIMEOUT, self.on_timeout)

        # We have registered a timeout which depends on the buffer: kill this
        # when the buffer is destroyed

        self.buffer.current_view().pywidget().connect (
            "destroy", self.cb_destroy)

    def cb_destroy(self, event):
        """ Callback on the destroy event on the view. """
        self.destroy()

    def destroy(self):
        """ Destroy self """

        # Unregister the idle callback

        if self.timeout:
            self.timeout.remove()
            self.timeout=None

        # Remove all messages

        if self.messages:
            for m in self.messages:
                m.remove()

            self.messages=[]


current_highlighter=None

def on_location_changed(hook, file, line, column):
    """ Called when the current location changes """
    global current_highlighter
    context=GPS.current_context()

    if context.__class__ == GPS.EntityContext:
        loc=context.location()

        try:
            entity = GPS.Entity(
            context.entity().name(), loc.file(), loc.line(), loc.column(),
                fast=True)
        except:
            entity = None

        if entity:
            if (current_highlighter
                and (not entity.declaration()
                         == current_highlighter.declaration)):
                current_highlighter.destroy()
                current_highlighter = None

            if not current_highlighter:
                current_highlighter=LocationHighlighter(context, entity)
        else:
            if current_highlighter:
                current_highlighter.destroy()
                current_highlighter = None

def on_gps_started (hook_name):
    """ Called when GPS is started """
    global editor_location_style

    # Destroy any locations potentially left over from a previous session
    # ??? This should be done when exiting GPS.

    for m in GPS.Message.list("dynamic occurrences"):
        m.remove()

    # Create preferences
    for k in default_colors:
        pref_name = "Plugins/auto_highlight_occurrences/color_" + k.replace("/", "_")

        # Create styles

        editor_location_styles[k]=GPS.Style("dynamic occurrences " + k)
        editor_location_styles[k].set_background(
            GPS.Preference (pref_name).get())
        editor_location_styles[k].set_in_speedbar(
          GPS.Preference ("Plugins/auto_highlight_occurrences/speedbar").get())

    GPS.Hook ("location_changed").add(on_location_changed)

GPS.Hook ("gps_started").add (on_gps_started)
