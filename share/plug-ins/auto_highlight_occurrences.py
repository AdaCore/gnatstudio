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
  "package/namespace" : "lightgreen",
  "type"              : "lightgreen",
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
        s_len = len(s)

        l = len(self.entity_name)

        # Find name and expand tabs at the same time

        index = 0
        tab_expanded_index = 0

        while index + l < s_len:
            if s[index:index+l] == self.entity_name:
                # Cet the entity at this match

                # GPS.Entity might raise an exception: catch it
                try:
                    e=GPS.Entity("", self.file, line, tab_expanded_index+1)
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
                            tab_expanded_index+1,
                            "",
                            2)
                        msg.set_style(self.style, len(self.entity_name))

                        self.messages += [msg]

                index += l+1
                tab_expanded_index += l

            else:
                index += 1

            if s[index-1]=='\t':
                # snap to the next multiple of 8
                tab_expanded_index += 9 - (tab_expanded_index+1) % 8
            else:
                tab_expanded_index += 1

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

        # Unregister the idle callback

        if self.timeout:
            self.timeout.remove()
            self.timeout=None

    def destroy(self):
        """ Destroy self """

        # Stop highlighting new messages

        if self.timeout:
            self.timeout.remove()
            self.timeout=None

        # Remove all messages

        if self.messages:
            for m in self.messages:
                m.remove()

            self.messages=[]


current_highlighter=None
exiting=False # Whether GPS is about to exit

def re_highlight():
    global current_highlighter

    if exiting:
        return

    context=GPS.current_context()

    # Is there an entity?
    if context.__class__ == GPS.EntityContext:
        try:
            entity = context.entity()
        except:
            entity = None

        if entity:
            # Destroy the current highlighter if it is highlighting another
            # entity
            if (current_highlighter
                and (not entity.declaration()
                         == current_highlighter.declaration)):
                current_highlighter.destroy()
                current_highlighter = None

            # Highlight the current entity
            if not current_highlighter:
                current_highlighter=LocationHighlighter(context, entity)

            return

    # If we reach this point, there is no entity in the context: remove
    # highlighting

    if current_highlighter:
        current_highlighter.destroy()
        current_highlighter = None


def on_location_changed(hook, file, line, column):
    """ Called when the current location changes """
    re_highlight()

def remove_all_messages():
    global current_highlighter
    if current_highlighter:
        current_highlighter.destroy()
        current_highlighter = None

    for m in GPS.Message.list("dynamic occurrences"):
        m.remove()

def on_preferences_changed (hook_name):
    remove_all_messages()

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

    # Re-highlight after preferences changed

    re_highlight()

def before_exit(hook_name):
    global exiting
    try:
        remove_all_messages()
        exiting=True
    except:
        pass
    return 1

GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("preferences_changed").add (on_preferences_changed)
