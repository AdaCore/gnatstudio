""" This script implements a contextual menu to launch the command "gnatfind"
    on the selected source entity.

    This works only when the project hierarchy consists in a single project.
"""

import GPS
import os.path

def on_gnatfind_exit (gprfind_process, status, full_output):
    """  Parse the output of the gprfind command and enter it in the
         GPS Locations View.
    """

    if status == 0:
        # Write OK
        GPS.Console().write("%s... done.\n" % full_output)
        GPS.Locations.parse (
            output=full_output,
            category="gnatfind %s" % gprfind_process.query,
            highlight_category="Search results")
    else:
        # Raise an error, listing the full output in the Messages
        GPS.Console().write("Error with gnatfind:\n %s\n" % full_output,
                            "error")


class Gnatfind_Contextual (GPS.Contextual):
    """ Define a contextual menu "gnatfind".
    """

    def __init__ (self):
        GPS.Contextual.__init__ (self, "gnatfind %E")
        self.create (on_activate = self.on_activate,
                     ref         = "Goto declaration of entity",
                     label       = self.label,
                     filter      = self.filter)

    def label (self, context):
       return "gnatfind <b>%s</b>" % context.entity().name()

    def filter (self, context):
       if not hasattr(context, "entity"):
           return False
       if not context.entity():
           return False
       return True

    def on_activate (self, context):
       entity = context.entity()

       loc = context.location()

       buf = GPS.EditorBuffer.get(loc.file())
       location = GPS.EditorLocation(buf, loc.line(), loc.column())

       entity_name = entity.name()
       length = len(entity_name)

       # Go to the beginning of the entity, as needed by gnatfind

       while not location.starts_word():
           location = location.forward_word(-1)

       entity_file = os.path.basename(loc.file().name())
       entity_line = location.line()
       entity_column = location.column()
       source_dirs = GPS.Project.root().get_attribute_as_list("source_dirs")
       object_dir = GPS.Project.root().get_attribute_as_string("object_dir")

       ais = " ".join(["-aI%s" % d for d in source_dirs])

       # Create the gnatfind command line

       command = "gnatfind -r %s -aO%s %s:%s:%s:%s" % (ais, object_dir, entity_name, entity_file.split('\\')[-1], entity_line, entity_column)

       # Write the command line in the Messages window

       GPS.Console().write(command + "\n")

       # Launch the process

       proc = GPS.Process(command, on_exit=on_gnatfind_exit)

       proc.query = "%s (%s:%s)" % (entity_name, entity_line, entity_column)


# Register the menu


def on_gps_started (hook_name):
    Gnatfind_Contextual()

GPS.Hook ("gps_started").add (on_gps_started)

