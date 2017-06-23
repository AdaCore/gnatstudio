"""
This script implements a contextual menu to launch the command "gnatfind"
on the selected source entity.

This works only when the project hierarchy consists in a single project.
"""

import GPS
import os.path
import gps_utils


def __on_gnatfind_exit(gprfind_process, status, full_output):
    """  Parse the output of the gprfind command and enter it in the
         GPS Locations View.
    """

    if status == 0:
        # Write OK
        GPS.Console().write("%s... done.\n" % full_output)
        GPS.Locations.parse(
            output=full_output,
            category="gnatfind %s" % gprfind_process.query,
            highlight_category="Search results")
    else:
        # Raise an error, listing the full output in the Messages
        GPS.Console().write("Error with gnatfind:\n %s\n" % full_output,
                            "error")


@gps_utils.interactive(
    'run gnatfind',
    contextual='gnatfind %e',
    contextual_ref='goto other file',
    filter=lambda ctx: hasattr(ctx, 'entity') and ctx.entity() is not None)
def __activate():
    context = GPS.contextual_context() or GPS.current_context()

    entity = context.entity()
    loc = context.location()

    buf = GPS.EditorBuffer.get(loc.file())
    location = buf.at(loc.line(), loc.column())

    entity_name = entity.name()

    # Go to the beginning of the entity, as needed by gnatfind

    while not location.starts_word():
        location = location.forward_word(-1)

    entity_file = os.path.basename(loc.file().path)
    entity_line = location.line()
    entity_column = location.column()
    source_dirs = GPS.Project.root().get_attribute_as_list("source_dirs")
    object_dir = GPS.Project.root().get_attribute_as_string("object_dir")

    # Create the gnatfind command line

    command = (["gnatfind", "-r"] +
               ["-aI%s" % d for d in source_dirs] +
               ["-aO", object_dir,
                "%s:%s:%s:%s" % (
                    entity_name, entity_file.split('\\')[-1],
                    entity_line, entity_column)])

    # Write the command line in the Messages window

    GPS.Console().write(" ".join(command) + "\n")

    # Launch the process

    proc = GPS.Process(command, on_exit=__on_gnatfind_exit)

    proc.query = "%s (%s:%s)" % (entity_name, entity_line, entity_column)
