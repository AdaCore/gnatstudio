"""
This file adds support for the gnatstub utility.
This external tool creates the Ada body from an Ada spec.
"""


import GPS
import gs_utils


can_update_body = True
# if gnatstub supports --update-body option


class OnExit(object):

    def __init__(self, file, location):
        self.file = file
        self.location = location

    def on_exit(self, proc, status, output):
        # ??? For some reason, status is always "0"
        GPS.Project.recompute()

        GPS.Hook("file_changed_on_disk").run()

        if self.location:
            # We were given a location when launching gnatstub:
            # if the context is still at the same location when
            # gnatstub returns, then goto the body that we have
            # just generated.
            if GPS.current_context().location() == self.location:
                GPS.execute_action("goto body")
        else:
            # We were not given a location when launching gnatstub:
            # if we are still on the original file, then go to the
            # body of this file.
            if GPS.current_context().file() == self.file:
                GPS.execute_action("goto other file")

        # In any case, parse the output for errors/warnings
        GPS.Locations.parse(output, "gnatstub")


def generate_body(as_separate, for_subprogram):
    """
    Run gnatstub on the current Ada spec to generate a matching
    body file.

    as_separate: whether we want to generate as a separate
    for_subprogram: whether we want to generate for the current subprogram
    """
    global can_update_body

    """ Doesn't work for now
    #  Check gnatstub for --update-body support
    command = [gs_utils.get_gnat_driver_cmd(), 'stub', '--help']
    if os_utils.locate_exec_on_path (command[0]):
        process = GPS.Process(command)
        output = process.get_result()
        can_update_body = output.find('--update-body') >= 0
    """

    GPS.MDI.save_all()
    context = GPS.current_context()

    file = context.file()
    sv = GPS.Project.scenario_variables()
    x_args = ['-X%s=%s' % (k, v) for k, v in sv.items()] if sv else []
    command = [gs_utils.get_gnat_driver_cmd(), 'stub']
    confirmation_msg = ""

    loc = None
    if for_subprogram:
        loc = context.location()
        command.append('--update-body=' + str(loc.line()))
        confirmation_msg = \
            "Are you sure you want to update the body of %s?" % (
                context.entity_name())
    else:
        confirmation_msg = \
            "Are you sure you want to generate the body of %s?" % (
                file.base_name())

    if as_separate:
        command.append('--subunits')

    proj = context.project()
    if proj:
        command.append('-P%s' % proj.file().path)
    command += x_args + [file.path, file.directory()]

    if GPS.MDI.yes_no_dialog(confirmation_msg):
        GPS.Process(
            command,
            task_manager=True,
            show_command=True,
            on_exit=OnExit(file, loc).on_exit)


@gs_utils.interactive(
    category="Ada",
    contextual="Generate/Generate Body",
    contextual_group=GPS.Contextual.Group.EDITING,
    filter=gs_utils.in_ada_file,
    name="generate body",
    for_learning=True,
    description="Run gnatstub on the selected Ada specification to " +
                "generate a matching body.",
    static_path="Generate Body")
def generate_plain_body():
    generate_body(as_separate=False, for_subprogram=False)


@gs_utils.interactive(
    category="Ada",
    contextual="Generate/Generate Body for %e",
    contextual_group=GPS.Contextual.Group.EDITING,
    filter=gs_utils.in_ada_file,
    name="generate body for subprogram",
    for_learning=False,
    description="Run gnatstub on the selected Ada subprogram to " +
                "generate a matching body.",
    static_path="Generate Body for Subprogram")
def generate_plain_body_subprogram():
    generate_body(as_separate=False, for_subprogram=True)


@gs_utils.interactive(
    category="Ada",
    contextual="Generate/Generate Body (as separate)",
    contextual_group=GPS.Contextual.Group.EDITING,
    filter=gs_utils.in_ada_file,
    name="generate body as separate",
    for_learning=False,
    description="Run gnatstub on the selected Ada specification " +
                "to generate a matching body stub and separate file.")
def generate_separate_body():
    generate_body(as_separate=True, for_subprogram=False)


@gs_utils.interactive(
    category="Ada",
    contextual="Generate/Generate Body for %e (as separate)",
    contextual_group=GPS.Contextual.Group.EDITING,
    filter=gs_utils.in_ada_file,
    name="generate body for subprogram as separate",
    for_learning=False,
    description="Run gnatstub on the selected Ada subprogram specification " +
                "to generate a matching body stub and separate file.")
def generate_separate_body_subprogram():
    generate_body(as_separate=True, for_subprogram=True)
