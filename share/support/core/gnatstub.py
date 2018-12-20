"""
This file adds support for the gnatstub utility.
This external tool creates the Ada body from an Ada spec.
"""


import GPS
import gps_utils
import os
import os_utils
from gi.repository import GLib


can_update_body = True
# if gnatstub supports --update-body option


class OnExit(object):

    def __init__(self, file, entity):
        self.file = file
        self.entity = entity

    def on_exit(self, proc, status, output):
        # ??? For some reason, status is always "0"
        GPS.Project.recompute()
        f2 = self.file.other_file()

        if os.path.isfile(f2.path):
            ed = GPS.EditorBuffer.get(f2, force=True)
            GPS.MDI.get_by_child(ed.current_view()).raise_window()

            if self.entity:
                body = self.entity.body()
                ed = GPS.EditorBuffer.get(body.file(), force=True)
                ed.current_view().goto(ed.at(body.line(), body.column()))
        else:
            GPS.Locations.parse(output, "gnatstub")


def __contextual_label_for_gnatstub(context):
    """
    Used to create the label for the gnatstub contextual menu.
    """
    fmt = "Generate Body of <b>{}</b>"

    if spec_has_no_body(context):
        name = os.path.basename(context.file().path)
        return fmt.format(os_utils.display_name(name))
    else:
        return fmt.format(context.entity_name())


def __contextual_label_for_gnatstub_separate(context):
    """
    Used to create the label for the gnatstub 'as separate' contextual menu.
    """
    fmt = "Generate Body of <b>{}</b> as separate"
    name = GLib.markup_escape_text(context.entity_name())
    return fmt.format(name)


def is_ada_file(context):
    file = context.file()
    return file and file.language().lower() == "ada"


def gnatstub_update_body(context):
    """
    Action filter to find subprogram without body.
    """
    global can_update_body

    if not can_update_body:
        return False
    elif not is_ada_file(context):
        return False
    elif not context.entity_name():
        return False

    entity = context.entity()

    return entity and entity.requires_body() and not entity.has_body()


def spec_has_no_body(context):
    """
    Action filter to check Ada spec file without a body
    """
    if not is_ada_file(context):
        return False

    file = context.file()
    other = file.other_file()
    project = context.project() or GPS.Project.root()
    suffix = project.get_attribute_as_string('spec_suffix', 'naming', 'ada')
    is_spec = file.path.endswith(suffix)
    has_other_file = (file != other) and os.path.isfile(other.path)

    return is_spec and not has_other_file


def enable_gnatstub(context):
    """
    Action filter to enable gnatstub action
    """
    return spec_has_no_body(context) or gnatstub_update_body(context)


def enable_gnatstub_separate(context):
    """
    Action filter to enable gnatstub 'as separate' action
    """
    return not spec_has_no_body(context) and gnatstub_update_body(context)


def generate_body(as_separate):
    """
    Run gnatstub on the current Ada spec to generate a matching
    body file.
    """
    global can_update_body

    """ Doesn't work for now
    #  Check gnatstub for --update-body support
    command = [gps_utils.get_gnat_driver_cmd(), 'stub', '--help']
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
    command = [gps_utils.get_gnat_driver_cmd(), 'stub']

    entity = context.entity() if context.entity_name() else None
    if entity and entity.requires_body() and not entity.has_body():
        command.append('--update-body=' + str(entity.declaration().line()))
    else:
        entity = None

    if as_separate:
        command.append('--subunits')

    proj = context.project()
    if proj:
        command.append('-P%s' % proj.file().path)
    command += x_args + [file.path, file.directory()]

    proc = GPS.Process(
        command,
        task_manager=True,
        show_command=True,
        on_exit=OnExit(file, entity).on_exit)
    proc.wait()


@gps_utils.interactive(
    category="Ada",
    contextual=__contextual_label_for_gnatstub,
    filter=enable_gnatstub,
    name="generate body",
    for_learning=True,
    description="Run gnatstub on the selected Ada specification to " +
                "generate a matching body.",
    static_path="Generate Body")
def generate_plain_body():
    generate_body(as_separate=False)


@gps_utils.interactive(
    category="Ada",
    contextual=__contextual_label_for_gnatstub_separate,
    filter=enable_gnatstub_separate,
    name="generate body as separate",
    for_learning=False,
    description="Run gnatstub on the selected Ada subprogram specification " +
                "to generate a matching body stub and separate file.")
def generate_separate_body():
    generate_body(as_separate=True)
