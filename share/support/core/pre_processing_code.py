"""
This file provides support for displaying Ada preprocessing code as generated
by GNAT (-gnateG switch).
"""

import os
import distutils.dep_util
import GPS
from gps_utils import in_ada_file, interactive, hook

inactive_code_pref = GPS.Preference("Editor/Fonts & Colors:" +
                                    "General/preprocessor")
# Preference used for the display of the inactive code


@hook("preferences_changed")
def __on_preferences_changed():
    for srcbuf in GPS.EditorBuffer.list():
        if hasattr(srcbuf, "gnatprep_overlay"):
            style, fg, bg = inactive_code_pref.get().split("@")
            if style != "DEFAULT":
                srcbuf.gnatprep_overlay.set_property("style", style.lower())
            srcbuf.gnatprep_overlay.set_property("foreground", fg)
            srcbuf.gnatprep_overlay.set_property("background", bg)


def create_overlay(file_name):
    """Create a new overlay"""
    srcbuf = GPS.EditorBuffer.get(GPS.File(file_name))
    overlay = srcbuf.create_overlay("Inactive code")
    style, fg, bg = inactive_code_pref.get().split("@")
    if style != "DEFAULT":
        overlay.set_property("style", style.lower())
    overlay.set_property("foreground", fg)
    overlay.set_property("background", bg)
    srcbuf.gnatprep_overlay = overlay


def subprogram_bounds(cursor):
    """Return the first and last line of the current subprogram, and (0,0) if
       the current subprogram could not be determined."""
    blocks = {"CAT_PROCEDURE": 1, "CAT_FUNCTION": 1, "CAT_ENTRY": 1,
              "CAT_PROTECTED": 1, "CAT_TASK": 1, "CAT_PACKAGE": 1}

    if cursor.block_type() == "CAT_UNKNOWN":
        return 0, 0

    min = cursor.buffer().beginning_of_buffer()
    while not (cursor.block_type() in blocks) and cursor > min:
        cursor = cursor.block_start() - 1

    if cursor > min:
        return cursor.block_start_line(), cursor.block_end_line()
    else:
        return 0, 0


def reset_state(file_name):
    """Reset the state of the file and delete the overlay"""
    srcbuf = GPS.EditorBuffer.get(GPS.File(file_name))
    if hasattr(srcbuf, "gnatprep_overlay"):
        srcbuf.remove_overlay(srcbuf.gnatprep_overlay)


def get_prep_file(for_subprogram=False, reset=False):
    """Retreive the .prep file in the project"""
    context = GPS.current_context()
    local_file = context.file().path
    file = context.file().name("Build_Server")

    if context.project():
        list_dir = context.project().object_dirs(False)
    else:
        list_dir = GPS.Project.root().object_dirs(False)

    if list_dir:
        objdir = list_dir[0]
    else:
        objdir = GPS.get_tmp_dir()
        GPS.Console("Messages").write(
            "Could not find an object directory for %s, reverting to %s" %
            (file, objdir))
    prep_file = os.path.join(objdir, os.path.basename(local_file)) + ".prep"
    return local_file, prep_file


def show_inactive_code(file_name, prep_name, for_subprogram=False):
    """Parse the file prep_name and show the inactive code"""
    # Explain to the user how to generate the .prep file
    if not os.path.isfile(prep_name):
        GPS.Console("Messages").write("No preprocessing data file " +
                                      str(prep_name) +
                                      " found: The project must be compiled " +
                                      "with -gnateG to generate .prep file.\n",
                                      mode="error")
    # Do nothing is the .prep file is too old
    elif distutils.dep_util.newer(file_name, prep_name):
        GPS.Console("Messages").write("The .prep file is too old," +
                                      " please recompile the ada file.\n",
                                      mode="error")
    else:
        reset_state(file_name)
        srcbuf = GPS.EditorBuffer.get(GPS.File(file_name))

        if for_subprogram:
            block_first, block_last = subprogram_bounds(
                srcbuf.current_view().cursor())
        else:
            block_first, block_last = (0, 0)

        # Get the text
        prep_file = open(prep_name)
        prep_text = prep_file.read()
        prep_file.close()

        # The file start at the line 1
        current_line = 1
        create_overlay(file_name)

        for line in prep_text.splitlines():
            if line.startswith("--! "):
                if (block_first == 0 or
                        (block_first < current_line <= block_last)):
                    frm = srcbuf.at(current_line, 1)
                    to = frm.end_of_line()
                    srcbuf.apply_overlay(srcbuf.gnatprep_overlay, frm, to)
            current_line += 1

#################################
# Register the contextual menus #
#################################


@interactive("Ada", in_ada_file,
             contextual="Preprocessing/Show subprogram",
             name="Show inactive code for subprogram", before="Align")
def show_inactive_subprogram():
    """Show the file state after the preprocessing"""
    file_name, prep_name = get_prep_file(for_subprogram=True)
    show_inactive_code(file_name, prep_name, for_subprogram=True)


@interactive("Ada", in_ada_file,
             contextual="Preprocessing/Show entire file",
             name="Show inactive code for file", before="Align")
def show_inactive_file():
    """Show the subprogram state after the preprocessing"""
    file_name, prep_name = get_prep_file()
    show_inactive_code(file_name, prep_name)


@interactive("Ada", in_ada_file,
             contextual="Preprocessing/Clear",
             name="Clear inactive code display", before="Align")
def clear_display():
    """Clear the preprocessing display"""
    file_name, _ = get_prep_file()
    reset_state(file_name)
