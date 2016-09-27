import GPS
import gps_utils

previous_perspective = "Default"


@gps_utils.interactive(name="maximize window")
def maximize_window():
    """
    Hide all views except for those in the central area (which
    are typically the editors).
    If you have split editors, they will all remain visible.
    If you have a single editor, it will be made full screen.

    Execute this action again to go back to the previous
    perspective.
    """

    global previous_perspective

    # This implementation depends on having a "Full Screen"
    # perspective defined in perspectives6.xml

    current = GPS.MDI.current_perspective()

    if current.lower() == "maximized editors":
        GPS.MDI.load_perspective(previous_perspective)
    else:
        previous_perspective = current
        GPS.MDI.load_perspective("Maximized Editors")
