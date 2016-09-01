"""Highlighting all dispatching calls in the current editor

This package will highlight with a special background color all
dispatching calls found in the current editor. In particular, at such
locations, the cross-references might not lead accurate result (for
instance "go to body"), since the exact subprogram that is called is
not known until run time.
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
from gps_utils.highlighter import Location_Highlighter, OverlayStyle

GPS.Preference("Plugins/dispatching/color").create(
    "Highlight color", "color",
    """Background color to use for dispatching calls""",
    "#FFF3C2")

GPS.Preference("Plugins/dispatching/context").create(
    "Search context", "integer",
    """When the cross-reference information is not up-to-date, \
GPS will search a few lines around the original location for \
matching entities. This preference indicates how many lines \
it will search -- the bigger the slower of course, and potentially \
less precise too""", 5, 0, 50)


class Dispatching_Highlighter(Location_Highlighter):

    def __init__(self):
        Location_Highlighter.__init__(self, style=None)
        self.background_color = None
        self.context = None
        self.__on_preferences_changed(hook=None)
        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        GPS.Hook("file_edited").add(self.__on_file_edited)
        GPS.Hook("file_changed_on_disk").add(self.__on_file_edited)

        if GPS.Logger("ENTITIES.SQLITE").active:
            GPS.Hook("xref_updated").add(self.__on_compilation_finished)
        else:
            GPS.Hook("compilation_finished").add(
                self.__on_compilation_finished)

    def __del__(self):
        Location_Highlighter.__del__(self)
        GPS.Hook("preferences_changed").remove(self.__on_preferences_changed)
        GPS.Hook("file_edited").remove(self.__on_file_edited)
        GPS.Hook("file_changed_on_disk").remove(self.__on_file_edited)

        if GPS.Logger("ENTITIES.SQLITE").active:
            GPS.Hook("xref_updated").remove(self.__on_compilation_finished)
        else:
            GPS.Hook("compilation_finished").remove(
                self.__on_compilation_finished)

    def __on_preferences_changed(self, hook):
        changed = False
        v = GPS.Preference("Plugins/dispatching/context").get()
        if v != self.context:
            self.context = v
            changed = True

        v = GPS.Preference("Plugins/dispatching/color").get()
        if v != self.background_color:
            self.background_color = v
            self.set_style(OverlayStyle(name="dispatchcalls", background=v))
            changed = True

        if changed:
            self.stop_highlight()
            self.__on_compilation_finished()

    def __on_file_edited(self, hook, file):
        # File might have been opened in a QGen browser
        buffer = GPS.EditorBuffer.get(file, open=False)
        if buffer:
            self.start_highlight(buffer)

    def __on_compilation_finished(
            self, hook=None, category="", target_name="",
            mode_name="", status=""):
        """Re-highlight all editors"""

        for b in GPS.EditorBuffer.list():
            self.start_highlight(b)  # automatically removes old highlights

    def recompute_refs(self, buffer):
        try:
            # Minor optimization to query the names of each entities only once.
            names = dict()
            result = []
            for e, r in buffer.file().references(kind="dispatching call"):
                n = names.get(e)
                if n is None:
                    n = names[e] = e.name()
                result.append((n, r))

            return result

        except Exception as e:
            GPS.Logger("DISPATCHING").log("recompute_refs exception %s" % e)
            # xref engine might not be up-to-date, or available yet
            return []


highlighter = None


def on_gps_started(h):
    global highlighter
    highlighter = Dispatching_Highlighter()
GPS.Hook("gps_started").add(on_gps_started)
