# This is an example showing how to force setting of GPS preferences
# at start up.

from GPS import Preference, Hook


def on_gps_started(hook):
    # Set the preferences. You can adjust them at your convenience.
    Preference("Ada-Format-Operators").set(True)
    Preference("Ada-Ident-Casing").set("Smart_Mixed")
    Preference("Warnings-Src-Highlight-Color").set("#FFFF6D6D6D6D")


Hook("gps_started").add(on_gps_started)
