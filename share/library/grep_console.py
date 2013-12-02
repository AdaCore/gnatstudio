"""
This plugin provides highlighting of text in Message View
matched by user defined regular expression.
"""


import GPS

# Initialize preference for regexp with number num
def gc_create_preference (num):
    gc_regexp = GPS.Preference("Plugins/grep_console/regexp" + str(num))
    gc_foreground = GPS.Preference("Plugins/grep_console/foreground" + str(num))
    gc_background = GPS.Preference("Plugins/grep_console/background" + str(num))

    gc_regexp.create(
        str(num) + ". Regexp for highlight", "string",
        "Enter regular expression to highlight in Messager View",
        ""
    )

    gc_foreground.create(
        str(num) + ". Foreground color for regexp", "color",
        "Choise foreground color for given regexp",
        "#3070A0"
    )

    gc_background.create(
        str(num) + ". Background color for regexp", "color",
        "Choise background color for given regexp",
        "white"
    )

# Noop callback to click on regexp
def gc_noop(text):
    None

# Load to console regexp with number num
def gc_load_preference (num):
    gc_regexp = GPS.Preference("Plugins/grep_console/regexp" + str(num))
    gc_foreground = GPS.Preference("Plugins/grep_console/foreground" + str(num))
    gc_background = GPS.Preference("Plugins/grep_console/background" + str(num))

    GPS.Console().delete_links ()

    if gc_regexp.get () == "":
        return

    try:
        GPS.Console().create_link (gc_regexp.get(), gc_noop,
                                   gc_foreground.get(),
                                   gc_background.get(),
                                   False)
    except GPS.Exception, e:
        return

# preferences_changed hook
def on_preferences_changed(hook, reload=True):
    gc_load_preference (1)

# gps_started hook
def on_gps_started (hook):
    gc_load_preference (1)

# Initialization
gc_create_preference (1)

GPS.Hook ("gps_started").add (on_gps_started)
GPS.Hook('preferences_changed').add(on_preferences_changed)
