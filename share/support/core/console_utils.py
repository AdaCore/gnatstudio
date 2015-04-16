"""
This plugin provides highlighting of text in Messages View
matched by user defined regular expressions.
"""

import GPS
from gps_utils import hook

# number of regexps
cu_count = 3


# Initialize preference for regexp with number num

def cu_create_preference(num):
    cu_regexp = GPS.Preference("Plugins/console_utils/regexp" + str(num))
    cu_foreground = GPS.Preference(
        "Plugins/console_utils/foreground" + str(num))
    cu_background = GPS.Preference(
        "Plugins/console_utils/background" + str(num))

    cu_regexp.create(
        str(num) + ". Regexp for highlight", "string",
        "Enter regular expression to highlight in Messages View",
        ""
    )

    cu_foreground.create(
        str(num) + ". Foreground color for regexp", "color",
        "Choose foreground color for given regexp",
        "#3070A0"
    )

    cu_background.create(
        str(num) + ". Background color for regexp", "color",
        "Choose background color for given regexp",
        "white"
    )


def cu_noop(text):
    None


def cu_load_preference(num):
    cu_regexp = GPS.Preference("Plugins/console_utils/regexp" + str(num))
    cu_foreground = GPS.Preference(
        "Plugins/console_utils/foreground" + str(num))
    cu_background = GPS.Preference(
        "Plugins/console_utils/background" + str(num))

    if cu_regexp.get() == "":
        return

    try:
        GPS.Console().create_link(cu_regexp.get(), cu_noop,
                                  cu_foreground.get(),
                                  cu_background.get(),
                                  False)
    except GPS.Exception, e:
        return


@hook('preferences_changed')
def on_preferences_changed(reload=True):
    cu_load_preferences()


def on_gps_started(hook):
    cu_load_preferences()


@hook('gps_started')
def __on_gps_started():
    cu_load_preferences()


def cu_load_preferences():
    GPS.Console().delete_links()
    for j in range(cu_count):
        cu_load_preference(j + 1)


for j in range(cu_count):
    cu_create_preference(j + 1)
