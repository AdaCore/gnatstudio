"""
This plugin provides highlighting of text in Messages View
matched by user defined regular expressions.
"""

import GPS
from gps_utils import hook

# number of regexps
cu_count = 5


# Initialize preference for regexp with number num

def cu_create_preference(num):
    cu_regexp = GPS.Preference(
        "Messages:Custom Highlighting " + str(num) + "/regexp")
    cu_style = GPS.Preference(
        "Messages:Custom Highlighting " + str(num) + "/variant")

    cu_regexp.create(
        "Regexp to highlight",
        "string",
        "Enter a regular expression to highlight in the Messages View.",
        "")
    cu_style.create_style(
        label="Regexp style",
        doc="")


def cu_load_preference(num):
    cu_regexp = GPS.Preference(
        "Messages:Custom Highlighting " + str(num) + "/regexp")
    cu_style = GPS.Preference(
        "Messages:Custom Highlighting " + str(num) + "/variant")

    if cu_regexp.get() == "":
        return

    style_value = cu_style.get().split('@')

    try:
        GPS.Console().create_link(regexp=cu_regexp.get(),
                                  on_click=lambda x: None,
                                  foreground=style_value[1],
                                  background=style_value[2],
                                  underline=False,
                                  font=style_value[0])
    except GPS.Exception:
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
