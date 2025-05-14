"""Provides support for some common URLs via the hyper mode in GPS:
   - http://
   - https://
   - file://
   - file:\\
"""


import GPS
import re

internal_url_regexp = None

# Callback for {file,http*}:// URLs


def view_url(url):
    try:
        if re.search(internal_url_regexp.get(), url):
            GPS.MDI.get_by_child(
                GPS.EditorBuffer.get(GPS.File(url[7:])).current_view()
            ).raise_window()
        else:
            GPS.HTML.browse(url)
    except:
        pass


# Register a highlighter to URLs
GPS.EditorHighlighter(
    r"(file:[\\/][\\/][^\s]*|http(s)?://[^\s:,]*)", view_url, 0, view_url
)

internal_url_regexp = \
    GPS.Preference("External Commands:Browser/gs_regexp").create(
        "Internal browsing URLs",
        "string",
        "URL pattern to be opened within GNAT Studio;"
        + " others will open externally.",
        "^file:",
    )
