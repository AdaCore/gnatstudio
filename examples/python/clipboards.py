# This module implements multiple-clipboards in GPS.
#
# Pressing the keys "alt-e 0" to "alt-e 9" will copy the current
# selection into one of the name clipboards 0 to 9.
# These clipboards can in turn be pasted with "control-t 0" to "control-t 9"
#
# This package is also an example on how to add your on extensions to GPS.
# The keys indicated above are the default keys. These can be overridden at
# the user level through the Edit->Key Shortcuts editor, and will be
# automatically reloaded from one session to the next
#
# This file should be put in the ~/.gps/plug-ins directory, so that
# it is automatically loaded when GPS starts.
#
# By default, only 10 clipboards are provided. By modifying the call to
# parse_xml below, you can create any number of clipboards

import GPS

copy_key = "alt-e"
paste_key = "primary-t"


def get_customization_string(suffix):
    return """
  <action name="copy_clipboard"""+suffix+"""" category="General">
    <filter id="Source editor" />
    <description>Copy the current selection to the named clipboard """+suffix+"""</description>
    <shell lang="python" >clipboards.copy_to_clipboard(\""""+suffix+"""")</shell>
  </action>
  <action name="paste_clipboard"""+suffix+"""" category="General">
    <filter id="Source editor" />
    <description>Paste the contents of the named clipboard """+suffix+"""</description>
    <shell lang="python">clipboards.paste_from_clipboard(\""""+suffix+"""")</shell>
  </action>
  <key action="copy_clipboard"""+suffix+"\">"+copy_key+" "+suffix+"""</key>
  <key action="paste_clipboard"""+suffix+"\">"+paste_key+" "+suffix+"""</key>
"""


GPS.parse_xml(get_customization_string("0") +
              get_customization_string("1") +
              get_customization_string("2") +
              get_customization_string("3") +
              get_customization_string("4") +
              get_customization_string("5") +
              get_customization_string("6") +
              get_customization_string("7") +
              get_customization_string("8") +
              get_customization_string("9"))
clipboard = {}


def copy_to_clipboard(suffix):
    try:
        clipboard[suffix] = GPS.Editor.get_chars(
            GPS.current_context().file().name())
    except:
        pass


def paste_from_clipboard(suffix):
    context = GPS.current_context()
    GPS.Editor.replace_text(context.file().name(), context.location().line(),
                            context.location().column(),
                            clipboard[suffix], 0, 0)
