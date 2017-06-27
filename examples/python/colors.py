# This script sets the background color of .adb files and .ads files
# automatically when they are loaded.
# It demonstrates the use of hooks, and the setting of preferences in GPS

import GPS


def set_bg_color(hook_name, file):
    name = file.name()
    if name[len(name) - 4:] == ".adb":
        GPS.Editor.set_background_color(
            file.name(), GPS.Preference("custom-adb-file-color").get())
    elif name[len(name) - 4:] == ".ads":
        GPS.Editor.set_background_color(
            file.name(), GPS.Preference("custom-ads-file-color").get())


GPS.parse_xml("""
   <preference name="custom-adb-file-color"
               label="Background color for .adb files"
               page="Editor:Fonts &amp; Colors"
               default="yellow"
               type="color" />
   <preference name="custom-ads-file-color"
               label="Background color for .ads files"
               page="Editor:Fonts &amp; Colors"
               default="red"
               type="color" />
""")

GPS.Hook("file_edited").add(set_bg_color)
