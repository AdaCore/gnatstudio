# This example demonstrates how to reformat all sources of a project
# You can do that with the following command line:
#
#    $ gps -P<your_project> --load=python:reformat_project.py
#
# where <your_project> is your project file.

import GPS

# You can update the following preference values to suit your needs:
GPS.Preference("Ada-Ident-Casing").set("Automatic")
GPS.Preference("Ada-Casing-Policy").set("On_The_Fly")
GPS.Preference("Ada-Align-On-Colons").set(True)
GPS.Preference("Ada-Align-On-Arrows").set(True)
GPS.Preference("Ada-Format-Operators").set(True)
GPS.Preference("Ada-Align-Decl-On_Colon").set(True)
GPS.Preference("Ada-Stick-Comments").set(False)
GPS.Preference("Ada-Reserved-Casing").set("Lower")

# Beginning of the processing
for buf in GPS.EditorBuffer.list():
    buf.close()

for f in GPS.Project.root().sources(recursive=True):
    buf = GPS.EditorBuffer.get(f)
    buf.select()
    GPS.execute_action("Format Selection")
    buf.save(interactive=False)
    buf.close()

GPS.exit()
