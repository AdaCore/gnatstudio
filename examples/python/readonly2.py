"""This file makes every line between a --@generated and --@usercode comment
   read only and the background colored in grey.
   This is an example of using overlays"""

import GPS
import os.path


def on_open_generated_file(name, file):

    ebuf = GPS.EditorBuffer.get()

    def apply_overlay(overlay, from_line, to_line, line_len):
        start_loc = ebuf.at(from_line, 1)
        end_loc = ebuf.at(to_line, line_len)
        ebuf.apply_overlay(overlay, start_loc, end_loc)

    f_path = os.path.abspath(file.name())
    f = open(f_path)
    text = f.readlines()
    f.close()

    start = 0
    start_found = False

    if text[0].strip() == "--@generated":

        grey = ebuf.create_overlay(f_path)
        grey.set_property("background", "grey")
        grey.set_property("editable", False)

        for line_num, line in enumerate(text[0:]):

            if not start_found and line.find('@usercode:') > -1:
                apply_overlay(grey, start, line_num + 1, len(line))
                start_found = True

            elif start_found and line.find('@end') > -1:
                start = line_num + 1
                start_found = False

        apply_overlay(grey, start, len(text), len(text[-1]))


GPS.Hook("file_edited").add(on_open_generated_file)
