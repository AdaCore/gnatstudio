"""Contextual menu for show coverage report in HTML Browser
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils


@gps_utils.interactive(
    contextual='Coverage/Show in HTML Browser',
    filter=lambda c: c.module_name == 'CodeAnalysis')
def show_in_html_browser():
    a = GPS.CodeAnalysis.get("Coverage")
    file_name = GPS.get_tmp_dir() + "cov.xml"
    a.dump_to_file(xml=GPS.File(file_name))
    GPS.HTML.browse("file://" + file_name)
