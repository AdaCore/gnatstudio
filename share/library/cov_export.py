"""Contextual menu for show coverage report in HTML Browser
"""

############################################################################
## No user customization below this line
############################################################################

import GPS
from gps_utils import *

def show_in_html_browser (menu):
   a = GPS.CodeAnalysis.get ("Coverage")
   file_name=GPS.get_tmp_dir () + "cov.xml"
   a.dump_to_file (xml=GPS.File (file_name))
   GPS.HTML.browse ("file://" + file_name)

def on_filter (context):
   return context.module_name == "CodeAnalysis"

GPS.Contextual ("Coverage/Show in HTML Browser").create (
  on_activate=show_in_html_browser,
  filter=on_filter)
