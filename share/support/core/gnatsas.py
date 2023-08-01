"""
This file provides support for using the GNATSAS toolsuite.

GNATSAS is a static analysis toolsuite for Ada code.
It allows the user to perform an automatic code review of
a project and integrates its output into GPS.
See menu GNATSAS.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import os_utils
import os.path
import re
import copy
import gnatsas_xml
import gs_utils.gnat_rules
from xml.sax.saxutils import escape


gnatsas = os_utils.locate_exec_on_path("gnatsas")


def cpms_combo_box(label, switch, tip):
    cpm_dir = GPS.Project.root().get_attribute_as_string(
        attribute="CPM_Directory", package="GNATSAS")
    head = ("<combo label='" + label
            + "' switch='" + switch
            + "' separator=' ' tip='" + tip
            + " '>")
    tail = "</combo>"
    body = ""
    for f in os.listdir(cpm_dir):
        if f.endswith(".cpm"):
            body += ("<combo-entry label='"
                     + os.path.basename(f)
                     + "' value='" + f
                     + "'/>")

    return head + body + tail


if gnatsas:
    root = os.path.dirname(os.path.dirname(gnatsas)).replace('\\', '/')
    example_root = root + '/share/examples/codepeer'
    try:
        with open(root + '/share/doc/codepeer/help.txt', 'r') as help_file:
            help_msg = escape(help_file.read())
    except Exception:
        help_msg = ''

    xml_formated = gnatsas_xml.xml_gnatsas.format(example=example_root,
                                                  root=root,
                                                  help=help_msg)
    GPS.parse_xml(xml_formated)
