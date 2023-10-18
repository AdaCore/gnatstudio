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


def get_help(option):
    try:
        p = GPS.Process(["gnatsas", option, "--help=plain"])
        raw_result = p.get_result()
        return escape(raw_result)
    except Exception:
        return ""


if gnatsas:
    root = os.path.dirname(os.path.dirname(gnatsas)).replace('\\', '/')
    example_root = root + '/share/examples/gnatsas'

    xml_formated = gnatsas_xml.xml_gnatsas.format(
        example=example_root,
        root=root,
        general_help=get_help(""),
        analyze_help=get_help("analyze"),
        report_help=get_help("report"))

    GPS.parse_xml(xml_formated)
