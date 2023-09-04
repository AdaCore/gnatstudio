"""
This file provides support for using the CodePeer tool.

CodePeer is a static analysis tool for Ada code.
This tool allows the user to perform an automatic code review of
a project and integrates its output into GPS.
See menu CodePeer.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import os_utils
import os.path
import re
import copy
import gs_utils.gnat_rules
from xml.sax.saxutils import escape
from codepeer_xml import xmlHead, xmlTrailer, xml_codepeer

codepeer = os_utils.locate_exec_on_path("codepeer")
gnatsas = os_utils.locate_exec_on_path("gnatsas")

prev_xml = ""


def get_supported_warnings():
    global prev_xml
    default_on = ""
    # Then retrieve warnings checks from gnatmake
    xml = """
       <popup label="Warnings">
       <check label="Activate Default Warnings"
              switch="--gnat-warnings={default_on}"
              column="1"
              tip="Use the default warnings of GNAT front-end" />
       <expansion switch="--gnat-warnings"
                  alias="--gnat-warnings={default_on}"/>
       <expansion switch="--gnat-warnings=" />"""

    rules = gs_utils.gnat_rules.get_warnings_list("codepeer-gnatmake", "-h")
    # Already have "Activate Default Warnings" in column 1
    cpt_rule = 1
    for rule in rules:
        r = copy.deepcopy(rule)
        default_on += r.switch[6:] if r.default else ""
        r.switch = re.sub("-gnatw", "--gnat-warnings=", r.switch)
        r.switchoff = re.sub("-gnatw", "--gnat-warnings=", r.switchoff)
        r.label = re.sub("-gnatw", "--gnat-warnings=", r.label)
        r.tip = re.sub("-gnatw", "--gnat-warnings=", r.tip)
        r.before = False
        for dep in r.dependencies:
            dep[0] = re.sub("-gnatw", "--gnat-warnings=", dep[0])
        # Split options between column 1 and 2
        xml += r.Xml(1, cpt_rule % 2 + 1)
        cpt_rule += 1
    xml += "</popup>"

    if prev_xml != xml:
        GPS.parse_xml(xmlHead
                      + xml.format(default_on=default_on)
                      + xmlTrailer)
        prev_xml = xml


def on_project_view_changed(hook):
    # Ensure supported warning if target changed
    get_supported_warnings()


if not gnatsas and codepeer:
    root = os.path.dirname(os.path.dirname(codepeer)).replace('\\', '/')
    example_root = root + '/share/examples/codepeer'
    try:
        with open(root + '/share/doc/codepeer/help.txt', 'r') as help_file:
            help_msg = escape(help_file.read())
    except Exception:
        help_msg = ''

    # Figure out if long switches support '--'. Assume this is the default.
    extra_dash = '-'
    version_string = GPS.Process("codepeer --version").get_result()
    m = re.match(r".* \((\d{8})\).*", version_string)
    if m is not None:
        date = int(m.group(1))
        if date < 20220823:
            extra_dash = ''

    xml_codepeer = xml_codepeer.format(example=example_root,
                                       root=root,
                                       s=extra_dash,
                                       help=help_msg)
    xmlHead = xmlHead.format(help=help_msg, s=extra_dash)
    xmlTrailer = xmlTrailer.format(s=extra_dash)
    GPS.parse_xml(xml_codepeer)
    GPS.Hook("project_view_changed").add(on_project_view_changed)
