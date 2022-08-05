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

codepeer = os_utils.locate_exec_on_path("codepeer")
is_cpl = os.path.lexists (os.path.join (os.path.dirname (codepeer), "cpm-gs-bridge"))

if is_cpl:
    from codepeer_xml import xmlHead, xmlTrailer, xml_codepeer
else:
    from codepeer_xml_old import xmlHead, xmlTrailer, xml_codepeer

prev_xml = ""

def get_supported_warnings():
    global prev_xml
    default_on = ""
    # Then retrieve warnings checks from gnatmake
    xml = """
       <popup label="Warnings">
       <expansion switch="--gnat-warnings="/>
    """
    rules = gs_utils.gnat_rules.get_warnings_list("codepeer-gnatmake", "-h")
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
        xml += r.Xml(1, 1)

    xml += "<expansion switch='--gnat-warnings' alias='--gnat-warnings="
    xml += default_on
    xml += "'/>"
    xml += "</popup>"

    if prev_xml != xml:
        GPS.parse_xml(xmlHead + xml + xmlTrailer)
        prev_xml = xml


def on_project_view_changed(hook):
    # Ensure supported warning if target changed
    get_supported_warnings()

if codepeer:
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
