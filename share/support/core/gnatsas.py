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

import os.path
from xml.sax.saxutils import escape
import os_utils
import GPS
import tool_output
import gnatsas_xml


gnatsas = os_utils.locate_exec_on_path("gnatsas")

OUTPUT_PARSERS = """
    output_chopper
    utf8_converter
    progress_parser
    gnatsas_parser
    console_writer
    end_of_build"""


class Gnatsas_Parser(tool_output.OutputParser):
    """
    Parse the GNAT SAS output in order to locate diagnostics such as warnings
    and display them in the Locations view
    """

    previous_messages_removed = False
    # Used to remove the previous messages when GNAT SAS is ran again

    def __init__(self, child=None):
        tool_output.OutputParser.__init__(self, child)

    def on_stdout(self, text, command):
        # List of identifiers that correspond to a GNAT SAS diagnostic. Note
        # that if no specific location appears in the output, it will not show
        # up in the Locations View (this is often the case for fatal errors).
        diagnostics = (
            r"(?:warning|error|"
            # fatal error names
            + r"User error|Setup error|Parsing error|Interrupted|"
            + r"Tool error|Internal error)"
        )
        # identify warnings and errors with specific location such as
        # warning(...)/file_name.file_ext:line:column:msg
        reg = rf"{diagnostics}.*[\/\\\s]([_\w\-\.]+):([0-9]+):([0-9]+):(.*)"

        # and gnat-style warnings/errors such as
        # file_name.file_ext:line:column:warning:msg
        #
        # NOTE: using this format is fine because we ensure that normal text
        # messages are not captured. Indeed GNAT SAS never formats
        # messages with rank "warning" or "error" (but it may use
        # e.g. "high warning", which is not captured here).
        reg2 = rf"([_\w\-\.]+):([0-9]+):([0-9]+):\s*{diagnostics}\s*:(.*)"

        category = "Diagnostics: GNATSAS"

        # cleanup diagnostics from last GNAT SAS execution, but not on
        # subsequent calls of stdout during a single execution
        if not self.previous_messages_removed:
            GPS.Locations.remove_category(category)
            self.previous_messages_removed = True

        for line in text.splitlines():
            GPS.Locations.parse(
                line,
                category,
                regexp=reg,
                file_index=1,
                line_index=2,
                column_index=3,
                msg_index=4,
            )
            GPS.Locations.parse(
                line,
                category,
                regexp=reg2,
                file_index=1,
                line_index=2,
                column_index=3,
                msg_index=4,
            )

        if self.child is not None:
            self.child.on_stdout(text, command)


def on_gps_started(_):
    """Set environment variable to instruct various GNAT SAS binaries that
    they are running in GNAT Studio."""
    GPS.setenv("GNATSAS_ENV_GNAT_STUDIO", "1")


def get_help(option):
    try:
        p = GPS.Process(["gnatsas", option, "--help=plain"])
        raw_result = p.get_result()
        return escape(raw_result)
    except Exception:
        return ""


if gnatsas:
    root = os.path.dirname(os.path.dirname(gnatsas)).replace("\\", "/")
    example_root = root + "/share/examples/gnatsas"

    xml_formatted = gnatsas_xml.xml_gnatsas.format(
        example=example_root,
        root=root,
        general_help=get_help(""),
        analyze_help=get_help("analyze"),
        report_help=get_help("report"),
        output_parsers=OUTPUT_PARSERS,
    )

    GPS.parse_xml(xml_formatted)
    GPS.Hook("gps_started").add(on_gps_started)
