import GPS


class TargetConnector(GPS.BuildTarget):

    """
    This file provides an interface for build targets associated to command
    line tools used to connect to a target (e.g: st-util).
    """

    def __init__(self, tool_name, default_args):
        """
        Initializes a new instance of the class :class:`TargetConnector` by
        creating a build target associated to ``tool_name``, with
        ``default_args`` used as default arguments.
        """

        xml = r"""
        <target model="target connector" category="Bareboard"
        name="%s">
        <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
        <command-line>
        <arg>%s</arg>""" % (tool_name, tool_name)

        for arg in default_args:
            xml += r"""<arg>%s</arg>\n""" % (arg)

        xml += """</command-line>
        </target>"""

        try:
            super(TargetConnector, self).__init__(tool_name)
        except GPS.Exception:
            GPS.parse_xml(xml)
            super(TargetConnector, self).__init__(tool_name)


# Register the build target model for target connectors
target_model_xml = r"""
        <target-model name="target connector" category="">
        <description>Connect to a target using a
        command line tool</description>
        <is-run>True</is-run>
        <iconname>gps-connect-to-target-symbolic</iconname>
        <switches command=""/>
        </target-model>"""

GPS.parse_xml(target_model_xml)
