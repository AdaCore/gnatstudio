"""common switch definition

This file provides common classes used to manipulate switches, either for
creating an xml description of them for parsing by GPS, or for creating
corresponding widgets for a GUI editor.
* Used by gnat_rules.py
* Used by gnatcheck_rules_editor.py
"""

# call Check() from xml values:


class Check:

    """Handle simple on/off switches"""

    def __init__(self, switch, switchoff, label, tip, defaultstate, before=False):
        """Constructor of the Check class"""
        self.switch = switch
        self.switchoff = switchoff
        self.default = defaultstate
        self.label = label
        self.tip = tip
        self.before = before
        self.dependencies = []
        self.widget = None

    def Add_Default_Val_Dependency(self, dependency, activate):
        """Add a default value dependency"""
        self.dependencies.append([dependency, activate])

    def Label(self):
        """Get the label of the switch"""
        return self.label

    def Tip(self):
        """Get the tip describing the switch"""
        return self.tip

    def Switch(self):
        """Get the switch value"""
        return self.switch

    def SwitchOff(self):
        """Get the switch-off value"""
        return self.switchoff

    def Default(self):
        if self.default:
            return "on"
        else:
            return "off"

    def Xml(self, line, col):
        xml = """<%s label="%s" switch="%s" line="%s" column="%s" """ % (
            self._tag(),
            self.label,
            self.Switch(),
            line,
            col,
        )

        if self.SwitchOff() != "":
            xml += """switch-off="%s" """ % (self.SwitchOff())

        if self.Default() != "":
            xml += """default="%s" """ % (self.Default())

        if self.before:
            xml += """before="true" """

        if self._xml_internal() != "":
            xml += self._xml_internal()

        if self.tip != "":
            xml += """><tip>%s</tip></%s>""" % (self.tip, self._tag())
        else:
            xml += "/>"

        for dep in self.dependencies:
            if dep[1]:
                xml += """<default-value-dependency """
                xml += """master-switch="%s" slave-switch="%s"/>\n""" % (
                    dep[0],
                    self.switch,
                )
            else:
                xml += """<default-value-dependency """
                xml += """master-switch="%s" slave-switch="%s"/>\n""" % (
                    dep[0],
                    self.switchoff,
                )
        return xml

    def _tag(self):
        return "check"

    def _xml_internal(self):
        return ""


class ComboEntry:

    """Handle combo entries for combo switches"""

    def __init__(self, value, tip, label=""):
        self.value = value
        self.tip = tip
        self.label = label or value

    def Xml(self):
        return """<combo-entry label="%s" value="%s" tip="%s"/>""" % (
            self.label,
            self.value,
            self.tip,
        )


class Combo:

    """Handle combo switches"""

    def __init__(self, switch, label, tip, separator, noswitch, values, before=False):
        """
        Initialize a combo switch. Use a ComboEntry list when specifying the
        'values' parameter.
        """

        self.switch = switch
        self.values = values
        self.noswitch = noswitch
        self.label = label
        self.tip = tip
        self.before = before

    def Default(self):
        return "%d" % (self.defaultval)

    def Label(self):
        """Get the label of the switch"""
        return self.label

    def Tip(self):
        """Get the tip describing the switch"""
        return self.tip

    def Switch(self):
        """Get the switch value"""
        return self.switch

    def NoSwitch(self):
        """Get the value used when the switch is not specified"""
        return self.noswitch

    def _tag(self):
        return "combo"

    def Xml(self, line, col):
        xml = """<%s label="%s" switch="%s" line="%s" column="%s" """ % (
            self._tag(),
            self.label,
            self.Switch(),
            line,
            col,
        )

        if self.tip:
            xml += """tip="%s" """ % self.tip

        if self.NoSwitch():
            xml += """noswitch="%s" """ % (self.NoSwitch())

        if self.before:
            xml += """before="true" """

        xml += ">"

        for val in self.values:
            xml += """\n%s""" % val.Xml()

        xml += "</combo>"

        return xml


class Spin(Check):

    """Handle switches that expect a numerical value parameter"""

    def __init__(
        self,
        switch,
        switchoff,
        label,
        tip,
        separator,
        default,
        minvalue,
        maxvalue,
        before=False,
    ):
        Check.__init__(self, switch, switchoff, label, tip, "", before)
        self.defaultval = float(default)
        self.minval = float(minvalue)
        self.maxval = float(maxvalue)
        self.separator = separator

    def Default(self):
        return "%d" % (self.defaultval)

    def _tag(self):
        return "spin"

    def _xml_internal(self):
        return """min="%d" max="%d" separator="%s" """ % (
            self.minval,
            self.maxval,
            self.separator,
        )


class Field(Check):
    def __init__(self, switch, switchoff, label, tip, separator, default, before=False):
        Check.__init__(self, switch, switchoff, label, tip, "", before)
        self.default = default
        self.separator = separator

    def Default(self):
        return self.default

    def _tag(self):
        return "field"

    def _xml_internal(self):
        return """separator="%s" """ % (self.separator)
