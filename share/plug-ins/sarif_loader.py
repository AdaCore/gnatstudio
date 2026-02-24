import GPS
import json
import os.path
from gi.repository import Gtk
from gs_utils import interactive

"""
This Sarif plugin creates several actions to load Sarif files in the Analysis
view.

You can access the action via the menu "Analyze > Load SARIF"
Or the contextual menu "Load current SARIF file"
"""

MENU_PATH = "Analyze/SARIF/"


class SarifFile:
    def __init__(self, file_path, data):
        self.path = os.path.abspath(file_path)
        self.data = data

    def get_runs(self):
        return self.data.get("runs", [])


class LoadedData:
    def __init__(self):
        self.files = []
        self.rules = []
        self.tools = {}

    def clean(self):
        self.files = []
        self.rules = []
        self.tools = {}
        GPS.Analysis.clean()

    def add_file(self, sarif_file):
        if sarif_file.path in [f.path for f in self.files]:
            GPS.Console("Message").write(
                "%s has already been loaded into the Analysis view" % str(path)
            )
        else:
            self.files[path] = sarif_file


current_data = LoadedData()


def log_exception(file, e):
    if file:
        GPS.Console("Messages").write(
            "Failed to load SARIF file '%s'. Check GNAT Studio's log file for more information.\n"
            % str(file),
            mode="error",
        )
    GPS.Logger("SARIF").log(str(e))


def get_severity(result):
    """
    This section is copied from sarif-tools and is following
    https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html#_Toc141790898
    """
    severity = result.get("level")
    if severity:
        return severity

    # If kind has any value other than "fail", then if level is absent,
    # it SHALL default to "none"
    kind = result.get("kind", "fail")
    if kind and kind != "fail":
        return "none"

    # If kind has the value "fail" and level is absent, then...
    rule, ruleIndex = read_result_rule(result, run)
    if rule:
        # Honor the invocation's configuration override if present...
        invocation = read_result_invocation(result, run)
        if invocation:
            ruleConfigurationOverrides = invocation.get(
                "ruleConfigurationOverrides", []
            )
            override = next(
                (
                    override
                    for override in ruleConfigurationOverrides
                    if (
                        override.get("descriptor", {}).get("id") == rule.get("id")
                        or override.get("descriptor", {}).get("index") == ruleIndex
                    )
                ),
                None,
            )

            if override:
                overrideLevel = override.get("configuration", {}).get("level")
                if overrideLevel:
                    return overrideLevel

        # Otherwise, use the rule's default configuraiton if present...
        defaultConfiguration = rule.get("defaultConfiguration")
        if defaultConfiguration:
            severity = defaultConfiguration.get("level")
            if severity:
                return severity

    # Otherwise, fall back to warning
    return "warning"


def severity_to_importance(severity):
    """
    The list of severities is ["error", "warning", "note", "none"]
    if include_none or if there are any records with severity "none",
    otherwise ["error", "warning", "note"]
    """
    if severity.lower() == "error":
        return GPS.Message.Importance.HIGH
    elif severity.lower() == "warning":
        return GPS.Message.Importance.MEDIUM
    elif severity.lower() == "note":
        return GPS.Message.Importance.INFORMATIONAL
    else:
        return GPS.Message.Importance.UNSPECIFIED


def load_files(filename_list):
    """
    Load a list of sarif files and return a set

    :type filenma_list: list[str]
    :param filename_list: List of sarif files to load
    """
    # Defensive code verifying the list content
    sarif_list = [f for f in filename_list if f.endswith("sarif")]
    if sarif_list:
        for filename in sarif_list:
            try:
                with open(filename, encoding="utf-8-sig") as f:
                    data = json.load(f)
                sarif_f = SarifFile(filename, data)
                current_data.files.append(sarif_f)
            except Exception as e:
                log_exception(filename, e)
        return current_data.files
    else:
        return None


def get_location(result):
    """
    Return the location in the form of a tuple (abs_path, line, column)
    line and column default to 1 in case they can't be found.

    :type result: Dict
    :param result: json data loaded from a sarif file
    """
    try:
        locations = result.get("locations", [])
        if locations and isinstance(locations, list):
            location = locations[0]
            physical_location = location.get("physicalLocation", {})
    except Exception:
        return

    column = physical_location.get("region", {}).get("startColumn", 1)
    line = physical_location.get("region", {}).get("startLine", 1)

    # The logic for file_path is copied from sarif-tools python package
    file_path = (
        location.get("physicalLocation", {})
        .get("address", {})
        .get("fullyQualifiedName", None)
    )
    if not file_path:
        # Next try the physical location written by MobSF and by SpotBugs
        # (for some errors)
        file_path = (
            location.get("physicalLocation", {})
            .get("artifactLocation", {})
            .get("uri", None)
        )
    if not file_path:
        logical_locations = location.get("logicalLocations", None)
        if logical_locations:
            # Finally, try the logical location written by SpotBugs
            # for some errors
            file_path = logical_locations[0].get("fullyQualifiedName", None)

    return (file_path, line, column)


def get_message(result):
    """
    Return the message.

    :type result: Dict
    :param result: a sarif result extracted by sarif-tools
    """
    message_data = result["message"]
    if "text" in message_data:
        return message_data["text"]
    elif "id" in message_data:
        return message_data["id"]
    else:
        return ""


def get_secondary_message(result):
    """
    Return the secondary message with extra information

    :type location: Dict (see threadFlows description)
    :param location: a location in the threadFlows
    """
    text = get_message(result["location"])
    nesting_level = result.get("nestingLevel", None)
    if nesting_level:
        text = "%s%s" % ("   " * nesting_level, text)
    return text


def get_tool_name(run, result):
    """
    Return the name of the tool for message

    :type location: Dict
    :param location: json data loaded from a sarif file
    """
    properties = result.get("properties", [])
    if properties:
        engine = properties.get("engine", "")
        if engine:
            return engine

    try:
        return run["tool"]["driver"]["name"]
    except Exception:
        pass

    return "Sarif Loader"


def get_rule(result):
    """
    Return the rule for message

    :type location: Dict
    :param location: json data loaded from a sarif file
    """
    return result.get("ruleId", "")


def create_message(tool, run, rule_id, result):
    """
    Create a message for result.

    :type tool: GPS.AnalysisTool
    :param tool: Tool which has generated the report

    :type rule_id: str
    :param rule_id: The name of the rule for result

    :type result: Dict
    :param result: json data loaded from a sarif file
    """
    abs_path, line, column = get_location(result)

    msg = tool.create_message(
        category=get_tool_name(run, result),
        file=GPS.File(abs_path),
        line=line,
        column=column,
        text=get_message(result),
        importance=severity_to_importance(get_severity(result)),
        rule_id=rule_id,
        # We only want messages from create_secondary_messages
        look_for_secondary=False,
    )
    create_secondary_messages(tool, result, msg)


def create_secondary_messages(tool, result, primary):
    """
    Create secondary messages for result

    :type tool: GPS.AnalysisTool
    :param tool: Tool which has generated the report

    :type result: Dict
    :param result: json data loaded from a sarif file

    :type primary: GPS.Message
    :param primary: the primary message
    """

    flows = result.get("codeFlows", [])

    if not flows:
        return

    for flow in flows:
        try:
            for thread in flow["threadFlows"]:
                locations = thread["locations"]
                for location in locations:
                    level = location["nestingLevel"]
                    loc = location["location"]
                    physloc = loc["physicalLocation"]
                    primary.create_nested_message(
                        file=GPS.File(physloc["artifactLocation"]["uri"]),
                        line=physloc["region"]["startLine"],
                        column=physloc["region"]["startColumn"],
                        text=get_secondary_message(location),
                    )
        except Exception as e:
            log_exception(None, e)
            continue


def load_messages(file_set):
    """
    Load the messages from a set of SarifFile

    :type file_set: set(SarifFile)
    :param file_set: Set of sarif files being loaded
    """

    if not file_set:
        # Handle the case where no files were valid
        return

    for file in file_set:
        for run in file.get_runs():
            results = run["results"]

            for result in results:
                rule_id = get_rule(result)
                tool_name = get_tool_name(run, result)
                if tool_name not in current_data.tools.keys():
                    tool = GPS.AnalysisTool(name=tool_name)
                    current_data.tools[tool_name] = tool
                if rule_id not in current_data.rules:
                    current_data.tools[tool_name].add_rule(rule_id, rule_id)
                    current_data.rules.append(rule_id)
                create_message(current_data.tools[tool_name], run, rule_id, result)

    if current_data.tools:
        # Don't set a tool here, we want to show all of them
        GPS.Analysis.display_report(None)


def is_sarif_file(context):
    if context.file():
        return context.file().path.endswith("sarif")
    else:
        return False


@interactive(
    name="Load SARIF contextual",
    description="Load current SARIF file",
    contextual="Load SARIF File in Analysis view",
    filter=is_sarif_file,
)
def load_current_sarif_file():
    current_data.clean()
    f = GPS.current_context().file()
    load_messages(load_files([f.path]))


@interactive(
    name="Load SARIF File",
    menu=MENU_PATH + "Load SARIF File",
    description="Select and load a SARIF file",
)
def load_sarif_file():
    current_data.clean()
    f = GPS.MDI.file_selector(file_filter="*.sarif")
    load_messages(load_files([f.path]))


@interactive(
    name="Load Multiple SARIF Files",
    menu=MENU_PATH + "Load Multiple SARIF Files",
    description="Select and load multiple SARIF files",
)
def load_sarif_file():
    current_data.clean()
    file_filter = Gtk.FileFilter()
    file_filter.set_name("SARIF Files")
    file_filter.add_pattern("*.sarif")
    file_chooser = Gtk.FileChooserDialog()
    file_chooser.add_buttons(
        Gtk.STOCK_CANCEL,
        Gtk.ResponseType.CANCEL,
        Gtk.STOCK_OPEN,
        Gtk.ResponseType.OK,
    )
    file_chooser.set_select_multiple(True)
    file_chooser.set_filter(file_filter)
    response = file_chooser.run()
    if response == Gtk.ResponseType.OK:
        files = file_chooser.get_filenames()
        GPS.Console().write(str(files))
    else:
        files = []
    file_chooser.destroy()
    if files:
        load_messages(load_files(files))


@interactive(
    name="Add SARIF File",
    menu=MENU_PATH + "Load and Add SARIF File",
    description="Select and add a SARIF file to the current report",
)
def load_sarif_file():
    f = GPS.MDI.file_selector(file_filter="*.sarif")
    load_messages(load_files([f.path]))


@interactive(
    name="Clean SARIF Messages",
    menu=MENU_PATH + "Clean Messages",
    description="Clean messages loaded from SARIF files",
)
def load_sarif_file():
    current_data.clean()
