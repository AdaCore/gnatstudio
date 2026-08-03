"""
This Sarif plugin creates several actions to load Sarif files in the Analysis
view.

You can access the action via the menu "Analyze > Load SARIF"
Or the contextual menu "Load current SARIF file"
"""

import json
import os.path
from urllib.parse import urlparse, unquote

import GPS
from gi.repository import Gdk, GLib, Gtk
from gs_utils import interactive
from pygps import place_window_under_cursor
from theme_handling import Color

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
            GPS.Console("Messages").write(
                f"{sarif_file.path} has already been loaded into the Analysis view\n"
            )
        else:
            self.files.append(sarif_file)


class PreparedReplacement:
    """A single text replacement tracked by EditorMarks."""

    def __init__(
        self, start_mark, end_mark, inserted_text, is_insertion_point, original_text
    ):
        self.start_mark = start_mark
        self.end_mark = end_mark
        self.inserted_text = inserted_text
        self.is_insertion_point = is_insertion_point
        self.original_text = original_text


class PreparedChange:
    """Set of replacements targeting a single file."""

    def __init__(self, file_path, replacements):
        self.file_path = file_path
        self.replacements = replacements


class PreparedFix:
    """A SARIF fix with EditorMarks tracking replacement positions."""

    def __init__(self, description, preview, changes):
        self.description = description
        self.preview = preview
        self.changes = changes


current_data = LoadedData()


def log_exception(file, e):
    if file:
        GPS.Console("Messages").write(
            f"Failed to load SARIF file '{file}'. Check GNAT Studio's log file for more information.\n",
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
    match severity.lower():
        case "error":
            return GPS.Message.Importance.HIGH
        case "warning":
            return GPS.Message.Importance.MEDIUM
        case "note":
            return GPS.Message.Importance.INFORMATIONAL
        case _:
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

    return (normalize_file_uri(file_path), line, column)


def get_message(result):
    """
    Return the message.

    :type result: Dict
    :param result: a sarif result extracted by sarif-tools
    """
    message_data = result["message"]
    return message_data.get("text", message_data.get("id", ""))


def get_secondary_message(result):
    """
    Return the secondary message with extra information

    :type location: Dict (see threadFlows description)
    :param location: a location in the threadFlows
    """
    text = get_message(result["location"])
    nesting_level = result.get("nestingLevel", 0)
    return f"{'   ' * nesting_level}{text}"


def get_tool_name(run, result):
    """
    Return the name of the tool for message

    :type location: Dict
    :param location: json data loaded from a sarif file
    """
    engine = result.get("properties", {}).get("engine", None)
    if engine is not None:
        return engine

    return run.get("tool", {}).get("driver", {}).get("name", "Sarif Loader")


def get_rule(result):
    """
    Return the rule for message

    :type location: Dict
    :param location: json data loaded from a sarif file
    """
    return result.get("ruleId", "")


def normalize_file_uri(uri):
    """Convert a URI to a local file path.

    Handles file:// scheme and percent-encoded characters.
    Non-file URIs and relative paths are returned as-is.
    """
    if not uri:
        return uri
    parsed = urlparse(uri)
    if parsed.scheme == "file":
        return unquote(parsed.path)
    if parsed.scheme == "":
        # Relative path or plain filename
        return unquote(uri)
    return uri


def get_fixes(result):
    """Return the list of fix objects from a SARIF result."""
    return result.get("fixes", [])


def get_fix_description(fix):
    """Return the description text of a fix, or a generated default."""
    desc = fix.get("description", {}).get("text", "")
    if desc:
        return desc
    changes = fix.get("artifactChanges", [])
    if changes:
        files = [
            os.path.basename(c.get("artifactLocation", {}).get("uri", ""))
            for c in changes
        ]
        return "Apply changes to %s" % ", ".join(files)
    return "Apply fix"


def get_fix_preview_markup(prepared_fix):
    """Generate a Pango markup preview showing the code after applying the fix.

    Deleted text is shown in red with strikethrough, inserted text in green bold.
    Surrounding context lines are shown in normal style.
    """
    CONTEXT_LINES = 1
    del_color = Color(
        from_pref=GPS.Preference("Diff-Side-Remove-Color").get()
    ).to_hex6_string()
    ins_color = Color(
        from_pref=GPS.Preference("Diff-Side-Append-Color").get()
    ).to_hex6_string()
    parts = []

    for change in prepared_fix.changes:
        try:
            buf = GPS.EditorBuffer.get(GPS.File(change.file_path))
        except Exception:
            parts.append(GLib.markup_escape_text(change.file_path))
            continue

        if len(prepared_fix.changes) > 1:
            parts.append(
                "<b>%s</b>"
                % GLib.markup_escape_text(os.path.basename(change.file_path))
            )

        # Determine the range of lines affected by all replacements
        min_line = None
        max_line = None
        for repl in change.replacements:
            sl = repl.start_mark.location().line()
            if repl.end_mark:
                el = repl.end_mark.location().line()
            else:
                el = sl
            if min_line is None or sl < min_line:
                min_line = sl
            if max_line is None or el > max_line:
                max_line = el

        if min_line is None:
            continue

        # Add context lines
        first_line = max(1, min_line - CONTEXT_LINES)
        last_line = min(buf.lines_count(), max_line + CONTEXT_LINES)

        # Get the source text for the affected range
        source = buf.get_chars(
            buf.at(first_line, 1), buf.at(last_line, 1).end_of_line()
        )
        source_lines = source.split("\n")

        # Build a list of (line_number, column_edits) for in-line markup.
        # Each edit: (start_col, end_col, inserted_text, is_insertion_point)
        # Columns are 1-based, end_col is exclusive (SARIF convention).
        line_edits = {}
        for repl in change.replacements:
            sl = repl.start_mark.location().line()
            sc = repl.start_mark.location().column()
            if repl.end_mark:
                el = repl.end_mark.location().line()
                ec = repl.end_mark.location().column() + 1  # make exclusive
            else:
                el = sl
                ec = sc

            # Handle single-line replacements inline,
            # multi-line ones as delete+insert blocks.
            if sl == el:
                edits = line_edits.setdefault(sl, [])
                edits.append((sc, ec, repl.inserted_text, repl.is_insertion_point))
            else:
                # Multi-line: mark first line from sc to end, middle lines
                # fully, last line from start to ec.
                edits = line_edits.setdefault(sl, [])
                edits.append((sc, None, repl.inserted_text, False))
                for mid in range(sl + 1, el):
                    edits_mid = line_edits.setdefault(mid, [])
                    edits_mid.append((1, None, "", False))
                edits_last = line_edits.setdefault(el, [])
                edits_last.append((1, ec, "", False))

        # Render each line with markup
        for i, line_text in enumerate(source_lines):
            line_num = first_line + i
            if line_num not in line_edits:
                parts.append(
                    '<span foreground="grey">%3d</span> %s'
                    % (line_num, GLib.markup_escape_text(line_text))
                )
            else:
                edits = sorted(line_edits[line_num], key=lambda e: e[0])
                markup = '<span foreground="grey">%3d</span> ' % line_num
                pos = 1  # 1-based column position
                for sc_e, ec_e, ins_text, is_ins in edits:
                    # Text before this edit
                    if sc_e > pos:
                        markup += GLib.markup_escape_text(line_text[pos - 1 : sc_e - 1])
                    # Deleted text (strikethrough red)
                    if not is_ins and ec_e is not None and ec_e > sc_e:
                        deleted = line_text[sc_e - 1 : ec_e - 1]
                        markup += (
                            '<span strikethrough="true" foreground="%s">%s</span>'
                            % (del_color, GLib.markup_escape_text(deleted))
                        )
                        pos = ec_e
                    elif not is_ins and ec_e is None:
                        # Delete to end of line
                        deleted = line_text[sc_e - 1 :]
                        markup += (
                            '<span strikethrough="true" foreground="%s">%s</span>'
                            % (del_color, GLib.markup_escape_text(deleted))
                        )
                        pos = len(line_text) + 1
                    else:
                        pos = sc_e
                    # Inserted text
                    if ins_text:
                        escaped = GLib.markup_escape_text(
                            ins_text.replace("\n", "\u21b5\n")
                        )
                        markup += '<span foreground="%s"><b>%s</b></span>' % (
                            ins_color,
                            escaped,
                        )
                # Remaining text after all edits
                if pos - 1 < len(line_text):
                    markup += GLib.markup_escape_text(line_text[pos - 1 :])
                parts.append(markup)

    font = GPS.Preference("Src-Editor-Reference-Style").get().split("@")[0]
    return (
        '<span font_desc="%s">' % GLib.markup_escape_text(font)
        + "\n".join(parts)
        + "</span>"
    )


def prepare_fix(fix):
    """
    Create EditorMarks for a fix's replacement positions.

    Marks track buffer modifications automatically, so fixes can be
    applied sequentially without coordinates becoming stale.
    """
    prepared_changes = []
    for change in fix.get("artifactChanges", []):
        uri = change.get("artifactLocation", {}).get("uri", "")
        file_path = normalize_file_uri(uri)

        try:
            buf = GPS.EditorBuffer.get(GPS.File(file_path))
        except Exception as e:
            log_exception(file_path, e)
            continue

        prepared_repls = []
        for repl in change.get("replacements", []):
            region = repl.get("deletedRegion", {})
            start_line = region.get("startLine", 1)
            start_col = region.get("startColumn", 1)
            end_line = region.get("endLine", start_line)
            end_col = region.get("endColumn", start_col)
            inserted = repl.get("insertedContent", {}).get("text", "")
            is_insertion_point = start_line == end_line and start_col == end_col

            start_mark = buf.at(start_line, start_col).create_mark(left_gravity=True)
            end_mark = None
            if not is_insertion_point:
                # SARIF endColumn is exclusive; mark the last char to delete
                end_mark = (buf.at(end_line, end_col) - 1).create_mark(
                    left_gravity=False
                )

            # Snapshot the original text for later corruption check
            if not is_insertion_point:
                original_text = buf.get_chars(
                    buf.at(start_line, start_col), buf.at(end_line, end_col) - 1
                )
            else:
                original_text = ""

            prepared_repls.append(
                PreparedReplacement(
                    start_mark, end_mark, inserted, is_insertion_point, original_text
                )
            )

        prepared_changes.append(PreparedChange(file_path, prepared_repls))

    pf = PreparedFix(
        description=get_fix_description(fix),
        preview="",
        changes=prepared_changes,
    )
    pf.preview = get_fix_preview_markup(pf)
    return pf


def apply_prepared_fix(prepared_fix):
    """
    Apply a fix using pre-created EditorMarks.

    Marks automatically track buffer modifications, so this works
    correctly even after other fixes have already been applied.
    Refuses to apply if the code at any replacement region has been
    manually modified since the fix was loaded.

    Returns True if the fix was applied, False otherwise.
    """
    for change in prepared_fix.changes:
        try:
            buf = GPS.EditorBuffer.get(GPS.File(change.file_path))
        except Exception as e:
            log_exception(change.file_path, e)
            return False

        # Check that none of the regions have been manually edited
        for repl in change.replacements:
            if not repl.is_insertion_point:
                current_text = buf.get_chars(
                    repl.start_mark.location(), repl.end_mark.location()
                )
                if current_text != repl.original_text:
                    GPS.Console("Messages").write(
                        "Cannot apply fix: code has been modified since "
                        "the fix was loaded.\n",
                        mode="error",
                    )
                    return False

        for repl in reversed(change.replacements):
            if not repl.is_insertion_point:
                buf.delete(repl.start_mark.location(), repl.end_mark.location())

            if repl.inserted_text:
                buf._insert_at_location(repl.start_mark.location(), repl.inserted_text)

    return True


def show_fix_proposals_menu(msg, prepared_fixes):
    """
    Display a popup menu listing fix proposals, similar to the entity
    proposals menu used for navigation.
    """
    NOTES_WIDTH = 400
    NOTES_HEIGHT = 150

    menu_win = Gtk.Window(type=Gtk.WindowType.TOPLEVEL)
    menu_win.set_type_hint(Gdk.WindowTypeHint.POPUP_MENU)
    menu_win.set_decorated(False)
    menu_win.set_resizable(False)
    menu_win.set_skip_taskbar_hint(True)
    menu_win.set_skip_pager_hint(True)
    menu_win.set_name("fix-proposals-menu")
    menu_win.get_style_context().add_class("menu")

    hbox = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=0)
    menu_win.add(hbox)

    # Left: scrolled tree view with fix descriptions
    scrolled = Gtk.ScrolledWindow()
    scrolled.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC)
    hbox.pack_start(scrolled, False, False, 0)

    store = Gtk.ListStore(str, int)
    for i, pf in enumerate(prepared_fixes):
        store.append([pf.description, i])

    tree = Gtk.TreeView(model=store)
    renderer = Gtk.CellRendererText()
    col = Gtk.TreeViewColumn("Fix", renderer, text=0)
    tree.append_column(col)
    tree.set_headers_visible(False)
    tree.set_activate_on_single_click(True)
    tree.set_hover_selection(True)
    scrolled.add(tree)

    # Separator
    sep = Gtk.Separator(orientation=Gtk.Orientation.VERTICAL)
    hbox.pack_start(sep, False, False, 0)

    # Right: preview/notes area
    notes_win = Gtk.ScrolledWindow()
    notes_win.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
    notes_win.set_size_request(NOTES_WIDTH, NOTES_HEIGHT)
    notes_win.get_style_context().add_class("notes")
    hbox.pack_start(notes_win, False, False, 0)

    def close_menu():
        try:
            GPS.Hook("mdi_child_selected").remove(on_child_selected)
        except Exception:
            pass
        menu_win.destroy()

    def on_row_activated(treeview, path, column):
        it = store.get_iter(path)
        idx = store.get_value(it, 1)
        close_menu()
        try:
            if apply_prepared_fix(prepared_fixes[idx]):
                msg.cancel_subprogram()
        except Exception as e:
            log_exception(None, e)

    def on_selection_changed(selection):
        model, it = selection.get_selected()
        if it is None:
            return
        idx = model.get_value(it, 1)
        for child in notes_win.get_children():
            notes_win.remove(child)
        label = Gtk.Label()
        label.set_markup(prepared_fixes[idx].preview)
        label.set_line_wrap(False)
        label.set_xalign(0)
        label.set_yalign(0)
        label.set_margin_start(5)
        label.set_margin_top(5)
        notes_win.add(label)
        notes_win.show_all()

    def on_key_press(widget, event):
        if event.keyval == Gdk.KEY_Escape:
            close_menu()
            return True
        return False

    def on_child_selected(hook_name, child):
        close_menu()

    tree.connect("row-activated", on_row_activated)
    tree.get_selection().connect("changed", on_selection_changed)
    menu_win.connect("key-press-event", on_key_press)

    menu_win.show_all()
    place_window_under_cursor(menu_win, flip_widget=notes_win)

    tree.grab_focus()

    # Close menu when MDI focus changes (user clicks elsewhere)
    GPS.Hook("mdi_child_selected").add(on_child_selected)


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

    fixes = get_fixes(result)
    if fixes:
        prepared = [prepare_fix(f) for f in fixes]

        def on_fix_applied(m, f):
            if apply_prepared_fix(f):
                m.cancel_subprogram()

        def on_fix_menu(m, f):
            show_fix_proposals_menu(m, f)

        if len(prepared) == 1:
            pf = prepared[0]
            msg.set_subprogram(
                lambda m, f=pf: on_fix_applied(m, f),
                "gps-codefix",
                pf.description,
            )
        else:
            msg.set_subprogram(
                lambda m, f=prepared: on_fix_menu(m, f),
                "gps-codefix",
                "Apply fix (%d available)" % len(prepared),
            )


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
