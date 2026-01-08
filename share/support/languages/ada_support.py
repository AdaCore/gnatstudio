"""GNAT support for GNAT Studio

This file provides support for switches for Ada and GNAT in the project editor.
This also includes predefined search patterns, as well as aliases to ease the
editing of Ada files.
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import gs_utils.gnat_rules
from gs_utils import hook
from workflows import run_as_workflow


@hook("project_editor")
def __on_switch_editor():
    gs_utils.gnat_rules.EnsureInitialized()


XML = r"""<?xml version="1.0" ?>
<GNAT_Studio>
   <vsearch-pattern>
     <name>Ada: type NAME is array (...)</name>
     <regexp>\btype\s+(\w+)\s+is\s+array\s+\((.*?)\)\s+of\s+\w+\s*;</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>Ada: when CHOICE =></name>
     <regexp>\bwhen\s+((\w+)\s+:\s+)?[\w\s|]+\s*=></regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>Ada: (sub)type NAME is</name>
     <regexp>\b((sub)?type\s+(\w+)|type\s+(\w+)\s+(\(.*?\))?)\s+is\b</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>Ada: type NAME (...) is</name>
     <regexp>\btype\s+(\w+)\s+\((.*?)\)\s+is\b</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>Ada: for VAR in ... loop</name>
     <regexp>\bfor\s+(\w+)\s+in\s+(reverse\s+)?(.+?)(\s+range\s+(.*?))?\s+loop\b</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>Ada: assignment</name>
     <regexp>\b(\w+)\s*:=</regexp>
   </vsearch-pattern>

  <alias name="procedure_is" >
    <param name="name"  />
    <param name="params"  />
    <text>procedure %(name) (%(params)) is
begin
   %_
end %(name);</text>
  </alias>

  <alias name="main_unit" >
    <param name="name"  description="The name of the Ada main unit."/>
    <text>procedure %(name) is
begin
   %_
end %(name);</text>
  </alias>

  <alias name="task_body" >
    <param name="name"  />
    <text>task body %(name) is
begin
   %_
end %(name);</text>
  </alias>

  <alias name="select" >
    <param name="entry2"  />
    <param name="entry"  />
    <text>select
   accept %(entry) do
      %_
   end %(entry);
     or
   accept %(entry2) do
      null;
   end %(entry2);
end select;</text>
  </alias>

  <alias name="function_is" >
    <param name="name"  />
    <param name="params"  />
    <param name="return_type"  />
    <text>function %(name) (%(params)) return %(return_type) is
begin
   return %_
end %(name);</text>
  </alias>

  <alias name="declare" >
    <param name="variables" />
    <text>declare
   %(variables)
begin
   %_
end;</text>
  </alias>

  <alias name="for" >
    <param name="range"  />
    <param name="index"  />
    <text>for %(index) in %(range) loop
   %_
end loop;</text>
  </alias>

  <alias name="procedure" >
    <param name="name"  />
    <text>procedure %(name)%_;</text>
  </alias>

  <alias name="package_body" >
    <param name="name"  />
    <text>package body %(name) is

   %_

end %(name);</text>
  </alias>

  <alias name="task" >
    <param name="name"  />
    <text>task %(name) is
   %_
end %(name);</text>
  </alias>

  <alias name="loop" >
    <param name="exit_condition"  />
    <text>loop
   %_
   exit when %(exit_condition);
end loop;</text>
  </alias>

  <alias name="case" >
    <param name="choice"  />
    <param name="expression"  />
    <text>case %(expression) is
   when %(choice) =&gt;
      %_
end case;</text>
  </alias>

  <alias name="while" >
    <param name="condition"  />
    <text>while %(condition) loop
   %_
end loop;</text>
  </alias>

  <alias name="package" >
    <param name="name" description="The name of the Ada package." />
    <text>package %(name) is

   %_

end %(name);</text>
  </alias>

  <alias name="if" >
    <param name="condition"  />
    <text>if %(condition) then
   %_
end if;</text>
  </alias>

  <alias name="function" >
    <param name="name"  />
    <param name="params"  />
    <text>function %(name) (%(params)) return %_;</text>
  </alias>

  <alias name="array" >
    <param name="range"  />
    <text>array (%(range)) of %_;</text>
  </alias>

  <alias name="exception" >
    <param name="error"  />
    <text>exception
   when %(error) =&gt;
      %_</text>
  </alias>

  <alias name="begin" >
    <text>begin
   %_
end;</text>
  </alias>
</GNAT_Studio>
"""

GPS.parse_xml(XML)


@run_as_workflow
def __add_to_main_units(project, file):
    """
    Ask the user if he wants to add the newly created main unit to the
    project's main units.
    """

    def find_main_attribute(symbols):
        """
        Find the Main attribute location in the LSP document symbols tree.
        """

        def search_children(items):
            """
            Recursively search for Main attribute in LSP document
            symbols tree
            """
            for item in items:
                if item.get("name") == "Main":
                    return item.get("range", {})
                children = item.get("children", [])
                if children:
                    found = search_children(children)
                    if found:
                        return found
            return None

        main_range = search_children(symbols)
        if main_range:
            return {
                "line": main_range.get("start", {}).get("line", 0),
                "character": main_range.get("start", {}).get("character", 0),
            }
        return None

    def add_to_existing_main_list(buffer, location, main_unit_name):
        """
        Add main unit to an existing Main attribute list.
        Returns the location range of the inserted text.
        """
        loc = buffer.at(location["line"] + 1, location["character"] + 1)

        # Find the Main attribute list boundaries
        open_paren_loc, _ = loc.search(r"\(", regexp=True)
        close_paren_loc, _ = open_paren_loc.forward_char().search(r"\)", regexp=True)

        # Check if list is empty
        content = buffer.get_chars(
            open_paren_loc.forward_char(), close_paren_loc
        ).strip()

        # Create marks at the insertion point - marks track their position automatically
        insert_mark = close_paren_loc.create_mark()

        # Insert with appropriate separator
        separator = ", " if content else ""
        main_text = f'{separator}"{main_unit_name}"'

        buffer.insert(insert_mark.location(), main_text)

        # Select only the main unit name (without quotes)
        # Skip separator and opening quote
        select_start = insert_mark.location().forward_char(len(separator) + 1)
        select_end = select_start.forward_char(len(main_unit_name))

        insert_mark.delete()

        return select_start, select_end

    def create_new_main_attribute(buffer, main_unit_name):
        """
        Create a new Main attribute in the project file.
        Returns the location range of the inserted main unit name.
        """
        loc = buffer.beginning_of_buffer()

        # Find the project declaration
        _, end_loc = loc.search(
            r"\bproject\s+\w+\s+is\b", regexp=True, case_sensitive=False
        )

        # Insert after "project ... is" line
        insert_loc = end_loc.end_of_line().forward_char()

        # Create marks to track positions during insertion
        start_mark = insert_loc.create_mark()

        # Insert the entire attribute line
        text_before = "\n   for Main use ("
        main_text = f'"{main_unit_name}"'
        text_after = ");\n"
        full_text = text_before + main_text + text_after

        buffer.insert(start_mark.location(), full_text)

        # Select only the main unit name (without quotes)
        # Skip text_before and opening quote
        select_start = start_mark.location().forward_char(len(text_before) + 1)
        select_end = select_start.forward_char(len(main_unit_name))

        start_mark.delete()

        return select_start, select_end

    def show_error(message):
        """
        Display error message to user.
        """
        GPS.Console().write(f"{message}\n", mode="error")

    # Main workflow
    unit = file.unit()
    dialog_msg = "Do you want to add '%s' to the main units of project '%s'?" % (
        unit,
        project.name(),
    )

    if not GPS.MDI.yes_no_dialog(dialog_msg):
        return True

    main_unit_name = file.base_name()
    project_file = project.file()

    # Get the GPR language server
    gpr_ls = GPS.LanguageServer.get_by_language_name("project file")
    if gpr_ls is None:
        show_error(
            "GPR language server not available. " "Please add the main unit manually."
        )
        return False

    # Query document symbols
    params = {"textDocument": {"uri": project_file.uri}}
    result = yield gpr_ls.request_promise("textDocument/documentSymbol", params)

    if not result.is_valid or result.is_error:
        show_error(
            "Could not get document symbols from GPR language server. "
            "Please add the main unit manually."
        )
        return False

    # Find Main attribute location
    symbols = result.data if isinstance(result.data, list) else []
    main_attr_location = find_main_attribute(symbols)

    # Open the project file
    buffer = GPS.EditorBuffer.get(project_file)

    # Split the view if not already done
    GPS.MDIWindow.split(
        GPS.MDI.get_by_child(buffer.current_view()), vertically=False, reuse=True
    )

    with buffer.new_undo_group():
        if main_attr_location:
            select_start, select_end = add_to_existing_main_list(
                buffer, main_attr_location, main_unit_name
            )
        else:
            select_start, select_end = create_new_main_attribute(buffer, main_unit_name)

    # Give focus to the .gpr file
    GPS.MDI.get_by_child(buffer.current_view()).raise_window()

    # Save the .gpr project file
    buffer.save(interactive=False)

    # Reload the project if auto-reload is disabled
    if not GPS.Preference("Prj-Editor-Auto-Reload-Project").get():
        GPS.execute_action("reload project")

    # Select the newly added main unit, and center the editor on it
    buffer.select(select_start, select_end)
    buffer.current_view().center()

    return True


@hook("gps_started")
def __on_gps_started():
    GPS.FileTemplate.register(
        alias_name="package",
        label="Ada Package",
        unit_param="name",
        language="ada",
        is_impl=False,
        impl_alias_name="package_body",
    )

    GPS.FileTemplate.register(
        alias_name="main_unit",
        label="Ada Main Unit",
        unit_param="name",
        language="ada",
        is_impl=True,
        post_action=__add_to_main_units,
    )
