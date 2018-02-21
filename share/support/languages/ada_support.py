"""GNAT support for GPS

This file provides support for switches for Ada and GNAT in the project editor.
This also includes predefined search patterns, as well as aliases to ease the
editing of Ada files.
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import gps_utils.gnat_rules
from gps_utils import hook


@hook('project_editor')
def __on_switch_editor():
    gps_utils.gnat_rules.EnsureInitialized()


XML = r"""<?xml version="1.0" ?>
<GPS>
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
</GPS>
"""

GPS.parse_xml(XML)


def __add_to_main_units(project, file):
    """
    Ask the user if he wants to add the newly created main unit to the
    project's main units.
    """

    unit = file.unit()
    dialog_msg = ("Do you want to add '%s' to the main units of "
                  "project '%s'?" % (unit, project.name()))

    if GPS.MDI.yes_no_dialog(dialog_msg):
        project.add_main_unit(unit)
        project.save()
        project.recompute()

    return True


@hook('gps_started')
def __on_gps_started():
    GPS.FileTemplate.register(
        alias_name="package",
        label="Ada Package",
        unit_param="name",
        language="ada",
        is_impl=False,
        impl_alias_name="package_body")

    GPS.FileTemplate.register(
        alias_name="main_unit",
        label="Ada Main Unit",
        unit_param="name",
        language="ada",
        is_impl=True,
        post_action=__add_to_main_units)
