import datetime
import GPS
import libadalang as lal
from gps_utils import interactive


def get_current_year():
    now = datetime.datetime.now()
    return str(now.year)


# Create some GPS specific file templates
gps_aliases_xml = """<alias name="package_gps_header" >
    <param name="name" description="The name of the GPS Ada package." />
    <text>------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) {y}, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package %(name) is

   %_

end %(name);</text>
  </alias>

  <alias name="package_gps_header_body" >
    <param name="name"  />
    <text>------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) {y}, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body %(name) is

   %_

end %(name);</text>
  </alias>""".format(y=get_current_year())


def flag_storage_of_project_type(source_file):
    """Flag all dangerous storage of a Project_Type in the given file"""
    c = lal.AnalysisContext()
    a = c.get_from_file(source_file.path)

    # for safety, in case libadalang cannot process a file
    if not a.root:
        GPS.Console().write(
            "libadalang could not process {}\n".format(source_file))
        return

    # Find all record definitions in the unit
    for record in a.root.findall(lal.RecordTypeDef):

        # ... look at all the fields in these records
        for field in record.findall(lal.ComponentDecl):

            # if the component def contains "Project_Type" flag the location
            if 'Project_Type' in field.f_component_def.text:
                GPS.Message("Dangerous storing of Project_Type",
                            source_file,
                            int(field.sloc_range.start.line),
                            field.sloc_range.start.column,
                            "warning: you should not store a Project_Type")


def initialize_project_plugin():
    """Called automatically when gps.gpr is loaded"""
    # Create a specific menu under Analyze/GPS to flag

    if not GPS.Action("find project types stored").exists():
        @interactive(category="General", name="find project types stored",
                     menu="/Analyze/GPS/Flag stored Project Types")
        def find_project_types_stored():
            all_ada_sources = filter(lambda x: x.language().lower() == 'ada',
                                     GPS.Project.root().sources(
                                         recursive=True))

            for source_file in all_ada_sources:
                flag_storage_of_project_type(source_file)

    if not GPS.Action("new gps ada package").exists():
        # Register the GPS package file templates
        GPS.parse_xml(gps_aliases_xml)
        GPS.FileTemplate.register(
            alias_name="package_gps_header",
            label="GPS Ada Package",
            unit_param="name",
            language="ada",
            is_impl=False,
            impl_alias_name="package_gps_header_body")


def finalize_project_plugin():
    """Called automatically when gps.gpr is unloaded"""
    pass
