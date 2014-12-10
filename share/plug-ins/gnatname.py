"""
This file adds target and target model for the gnatname utility.
See the menu /Build/Settings/Targets to configure toolbar icons to
easily launch gnatname.
"""


import GPS

XML = r"""<?xml version="1.0" ?>
<gnatname>
  <target-model name="gnatname" category="">
    <icon>gps-build-all</icon>
    <description>Generic launch of gnatname</description>
    <command-line>
      <arg>%gnat</arg>
      <arg>name</arg>
    </command-line>
    <switches/>
  </target-model>

  <target model="gnatname" category="_Project_" name="gnatname">
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnat</arg>
      <arg>name</arg>
    </command-line>
  </target>
</gnatname>
"""

GPS.parse_xml(XML)
