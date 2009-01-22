-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Xml_Int;

package body Code_Peer.Bridge.Commands is

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection
     (Command_File_Name : String;
      Output_Directory  : String;
      Export_File_Name  : String)
   is
      Database_Node      : Glib.Xml_Int.Node_Ptr :=
        new Glib.Xml_Int.Node'
          (Tag    => new String'("database"),
           others => <>);
      Inspection_Node    : constant Glib.Xml_Int.Node_Ptr :=
        new Glib.Xml_Int.Node'
          (Tag    => new String'("inspection"),
           others => <>);

   begin
      Glib.Xml_Int.Set_Attribute
        (Database_Node, "output_directory", Output_Directory);
      Glib.Xml_Int.Set_Attribute
        (Inspection_Node, "output_file", Export_File_Name);
      Glib.Xml_Int.Add_Child (Database_Node, Inspection_Node);
      Glib.Xml_Int.Print (Database_Node, Command_File_Name);
      Glib.Xml_Int.Free (Database_Node);
   end Inspection;

end Code_Peer.Bridge.Commands;
