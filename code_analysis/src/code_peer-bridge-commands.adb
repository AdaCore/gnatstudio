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

   ----------------------
   -- Add_Audit_Record --
   ----------------------

   procedure Add_Audit_Record
     (Command_File_Name   : Filesystem_String;
      Output_Directory    : Filesystem_String;
      Message_Id          : Positive;
      Probability_Changed : Boolean;
      New_Probability     : Code_Peer.Message_Probability_Level;
      Comment             : String)
   is
      Database_Node  : Glib.Xml_Int.Node_Ptr :=
                         new Glib.Xml_Int.Node'
                               (Tag    => new String'("database"),
                                others => <>);
      Add_Audit_Node : constant Glib.Xml_Int.Node_Ptr :=
                         new Glib.Xml_Int.Node'
                               (Tag    => new String'("add_audit_record"),
                                Value  => new String'(Comment),
                                others => <>);

   begin
      Glib.Xml_Int.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      Glib.Xml_Int.Set_Attribute
        (Add_Audit_Node, "message", Positive'Image (Message_Id));

      if Probability_Changed then
         Glib.Xml_Int.Set_Attribute
           (Add_Audit_Node,
            "probability",
            Code_Peer.Message_Probability_Level'Image (New_Probability));
      end if;

      Glib.Xml_Int.Add_Child (Database_Node, Add_Audit_Node);
      Glib.Xml_Int.Print (Database_Node, +Command_File_Name);
      Glib.Xml_Int.Free (Database_Node);
   end Add_Audit_Record;

   -----------------
   -- Audit_Trail --
   -----------------

   procedure Audit_Trail
     (Command_File_Name : Filesystem_String;
      Output_Directory  : Filesystem_String;
      Export_File_Name  : Filesystem_String;
      Message_Id        : Positive)
   is
      Database_Node      : Glib.Xml_Int.Node_Ptr :=
                             new Glib.Xml_Int.Node'
                                   (Tag    => new String'("database"),
                                    others => <>);
      Audit_Trail_Node   : constant Glib.Xml_Int.Node_Ptr :=
                             new Glib.Xml_Int.Node'
                                   (Tag    => new String'("audit_trail"),
                                    others => <>);

   begin
      Glib.Xml_Int.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      Glib.Xml_Int.Set_Attribute
        (Audit_Trail_Node, "message", Positive'Image (Message_Id));
      Glib.Xml_Int.Set_Attribute
        (Audit_Trail_Node, "output_file", +Export_File_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      Glib.Xml_Int.Add_Child (Database_Node, Audit_Trail_Node);
      Glib.Xml_Int.Print (Database_Node, +Command_File_Name);
      Glib.Xml_Int.Free (Database_Node);
   end Audit_Trail;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection
     (Command_File_Name : Filesystem_String;
      Output_Directory  : Filesystem_String;
      Export_File_Name  : Filesystem_String)
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
        (Database_Node, "output_directory", +Output_Directory);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      Glib.Xml_Int.Set_Attribute
        (Inspection_Node, "output_file", +Export_File_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      Glib.Xml_Int.Add_Child (Database_Node, Inspection_Node);
      Glib.Xml_Int.Print (Database_Node, +Command_File_Name);
      Glib.Xml_Int.Free (Database_Node);
   end Inspection;

end Code_Peer.Bridge.Commands;
