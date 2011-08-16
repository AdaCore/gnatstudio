-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2009-2011, AdaCore                 --
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

with XML_Utils;

package body Code_Peer.Bridge.Commands is

   ----------------------
   -- Add_Audit_Record --
   ----------------------

   procedure Add_Audit_Record
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      Ids                 : Natural_Sets.Set;
      Probability_Changed : Boolean;
      New_Ranking         : Code_Peer.Message_Ranking_Level;
      Comment             : String)
   is
      Database_Node  : XML_Utils.Node_Ptr :=
                         new XML_Utils.Node'
                               (Tag    => new String'("database"),
                                others => <>);
      Add_Audit_Node : XML_Utils.Node_Ptr;
      Position       : Natural_Sets.Cursor := Ids.First;

   begin
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.

      while Natural_Sets.Has_Element (Position) loop
         Add_Audit_Node :=
           new XML_Utils.Node'
             (Tag    => new String'("add_audit_record"),
              Value  => new String'(Comment),
              others => <>);
         XML_Utils.Set_Attribute
           (Add_Audit_Node,
            "message",
            Positive'Image (Natural_Sets.Element (Position)));

         if Probability_Changed then
            XML_Utils.Set_Attribute
              (Add_Audit_Node,
               "probability",
               Code_Peer.Message_Ranking_Level'Image (New_Ranking));
         end if;

         XML_Utils.Add_Child (Database_Node, Add_Audit_Node);

         Natural_Sets.Next (Position);
      end loop;

      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Add_Audit_Record;

   -----------------
   -- Audit_Trail --
   -----------------

   procedure Audit_Trail
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File;
      Message_Id        : Positive)
   is
      Database_Node      : XML_Utils.Node_Ptr :=
                             new XML_Utils.Node'
                                   (Tag    => new String'("database"),
                                    others => <>);
      Audit_Trail_Node   : constant XML_Utils.Node_Ptr :=
                             new XML_Utils.Node'
                                   (Tag    => new String'("audit_trail"),
                                    others => <>);

   begin
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute
        (Audit_Trail_Node, "message", Positive'Image (Message_Id));
      XML_Utils.Set_Attribute
        (Audit_Trail_Node, "output_file", +Export_File_Name.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Add_Child (Database_Node, Audit_Trail_Node);
      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Audit_Trail;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File)
   is
      Database_Node      : XML_Utils.Node_Ptr :=
        new XML_Utils.Node'
          (Tag    => new String'("database"),
           others => <>);
      Inspection_Node    : constant XML_Utils.Node_Ptr :=
        new XML_Utils.Node'
          (Tag    => new String'("inspection"),
           others => <>);

   begin
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute
        (Inspection_Node, "output_file", +Export_File_Name.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Add_Child (Database_Node, Inspection_Node);
      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Inspection;

end Code_Peer.Bridge.Commands;
