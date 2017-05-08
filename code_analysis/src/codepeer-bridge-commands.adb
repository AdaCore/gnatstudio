------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with Ada.Characters.Handling;

with XML_Utils;

package body CodePeer.Bridge.Commands is

   procedure Audit_Trail_V3
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File;
      Message_Id        : Positive);
   --  Generates request of message's audit trail in format version 3.

   procedure Audit_Trail_V4_V5
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Export_File_Name    : Virtual_File;
      Messages            : CodePeer.Message_Vectors.Vector;
      Version             : Supported_Format_Version);
   --  Generates request of messages' audit trail in format version 4 and 5.

   -------------------------
   -- Add_Audit_Record_V3 --
   -------------------------

   procedure Add_Audit_Record_V3
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Ids               : Natural_Sets.Set;
      Status            : CodePeer.Audit_Status_Kinds;
      Approved_By       : Unbounded_String;
      Comment           : Unbounded_String)
   is
      Database_Node  : XML_Utils.Node_Ptr :=
                         new XML_Utils.Node'
                               (Tag    => new String'("database"),
                                others => <>);
      Add_Audit_Node : XML_Utils.Node_Ptr;
      Position       : Natural_Sets.Cursor := Ids.First;

   begin
      XML_Utils.Set_Attribute (Database_Node, "format", "3");
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.

      while Natural_Sets.Has_Element (Position) loop
         Add_Audit_Node :=
           new XML_Utils.Node'
             (Tag    => new String'("add_audit_record"),
              Value  => new String'(To_String (Comment)),
              others => <>);
         XML_Utils.Set_Attribute
           (Add_Audit_Node,
            "message",
            Positive'Image (Natural_Sets.Element (Position)));
         XML_Utils.Set_Attribute
           (Add_Audit_Node, "status", Audit_Status_Kinds'Image (Status));
         XML_Utils.Set_Attribute
           (Add_Audit_Node, "approved", To_String (Approved_By));

         XML_Utils.Add_Child (Database_Node, Add_Audit_Node);

         Natural_Sets.Next (Position);
      end loop;

      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Add_Audit_Record_V3;

   ----------------------------
   -- Add_Audit_Record_V4_V5 --
   ----------------------------

   procedure Add_Audit_Record_V4_V5
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Messages            : Message_Vectors.Vector;
      Version             : Supported_Format_Version)
   is
      Database_Node  : XML_Utils.Node_Ptr :=
                         new XML_Utils.Node'
                               (Tag    => new String'("database"),
                                others => <>);
      Add_Audit_Node : constant XML_Utils.Node_Ptr :=
                         new XML_Utils.Node'
                               (Tag    => new String'("add_audit_record"),
                                others => <>);
      Message_Node   : XML_Utils.Node_Ptr;

   begin
      XML_Utils.Set_Attribute
        (Database_Node, "format", Supported_Format_Version'Image (Version));
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      XML_Utils.Set_Attribute
        (Database_Node, "db_directory", +DB_Directory.Full_Name);

      if Message_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node, "message_patterns", +Message_Patterns.Full_Name);
      end if;

      if Additional_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node,
            "additional_patterns", +Additional_Patterns.Full_Name);
      end if;

      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Add_Child (Database_Node, Add_Audit_Node);

      for Message of Messages loop
         Message_Node :=
           new XML_Utils.Node'
             (Tag    => new String'("message"),
              Value  =>
                 new String'
                       (To_String (Message.Audit_V3.First_Element.Comment)),
              others => <>);
         XML_Utils.Set_Attribute
           (Message_Node, "identifier", Positive'Image (Message.Id));
         XML_Utils.Set_Attribute
           (Message_Node,
            "status",
            Audit_Status_Kinds'Image (Message.Audit_V3.First_Element.Status));
         XML_Utils.Set_Attribute
           (Message_Node,
            "approved",
            To_String (Message.Audit_V3.First_Element.Approved_By));

         XML_Utils.Add_Child (Add_Audit_Node, Message_Node);
      end loop;

      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Add_Audit_Record_V4_V5;

   -----------------
   -- Audit_Trail --
   -----------------

   procedure Audit_Trail
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Export_File_Name    : Virtual_File;
      Messages            : CodePeer.Message_Vectors.Vector;
      Version             : Supported_Format_Version) is
   begin
      case Version is
         when 3 =>
            Audit_Trail_V3
              (Command_File_Name,
               Output_Directory,
               Export_File_Name,
               Messages.First_Element.Id);

         when 4 | 5 =>
            Audit_Trail_V4_V5
              (Command_File_Name,
               Output_Directory,
               DB_Directory,
               Message_Patterns,
               Additional_Patterns,
               Export_File_Name,
               Messages,
               Version);
      end case;
   end Audit_Trail;

   --------------------
   -- Audit_Trail_V3 --
   --------------------

   procedure Audit_Trail_V3
     (Command_File_Name : Virtual_File;
      Output_Directory  : Virtual_File;
      Export_File_Name  : Virtual_File;
      Message_Id        : Positive)
   is
      Database_Node    : XML_Utils.Node_Ptr :=
                           new XML_Utils.Node'
                             (Tag    => new String'("database"),
                              others => <>);
      Audit_Trail_Node : constant XML_Utils.Node_Ptr :=
                           new XML_Utils.Node'
                             (Tag    => new String'("audit_trail"),
                              others => <>);

   begin
      XML_Utils.Set_Attribute (Database_Node, "format", "3");
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
   end Audit_Trail_V3;

   -----------------------
   -- Audit_Trail_V4_V5 --
   -----------------------

   procedure Audit_Trail_V4_V5
     (Command_File_Name   : Virtual_File;
      Output_Directory    : Virtual_File;
      DB_Directory        : Virtual_File;
      Message_Patterns    : Virtual_File;
      Additional_Patterns : Virtual_File;
      Export_File_Name    : Virtual_File;
      Messages            : CodePeer.Message_Vectors.Vector;
      Version             : Supported_Format_Version)
   is
      Database_Node    : XML_Utils.Node_Ptr :=
                           new XML_Utils.Node'
                             (Tag    => new String'("database"),
                              others => <>);
      Audit_Trail_Node : constant XML_Utils.Node_Ptr :=
                           new XML_Utils.Node'
                             (Tag    => new String'("audit_trail"),
                              others => <>);
      Ids              : Unbounded_String;

   begin
      --  Create list of identifiers of messages for which audit trail was not
      --  loaded.

      for Message of Messages loop
         if not Message.Audit_Loaded then
            Append (Ids, Natural'Image (Message.Id));
         end if;
      end loop;

      XML_Utils.Set_Attribute
        (Database_Node, "format", Supported_Format_Version'Image (Version));
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      XML_Utils.Set_Attribute
        (Database_Node, "db_directory", +DB_Directory.Full_Name);

      if Message_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node, "message_patterns", +Message_Patterns.Full_Name);
      end if;

      if Additional_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node,
            "additional_patterns", +Additional_Patterns.Full_Name);
      end if;

      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute (Audit_Trail_Node, "messages", To_String (Ids));
      XML_Utils.Set_Attribute
        (Audit_Trail_Node, "output_file", +Export_File_Name.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Add_Child (Database_Node, Audit_Trail_Node);
      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Audit_Trail_V4_V5;

   ----------------
   -- Inspection --
   ----------------

   procedure Inspection
     (Command_File_Name    : Virtual_File;
      Output_Directory     : Virtual_File;
      DB_Directory         : Virtual_File;
      Message_Patterns     : Virtual_File;
      Additional_Patterns  : Virtual_File;
      Inspection_File_Name : Virtual_File;
      Status_File_Name     : Virtual_File;
      Import_Annotations   : Boolean;
      Maximum_Version      : Format_Version)
   is
      Database_Node   : XML_Utils.Node_Ptr :=
                          new XML_Utils.Node'
                            (Tag    => new String'("database"),
                             others => <>);
      Inspection_Node : constant XML_Utils.Node_Ptr :=
                          new XML_Utils.Node'
                            (Tag    => new String'("inspection"),
                             others => <>);

   begin
      XML_Utils.Set_Attribute
        (Database_Node,
         "maximum_format",
         Format_Version'Image (Maximum_Version));
      XML_Utils.Set_Attribute
        (Database_Node, "output_directory", +Output_Directory.Full_Name);
      XML_Utils.Set_Attribute
        (Database_Node, "db_directory", +DB_Directory.Full_Name);

      if Message_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node, "message_patterns", +Message_Patterns.Full_Name);
      end if;

      if Additional_Patterns /= No_File then
         XML_Utils.Set_Attribute
           (Database_Node,
            "additional_patterns", +Additional_Patterns.Full_Name);
      end if;

      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute
        (Database_Node,
         "display_progress",
         "true");
      XML_Utils.Set_Attribute
        (Inspection_Node, "output_file", +Inspection_File_Name.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute
        (Inspection_Node, "status_file", +Status_File_Name.Full_Name);
      --  ??? Potentially non-utf8 string should not be
      --  stored in an XML attribute.
      XML_Utils.Set_Attribute
        (Inspection_Node,
         "export_annotations",
         Ada.Characters.Handling.To_Lower
           (Boolean'Image (Import_Annotations)));
      XML_Utils.Add_Child (Database_Node, Inspection_Node);
      XML_Utils.Print (Database_Node, Command_File_Name);
      XML_Utils.Free (Database_Node);
   end Inspection;

end CodePeer.Bridge.Commands;
