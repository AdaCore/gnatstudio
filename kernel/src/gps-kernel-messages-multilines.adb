------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

with GPS.Kernel.Style_Manager; use GPS.Kernel.Style_Manager;

package body GPS.Kernel.Messages.Multilines is

   use XML_Utils;

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr);
   --  Saves additional data in the XML node

   function Load
     (XML_Node      : not null Node_Ptr;
      Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Importance    : Message_Importance_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Message_Access;
   --  Loads additional data from the XML node and creates primary simple
   --  message.

   procedure Load
     (XML_Node      : not null Node_Ptr;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags);
   --  Loads additional data from the XML node and creates secondary simple
   --  message.

   --------------------
   -- Create_Message --
   --------------------

   function Create_Message
     (Container                : not null Messages_Container_Access;
      Category                 : String;
      File                     : GNATCOLL.VFS.Virtual_File;
      Line                     : Natural;
      Column                   : Basic_Types.Visible_Column_Type;
      End_Line                 : Natural;
      End_Column               : Basic_Types.Visible_Column_Type;
      Text                     : String;
      Highlight_Category       : GPS.Kernel.Style_Manager.Style_Access;
      Importance               : Message_Importance_Type;
      Show_In_Locations        : Boolean;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Multiline_Message_Access
   is
      Result : constant Multiline_Message_Access :=
        new Multiline_Message (Primary);
   begin
      Result.End_Line := End_Line;
      Result.End_Column := End_Column;

      Initialize
        (Message                  => Result,
         Container                => Container,
         Category                 => Category,
         File                     => File,
         Line                     => Line,
         Column                   => Column,
         Text                     => Text,
         Importance               => Importance,
         Actual_Line              => Line,
         Actual_Column            => Integer (Column),
         Flags                    => (Editor_Side => True,
                                      Editor_Line => False,
                                      Locations   => Show_In_Locations),
         Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);

      if Highlight_Category /= null then
         Result.Set_Highlighting (Highlight_Category, 0);
      end if;
      return Result;
   end Create_Message;

   ------------------------------
   -- Has_Multine_Highlighting --
   ------------------------------

   overriding function Has_Multiline_Highlighting
     (Self : not null access constant Multiline_Message)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Has_Multiline_Highlighting;

   --------------------------------------
   -- Get_Multiline_Highlighting_Range --
   --------------------------------------

   overriding procedure Get_Multiline_Highlighting_Range
     (Self         : not null access constant Multiline_Message;
      Start_Line   : out Natural;
      Start_Column : out Basic_Types.Visible_Column_Type;
      End_Line     : out Natural;
      End_Column   : out Basic_Types.Visible_Column_Type) is
   begin
      Start_Line := Self.Line;
      Start_Column := Self.Column;
      End_Line := Self.End_Line;
      End_Column := Self.End_Column;
   end Get_Multiline_Highlighting_Range;

   --------------
   -- Register --
   --------------

   procedure Register (Container : not null access Messages_Container'Class) is
   begin
      Container.Register_Message_Class
        (Multiline_Message'Tag, Save'Access, Load'Access, Load'Access);
   end Register;

   ----------
   -- Load --
   ----------

   function Load
     (XML_Node      : not null Node_Ptr;
      Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Importance    : Message_Importance_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Message_Access
   is
      pragma Unreferenced (Actual_Line, Actual_Column);
      Text       : constant String := Get_Attribute_S (XML_Node, "text", "");
      End_Line   : constant Natural
        := Natural'Value (Get_Attribute_S (XML_Node, "end_line", "1"));
      End_Column : constant Basic_Types.Visible_Column_Type
        := Basic_Types.Visible_Column_Type'Value
          (Get_Attribute_S (XML_Node, "end_column", "1"));
   begin
      return
        Message_Access
          (Create_Message
             (Container                => Container,
              Category                 => Category,
              File                     => File,
              Line                     => Line,
              Column                   => Column,
              End_Line                 => End_Line,
              End_Column               => End_Column,
              Text                     => Text,
              Highlight_Category       => null,
              Importance               => Importance,
              Show_In_Locations        => Flags (Locations),
              Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First));
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (XML_Node      : not null Node_Ptr;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags) is
   begin
      --  Multiline secondary messages doesn't exist => do nothing
      null;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr)
   is
      Self : constant Multiline_Message_Access :=
        Multiline_Message_Access (Message_Node);
   begin
      Set_Attribute_S (XML_Node, "text", To_String (Self.Get_Text));
      Set_Attribute_S (XML_Node, "end_line", Self.End_Line'Image);
      Set_Attribute_S (XML_Node, "end_column", Self.End_Column'Image);
   end Save;

end GPS.Kernel.Messages.Multilines;
