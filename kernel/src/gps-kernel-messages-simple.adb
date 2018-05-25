------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Glib.Convert;

package body GPS.Kernel.Messages.Simple is

   use Ada.Strings.Unbounded;
   use Glib.Convert;
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

   function Create_Simple_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Importance    : Message_Importance_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Simple_Message_Access;
   --  Internal create subprogram

   function Create_Simple_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags) return Simple_Message_Access;
   --  Creates new instance of secondary Simple_Message. For internal use only.

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   procedure Create_Simple_Message
     (Container  : not null Messages_Container_Access;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Basic_Types.Visible_Column_Type;
      Text       : String;
      Importance : Message_Importance_Type;
      Flags      : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
   is
      Aux : constant Simple_Message_Access :=
        Create_Simple_Message
          (Container, Category, File, Line, Column, Text, Importance, Flags,
           Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
      pragma Unreferenced (Aux);

   begin
      null;
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Container  : not null Messages_Container_Access;
      Category   : String;
      File       : GNATCOLL.VFS.Virtual_File;
      Line       : Natural;
      Column     : Basic_Types.Visible_Column_Type;
      Text       : String;
      Importance : Message_Importance_Type;
      Flags      : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Simple_Message_Access is
   begin
      return
        Create_Simple_Message
          (Container,
           Category,
           File,
           Line,
           Column,
           Text,
           Importance,
           Line,
           Integer (Column),
           Flags,
           Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Importance    : Message_Importance_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags;
      Allow_Auto_Jump_To_First : Boolean := True)
      return not null Simple_Message_Access
   is
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Primary);

   begin
      Result.Text := To_Unbounded_String (Text);

      Initialize
        (Result,
         Container,
         Category,
         File,
         Line,
         Column,
         Importance,
         Actual_Line,
         Actual_Column,
         Flags,
         Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);

      return Result;
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      Flags  : Message_Flags) return Simple_Message_Access is
   begin
      return Create_Simple_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         Line,
         Integer (Column),
         Flags);
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags) return Simple_Message_Access
   is
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Secondary);

   begin
      Result.Text := To_Unbounded_String (Text);

      Initialize
        (Result, Parent, File, Line, Column, Actual_Line, Actual_Column,
         Flags);

      return Result;
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   procedure Create_Simple_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      Flags  : Message_Flags)
   is
      Dummy : Simple_Message_Access;

   begin
      Dummy := Create_Simple_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         Line,
         Integer (Column),
         Flags);
   end Create_Simple_Message;

   ----------------
   -- Get_Markup --
   ----------------

   overriding function Get_Markup
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return To_Unbounded_String (Escape_Text (To_String (Self.Text)));
   end Get_Markup;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Self.Text;
   end Get_Text;

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
      Text : constant String := Get_Attribute (XML_Node, "text", "");

   begin
      return
        Message_Access
          (Create_Simple_Message
               (Container,
                Category,
                File,
                Line,
                Column,
                Text,
                Importance,
                Actual_Line,
                Actual_Column,
                Flags,
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
      Flags         : Message_Flags)
   is
      Text  : constant String := Get_Attribute (XML_Node, "text", "");
      Dummy : Simple_Message_Access;

   begin
      Dummy := Create_Simple_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         Actual_Line,
         Actual_Column,
         Flags);
   end Load;

   --------------
   -- Register --
   --------------

   procedure Register (Container : not null access Messages_Container'Class) is
   begin
      Container.Register_Message_Class
        (Simple_Message'Tag, Save'Access, Load'Access, Load'Access);
   end Register;

   ----------
   -- Save --
   ----------

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr)
   is
      Self : constant Simple_Message_Access :=
               Simple_Message_Access (Message_Node);

   begin
      Set_Attribute (XML_Node, "text", To_String (Self.Text));
   end Save;

end GPS.Kernel.Messages.Simple;
