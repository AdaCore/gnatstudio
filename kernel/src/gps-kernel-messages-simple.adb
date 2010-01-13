-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glib.Convert;

package body GPS.Kernel.Messages.Simple is

   use Ada.Strings.Unbounded;
   use Glib.Convert;
   use XML_Utils;

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr);

   function Load
     (XML_Node      : not null Node_Ptr;
      Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer)
      return not null Message_Access;

   procedure Load
     (XML_Node      : not null Node_Ptr;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer);

   function Create_Simple_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Actual_Line   : Integer;
      Actual_Column : Integer)
      return not null Simple_Message_Access;
   --  Internal create subprogram

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String)
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
           Line,
           Integer (Column));
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
      Actual_Line   : Integer;
      Actual_Column : Integer)
      return not null Simple_Message_Access
   is
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Primary);

   begin
      Initialize
        (Result,
         Container,
         Category,
         File,
         Line,
         Column,
         Actual_Line,
         Actual_Column);
      Result.Text := To_Unbounded_String (Text);

      return Result;
   end Create_Simple_Message;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   procedure Create_Simple_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Actual_Line   : Integer;
      Actual_Column : Integer)
   is
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Secondary);

   begin
      Initialize
        (Result, Parent, File, Line, Column, Actual_Line, Actual_Column);
      Result.Text := To_Unbounded_String (Text);
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
      Actual_Line   : Integer;
      Actual_Column : Integer)
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
                Actual_Line,
                Actual_Column));
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
      Actual_Column : Integer)
   is
      Text : constant String := Get_Attribute (XML_Node, "text", "");

   begin
      Create_Simple_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         Actual_Line,
         Actual_Column);
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
