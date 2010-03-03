-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Ada.Strings.Fixed;

with Glib.Convert;

package body GPS.Kernel.Messages.Hyperlink is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Glib.Convert;
   use XML_Utils;

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr);

   procedure Load
     (XML_Node      : not null Node_Ptr;
      Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Actual_Line   : Integer;
      Actual_Column : Integer);

   procedure Create_Hyperlink_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      First         : Positive;
      Last          : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer);
   --  Internal create subprogram

   ------------------------------
   -- Create_Hyperlink_Message --
   ------------------------------

   procedure Create_Hyperlink_Message
     (Parent : not null Message_Access;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type;
      Text   : String;
      First  : Positive;
      Last   : Natural) is
   begin
      Create_Hyperlink_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         First,
         Last,
         Line,
         Integer (Column));
   end Create_Hyperlink_Message;

   ------------------------------
   -- Create_Hyperlink_Message --
   ------------------------------

   procedure Create_Hyperlink_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      First         : Positive;
      Last          : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer)
   is
      Offset : constant Natural := Text'First - 1;
      Result : constant not null Hyperlink_Message_Access :=
                 new Hyperlink_Message;

   begin
      Result.Text  := To_Unbounded_String (Text);
      Result.First := First - Offset;
      Result.Last  := Last - Offset;

      Initialize
        (Result, Parent, File, Line, Column, Actual_Line, Actual_Column);
   end Create_Hyperlink_Message;

   ----------------
   -- Get_Markup --
   ----------------

   overriding function Get_Markup
     (Self : not null access constant Hyperlink_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      if Self.First > Self.Last then
         return To_Unbounded_String (Escape_Text (To_String (Self.Text)));

      else
         return
           To_Unbounded_String
             (Escape_Text (Slice (Self.Text, 1, Self.First - 1))
              & "<span color=""blue""><u>"
              & Escape_Text (Slice (Self.Text, Self.First, Self.Last))
              & "</u></span>"
              & Escape_Text
                (Slice (Self.Text, Self.Last + 1, Length (Self.Text))));
      end if;
   end Get_Markup;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant Hyperlink_Message)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Self.Text;
   end Get_Text;

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
      Text  : constant String := Get_Attribute (XML_Node, "text", "");
      First : constant Positive :=
                Positive'Value (Get_Attribute (XML_Node, "first", "1"));
      Last  : constant Natural :=
                Natural'Value (Get_Attribute (XML_Node, "last", "0"));

   begin
      Create_Hyperlink_Message
        (Parent,
         File,
         Line,
         Column,
         Text,
         First,
         Last,
         Actual_Line,
         Actual_Column);
   end Load;

   --------------
   -- Register --
   --------------

   procedure Register (Container : not null access Messages_Container'Class) is
   begin
      Container.Register_Message_Class
        (Hyperlink_Message'Tag, Save'Access, null, Load'Access);
   end Register;

   ----------
   -- Save --
   ----------

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr)
   is
      Self : constant Hyperlink_Message_Access :=
               Hyperlink_Message_Access (Message_Node);

   begin
      Set_Attribute (XML_Node, "text", To_String (Self.Text));

      if Self.Level = Secondary
        and then Self.First <= Self.Last
      then
         Set_Attribute
           (XML_Node, "first", Trim (Integer'Image (Self.First), Both));
         Set_Attribute
           (XML_Node, "last", Trim (Integer'Image (Self.Last), Both));
      end if;
   end Save;

end GPS.Kernel.Messages.Hyperlink;
