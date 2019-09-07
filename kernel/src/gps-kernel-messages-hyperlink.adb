------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Strings.Fixed;

with Glib.Convert;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Gtkada.Style; use Gtkada.Style;

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
      Actual_Column : Integer;
      Flags         : Message_Flags);

   procedure Create_Hyperlink_Message
     (Parent        : not null Message_Access;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      First         : Positive;
      Last          : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags);
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
      Last   : Natural;
      Flags  : Message_Flags) is
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
         Integer (Column),
         Flags);
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
      Actual_Column : Integer;
      Flags         : Message_Flags)
   is
      Offset : constant Natural := Text'First - 1;
      Result : constant not null Hyperlink_Message_Access :=
                 new Hyperlink_Message;

   begin
      Result.Text  := To_Unbounded_String (Text);
      Result.First := First - Offset;
      Result.Last  := Last - Offset;

      Initialize
        (Result, Parent, File, Line, Column, Actual_Line, Actual_Column,
         Flags);
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
              & "<span color="""
              & To_Hex (Hyper_Links_Style.Get_Pref_Fg) & """><u>"
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
      Actual_Column : Integer;
      Flags         : Message_Flags)
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
         Actual_Column,
         Flags);
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
