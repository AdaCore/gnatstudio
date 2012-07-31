------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
with GNAT.Regpat;

package body GPS.Kernel.Messages.Markup is

   use Ada.Strings.Unbounded;
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
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags)
      return not null Message_Access;

   function Create_Markup_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags)
      return not null Markup_Message_Access;

   Markup_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("<.*?>");

   ---------------------------
   -- Create_Markup_Message --
   ---------------------------

   function Create_Markup_Message
     (Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
      Text      : String;
      Weight    : Natural;
      Flags     : Message_Flags)
      return not null Markup_Message_Access is
   begin
      return
        Create_Markup_Message
          (Container,
           Category,
           File,
           Line,
           Column,
           Text,
           Weight,
           Line,
           Integer (Column),
           Flags);
   end Create_Markup_Message;

   ---------------------------
   -- Create_Markup_Message --
   ---------------------------

   function Create_Markup_Message
     (Container     : not null Messages_Container_Access;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Line          : Natural;
      Column        : Basic_Types.Visible_Column_Type;
      Text          : String;
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags)
      return not null Markup_Message_Access
   is
      Result : constant not null Markup_Message_Access := new Markup_Message;

   begin
      Result.Text := To_Unbounded_String (Text);

      Initialize
        (Result,
         Container,
         Category,
         File,
         Line,
         Column,
         Weight,
         Actual_Line,
         Actual_Column,
         Flags);

      return Result;
   end Create_Markup_Message;

   ----------------
   -- Get_Markup --
   ----------------

   overriding function Get_Markup
     (Self : not null access constant Markup_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Self.Text;
   end Get_Markup;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant Markup_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use type GNAT.Regpat.Match_Location;

      Source : constant String := Ada.Strings.Unbounded.To_String (Self.Text);
      Match  : GNAT.Regpat.Match_Array (0 .. 1);
      First  : Natural := Source'First;
      Result : Ada.Strings.Unbounded.Unbounded_String;

   begin
      --  Lookup for and remove all markups.

      loop
         GNAT.Regpat.Match (Markup_Pattern, Source, Match, First);

         if Match (0) = GNAT.Regpat.No_Match then
            Ada.Strings.Unbounded.Append
              (Result, Source (First .. Source'Last));

            exit;

         else
            Ada.Strings.Unbounded.Append
              (Result, Source (First .. Match (0).First - 1));
            First := Match (0).Last + 1;
         end if;
      end loop;

      return Result;
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
      Weight        : Natural;
      Actual_Line   : Integer;
      Actual_Column : Integer;
      Flags         : Message_Flags)
      return not null Message_Access
   is
      Text : constant String := Get_Attribute (XML_Node, "text", "");

   begin
      return
        Message_Access
          (Create_Markup_Message
               (Container,
                Category,
                File,
                Line,
                Column,
                Text,
                Weight,
                Actual_Line,
                Actual_Column,
                Flags));
   end Load;

   --------------
   -- Register --
   --------------

   procedure Register (Container : not null access Messages_Container'Class) is
   begin
      Container.Register_Message_Class
        (Markup_Message'Tag, Save'Access, Load'Access, null);
   end Register;

   ----------
   -- Save --
   ----------

   procedure Save
     (Message_Node : not null Message_Access;
      XML_Node     : not null Node_Ptr)
   is
      Self : constant Markup_Message_Access :=
               Markup_Message_Access (Message_Node);

   begin
      Set_Attribute (XML_Node, "text", To_String (Self.Text));
   end Save;

end GPS.Kernel.Messages.Markup;
