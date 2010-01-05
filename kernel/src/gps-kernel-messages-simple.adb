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

package body GPS.Kernel.Messages.Simple is

   use Ada.Strings.Unbounded;

   ---------------------------
   -- Create_Simple_Message --
   ---------------------------

   function Create_Simple_Message
     (Container : not null Messages_Container_Access;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Basic_Types.Visible_Column_Type;
      Text      : String)
      return not null Simple_Message_Access
   is
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Primary);

   begin
      Initialize (Result, Container, Category, File, Line, Column);
      Result.Text := To_Unbounded_String (Text);

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
      First  : Positive;
      Last   : Natural)
   is
      Offset : constant Natural := Text'First - 1;
      Result : constant not null Simple_Message_Access :=
        new Simple_Message (Secondary);

   begin
      Initialize (Result, Parent, File, Line, Column);
      Result.Text  := To_Unbounded_String (Text);
      Result.First := First - Offset;
      Result.Last  := Last - Offset;
   end Create_Simple_Message;

   ----------------
   -- Get_Markup --
   ----------------

   overriding function Get_Markup
     (Self : not null access constant Simple_Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      if Self.First > Self.Last then
         return Self.Text;

      else
         return
           Unbounded_Slice (Self.Text, 1, Self.First - 1)
           & "<span color=""blue""><u>"
           & Unbounded_Slice (Self.Text, Self.First, Self.Last)
           & "</u></span>"
           & Unbounded_Slice (Self.Text, Self.Last + 1, Length (Self.Text));
      end if;
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

end GPS.Kernel.Messages.Simple;
