------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Default_Preferences.Enums;
with Glib.Unicode;
with GNATCOLL.Traces; use GNATCOLL.Traces;
with Src_Editor_Buffer.Line_Information;

package body Src_Editor_Buffer.Formatters is

   Me : constant Trace_Handle :=
     Create ("GPS.Source_Editor.Buffer.FORMATTERS");

   type Providers_Array
     is array (Known_Provider) of Editor_Formatting_Provider_Access;

   Providers : Providers_Array := (others => null);

   procedure Save_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : out Gint;
      Cursor_Offset : out Gint);
   --  Save the location of the Mark

   procedure Place_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : Gint;
      Cursor_Offset : Gint);
   --  Move the mark to the new location

   package Formatter_Selector is new
     Default_Preferences.Enums.Generics (Known_Provider);

   Range_Formatting_Provider_Pref   : Formatter_Selector.Preference;
   On_Type_Formatting_Provider_Pref : Formatter_Selector.Preference;
   Move_Cursor_When_Formatting      : Default_Preferences.Boolean_Preference;

   ----------------------
   -- Range_Formatting --
   ----------------------

   procedure Range_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False)
   is
      pragma Unreferenced (Force);
      G : Group_Block := New_Group (Buffer.Queue);

      Cursor_Line   : Gint;
      Cursor_Offset : Gint;

      From_Column : Visible_Column_Type;
      To_Column   : Visible_Column_Type;

      From_Line : Editable_Line_Type;
      To_Line   : Editable_Line_Type;

               Selected : constant Known_Provider :=
           Range_Formatting_Provider_Pref.Get_Pref;
         Provider : constant Editor_Formatting_Provider_Access :=
           Providers (Selected);
   begin
      if Provider = null then
         Trace
           (Me, "rangeFormatting Provider is not defined: " & Selected'Image);
         return;
      end if;

      Save_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);

      if Src_Editor_Buffer.Line_Information.Lines_Are_Real (Buffer) then
         Get_Iter_Position (Buffer, From, From_Line, From_Column);
         Get_Iter_Position (Buffer, To, To_Line, To_Column);
      else
         Trace (Me, "Unreal lines found when formatting");
         declare
            Search_Line : Gint;
            Start_Line  : Gint;
            End_Line    : Gint;
         begin
            Start_Line := Get_Line (From);
            End_Line := Get_Line (To);

            --  Gint and Buffer_Line_Type are off by 1
            From_Line :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (Start_Line + 1));
            To_Line :=
              Get_Editable_Line (Buffer, Buffer_Line_Type (End_Line + 1));

            --  Get_Editable_Line will return 0 for non-editable line.
            --  Search for the first and last editable lines between
            --  Start_Line and End_Line.

            --  Search upward for the first editable line
            Search_Line := Start_Line;
            while From_Line = 0 and then Search_Line < End_Line loop
               Search_Line := Search_Line + 1;
               --  Gint and Buffer_Line_Type are off by 1
               From_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Search_Line + 1));
            end loop;

            --  Search downward for the last editable line
            Search_Line := End_Line;
            while To_Line = 0 and then Search_Line > Start_Line loop
               Search_Line := Search_Line - 1;
               --  Gint and Buffer_Line_Type are off by 1
               To_Line :=
                 Get_Editable_Line
                   (Buffer, Buffer_Line_Type (Search_Line + 1));
            end loop;

            --  Include all characters from the last line
            To_Column :=
              Buffer.Editor_Buffer.New_Location (Integer (To_Line))
                .End_Of_Line
                .Column;
         end;
      end if;

      if not Provider.On_Range_Formatting
               (Buffer.Editor_Buffer.New_Location (Integer (From_Line), 1),
                Buffer.Editor_Buffer.New_Location
                  (Integer (To_Line), To_Column),
                Cursor_Line => Natural (Cursor_Line) + 1,
                Cursor_Move => Integer (Cursor_Offset))
      then
         Trace (Me, "Formatting failed");
      end if;

      Place_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);
   end Range_Formatting;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   procedure On_Type_Formatting
     (Buffer   : Source_Buffer;
      Mark     : Gtk_Text_Mark;
      From, To : Gtk_Text_Iter;
      Force    : Boolean := False)
   is
      pragma Unreferenced (Force);
      Cursor_Line   : Gint;
      Cursor_Offset : Gint;

      Start_Line, End_Line     : Editable_Line_Type;
      Start_Column, End_Column : Visible_Column_Type;
      Selected                 : constant Known_Provider :=
        On_Type_Formatting_Provider_Pref.Get_Pref;
      Provider                 : constant Editor_Formatting_Provider_Access :=
        Providers (Selected);
   begin
      if Provider = null then
         Trace
           (Me, "onTypeFormatting Provider is not defined: " & Selected'Image);
         return;
      end if;

      Save_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);

      Get_Iter_Position (Buffer, From, Start_Line, Start_Column);
      Get_Iter_Position (Buffer, To, End_Line, End_Column);

      if not Provider.On_Type_Formatting
               (Buffer.Editor_Buffer.New_Location
                  (Integer (Start_Line), Start_Column),
                Buffer.Editor_Buffer.New_Location
                  (Integer (End_Line), End_Column),
                Cursor_Line => Natural (Cursor_Line) + 1)
      then
         Trace (Me, "Fails to format on new line");
      end if;

      Place_Cursor
        (Buffer        => Buffer,
         Mark          => Mark,
         Cursor_Line   => Cursor_Line,
         Cursor_Offset => Cursor_Offset);
   end On_Type_Formatting;

   -----------------
   -- Save_Cursor --
   -----------------

   procedure Save_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : out Gint;
      Cursor_Offset : out Gint)
   is
      Cursor_Iter : Gtk_Text_Iter;
   begin
      Buffer.Do_Not_Move_Cursor := True;
      Get_Iter_At_Mark (Buffer, Cursor_Iter, Mark);
      Cursor_Line := Get_Line (Cursor_Iter);
      Cursor_Offset := Get_Line_Offset (Cursor_Iter);
   end Save_Cursor;

   ------------------
   -- Place_Cursor --
   ------------------

   procedure Place_Cursor
     (Buffer        : Source_Buffer;
      Mark          : Gtk_Text_Mark;
      Cursor_Line   : Gint;
      Cursor_Offset : Gint)
   is
      Cursor_Move : Gint := Cursor_Offset;
      Result      : Boolean := True;
      Offset      : Gint := 0;
      Iter        : Gtk_Text_Iter;
   begin
      if Move_Cursor_When_Formatting.Get_Pref and then Mark.Get_Deleted then
         return;
      end if;

      --  If the cursor was located before the first non-blank character,
      --  move it to that character. This is more usual for Emacs users,
      --  and more user friendly generally.
      Get_Iter_At_Line (Buffer, Iter, Cursor_Line);
      Set_Line_Offset (Iter, 0);

      while Result
        and then not Ends_Line (Iter)
        and then Glib.Unicode.Is_Space (Get_Char (Iter))
      loop
         Forward_Char (Iter, Result);
         Offset := Offset + 1;
      end loop;

      Cursor_Move := Cursor_Move - Offset;

      if Cursor_Move > 0 then
         Forward_Chars (Iter, Cursor_Move, Result);
      end if;

      Buffer.Do_Not_Move_Cursor := False;
      if Buffer.Insert_Mark = Mark then
         --  Move the main cursor
         Place_Cursor (Buffer, Iter);
      else
         --  Move multiline cursor
         Move_Mark (Buffer, Mark, Iter);
      end if;
   end Place_Cursor;

   ------------------
   -- Add_Provider --
   ------------------

   procedure Add_Provider
     (Name : Known_Provider; Provider : Editor_Formatting_Provider_Access) is
   begin
      Providers (Name) := Provider;
   end Add_Provider;

   ---------------------
   -- Delete_Provider --
   ---------------------

   procedure Delete_Provider (Name : Known_Provider) is
   begin
      Providers (Name) := null;
   end Delete_Provider;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Range_Formatting_Provider_Pref :=
        Formatter_Selector.Create
          (Manager => Kernel.Get_Preferences,
           Name    => "Editor-Range-Formatter",
           Default => Construct,
           Label   => "Formatter for range formatting",
           Doc     =>
             "Choose which formatter should be used when"
             & "formatting a range or a file.",
           Path    => "Editor:Formatting");
      On_Type_Formatting_Provider_Pref :=
        Formatter_Selector.Create
          (Manager => Kernel.Get_Preferences,
           Name    => "Editor-On-Type-Formatter",
           Default => Construct,
           Label   => "Formatter on enter",
           Doc     =>
             "Choose which formatter should be used when pressing enter.",
           Path    => "Editor:Formatting");
      Move_Cursor_When_Formatting :=
        Kernel.Get_Preferences.Create
          (Name    => "Editor-Move-Cursor-Formatter",
           Default => True,
           Label   => "Move cursor when formatting",
           Doc     =>
             "Should the cursor move to the end of the formatting edit.",
           Path    => "Editor:Formatting");
   end Register_Module;
end Src_Editor_Buffer.Formatters;
