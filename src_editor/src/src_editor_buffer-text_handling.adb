-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2003 - 2004                     --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Commands.Editor;   use Commands.Editor;

with Language;          use Language;
with Case_Handling;     use Case_Handling;
with Casing_Exceptions; use Casing_Exceptions;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;


package body Src_Editor_Buffer.Text_Handling is

   --  ??? Must implement functions that do not require unfolding of lines.
   --  ??? Must deal with the case of someone doing Get_Chars/Replace_Slice
   --  over blank lines.

   procedure Get_Iter
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : out Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Natural);
   --  Return the iterator at Line, Col.
   --  Unfold Line if necessary.

   procedure Get_Location
     (Buffer       : access Source_Buffer_Record'Class;
      Line         : Editable_Line_Type;
      Col          : Natural;
      Before       : Integer;
      After        : Integer;
      Line_Begin   : out Editable_Line_Type;
      Column_Begin : out Natural;
      Line_End     : out Editable_Line_Type;
      Column_End   : out Natural);
   --  Get the positions around the given position.

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Buffer       : access Source_Buffer_Record'Class;
      Line         : Editable_Line_Type;
      Col          : Natural;
      Before       : Integer;
      After        : Integer;
      Line_Begin   : out Editable_Line_Type;
      Column_Begin : out Natural;
      Line_End     : out Editable_Line_Type;
      Column_End   : out Natural)
   is
      Iter   : Gtk_Text_Iter;
      Result : Boolean := True;
   begin
      Get_Iter (Buffer, Iter, Line, Col);
      Line_Begin := Line;

      if Before = -1 then
         Column_Begin := 1;

      else
         for J in 1 .. Before loop
            if Get_Line_Offset (Iter) = 0 then
               Line_Begin := Line_Begin - 1;
               Unfold_Line (Buffer, Line_Begin);
            end if;

            Backward_Char (Iter, Result);
            exit when not Result;
         end loop;

         Column_Begin := Natural (Get_Line_Offset (Iter)) + 1;

         if Line_Begin = 0 then
            Line_Begin := 1;
         end if;
      end if;

      Get_Iter (Buffer, Iter, Line, Col);
      Line_End := Line;

      if After = -1 then
         if not Ends_Line (Iter) then
            Forward_To_Line_End (Iter, Result);
         end if;

         Column_End := Natural (Get_Line_Offset (Iter)) + 1;

         Forward_Char (Iter, Result);

         if Result and then Line_End < Buffer.Last_Editable_Line then
            Line_End   := Line_End + 1;
            Column_End := 1;
         end if;

      else
         for J in 1 .. After loop
            if Ends_Line (Iter) then
               Line_End := Line_End + 1;
               Unfold_Line (Buffer, Line_End);

               --  After unfolding the line, Iter might be invalid, therefore
               --  we re-generate it here.
               Get_Iter (Buffer, Iter, Line_End, 0);
            else
               Forward_Char (Iter, Result);
            end if;

            exit when not Result;
         end loop;

         Column_End := Natural (Get_Line_Offset (Iter)) + 1;

         if Line_End > Buffer.Last_Editable_Line then
            Line_End := Buffer.Last_Editable_Line;
         end if;
      end if;
   end Get_Location;

   --------------
   -- Get_Iter --
   --------------

   procedure Get_Iter
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : out Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Natural)
   is
      Buffer_Line : Buffer_Line_Type;
      Col         : Gint := 0;
   begin
      Unfold_Line (Buffer, Line);

      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Column /= 0 then
         Col := Gint (Column - 1);
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Gint (Buffer_Line - 1), Col);
   end Get_Iter;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Buffer       : access Source_Buffer_Record'Class;
      Line_Begin   : Editable_Line_Type;
      Column_Begin : Natural;
      Line_End     : Editable_Line_Type;
      Column_End   : Natural) return UTF8_String
   is
      Iter_Begin, Iter_End : Gtk_Text_Iter;
   begin
      for J in Line_Begin + 1 .. Line_End - 1 loop
         Unfold_Line (Buffer, J);
      end loop;

      Get_Iter (Buffer, Iter_Begin, Line_Begin, Column_Begin);
      Get_Iter (Buffer, Iter_End, Line_End, Column_End);

      return Get_Text (Buffer, Iter_Begin, Iter_End, False);
   end Get_Chars;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record'Class;
      Text         : String;
      Line_Begin   : Editable_Line_Type;
      Column_Begin : Natural;
      Line_End     : Editable_Line_Type;
      Column_End   : Natural)
   is
      C : Editor_Replace_Slice;
   begin
      Create
        (C, Source_Buffer (Buffer),
         Line_Begin, Column_Begin, Line_End, Column_End, Text);

      External_End_Action (Buffer);
      Enqueue (Buffer, Command_Access (C));
   end Replace_Slice;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Buffer     : access Source_Buffer_Record'Class;
      Line       : Editable_Line_Type := 0;
      Column     : Natural := 0;
      Before     : Integer := -1;
      After      : Integer := -1) return UTF8_String
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Natural;
      Start_Iter               : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;
      Has_Selection            : Boolean;
   begin
      if Line = 0 then
         Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Has_Selection);

         if Has_Selection then
            return Get_Text (Buffer, Start_Iter, End_Iter, False);
         else
            return "";
         end if;

      else
         Get_Location
           (Buffer, Line, Column, Before, After,
            Line_Begin, Column_Begin, Line_End, Column_End);

         return
           Get_Chars (Buffer, Line_Begin, Column_Begin, Line_End, Column_End);
      end if;
   end Get_Chars;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Text       : String;
      Line       : Editable_Line_Type;
      Column     : Natural := 0;
      Before     : Integer := -1;
      After      : Integer := -1)
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Natural;
   begin
      Get_Location
        (Buffer, Line, Column, Before, After,
         Line_Begin, Column_Begin, Line_End, Column_End);

      Replace_Slice
        (Buffer, Text, Line_Begin, Column_Begin, Line_End, Column_End);
   end Replace_Slice;

   ------------------------
   -- Autocase_Last_Word --
   ------------------------

   procedure Autocase_Last_Word
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Replace_Text
        (Ln, F, L : Natural;
         Replace  : String);
      --  Replace text callback. Note that we do not use Ln, F, L here as
      --  these are values from the parsed buffer which is a single word here.
      --  We use insted the Line, First and Last variable below which represent
      --  the real word position on the line.

      Lang          : Language_Access;
      Line          : Editable_Line_Type;
      Column        : Positive;
      First         : Natural;
      W_End         : Gtk_Text_Iter;
      W_Start       : Gtk_Text_Iter;
      Indent_Params : Indent_Parameters;
      Indent_Kind   : Indentation_Kind;

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (Ln, F, L : Natural;
         Replace  : String)
      is
         pragma Unreferenced (Ln, F, L);
      begin
         if Replace'Length > 0
           and then Count (Replace, To_Set (" " & ASCII.HT)) /= Replace'Length
         then
            --  ??? The parser sometimes callback with an empty or null
            --  replacement. Ignore those cases. This should probably be fixed
            --  in the parser.
            Replace_Slice
              (Buffer, Replace, Line, First,
               Before => 0, After => Replace'Length);
         end if;
      end Replace_Text;

   begin
      Lang := Get_Language (Buffer);

      if Lang = null then
         --  No language information
         return;

      else
         Get_Cursor_Position (Buffer, W_End);
         Get_Indentation_Parameters (Lang, Indent_Params, Indent_Kind);

         if Indent_Params.Casing_Policy /= On_The_Fly
           or else Get_Language_Context (Lang).Case_Sensitive
           or else Is_In_Comment (Source_Buffer (Buffer), W_End)
           or else Is_In_String (Source_Buffer (Buffer), W_End)
         then
            --  On-the-fly casing not activated, the language is case sensitive
            --  or we are in a comment or a string.
            return;
         end if;
      end if;

      Get_Cursor_Position (Source_Buffer (Buffer), W_End, Line, Column);

      Copy (W_End, W_Start);

      --  Look for the start of the word

      declare
         Result : Boolean;
      begin
         First := Column;
         loop
            Backward_Char (W_Start, Result);
            exit when not Result
              or else
                not Is_In (Get_Char (W_Start), Word_Character_Set (Lang));
            First := First - 1;
         end loop;
      end;

      if First /= Column then
         --  We have a word, set casing
         Format_Buffer
           (Lang,
            Get_Slice (W_Start, W_End),
            Replace         => Replace_Text'Unrestricted_Access,
            Indent_Params   => Indent_Params,
            Case_Exceptions => Get_Case_Exceptions);
      end if;
   end Autocase_Last_Word;

end Src_Editor_Buffer.Text_Handling;
