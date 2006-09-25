-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2003-2006                       --
--                             AdaCore                               --
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

with Ada.Strings.Maps;   use Ada.Strings.Maps;

with Glib.Unicode;       use Glib.Unicode;
with Gtk.Text_Iter;      use Gtk.Text_Iter;

with Case_Handling;      use Case_Handling;
with Casing_Exceptions;  use Casing_Exceptions;
with Commands.Editor;    use Commands.Editor;
with Language;           use Language;
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
      Column : Character_Offset_Type);
   --  Return the iterator at Line, Col.
   --  Unfold Line if necessary.
   --  ??? What should be the behavior if Line/Column does not point to a
   --  valid location ?

   procedure Get_Location
     (Buffer       : access Source_Buffer_Record'Class;
      Line         : Editable_Line_Type;
      Col          : Character_Offset_Type;
      Before       : Integer;
      After        : Integer;
      Line_Begin   : out Editable_Line_Type;
      Column_Begin : out Character_Offset_Type;
      Line_End     : out Editable_Line_Type;
      Column_End   : out Character_Offset_Type);
   --  Get the positions around the given position.

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Buffer       : access Source_Buffer_Record'Class;
      Line         : Editable_Line_Type;
      Col          : Character_Offset_Type;
      Before       : Integer;
      After        : Integer;
      Line_Begin   : out Editable_Line_Type;
      Column_Begin : out Character_Offset_Type;
      Line_End     : out Editable_Line_Type;
      Column_End   : out Character_Offset_Type)
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
               exit when Line_Begin = 0;

               Unfold_Line (Buffer, Line_Begin);
            end if;

            Backward_Char (Iter, Result);
            exit when not Result;
         end loop;

         Column_Begin := Character_Offset_Type (Get_Line_Offset (Iter) + 1);

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

         Column_End := Character_Offset_Type (Get_Line_Offset (Iter) + 1);

         Forward_Char (Iter, Result);

         if Result and then Line_End < Buffer.Last_Editable_Line then
            Line_End   := Line_End + 1;
            Column_End := 1;
         end if;

      else
         for J in 1 .. After loop
            if Ends_Line (Iter) then
               Line_End := Line_End + 1;

               exit when not Is_Valid_Position (Buffer, Line_End);

               Unfold_Line (Buffer, Line_End);

               --  After unfolding the line, Iter might be invalid, therefore
               --  we re-generate it here.
               Get_Iter (Buffer, Iter, Line_End, 0);
            else
               Forward_Char (Iter, Result);
            end if;

            exit when not Result;
         end loop;

         Column_End := Character_Offset_Type (Get_Line_Offset (Iter) + 1);

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
      Column : Character_Offset_Type)
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
      Column_Begin : Character_Offset_Type;
      Line_End     : Editable_Line_Type;
      Column_End   : Character_Offset_Type) return UTF8_String
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
      Column_Begin : Character_Offset_Type;
      Line_End     : Editable_Line_Type;
      Column_End   : Character_Offset_Type)
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
      Column     : Character_Offset_Type := 0;
      Before     : Integer := -1;
      After      : Integer := -1) return UTF8_String
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Character_Offset_Type;
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
      Column     : Character_Offset_Type := 0;
      Before     : Integer := -1;
      After      : Integer := -1)
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Character_Offset_Type;
   begin
      if not Get_Writable (Buffer) then
         return;
      end if;

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
         Replace  : UTF8_String);
      --  Replace text callback. Note that we do not use Ln, F, L here as these
      --  are values from the parsed buffer which is a single word here. We use
      --  instead the Line, First and Last variable below which represent the
      --  real word position on the line.

      Lang          : Language_Access;
      Line          : Editable_Line_Type;
      Column        : Character_Offset_Type;
      First         : Character_Offset_Type;
      W_End         : Gtk_Text_Iter;
      W_Start       : Gtk_Text_Iter;
      Indent_Params : Indent_Parameters;
      Indent_Kind   : Indentation_Kind;
      Result        : Boolean;
      Char, Prev    : Character;

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (Ln, F, L : Natural;
         Replace  : UTF8_String)
      is
         pragma Unreferenced (Ln);
         Length : constant Natural := Natural (UTF8_Strlen (Replace));
      begin
         if Replace'Length > 0 and then L - F > 0 then
            --  The parser sometimes callback with a null replacement.
            --  Ignore those cases as we do not want to indent the code here.
            Replace_Slice
              (Buffer, Replace, Line, First,
               Before => 0, After => Length);

            --  Compute position of the next insert point. This can happen only
            --  in the case of an attribute. In String'Access for example the
            --  first call is for String and the second for Access.
            First := First + Character_Offset_Type (Length) + 1;
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

      Get_Iter_Position (Source_Buffer (Buffer), W_End, Line, Column);

      Copy (W_End, W_Start);

      --  Look for the start of the word, note that we do not want to stop at
      --  quote as it can be part of a specific construct in some languages and
      --  needs proper parsing. This is the case for example in Ada for
      --  attributes. So we either handle the word on the left or two words
      --  separated by a single quote.

      First := Column;
      Char  := ' ';

      loop
         Backward_Char (W_Start, Result);
         Prev := Char;
         Char := Get_Char (W_Start);

         if not Is_In (Char, Word_Character_Set (Lang))
           and then Prev = ' '
         then
            --  Nothing to do in this case as we do not have a word
            return;
         end if;

         if Char = ''' and Prev = ''' then
            --  We don't want to parse past the second quote
            Forward_Char (W_Start, Result);
            First := First + 1;
            exit;
         end if;

         exit when not Result
           or else (Char /= '''
                    and then not Is_In (Char, Word_Character_Set (Lang)));
         First := First - 1;
         exit when Is_Start (W_Start);
      end loop;

      --  Move one character forward, the first character in the word

      if not Is_Start (W_Start) then
         Forward_Char (W_Start, Result);
      end if;

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
