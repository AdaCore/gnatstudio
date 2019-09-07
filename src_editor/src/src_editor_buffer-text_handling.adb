------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Strings.Maps;   use Ada.Strings.Maps;

with Glib.Unicode;       use Glib.Unicode;
with Gtk.Text_Iter;      use Gtk.Text_Iter;

with Casing_Exceptions;  use Casing_Exceptions;
with Commands.Editor;    use Commands.Editor;
with Language;           use Language;

with GPS.Kernel; use GPS.Kernel;
with Src_Editor_Buffer.Cursors; use Src_Editor_Buffer.Cursors;
with GNAT.Regpat; use GNAT.Regpat;
with Language.Ada;

package body Src_Editor_Buffer.Text_Handling is

   --  ??? Must deal with the case of someone doing Get_Chars/Replace_Slice
   --  over blank lines.

   procedure Get_Iter
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : out Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type);
   --  Return the iterator at Line, Col.

   procedure Get_Location
     (Buffer       : access Source_Buffer_Record'Class;
      Line         : Editable_Line_Type;
      Col          : Character_Offset_Type;
      Before       : Integer;
      After        : Integer;
      Line_Begin   : out Editable_Line_Type;
      Column_Begin : out Character_Offset_Type;
      Line_End     : out Editable_Line_Type;
      Column_End   : out Character_Offset_Type;
      Valid        : out Boolean);
   --  Get the positions around the given position

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
      Column_End   : out Character_Offset_Type;
      Valid        : out Boolean)
   is
      Iter   : Gtk_Text_Iter;
      Result : Boolean := True;
   begin
      if not Is_Valid_Position (Buffer, Line, Col) then
         Valid        := False;
         Line_Begin   := Editable_Line_Type'First;
         Column_Begin := Character_Offset_Type'First;
         Line_End     := Editable_Line_Type'First;
         Column_End   := Character_Offset_Type'First;

         return;
      end if;

      Valid := True;
      Get_Iter (Buffer, Iter, Line, Col);
      Line_Begin := Line;

      if Before = -1 then
         Column_Begin := 1;
      else
         for J in 1 .. Before loop
            if Get_Line_Offset (Iter) = 0 then
               Line_Begin := Line_Begin - 1;
               exit when Line_Begin = 0;

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
      Buffer_Line := Get_Buffer_Line (Buffer, Line);

      if Column /= 0 then
         Col := Gint (Column - 1);
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Gint (Buffer_Line - 1), Col);
   end Get_Iter;

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
      Enqueue (Buffer, Command_Access (C), External);
   end Replace_Slice;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Buffer               : access Source_Buffer_Record'Class;
      Line                 : Editable_Line_Type := 0;
      Column               : Character_Offset_Type := 0;
      Before               : Integer := -1;
      After                : Integer := -1;
      Include_Hidden_Chars : Boolean := True) return Basic_Types.UTF8_String
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Character_Offset_Type;
      Start_Iter               : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;
      Has_Selection            : Boolean;
      Valid                    : Boolean;
   begin
      if Line = 0 then
         Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Has_Selection);

         if Has_Selection then
            return To_String
              (Get_Text
                 (Source_Buffer (Buffer),
                  Get_Line (Start_Iter),
                  Get_Line_Offset (Start_Iter),
                  Get_Line (End_Iter),
                  Get_Line_Offset (End_Iter)));
         else
            return "";
         end if;

      else
         Get_Location
           (Buffer, Line, Column, Before, After,
            Line_Begin, Column_Begin, Line_End, Column_End, Valid);

         if not Valid then
            Insert
              (Buffer.Get_Kernel,
               Text   => "Invalid location, cannot get chars",
               Mode   => Error);
            return "";
         end if;

         return To_String
           (Get_Text
              (Buffer,
               Line_Begin,
               Column_Begin,
               Line_End,
               Column_End,
               Include_Hidden_Chars));
      end if;
   end Get_Chars;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Buffer : access Source_Buffer_Record'Class;
      Text   : String;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type := 0;
      Before : Integer := -1;
      After  : Integer := -1)
   is
      Line_Begin, Line_End     : Editable_Line_Type;
      Column_Begin, Column_End : Character_Offset_Type;
      Valid                    : Boolean;
   begin
      if not Get_Writable (Buffer) then
         return;
      end if;

      Get_Location
        (Buffer, Line, Column, Before, After,
         Line_Begin, Column_Begin, Line_End, Column_End,
         Valid);

      if Valid then
         Replace_Slice
           (Buffer, Text, Line_Begin, Column_Begin, Line_End, Column_End);
      else
         Insert
           (Buffer.Get_Kernel,
            Text   => "Invalid location, cannot replace",
            Mode   => Error);
      end if;
   end Replace_Slice;

   -------------------
   -- Autocase_Text --
   -------------------

   Match_Ada_Comments : constant Pattern_Matcher :=
     Compile ("^([^""]|"".*?""|'.')*--.*$");

   procedure Autocase_Text
     (Buffer : access Source_Buffer_Record'Class;
      Casing : Casing_Policy)
   is
      function Is_In_Comment (Iter : Gtk_Text_Iter) return Boolean;

      function Is_In_Comment (Iter : Gtk_Text_Iter) return Boolean
      is
         Pos, Line_Start : Gtk_Text_Iter;
         Success : Boolean;
      begin
         Copy (Iter, Pos);

         --  ??? Special case for Ada language. This is a dirty fix, and is
         --  there only because we don't want to rehighlight synchronously for
         --  performance reasons (current Ada highlighter needs to highlight
         --  everything from beginning of file to current point)

         if Buffer.Get_Language = Language.Ada.Ada_Lang then
            Forward_Char (Pos, Success);
            Copy (Pos, Line_Start);
            Set_Line_Offset (Line_Start, 0);
            declare
               Ret : Boolean;
               Text : constant String :=
                 To_String
                   (Get_Text
                      (Source_Buffer (Buffer),
                       Get_Line (Line_Start),
                       Get_Line_Offset (Line_Start),
                       Get_Line (Pos),
                       Get_Line_Offset (Pos),
                       Include_Hidden_Chars => False));
            begin
               Ret := Match (Match_Ada_Comments, Text);
               return Ret;
            end;
         else
            return Is_In_Comment (Source_Buffer (Buffer), Iter);
         end if;
      end Is_In_Comment;

      procedure Replace_Text
        (Ln, F, L : Natural;
         Replace  : Basic_Types.UTF8_String);
      --  Replace text callback. Note that we do not use Ln, F, L here as these
      --  are values from the parsed buffer which is a single word here. We use
      --  instead the Line, First and Last variable below which represent the
      --  real word position on the line.

      Lang              : Language_Access;
      Line              : Editable_Line_Type;
      Column            : Character_Offset_Type;
      First             : Character_Offset_Type;
      W_Start, W_End    : Gtk_Text_Iter;
      Indent_Params     : Indent_Parameters;
      Indent_Kind       : Indentation_Kind;
      Result            : Boolean;
      Char, Prev, PPrev : Character;
      Forward_Moves     : Natural := 0;
      Text_Replaced     : Boolean := False;
      --  Record the number of forward moves done to replace the cursor at the
      --  right place in On_The_Fly mode while inserting a character inside a
      --  word. This is needed as the mark of the cursor will be replaced.

      Char_Set          : Character_Set;

      ------------------
      -- Replace_Text --
      ------------------

      procedure Replace_Text
        (Ln, F, L : Natural;
         Replace  : Basic_Types.UTF8_String)
      is
         pragma Unreferenced (Ln);
         Length : constant Natural := Natural (UTF8_Strlen (Replace));
      begin
         if Replace'Length > 0 and then L - F > 0 then
            --  The parser sometimes callback with a null replacement.
            --  Ignore those cases as we do not want to indent the code here.

            Text_Replaced := False;

            --  Replace only if the relacement is actually different

            if Get_Text
              (Buffer, Line, First,
               Line, First + Character_Offset_Type (Length)) /= Replace
            then
               declare
                  G : Group_Block := Current_Group (Buffer.Queue);
               begin
                  Replace_Slice
                    (Buffer, Replace, Line, First,
                     Before => 0, After => Length);
                  Text_Replaced := True;
               end;
            end if;

            --  Compute position of the next insert point. This can happen only
            --  in the case of an attribute. In String'Access for example the
            --  first call is for String and the second for Access.
            First := First + Character_Offset_Type (Length) + 1;
         end if;
      end Replace_Text;

   begin
      Lang := Get_Language (Buffer);

      if Lang = null
        --  No language information
        or else Has_Slave_Cursors (+Buffer)
        --  Multi cursors active
      then
         return;
      else

         Get_Cursor_Position (Buffer, W_End);

         Result := False;

         Char := Get_Char (W_End);

         if Char = ASCII.LF and then not Is_Start (W_End) then
            Backward_Char (W_End, Result);
         end if;

         Get_Indentation_Parameters (Lang, Indent_Params, Indent_Kind);

         if Indent_Params.Casing_Policy not in End_Of_Word .. On_The_Fly
           or else (Indent_Params.Reserved_Casing = Unchanged
                      and then Indent_Params.Ident_Casing = Unchanged)
           or else (In_Completion (Source_Buffer (Buffer))
                     and then Casing = On_The_Fly)
           or else (Indent_Params.Casing_Policy = End_Of_Word
                    and then Casing = On_The_Fly)
           or else Get_Language_Context (Lang).Case_Sensitive
           or else Is_In_Comment (W_End)
           or else Is_In_String (Source_Buffer (Buffer), W_End)
         then
            --  On-the-fly casing not activated, the language is case sensitive
            --  or we are in a comment or a string. We also disable on-the-fly
            --  casing when end-of-word selected. Note that when on-thy-fly is
            --  selected we still case after end-of-word. This is because when
            --  typing fast some characters won't get cased properly.
            return;
         end if;

         if Result then
            Forward_Char (W_End, Result);
         end if;

         if Casing = On_The_Fly then
            --  We can have added a character inside a word, let's look at the
            --  word ending.
            Look_For_End_Of_Word : loop
               Char := Get_Char (W_End);
               if not Is_In (Char, Word_Character_Set (Lang)) then
                  exit Look_For_End_Of_Word;
               end if;

               Forward_Char (W_End, Result);
               exit Look_For_End_Of_Word
                 when not Result or else Is_End (W_End);
               Forward_Moves := Forward_Moves + 1;
            end loop Look_For_End_Of_Word;
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
      Prev  := ' ';
      PPrev := ' ';

      Char_Set := Word_Character_Set (Lang) or To_Set ('#');
      --  Include any # sign to get based literals. This is needed as we want
      --  the Ada parser to dectect such case and disable any casing on this
      --  literal.

      Look_For_Start_Of_Word : loop
         Backward_Char (W_Start, Result);
         PPrev := Prev;
         Prev := Char;
         Char := Get_Char (W_Start);

         if not Is_In (Char, Char_Set) and then Prev = ' ' then
            --  Nothing to do in this case as we do not have a word
            return;
         end if;

         if Char = ''' and then PPrev = ''' then
            --  This is a character, we do not want to parse it
            Forward_Char (W_Start, Result);
            Forward_Char (W_Start, Result);
            First := First + 2;
            exit Look_For_Start_Of_Word;
         end if;

         exit Look_For_Start_Of_Word when not Result
           or else (Char /= ''' and then not Is_In (Char, Char_Set));
         First := First - 1;
         exit Look_For_Start_Of_Word when Is_Start (W_Start);
      end loop Look_For_Start_Of_Word;

      --  Move one character forward, the first character in the word

      if not Is_Start (W_Start) then
         Forward_Char (W_Start, Result);
      end if;

      if First /= Column then
         --  We have a word, set casing
         declare
            W : constant Basic_Types.UTF8_String := Get_Slice (W_Start, W_End);
            B : constant Basic_Types.UTF8_String :=
                  Get_Typed_Chars (Buffer, Integer (Column - First));
         begin
            if UTF8_Strdown (W) = UTF8_Strdown (B) then
               --  If typed chars and current buffer differ only on the casing
               --  use the recorded typed chars to have a conservative casing.
               Format_Buffer
                 (Lang,
                  B,
                  Replace         => Replace_Text'Unrestricted_Access,
                  Indent_Params   => Indent_Params,
                  Case_Exceptions => Get_Case_Exceptions);
            else
               Format_Buffer
                 (Lang,
                  W,
                  Replace         => Replace_Text'Unrestricted_Access,
                  Indent_Params   => Indent_Params,
                  Case_Exceptions => Get_Case_Exceptions);
            end if;
         end;

         if Casing = On_The_Fly and then Text_Replaced then
            Get_Cursor_Position (Buffer, W_End);
            if Forward_Moves /= 0 then
               Backward_Chars (W_End, Gint (Forward_Moves), Result);
               Place_Cursor (Buffer, W_End);
            end if;
         end if;
      end if;
   end Autocase_Text;

end Src_Editor_Buffer.Text_Handling;
