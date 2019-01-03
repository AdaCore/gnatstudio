------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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

with Ada.Calendar;  use Ada.Calendar;
with Ada.Strings.Unbounded;

with GNAT.Strings;  use GNAT.Strings;
with GNATCOLL.Mmap; use GNATCOLL.Mmap;
with GPS.Editors;   use GPS.Editors;

with Language;      use Language;
with Language.C;    use Language.C;
with Language.Cpp;  use Language.Cpp;

package body Clang_Buffer_Facilities is

   --  Implement a cache of buffers

   type Stored_Entry is record
      Timestamp   : Ada.Calendar.Time; --  The file time stamp
      File        : Virtual_File;

      --  We're using a String_Access here rather than an Unbouded_String.
      --  The reason for this is that this contains the full text from an
      --  editor, and is prone to be updated often, so we want to avoid
      --  making copies.
      Buffer      : GNAT.Strings.String_Access;

      --  The last offset, line, and column resolved
      Offset : Integer;
      Line   : Integer;
      Column : Visible_Column_Type;
   end record;

   Last_Entry : Stored_Entry;
   --  A global variable. ??? Store this in a module? in the kernel?

   procedure Offset_To_Line_Column
     (C      : GNAT.Strings.String_Access;
      Offset : Integer;
      Line   : out Integer;
      Column : out Visible_Column_Type);
   --  Same as Offset_To_Line_Column, working on the contents of the file

   -----------------------
   -- Get_Unsaved_Files --
   -----------------------

   function Get_Unsaved_Files (K : Core_Kernel) return Unsaved_File_Array is
      Buffers : constant Buffer_Lists.List := K.Get_Buffer_Factory.Buffers;

      Result : Unsaved_File_Array (1 .. Integer (Buffers.Length));
      Index  : Natural := 1;  --  First free item in Result
   begin
      for B of Buffers loop
         if B.Is_Modified then
            declare
               Lang        : constant Language_Access := B.Get_Language;
               Buffer_Text : Ada.Strings.Unbounded.String_Access;
            begin
               if (Lang = C_Lang or else Lang = Cpp_Lang)
                 and then B.File /= No_File
               then
                  Buffer_Text := new String'(B.Get_Chars);
                  Result (Index) := Create_Unsaved_File
                    (Filename => +B.File.Full_Name.all,
                     Buffer   => Buffer_Text);
                  Index := Index + 1;
               end if;
            end;
         end if;
      end loop;

      return Result (1 .. Index);
   end Get_Unsaved_Files;

   ---------------------------
   -- Offset_To_Line_Column --
   ---------------------------

   procedure Offset_To_Line_Column
     (C      : GNAT.Strings.String_Access;
      Offset : Integer;
      Line   : out Integer;
      Column : out Visible_Column_Type)
   is
      Index : Natural := C.all'First;
      Count : Natural := 0;
      Last_Was_A_Line_Break : Boolean := False;
      Last_Line_Begin : Integer := 0;
   begin
      Line := 1;
      Column := 0;
      --  First walk to find the line number...
      loop
         case C (Index) is
            when ASCII.CR =>
               Last_Was_A_Line_Break :=
                 not (Index < C'Last
                        and then C (Index + 1) = ASCII.LF);

            when ASCII.LF =>
               Last_Was_A_Line_Break := True;

            when others =>
               Last_Was_A_Line_Break := False;
         end case;

         Count := Count + 1;
         exit when Count > Offset;

         Index := Index + 1;
         exit when Index > C.all'Length;

         if Last_Was_A_Line_Break then
            Last_Line_Begin := Count;
            Line := Line + 1;
         end if;
      end loop;

      --  ??? Do we need to unravel tab expansion here?
      Column := Visible_Column_Type (Offset - Last_Line_Begin + 1);
   end Offset_To_Line_Column;

   ---------------------------
   -- Offset_To_Line_Column --
   ---------------------------

   procedure Offset_To_Line_Column
     (K      : Core_Kernel;
      File   : Virtual_File;
      Offset : Integer;
      Line   : out Integer;
      Column : out Visible_Column_Type)
   is
      B : constant Editor_Buffer'Class := K.Get_Buffer_Factory.Get
        (File        => File,
         Force       => False,
         Open_Buffer => False,
         Open_View   => False);
      Timestamp : Ada.Calendar.Time;
   begin
      Line := 0;
      Column := 0;

      if B = Nil_Editor_Buffer
        or else not B.Is_Modified
      then
         --  Either there is no editor open for this file, or there is an
         --  editor but it is currently saved on disk: this means that
         --  libclang computed the offset from the disk, so we should
         --  compute the line/column from disk as well here.

         if not File.Is_Regular_File then
            return;
         end if;

         --  It is very likely that GPS will attempt to resolve offsets for
         --  the same file multiple times in succession. To optimize this,
         --  we use a simple-minded cache where Last_Entry contains the
         --  entry being worked on.

         if Last_Entry.File /= File then
            --  Not in the cache
            Last_Entry.File := File;
            Free (Last_Entry.Buffer);
         else
            --  We might have this in the cache
            Timestamp := File.File_Time_Stamp;

            if Last_Entry.Timestamp = Timestamp then
               --  We have the right buffer in the cache.

               if Last_Entry.Offset = Offset then
                  --  Even better, we have just resolved this
                  Line := Last_Entry.Line;
                  Column := Last_Entry.Column;
                  return;
               end if;
            else
               --  We do not have the right buffer in the cache
               Free (Last_Entry.Buffer);
            end if;
         end if;

         if Last_Entry.Buffer = null then
            Last_Entry.Buffer := Read_Whole_File (+File.Full_Name.all);
            if Last_Entry.Buffer = null then
               return;
            end if;
         end if;

         Offset_To_Line_Column (C      => Last_Entry.Buffer,
                                Offset => Offset,
                                Line   => Line,
                                Column => Column);

         --  We have just resolved an offset: store this in our Last_Entry.
         --  Last_Entry.Buffer has been set above.
         Last_Entry.Offset := Offset;
         Last_Entry.Line := Line;
         Last_Entry.Column := Column;
      else
         --  There is a buffer open for this file, and modified: this means
         --  that the contents of this editor was passed to libclang
         --  through the contents of the buffer, so compute the offset from
         --  the buffer as well.
         declare
            Loc : constant Editor_Location'Class :=
              B.New_Location (Offset => Offset);
         begin
            Line := Loc.Line;
            Column := Loc.Column;
         end;
      end if;
   end Offset_To_Line_Column;

end Clang_Buffer_Facilities;
