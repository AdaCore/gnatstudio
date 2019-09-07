------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2014-2019, AdaCore                   --
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

--  Mini example program
--  call with:
--    test_index <source> line column [argument1 .. argumentN]

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Utils; use GNATCOLL.Utils;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Libclang.Index; use Libclang.Index;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Strings;
with System; use System;
with System.Address_Image;

----------------
-- Test_Index --
----------------

procedure Test_Index is
begin
   if Argument_Count < 3 then
      Put_Line ("Usage : test_index <source_file> <line> <column>"
                & " <compiler_args*>");
      return;
   end if;

   declare
      Index : Clang_Index;
      Source_Filename : constant String := Argument (1);
      CL : Unbounded_String_Array (4 .. Argument_Count);
      Line, Column : Natural;
      Source_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Source_Filename);
      Content : constant GNAT.Strings.String_Access := Source_File.Read_File;

   begin

      Index := Create_Index (Exclude_Declarations_From_PCH => False,
                             Display_Diagnostics           => False);

      --  Parse args

      Line := Integer'Value (Argument (2));
      Column := Integer'Value (Argument (3));

      for J in 4 .. Argument_Count loop
         CL (J) := To_Unbounded_String (Argument (J));
      end loop;

      --  Create TU

      declare
         TU : Clang_Translation_Unit :=
           Parse_Translation_Unit
             (Index,
              Source_Filename   => Source_Filename,
              Command_Line_Args => CL);

         Unsaved_Files : constant Unsaved_File_Array :=
           (1 => Create_Unsaved_File
              (Source_Filename,
               Ada.Strings.Unbounded.String_Access (Content)));

         Success : Boolean;
         pragma Unreferenced (Success);
      begin
         Success := Reparse_Translation_Unit (TU, Unsaved_Files);
         Put_Line (Content.all);

         --  Get completion

         declare
            Completion : Clang_Complete_Results :=
              Complete_At (TU,
                           Filename => Source_Filename,
                           Line     => Line,
                           Column   => Column);

            Strings : Completion_Strings;
            pragma Unreferenced (Strings);
         begin
            --  This code is an example of libclang completions
--              for J in 1 .. Num_Results (Completion) loop
--                 Strings := Spelling (Nth_Result (Completion, J));
--              end loop;

            Dispose (Completion);
         end;

         --  Get_Cursor

         declare
            Ret : Clang_Cursor;
         begin
            Ret := Cursor_At (TU, Source_File, Line, Column);

            Put_Line ("Cursor: " & ASCII.LF
                      & "   Kind: " & Ret.kind'Img
                      & "   Data: " & ASCII.LF
                      & "       " & System.Address_Image (Ret.data (0))
                      & "       " & System.Address_Image (Ret.data (1))
                      & "       " & System.Address_Image (Ret.data (2)));

         end;

         Dispose (TU);
      end;

      Dispose (Index);
   end;
end Test_Index;
