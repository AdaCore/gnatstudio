------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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
with Libclang.Index; use Libclang.Index;
with Ada.Text_IO; use Ada.Text_IO;

----------------
-- Test_Index --
----------------

procedure Test_Index is

   Index : Clang_Index;
   Source_Filename : constant String := Argument (1);
   CL : Unbounded_String_Array (4 .. Argument_Count);

   Line, Column : Natural;
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

      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Success := Reparse_Translation_Unit (TU);

      --  Get completion

      declare
         Completion : Clang_Complete_Results :=
           Complete_At (TU,
                        Filename => Source_Filename,
                        Line     => Line,
                        Column   => Column);

         Strings : Completion_Strings;
      begin
         for J in 1 .. Num_Results (Completion) loop
            Strings := Spelling (Nth_Result (Completion, J));

            Put_Line (To_String (Strings.Completion)
                      & ASCII.HT & To_String (Strings.Doc));
         end loop;

         Dispose (Completion);
      end;

      Dispose (TU);
   end;

   Dispose (Index);
end Test_Index;
