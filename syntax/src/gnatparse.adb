-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada_Analyzer;     use Ada_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Ada.Text_IO;  use Ada.Text_IO;
with String_Utils; use String_Utils;
with Basic_Types;      use Basic_Types;
with Language;         use Language;

procedure Gnatparse is
   subtype String_Access is Basic_Types.String_Access;

   F           : File_Descriptor;
   Name        : constant String := Argument (1) & ASCII.NUL;
   Buffer      : String_Access;
   Length      : Integer;
   Info        : Construct_Access;
   Indent      : Natural;
   Next_Indent : Natural;
   New_Buffer  : Extended_Line_Buffer;
   Constructs  : aliased Construct_List;

begin
   F      := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);

   Analyze_Ada_Source
     (To_Unchecked_String (Buffer.all'Address), Length,
      New_Buffer, Default_Indent_Parameters,
      Reserved_Casing  => Unchanged,
      Ident_Casing     => Unchanged,
      Format_Operators => False,
      Indent           => False,
      Constructs       => Constructs'Unchecked_Access,
      Current_Indent   => Next_Indent,
      Prev_Indent      => Indent);

   Free (Buffer);
   Put_Line ("Indent = " & Image (Indent));
   Put_Line ("Next Indent = " & Image (Next_Indent));
   Info := Constructs.First;

   loop
      exit when Info = null;

      declare
         Cat : String := Info.Category'Img;
      begin
         Lower_Case (Cat (5 .. Cat'Last));
         Put (Cat (5 .. Cat'Last) & " ");
      end;

      if Info.Name /= null then
         Put (Info.Name.all & " ");
      end if;

      if Info.Profile /= null then
         Put (Reduce (Info.Profile.all) & " ");
      end if;

      if Info.Is_Declaration then
         Put ("(spec) ");
      end if;

      Put ("First => " &
           Image (Info.Sloc_Start.Line) & ":" &
           Image (Info.Sloc_Start.Column) & ", ");
      Put ("Last => " &
           Image (Info.Sloc_End.Line) & ":" &
           Image (Info.Sloc_End.Column));
      New_Line;
      Info := Info.Next;
   end loop;

   Free (Constructs);
end Gnatparse;
