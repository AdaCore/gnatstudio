-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Ada_Analyzer;     use Ada_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.IO;          use GNAT.IO;
with Basic_Types;      use Basic_Types;
with Language;         use Language;

procedure Gnatunit is
   subtype String_Access is Basic_Types.String_Access;

   F           : File_Descriptor;
   Name        : constant String := Argument (1);
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
     (Buffer.all, New_Buffer, Default_Indent_Parameters,
      Reserved_Casing  => Unchanged,
      Ident_Casing     => Unchanged,
      Format_Operators => False,
      Indent           => False,
      Constructs       => Constructs'Unchecked_Access,
      Current_Indent   => Next_Indent,
      Prev_Indent      => Indent);

   Free (Buffer);

   Info := Constructs.Last;

   if Info = null
     or else
       (Info.Category /= Cat_Procedure
        and then Info.Category /= Cat_Function
        and then Info.Category /= Cat_Package)
     or else Info.Name = null
   then
      Put_Line ("No unit found in file " & Name);
   else
      Put ("Unit " & Info.Name.all);

      if Info.Is_Declaration then
         Put (" (spec)");
      else
         Put (" (body)");
      end if;

      Put_Line (", file name " & Name);
   end if;

   Free (Constructs);
end Gnatunit;
