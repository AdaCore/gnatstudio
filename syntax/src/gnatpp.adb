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
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Basic_Types;      use Basic_Types;
with Language;         use Language;

procedure Gnatpp is
   subtype String_Access is Basic_Types.String_Access;

   F          : File_Descriptor;
   Name       : constant String := Argument (1) & ASCII.NUL;
   Buffer     : String_Access;
   Length     : Integer;
   New_Buffer : Extended_Line_Buffer;
   Ignore     : Natural;

begin
   F := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);
   New_Buffer := To_Line_Buffer (Buffer.all);
   Analyze_Ada_Source
     (To_Unchecked_String (Buffer.all'Address), Buffer'Length,
      New_Buffer,
      Indent_Params  => Default_Indent_Parameters,
      Current_Indent => Ignore,
      Prev_Indent    => Ignore);
   Print (New_Buffer);
   Free (New_Buffer);
   Free (Buffer);
end Gnatpp;
