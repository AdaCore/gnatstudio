-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Basic_Types;      use Basic_Types;
with Language;         use Language;
with Line_Buffers;     use Line_Buffers;

procedure Gnatpp is
   subtype String_Access is Basic_Types.String_Access;

   F          : File_Descriptor;
   Name       : constant String := Argument (1) & ASCII.NUL;
   Buffer     : String_Access;
   Length     : Integer;
   pragma Unreferenced (Length);
   New_Buffer : Extended_Line_Buffer;

   procedure Replace_Cb
     (Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String);
   --  Callback for Analyze_Ada_Source.

   ----------------
   -- Replace_Cb --
   ----------------

   procedure Replace_Cb
     (Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String) is
   begin
      Replace_Text (New_Buffer, Line, First, Last, Replace);
   end Replace_Cb;

begin
   F := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);
   New_Buffer := To_Line_Buffer (Buffer.all);
   Analyze_Ada_Source
     (Buffer.all,
      Indent_Params => (3, 2, 2, 8, True, Lower, Mixed, True, False),
      Replace => Replace_Cb'Unrestricted_Access);
   Print (New_Buffer);
   Free (New_Buffer);
   Free (Buffer);
end Gnatpp;
