------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada_Analyzer;              use Ada_Analyzer;
with C_Analyzer;                use C_Analyzer;
with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Strings;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with Language;                  use Language;
with Line_Buffers;              use Line_Buffers;
with Case_Handling;             use Case_Handling;

procedure Gnatpp is
   subtype String_Access is GNAT.Strings.String_Access;

   Symbols    : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
   F          : File_Descriptor;
   Name       : constant String := Argument (1);
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

   if File_Extension (Name) = ".c" then
      Analyze_C_Source
        (Buffer.all, Symbols,
         Indent_Params =>
           (2, 2, 2, 0, 2, Automatic, End_Of_Line, Unchanged, Unchanged,
            False, True, True, True, False, True, False),
         Replace => Replace_Cb'Unrestricted_Access);

   else
      Analyze_Ada_Source
        (Buffer.all, Symbols,
         Indent_Params =>
           (3, 2, 2, 1, 3, Automatic, End_Of_Line,
            Lower, Smart_Mixed, True, False, True, True,
            True, True, False),
         Replace => Replace_Cb'Unrestricted_Access);
   end if;

   Print (New_Buffer);
   Free (New_Buffer);
   Free (Buffer);
end Gnatpp;
