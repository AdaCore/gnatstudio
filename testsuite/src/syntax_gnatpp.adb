------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
with UTF8_Utils;

procedure Syntax_Gnatpp is
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

   declare
      UTF8_Text : String_Access;
      Ok        : Boolean;
   begin
      UTF8_Utils.Unknown_To_UTF8 (Buffer.all, UTF8_Text, Ok);

      if Ok and UTF8_Text /= null then
         Free (Buffer);
         Buffer := UTF8_Text;
      end if;
   end;

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
           (Indent_Level        => 3,
            Indent_Continue     => 2,
            Indent_Decl         => 2,
            Indent_Conditional  => 1,
            Indent_Record       => 3,
            Indent_Case_Extra   => Automatic,
            Casing_Policy       => End_Of_Line,
            Reserved_Casing     => Lower,
            Ident_Casing        => Smart_Mixed,
            Format_Operators    => True,
            Use_Tabs            => False,
            Align_On_Colons     => True,
            Align_On_Arrows     => True,
            Align_Decl_On_Colon => True,
            Indent_Comments     => True,
            Stick_Comments      => False),
         Replace => Replace_Cb'Unrestricted_Access);
   end if;

   Print (New_Buffer);
   Free (New_Buffer);
   Free (Buffer);
end Syntax_Gnatpp;
