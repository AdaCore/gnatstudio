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

with Ada_Analyzer;     use Ada_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.IO;          use GNAT.IO;
with GNAT.Strings;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with Language;         use Language;

procedure Syntax_Gnatunit is
   subtype String_Access is GNAT.Strings.String_Access;

   Symbols     : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
   F           : File_Descriptor;
   Name        : constant String := Argument (1);
   Buffer      : String_Access;
   Length      : Integer;
   pragma Unreferenced (Length);
   Info        : Construct_Access;
   Constructs  : aliased Construct_List;

begin
   F      := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);

   Analyze_Ada_Source
     (Buffer.all, Symbols, Default_Indent_Parameters,
      Format     => False,
      Constructs => Constructs'Unchecked_Access);
   Free (Buffer);

   Info := Constructs.Last;

   if Info = null
     or else
       (Info.Info.Category /= Cat_Procedure
        and then Info.Info.Category /= Cat_Function
        and then Info.Info.Category /= Cat_Package)
     or else Info.Info.Name = No_Symbol
   then
      Put_Line ("No unit found in file " & Name);
   else
      Put ("Unit " & Get (Info.Info.Name).all);

      if Info.Info.Is_Declaration then
         Put (" (spec)");
      else
         Put (" (body)");
      end if;

      Put_Line (", file name " & Name);
   end if;

   Free (Constructs);
end Syntax_Gnatunit;
