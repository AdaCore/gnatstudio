------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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
with Ada.Text_IO;               use Ada.Text_IO;

procedure Syntax_Gnathighlight is
   subtype String_Access is GNAT.Strings.String_Access;

   Symbols     : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
   F           : File_Descriptor;
   Name        : constant String := Argument (1);
   Buffer      : String_Access;
   Length      : Integer;
   pragma Unreferenced (Length);

   function Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean;

   function Callback
     (Entity         : Language_Entity;
      Sloc_Start     : Source_Location;
      Sloc_End       : Source_Location;
      Partial_Entity : Boolean) return Boolean
   is
      pragma Unreferenced (Partial_Entity);
   begin
      Put (Language_Entity'Image (Entity) & ": ");
      Put ("(" & Natural'Image (Sloc_Start.Line) & ", "
           &  Natural'Image (Sloc_Start.Column) & ")");
      Put (" - (" & Natural'Image (Sloc_End.Line) & ", "
           &  Natural'Image (Sloc_End.Column) & ")");
      New_Line;

      return False;
   end Callback;

begin
   F      := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);

   if File_Extension (Name) = ".c"
     or else (Name'Length > 12
              and then Name (Name'Last - 11 .. Name'Last) = ".c.highlight")
   then
      Analyze_C_Source
        (Buffer.all, Symbols, Default_Indent_Parameters,
         Format   => False,
         Callback => Callback'Unrestricted_Access);

   else
      Analyze_Ada_Source
        (Buffer.all, Symbols, Default_Indent_Parameters,
         Format   => False,
         Callback => Callback'Unrestricted_Access);
   end if;
end Syntax_Gnathighlight;
