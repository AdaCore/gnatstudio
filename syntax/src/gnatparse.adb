------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with Ada.Text_IO;               use Ada.Text_IO;
with String_Utils;              use String_Utils;
with GNAT.Strings;
with Language;                  use Language;

procedure Gnatparse is
   subtype String_Access is GNAT.Strings.String_Access;

   Symbols     : constant Symbol_Table_Access := Allocate;
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

   if File_Extension (Name) = ".c" then
      Analyze_C_Source
        (Buffer.all, Symbols, Default_Indent_Parameters,
         Format     => False,
         Constructs => Constructs'Unchecked_Access);

   else
      Analyze_Ada_Source
        (Buffer.all, Symbols, Default_Indent_Parameters,
         Format     => False,
         Constructs => Constructs'Unchecked_Access);
   end if;

   Free (Buffer);
   Info := Constructs.First;

   loop
      exit when Info = null;

      declare
         Cat : String := Info.Category'Img;
      begin
         To_Lower (Cat (5 .. Cat'Last));
         Put (Cat (5 .. Cat'Last) & " ");
      end;

      if Info.Name /= No_Symbol then
         Put (Get (Info.Name).all & " ");
      end if;

      if Info.Profile /= null then
         Put (Reduce (Info.Profile.all) & " ");
      end if;

      if Info.Is_Generic_Spec then
         Put ("(gen) ");
      elsif Info.Is_Declaration then
         Put ("(spec) ");
      end if;

      Put ("First => " &
           Image (Info.Sloc_Start.Line) & ":" &
           Image (Info.Sloc_Start.Column) & ", ");
      Put ("Last => " &
           Image (Info.Sloc_End.Line) & ":" &
           Image (Info.Sloc_End.Column) & ", ");
      Put ("Visibility => " &
        Construct_Visibility'Image (Info.Visibility));
      New_Line;
      Info := Info.Next;
   end loop;

   Free (Constructs);
end Gnatparse;
