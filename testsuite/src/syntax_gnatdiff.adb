------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Syntax_Diff;      use Syntax_Diff;
with Ada_Analyzer;     use Ada_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNATCOLL.Mmap;    use GNATCOLL.Mmap;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Ada.Text_IO;      use Ada.Text_IO;
with Language;         use Language;

--  ??? TODO:
--  - add switch to choose which category to take into account
--  - have a better match algorithm for Construct_Category (e.g. based on
--    enclosed constructs, and/or line numbers, and/or semicolon count).
--  - associate a Diff_Kind with each Construct_Access
--  - add filtering of added entities inside already added constructs
--  - improve detection of moved constructs

procedure Syntax_Gnatdiff is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   Symbols     : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
   Constructs  : Construct_List;
   Constructs2 : Construct_List;
   Results     : Result_Link;

   procedure Analyze_File (File : String; Constructs : out Construct_List);
   --  Analyze File and fill Construct in return.

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (File : String; Constructs : out Construct_List) is
      Buffer     : String_Access;
      Result     : aliased Construct_List;

   begin
      Buffer := Read_Whole_File (File);
      Analyze_Ada_Source
        (Buffer.all, Symbols, Default_Indent_Parameters,
         Format           => False,
         Constructs       => Result'Unchecked_Access);
      Constructs := Result;
      Free (Buffer);
   end Analyze_File;

begin
   if Argument_Count /= 2 then
      Put_Line ("Usage: gnatdiff file1 file2");
      return;
   end if;

   Analyze_File (Argument (1), Constructs);
   Analyze_File (Argument (2), Constructs2);

   Results := Syntax_Diff.Syntax_Diff (Constructs, Constructs2);
   Print_Results (Results);
   Free (Constructs);
   Free (Constructs2);
end Syntax_Gnatdiff;
