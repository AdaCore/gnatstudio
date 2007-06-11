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

with Syntax_Diff;      use Syntax_Diff;
with Ada_Analyzer;     use Ada_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Ada.Text_IO;      use Ada.Text_IO;
with OS_Utils;         use OS_Utils;
with Language;         use Language;

--  ??? TODO:
--  - add switch to choose which category to take into account
--  - have a better match algorithm for Construct_Category (e.g. based on
--    enclosed constructs, and/or line numbers, and/or semicolon count).
--  - associate a Diff_Kind with each Construct_Access
--  - add filtering of added entities inside already added constructs
--  - improve detection of moved constructs

procedure Gnatdiff is

   subtype String_Access is GNAT.OS_Lib.String_Access;

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
      Buffer := Read_File (File);
      Analyze_Ada_Source
        (Buffer.all, Default_Indent_Parameters,
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
end Gnatdiff;
